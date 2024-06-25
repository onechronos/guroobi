open Guroobi
open Raw
open Utils
open U

(* Facility location: a company currently ships its product from 5 plants
   to 4 warehouses. It is considering closing some plants to reduce
   costs. What plant(s) should the company close, in order to minimize
   transportation and fixed costs?
 *)

(* Number of plants and warehouses *)
let n_plants = 5
let n_warehouses = 4

(* Warehouse demand in thousands of units *)
let demand = [| 15.; 18.; 14.; 20. |]

(* Plant capacity in thousands of units *)
let capacity = [| 20.; 22.; 17.; 19.; 18. |]

(* Fixed costs for each plant *)
let fixed_costs = [| 12000.; 15000.; 17000.; 13000.; 16000. |]

(* Transportation costs per thousand units *)
let trans_costs = [|
  [| 4000.; 2000.; 3000.; 2500.; 4500. |];
  [| 2500.; 2600.; 3400.; 3000.; 4000. |];
  [| 1200.; 1800.; 2600.; 4100.; 3000. |];
  [| 2200.; 2600.; 3100.; 3700.; 3200. |];
|]

let opencol p = p
let transportcol w p = n_plants * (w+1) + p

let main () =

  (* Create environment *)

  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"facility.log");
      az (start_env env);

      (* Create initial model *)
      let model =
        eer "new_mode"
          (new_model ~env ~name:(Some "facility") ~num_vars:(n_plants * (n_warehouses + 1)) 
          ~objective:None ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in
      
      (* Initialize decision variables for plant open variables *)

      for p = 0 to n_plants - 1 do
        let col = opencol p in
        az (set_char_attr_element ~model ~name:"VType" ~index:col ~value:GRB.binary);
        az (set_float_attr_element ~model ~name:"Obj" ~index:col ~value:fixed_costs.(p));
        let vname = sp "Open%d" p in
        az (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:vname);
      done;
      
      (* Initialize decision variables for transportation decision variables:
        how much to transport from a plant p to a warehouse w *)

      for w = 0 to n_warehouses - 1 do
        for p = 0 to n_plants - 1 do
          let col = transportcol w p in
          az (set_float_attr_element ~model ~name:"Obj" ~index:col ~value:trans_costs.(w).(p));
          let vname = sp "Trans%d.%d" p w in
          az (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:vname);
        done
      done;

      (* The objective is to minimize the total fixed and variable costs *)

      az (set_int_attr ~model ~name:"ModelSense" ~value:GRB.minimize);

      let rowct = if n_plants > n_warehouses then n_plants else n_warehouses in
      let c_beg = i32a rowct in
      let c_ind = i32a (n_plants * (n_warehouses + 1)) in
      let c_val = fa (n_plants * (n_warehouses + 1)) in
      let rhs = fa rowct in
      let sense = ca rowct in
      let cname = Array.make n_plants "" in

      (* Production constraints
        Note that the limit sets the production to zero if
        the plant is closed *)

      let idx = ref 0 in
      for p = 0 to n_plants - 1 do
        c_beg.{p} <- Int32.of_int !idx;
        rhs.{p} <- 0.0;
        sense.{p} <- GRB.less_equal;
        cname.(p) <- (sp "Capacity%d" p);
        for w = 0 to n_warehouses - 1 do
          c_ind.{!idx} <- Int32.of_int (transportcol w p);
          c_val.{!idx} <- 1.0;
          incr idx
        done;
        c_ind.{!idx} <- Int32.of_int (opencol p);
        c_val.{!idx} <- -.capacity.(p);
        incr idx
      done;

      let csc = { num_nz = !idx; xbeg = c_beg; xind = c_ind; xval = c_val } in
      az
        (add_constrs ~model ~num:n_plants ~matrix:(Some csc) ~sense ~rhs
           ~name:(Some cname));
      
      let cname = Array.make n_warehouses "" in

      (* Demand constraints *)
      let idx = ref 0 in
      for w = 0 to n_warehouses - 1 do
        c_beg.{w} <- Int32.of_int !idx;
        sense.{w} <- GRB.equal;
        cname.(w) <- (sp "Demand%d" w);
        for p = 0 to n_plants - 1 do
          c_ind.{!idx} <- Int32.of_int (transportcol w p);
          c_val.{!idx} <- 1.0;
          incr idx
        done;
      done;

      let csc = { num_nz = !idx; xbeg = c_beg; xind = c_ind; xval = c_val } in

      az
        (add_constrs ~model ~num:n_warehouses ~matrix:(Some csc) ~sense ~rhs:(to_fa demand)
           ~name:(Some cname));

      (* Guess at the starting point: close the plant with the highest
        fixed costs; open all others *)
      
      (* First, open all plants *)

      for p = 0 to n_plants - 1 do
        az (set_float_attr_element ~model ~name:"Start" ~index:(opencol p) ~value:1.0);
      done;

      (* Now close the plant with the highest fixed cost *)

      pr "Initial guess:\n";

      let max_index = ref 0 in
      for p = 0 to n_plants - 1 do
        if fixed_costs.(p) > fixed_costs.(!max_index) then
          max_index := p
      done;
      az (set_float_attr_element ~model ~name:"Start" ~index:(opencol !max_index) ~value:0.0);
      pr "Closing plant %d\n\n" !max_index;

      (* Use barrier to solve root relaxation *)

      az (set_int_param ~env ~name:GRB.int_par_method ~value:GRB.method_barrier);
      
      (* Solve *)

      az (optimize model);
      az (write ~model ~path:"facility.lp");

      (* Print solution *)

      let obj_val =
        eer "get_float_attr" (get_float_attr ~model ~name:GRB.dbl_attr_objval)
      in
      pr "\nTOTAL COSTS: %f\n" obj_val;
      pr "SOLUTION:\n";
      for p = 0 to n_plants - 1 do
        let sol =
          eer "get_float_attr_element" (get_float_attr_element ~model ~name:"X" ~index:(opencol p))
        in
        if sol > 0.99 then (
          pr "Plant %d open:\n" p;
          for w = 0 to n_warehouses - 1 do
            let sol =
              eer "get_float_attr_element" (get_float_attr_element ~model ~name:"X" ~index:(transportcol w p))
            in
            if sol > 0.0001 then
              pr "  Transport %f units to warehouse %d\n" sol w
          done)
        else
          pr "Plant %d closed!\n" p
      done

let () = main ()
