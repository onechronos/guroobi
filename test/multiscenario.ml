open Guroobi
open Raw
open Utils
open U

(* Facility location: a company currently ships its product from 5 plants to 4
   warehouses. It is considering closing some plants to reduce costs. What
   plant(s) should the company close, in order to minimize transportation and
   fixed costs?

   Since the plant fixed costs and the warehouse demands are uncertain, a
   scenario approach is chosen.

   Note that this example is similar to the facility.ml example. Here we added
   scenarios in order to illustrate the multi-scenario feature. *)

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
let trans_costs =
  [|
    [| 4000.; 2000.; 3000.; 2500.; 4500. |];
    [| 2500.; 2600.; 3400.; 3000.; 4000. |];
    [| 1200.; 1800.; 2600.; 4100.; 3000. |];
    [| 2200.; 2600.; 3100.; 3700.; 3200. |];
  |]

let opencol p = p
let transportcol w p = (n_plants * (w + 1)) + p
let demandconstr w = n_plants + w

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
          (new_model ~env ~name:(Some "multiscenario")
             ~num_vars:(n_plants * (n_warehouses + 1))
             ~objective:None ~lower_bound:None ~upper_bound:None ~var_type:None
             ~var_name:None)
      in

      (* Compute indexes of minimal and maximal fixed cost *)
      let max_index = ref 0 in
      let min_index = ref 0 in
      for p = 0 to n_plants - 1 do
        if fixed_costs.(p) > fixed_costs.(!max_index) then max_index := p
        else if fixed_costs.(p) < fixed_costs.(!min_index) then min_index := p
      done;

      (* Initialize decision variables for plant open variables *)
      for p = 0 to n_plants - 1 do
        let col = opencol p in
        az
          (set_char_attr_element ~model ~name:"VType" ~index:col
             ~value:GRB.binary);
        az
          (set_float_attr_element ~model ~name:"Obj" ~index:col
             ~value:fixed_costs.(p));
        let vname = sp "Open%d" p in
        az (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:vname)
      done;

      (* Initialize decision variables for transportation decision variables:
         how much to transport from a plant p to a warehouse w *)
      for w = 0 to n_warehouses - 1 do
        for p = 0 to n_plants - 1 do
          let col = transportcol w p in
          az
            (set_float_attr_element ~model ~name:"Obj" ~index:col
               ~value:trans_costs.(w).(p));
          let vname = sp "Trans%d.%d" p w in
          az
            (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:vname)
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

      (* Production constraints Note that the limit sets the production to zero
         if the plant is closed *)
      let idx = ref 0 in
      for p = 0 to n_plants - 1 do
        c_beg.{p} <- Int32.of_int !idx;
        rhs.{p} <- 0.0;
        sense.{p} <- GRB.less_equal;
        cname.(p) <- sp "Capacity%d" p;
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
        cname.(w) <- sp "Demand%d" w;
        for p = 0 to n_plants - 1 do
          c_ind.{!idx} <- Int32.of_int (transportcol w p);
          c_val.{!idx} <- 1.0;
          incr idx
        done
      done;

      let csc = { num_nz = !idx; xbeg = c_beg; xind = c_ind; xval = c_val } in

      az
        (add_constrs ~model ~num:n_warehouses ~matrix:(Some csc) ~sense
           ~rhs:(to_fa demand) ~name:(Some cname));

      (* We constructed the base model, now we add 7 scenarios

         Scenario 0: Represents the base model, hence, no manipulations.
         Scenario 1: Manipulate the warehouses demands slightly (constraint
         right hand sides). Scenario 2: Double the warehouses demands
         (constraint right hand sides). Scenario 3: Manipulate the plant fixed
         costs (objective coefficients). Scenario 4: Manipulate the warehouses
         demands and fixed costs. Scenario 5: Force the plant with the largest
         fixed cost to stay open (variable bounds). Scenario 6: Force the plant
         with the smallest fixed cost to be closed (variable bounds). *)
      az (set_int_attr ~model ~name:GRB.int_attr_numscenarios ~value:7);

      (* Scenario 0: Base model, hence, nothing to do except giving the scenario
         a name *)
      az (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:0);
      az (set_str_attr ~model ~name:GRB.str_attr_scennname ~value:"Base model");

      (* Scenario 1: Increase the warehouse demands by 10% *)
      az (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:1);
      az
        (set_str_attr ~model ~name:GRB.str_attr_scennname
           ~value:"Increased warehouse demands");

      for w = 0 to n_warehouses - 1 do
        az
          (set_float_attr_element ~model ~name:GRB.dbl_attr_scennrhs
             ~index:(demandconstr w)
             ~value:(demand.(w) *. 1.1))
      done;

      (* Scenario 2: Double the warehouse demands *)
      az (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:2);
      az
        (set_str_attr ~model ~name:GRB.str_attr_scennname
           ~value:"Double the warehouse demands");

      for w = 0 to n_warehouses - 1 do
        az
          (set_float_attr_element ~model ~name:GRB.dbl_attr_scennrhs
             ~index:(demandconstr w)
             ~value:(demand.(w) *. 2.0))
      done;

      (* Scenario 3: Decrease the plant fixed costs by 5% *)
      az (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:3);
      az
        (set_str_attr ~model ~name:GRB.str_attr_scennname
           ~value:"Decreased plant fixed costs");

      for p = 0 to n_plants - 1 do
        az
          (set_float_attr_element ~model ~name:GRB.dbl_attr_scennobj
             ~index:(opencol p)
             ~value:(fixed_costs.(p) *. 0.95))
      done;

      (* Scenario 4: Combine scenario 1 and scenario 3 *)
      az (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:4);
      az
        (set_str_attr ~model ~name:GRB.str_attr_scennname
           ~value:"Increased warehouse demands and decreased plant fixed costs");

      for w = 0 to n_warehouses - 1 do
        az
          (set_float_attr_element ~model ~name:GRB.dbl_attr_scennrhs
             ~index:(demandconstr w)
             ~value:(demand.(w) *. 1.1))
      done;

      for p = 0 to n_plants - 1 do
        az
          (set_float_attr_element ~model ~name:GRB.dbl_attr_scennobj
             ~index:(opencol p)
             ~value:(fixed_costs.(p) *. 0.95))
      done;

      (* Scenario 5: Force the plant with the largest fixed cost to stay open *)
      az (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:5);
      az
        (set_str_attr ~model ~name:GRB.str_attr_scennname
           ~value:"Force plant with largest fixed cost to stay open");

      az
        (set_float_attr_element ~model ~name:GRB.dbl_attr_scennlb
           ~index:(opencol !max_index) ~value:1.0);

      (* Scenario 6: Force the plant with the smallest fixed cost to be
         closed *)
      az (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:6);
      az
        (set_str_attr ~model ~name:GRB.str_attr_scennname
           ~value:"Force plant with smallest fixed cost to be closed");

      az
        (set_float_attr_element ~model ~name:GRB.dbl_attr_scennub
           ~index:(opencol !min_index) ~value:0.0);

      (* Guess at the starting point: close the plant with the highest fixed
         costs; open all others *)

      (* First, open all plants *)
      for p = 0 to n_plants - 1 do
        az
          (set_float_attr_element ~model ~name:GRB.dbl_attr_start
             ~index:(opencol p) ~value:1.0)
      done;

      (* Now close the plant with the highest fixed cost *)
      pr "Initial guess:\n";

      az
        (set_float_attr_element ~model ~name:"Start" ~index:(opencol !max_index)
           ~value:0.0);
      pr "Closing plant %d\n\n" !max_index;

      (* Use barrier to solve root relaxation *)
      az (set_int_param ~env ~name:GRB.int_par_method ~value:GRB.method_barrier);

      (* Solve multi-scenario model *)
      az (optimize model);
      az (write ~model ~path:"multiscenario.lp");

      let n_scenarios =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_numscenarios)
      in

      (* Print solution for each *)
      for s = 0 to n_scenarios - 1 do
        (* Set the scenario number to query the information for this scenario *)
        az
          (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:s);

        (* Collect result for the scenario *)
        let scenario_name =
          eer "get_str_attr" (get_str_attr ~model ~name:GRB.str_attr_scennname)
        in
        let scen_n_obj_bound =
          eer "get_float_attr"
            (get_float_attr ~model ~name:GRB.dbl_attr_scennobjbound)
        in

        let scen_n_obj_val =
          eer "get_float_attr"
            (get_float_attr ~model ~name:GRB.dbl_attr_scennobjval)
        in
        pr "\n\n------ Scenario %d (%s)\n" s scenario_name;

        (* Check if we found a feasible solution for this scenario *)
        if float_of_int GRB.minimize *. scen_n_obj_val >= GRB.infinity then
          if float_of_int GRB.minimize *. scen_n_obj_bound >= GRB.infinity then
            (* Scenario was proven to be infeasible *)
            pr "\nINFEASIBLE\n"
          else
            (* We did not find any feasible solution - should not happen in this
               case, because we did not set any limit (like a time limit) on the
               optimization process *)
            pr "\nNO SOLUTION\n"
        else (
          pr "\nTOTAL COSTS: %.0f\n" scen_n_obj_val;
          pr "SOLUTION:\n";
          for p = 0 to n_plants - 1 do
            let scen_n_x =
              eer "get_float_attr_element"
                (get_float_attr_element ~model ~name:GRB.dbl_attr_scennx
                   ~index:(opencol p))
            in
            if scen_n_x > 0.5 then (
              pr "Plant %d open\n" p;
              for w = 0 to n_warehouses - 1 do
                let scen_n_x =
                  eer "get_float_attr_element"
                    (get_float_attr_element ~model ~name:GRB.dbl_attr_scennx
                       ~index:(transportcol w p))
                in
                if scen_n_x > 0.0001 then
                  let rounded = floor ((scen_n_x *. 10.0) +. 0.5) /. 10.0 in
                  if floor rounded = rounded then
                    pr "  Transport %.0f units to warehouse %d\n" scen_n_x w
                  else pr "  Transport %.1f units to warehouse %d\n" scen_n_x w
              done)
            else pr "Plant %d closed!\n" p
          done)
      done;
      (* Print a summary table: for each scenario we add a single summary
         line *)
      pr "\n\nSummary: Closed plants depending on scenario\n\n";
      pr "%8s | %17s %13s\n" "" "Plant" "|";

      pr "%8s |" "Scenario";
      for p = 0 to n_plants - 1 do
        pr " %5d" p
      done;
      pr " | %6s  %s\n" "Costs" "Name";

      for s = 0 to n_scenarios - 1 do
        (* Set the scenario number to query the information for this scenario *)
        az
          (set_int_model_param ~model ~name:GRB.int_par_scenarionumber ~value:s);

        (* Collect result for the scenario *)
        let scenario_name =
          eer "get_str_attr" (get_str_attr ~model ~name:GRB.str_attr_scennname)
        in
        let scen_n_obj_bound =
          eer "get_float_attr"
            (get_float_attr ~model ~name:GRB.dbl_attr_scennobjbound)
        in
        let scen_n_obj_val =
          eer "get_float_attr"
            (get_float_attr ~model ~name:GRB.dbl_attr_scennobjval)
        in

        pr "%-8d |" s;

        if float_of_int GRB.minimize *. scen_n_obj_val >= GRB.infinity then
          if float_of_int GRB.minimize *. scen_n_obj_bound >= GRB.infinity then
            (* Scenario was proven to be infeasible *)
            pr " %-30s| %6s  %s\n" "infeasible" "-" scenario_name
          else
            (* We did not find any feasible solution - should not happen in this
               case, because we did not set any limit (like a time limit) on the
               optimization process *)
            pr " %-30s| %6s  %s\n" "no solution found" "-" scenario_name
        else (
          for p = 0 to n_plants - 1 do
            let scen_n_x =
              eer "get_float_attr_element"
                (get_float_attr_element ~model ~name:GRB.dbl_attr_scennx
                   ~index:(opencol p))
            in
            if scen_n_x > 0.5 then pr " %5s" " " else pr " %5s" "x"
          done;
          pr " | %6g  %s\n" scen_n_obj_val scenario_name)
      done

let () = main ()
