open Guroobi
open Raw
open Utils
open U

(* We find alternative epsilon-optimal solutions to a given knapsack
 * problem by using PoolSearchMode *)

let ground_set_size = 10

let obj_coef =   [|32.; 32.; 15.; 15.; 6.; 6.; 1.; 1.; 1.; 1.|]

let knapsack_coef =   [|16.; 16.; 8.; 8.; 4.; 4.; 2.; 2.; 1.; 1.|]

let budget = 33.

let main () =
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"poolsearch.log");
      az (start_env env);

      (* Create an empty model *)

      let model =
        eer "new_model"
          (new_model ~env ~name:(Some "poolsearch") ~num_vars:ground_set_size ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in

      (* set objective function *)

      az (set_float_attr_array ~model ~name:"Obj" ~start:0 ~len:ground_set_size ~values:(to_fa obj_coef));

      (* set variable types and names *)

      for e = 0 to ground_set_size - 1 do
        let buffer = sp "El%d" e in
        az (set_char_attr_element ~model ~name:"VType" ~index:e ~value:GRB.binary);
        az (set_str_attr_element ~model ~name:"VarName" ~index:e ~value:buffer)
      done;

      let c_ind = i32a ground_set_size in
      for e = 0 to ground_set_size - 1 do
        c_ind.{e} <- Int32.of_int e;
      done;

      (* Constraint: limit total number of elements to be picked to be at most Budget *)

      let buffer = sp "Budget" in
      az (add_constr ~model ~num_nz:ground_set_size ~var_index:c_ind ~nz:(to_fa knapsack_coef)
        ~sense:GRB.less_equal ~rhs:budget ~name:(Some buffer));

      (* set global sense for ALL objectives *)

      az (set_int_attr ~model ~name:GRB.int_attr_modelsense ~value:GRB.maximize);

      (* Limit how many solutions to collect *)

      az (set_int_model_param ~model ~name:GRB.int_par_poolsolutions ~value:1024);

      (* Limit the search space by setting a gap for the worst possible solution that will be accepted *)

      az (set_float_model_param ~model ~name:GRB.dbl_par_poolgap ~value:0.10);

      (* do a systematic search for the k-best solutions *)

      az (set_int_model_param ~model ~name:GRB.int_par_poolsearchmode ~value:2);

      (* save problem *)

      az (write ~model ~path:"poolsearch.lp");
      az (write ~model ~path:"poolsearch.mps");

      (* Optimize model *)

      az (optimize model);
     

      (* Status Checking *)

      let status =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
      in
      if status = GRB.inf_or_unbd || status = GRB.infeasible || status = GRB.unbounded then
        pr ("The model cannot be solved because it is infeasible or unbounded\n")
      else if status <> GRB.optimal then
        pr "Optimization was stopped with status %d\n" status
      else (

        (* Print best selected set *)

        let c_val = 
          eer "get_float_attr_array" (get_float_attr_array ~model ~name:GRB.dbl_attr_x ~start:0 ~len:ground_set_size)
        in
        pr "Selected elements in best solution:\n\t";
        for e = 0 to ground_set_size - 1 do
          if c_val.{e} >= 0.9 then
            pr "El%d " e
        done;

        (* print number of solutions stored *)

        let n_solutions =
          eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_solcount)
        in
        pr "\nNumber of solutions found: %d\nValues:" n_solutions;

        (* print objective values of alternative solutions *)

        let prlen = ref 0 in
        for e = 0 to n_solutions - 1 do
          az (set_int_model_param ~model ~name:GRB.int_par_solutionnumber ~value:e);
          let obj_val = 
            eer "get_float_attr" (get_float_attr ~model ~name:GRB.dbl_attr_poolobjval)
          in
          let obj_val_str = sp " %g" obj_val in
          pr "%s" obj_val_str;
          prlen := !prlen + (String.length obj_val_str);

          if !prlen >= 75 && e + 1 < n_solutions then (
            let new_line_str = "\n    " in
            pr "%s" new_line_str;
            prlen := String.length new_line_str
          )
        done;
        pr "\n";

        (* print fourth best set if available *)

        if n_solutions >= 4 then (
          az (set_int_model_param ~model ~name:GRB.int_par_solutionnumber ~value:3);

          (* get the solution vector *)
          
          let c_val =
            eer "get_float_attr_array" (get_float_attr_array ~model ~name:GRB.dbl_attr_xn ~start:0 ~len:ground_set_size)
          in

          pr "Selected elements in fourth best solution:\n\t";

          for e = 0 to ground_set_size - 1 do
            if c_val.{e} >= 0.9 then
              pr "El%d " e
          done;
          pr "\n"
        )
      )


let () = main ()
