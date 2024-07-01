open Guroobi
open Raw
open Utils
open U

let main () =
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"sos.log");
      az (start_env env);

      (* Create an empty model *)
      let model =
        eer "new_model"
          (new_model ~env ~name:(Some "sos") ~num_vars:0 ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in

      let obj = to_fa [| -2.; -1.; -1. |] in
      let ub = to_fa [| 1.; 1.; 2. |] in
      az
        (add_vars ~model ~num_vars:3 ~matrix:None ~objective:(Some obj)
           ~lower_bound:None ~upper_bound:(Some ub) ~var_type:None ~name:None);

      let sos_ind = to_i32a [| 0; 1; 0; 2 |] in
      let sos_wt = to_fa [| 1.; 2.; 1.; 2. |] in
      let sos_beg = to_i32a [| 0; 2 |] in
      let sos_type = to_i32a [| GRB.sos_type1; GRB.sos_type1 |] in
      az
        (add_sos ~model ~num_sos:2 ~num_members:4 ~types:sos_type ~beg:sos_beg
           ~ind:sos_ind ~weight:sos_wt);

      az (optimize model);

      az (write ~model ~path:"sos.lp");

      let status =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
      in
      if status = GRB.optimal then (
        let obj_val =
          eer "get_float_attr" (get_float_attr ~model ~name:GRB.dbl_attr_objval)
        in
        pr "Optimal objective: %.4e\n" obj_val;

        let sol =
          eer "get_float_attr_array"
            (get_float_attr_array ~model ~name:GRB.dbl_attr_x ~start:0 ~len:3)
        in
        pr "x=%.4f\ny=%.4f\nz=%.4f\n" sol.{0} sol.{1} sol.{2})
      else if status = GRB.inf_or_unbd then
        pr "Model is infeasible or unbounded\n"
      else pr "Optimization stopped early\n"

let () = main ()
