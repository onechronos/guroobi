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
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"mip1.log");
      az (start_env env);

      let model =
        eer "new_mode"
          (new_model ~env ~name:(Some "mip1") ~num_vars:0 ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in

      let num_vars = 3 in
      let obj = fa num_vars in
      obj.{0} <- 1.0;
      obj.{1} <- 1.0;
      obj.{2} <- 2.0;

      let var_type = ca num_vars in
      var_type.{0} <- GRB.binary;
      var_type.{1} <- GRB.binary;
      var_type.{2} <- GRB.binary;

      az
        (add_vars ~model ~num_vars ~matrix:None ~objective:(Some obj)
           ~lower_bound:None ~upper_bound:None ~var_type:(Some var_type)
           ~name:None);
      az (set_int_attr ~model ~name:GRB.int_attr_modelsense ~value:GRB.maximize);

      let num_nz = 3 in
      let c_ind = i32a num_nz in
      let c_val = fa num_nz in

      c_ind.{0} <- 0l;
      c_ind.{1} <- 1l;
      c_ind.{2} <- 2l;
      c_val.{0} <- 1.0;
      c_val.{1} <- 2.0;
      c_val.{2} <- 3.0;

      az
        (add_constr ~model ~num_nz:3 ~var_index:c_ind ~nz:c_val
           ~sense:GRB.less_equal ~rhs:4.0 ~name:(Some "c0"));

      c_ind.{0} <- 0l;
      c_ind.{1} <- 1l;
      c_val.{0} <- 1.0;
      c_val.{1} <- 1.0;

      az
        (add_constr ~model ~num_nz:2 ~var_index:c_ind ~nz:c_val
           ~sense:GRB.greater_equal ~rhs:1.0 ~name:(Some "c1"));
      az (optimize model);
      az (write ~model ~path:"mip1.lp");

      let status =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
      in
      if status = GRB.optimal then (
        let obj_val =
          eer "get_float_attr" (get_float_attr ~model ~name:GRB.dbl_attr_objval)
        in
        pr "obj: %.4e\n" obj_val;

        let sol =
          eer "get_float_attr_array"
            (get_float_attr_array ~model ~name:GRB.dbl_attr_x ~start:0 ~len:3)
        in
        pr "x=%.0f\ny=%.0f\nz=%.0f\n" sol.{0} sol.{1} sol.{2})
      else if status = GRB.inf_or_unbd then
        pr "model is infeasible or unbounded\n"
      else pr "optimization stopped early\n"

let () = main ()
