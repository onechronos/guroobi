open Guroobi
open Raw
open Utils
open U

let main () =

  (* Create environment *)
  
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"bilinear.log");
      az (start_env env);

      (* Create an empty model *)

      let model =
        eer "new_mode"
          (new_model ~env ~name:(Some "bilinear") ~num_vars:0 ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in

      (* Add variables *)

      let num_vars = 3 in
      let obj = fa num_vars in
      obj.{0} <- 1.0;
      obj.{1} <- 0.0;
      obj.{2} <- 0.0;

      az
        (add_vars ~model ~num_vars ~matrix:None ~objective:(Some obj)
           ~lower_bound:None ~upper_bound:None ~var_type:None
           ~name:None);

      (* Change sense to maximization *)

      az (set_int_attr ~model ~name:GRB.int_attr_modelsense ~value:GRB.maximize);
      
      (* Linear constraint: x + y + z <= 10 *)

      let num_nz = 3 in
      let c_ind = i32a num_nz in
      let c_val = fa num_nz in

      c_ind.{0} <- 0l;
      c_ind.{1} <- 1l;
      c_ind.{2} <- 2l;
      c_val.{0} <- 1.0;
      c_val.{1} <- 1.0;
      c_val.{2} <- 1.0;

      az
        (add_constr ~model ~num_nz:3 ~var_index:c_ind ~nz:c_val
           ~sense:GRB.less_equal ~rhs:10.0 ~name:(Some "c0"));

      (* Bilinear inequality: x * y <= 2 *)

      let q_num_nz = 1 in
      let q_row = i32a num_nz in
      let q_col = i32a num_nz in
      let q_val = fa num_nz in

      q_row.{0} <- 0l;
      q_col.{0} <- 1l;
      q_val.{0} <- 1.0;

      az
        (add_q_constr ~model ~linear:None ~q_num_nz ~q_row ~q_col ~q_val
           ~sense:GRB.less_equal ~rhs:2.0 ~name:(Some "bilinear0"));

      (* Bilinear equality: x * z + y * z == 1 *)

      let q_num_nz = 2 in
      let q_row = i32a num_nz in
      let q_col = i32a num_nz in
      let q_val = fa num_nz in

      q_row.{0} <- 0l;
      q_row.{1} <- 1l;
      q_col.{0} <- 2l;
      q_col.{1} <- 2l;
      q_val.{0} <- 1.0;
      q_val.{1} <- 1.0;

      az
        (add_q_constr ~model ~linear:None ~q_num_nz ~q_row ~q_col ~q_val
           ~sense:GRB.equal ~rhs:1.0 ~name:(Some "bilinear1"));

      (* Optimize model *)
      
      az (optimize model);

      (* Write model to 'bilinear.lp' *)

      az (write ~model ~path:"bilinear.lp");

      (* Capture solution information *)

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
        pr "x=%.2f\ny=%.2f\nz=%.2f\n" sol.{0} sol.{1} sol.{2})
      else if status = GRB.inf_or_unbd then
        pr "model is infeasible or unbounded\n"
      else pr "optimization stopped early\n";

      (* Now constrain 'x' to be integral and solve again *)

      az (set_char_attr_element ~model ~name:GRB.char_attr_vtype ~index:0 ~value:GRB.integer);

      az (optimize model);

      (* Capture solution information *)

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
        pr "x=%.2f\ny=%.2f\nz=%.2f\n" sol.{0} sol.{1} sol.{2})
      else if status = GRB.inf_or_unbd then
        pr "model is infeasible or unbounded\n"
      else pr "optimization stopped early\n"

let () = main ()
