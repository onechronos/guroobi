open Guroobi
open Raw
open Utils
open U

(* This example formulates and solves the following simple QP model:

   minimize x^2 + x*y + y^2 + y*z + z^2 + 2 x subject to x + 2 y + 3 z >= 4 x +
   y >= 1 x, y, z non-negative

   It solves it once as a continuous model, and once as an integer model. *)

let main () =
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"qp.log");
      az (start_env env);

      (* Create an empty model *)
      let model =
        eer "new_model"
          (new_model ~env ~name:(Some "qp") ~num_vars:0 ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in

      (* Add variables *)
      az
        (add_vars ~model ~num_vars:3 ~matrix:None ~objective:None
           ~lower_bound:None ~upper_bound:None ~var_type:None ~name:None);

      (* Quadratic objective terms *)
      let q_row = to_i32a [| 0; 0; 1; 1; 2 |] in
      let q_col = to_i32a [| 0; 1; 1; 2; 2 |] in
      let q_val = to_fa [| 1.; 1.; 1.; 1.; 1. |] in
      az (add_q_p_terms ~model ~num_qnz:5 ~q_row ~q_col ~q_val);

      (* Linear objective term *)
      az
        (set_float_attr_element ~model ~name:GRB.dbl_attr_obj ~index:0
           ~value:2.0);

      (* First constraint: x + 2 y + 3 z <= 4 *)
      let var_index = to_i32a [| 0; 1; 2 |] in
      let nz = to_fa [| 1.; 2.; 3. |] in
      az
        (add_constr ~model ~num_nz:3 ~var_index ~nz ~sense:GRB.greater_equal
           ~rhs:4.0 ~name:(Some "c0"));

      (* Second constraint: x + y >= 1 *)
      let var_index = to_i32a [| 0; 1 |] in
      let nz = to_fa [| 1.; 1. |] in
      az
        (add_constr ~model ~num_nz:2 ~var_index ~nz ~sense:GRB.greater_equal
           ~rhs:1.0 ~name:(Some "c1"));

      (* Optimize model *)
      az (optimize model);
      az (write ~model ~path:"qp.lp");

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
        pr "x=%.4f\ny=%.4f\nz=%.4f\n" sol.{0} sol.{1} sol.{2})
      else if status = GRB.inf_or_unbd then
        pr "model is infeasible or unbounded\n"
      else pr "optimization stopped early\n";

      (* Modify variable types *)
      let vtype = to_ca [| GRB.integer; GRB.integer; GRB.integer |] in
      az
        (set_char_attr_array ~model ~name:GRB.char_attr_vtype ~start:0 ~len:3
           ~values:vtype);

      (* Optimize model *)
      az (optimize model);
      az (write ~model ~path:"qp2.lp");

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
        pr "x=%.4f\ny=%.4f\nz=%.4f\n" sol.{0} sol.{1} sol.{2})
      else if status = GRB.inf_or_unbd then
        pr "model is infeasible or unbounded\n"
      else pr "optimization stopped early\n"

let () = main ()
