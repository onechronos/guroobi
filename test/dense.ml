open Guroobi
open Raw
open Utils
open U

let c = [|1.; 1.; 0.|]

let q =   [|
  [| 1.; 1.; 0. |];
  [| 0.; 1.; 1. |];
  [| 0.; 0.; 1. |];
|]

let a =   [|
  [| 1.; 2.; 3. |];
  [| 1.; 1.; 0. |];
|]

let sense = [|'>'; '>'|]

let rhs = [|4.; 1.|]

let lb = [|0.; 0.; 0.|]

let rows = 2

let cols = 3

let main () =
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"dense.log");
      az (start_env env);

      let model =
        eer "new_model"
          (new_model ~env ~name:(Some "dense") ~num_vars:cols ~objective:(Some (to_fa c))
             ~lower_bound:(Some (to_fa lb)) ~upper_bound:None ~var_type:None ~var_name:None)
      in

      az
        (add_constrs ~model ~num:rows ~matrix:None ~sense:(to_ca sense)
           ~rhs:(to_fa rhs) ~name:None);
      
      for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
          if a.(i).(j) <> 0. then
            let num_chgs = 1 in
            let c_ind = i32a num_chgs in
            let v_ind = i32a num_chgs in
            let value = fa num_chgs in
            c_ind.{0} <- Int32.of_int i;
            v_ind.{0} <- Int32.of_int j;
            value.{0} <- a.(i).(j);
            az (chg_coeffs ~model ~num_chgs ~c_ind ~v_ind ~value)
        done
      done;

      for i = 0 to cols - 1 do
        for j = 0 to cols - 1 do
          if q.(i).(j) <> 0. then
            let num_qnz = 1 in
            let q_row = i32a num_qnz in
            let q_col = i32a num_qnz in
            let q_val = fa num_qnz in
            q_row.{0} <- Int32.of_int i;
            q_col.{0} <- Int32.of_int j;
            q_val.{0} <- q.(i).(j);
            az (add_q_p_terms ~model ~num_qnz ~q_row ~q_col ~q_val)
        done
      done;
      
      az (optimize model);
      az (write ~model ~path:"dense.lp");

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
