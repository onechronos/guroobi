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
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"gc_pwl_func.log");
      az (start_env env);

      (* Create an empty model *)
      let model =
        eer "new_model"
          (new_model ~env ~name:None ~num_vars:0 ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in

      let lb = 0.0 in
      let ub = GRB.infinity in
      az
        (add_var ~model ~num_nz:0 ~v_ind:None ~v_val:None ~obj:2.0 ~lb ~ub
           ~v_type:GRB.continuous ~var_name:(Some "x"));
      az
        (add_var ~model ~num_nz:0 ~v_ind:None ~v_val:None ~obj:1.0 ~lb ~ub
           ~v_type:GRB.continuous ~var_name:(Some "y"));
      az
        (add_var ~model ~num_nz:0 ~v_ind:None ~v_val:None ~obj:0.0 ~lb ~ub
           ~v_type:GRB.continuous ~var_name:(Some "u"));
      az
        (add_var ~model ~num_nz:0 ~v_ind:None ~v_val:None ~obj:0.0 ~lb ~ub
           ~v_type:GRB.continuous ~var_name:(Some "v"));

      az (set_int_attr ~model ~name:GRB.int_attr_modelsense ~value:GRB.maximize);

      let ind = to_i32a [| 2; 3 |] in
      let value = to_fa [| 1.; 4. |] in
      az
        (add_constr ~model ~num_nz:2 ~var_index:ind ~nz:value
           ~sense:GRB.less_equal ~rhs:9.0 ~name:(Some "c1"));

      let intv = 1e-3 in
      let xmax = Float.log 9.0 in
      let len = int_of_float (Float.ceil (xmax /. intv)) + 1 in
      let x_pts = fa len in
      let u_pts = fa len in
      for i = 0 to len - 1 do
        x_pts.{i} <- float_of_int i *. intv;
        u_pts.{i} <- Float.exp (float_of_int i *. intv)
      done;

      az
        (add_gen_constr_pwl ~model ~name:(Some "gc1") ~x_var:0 ~y_var:2
           ~n_pts:len ~x_pts ~y_pts:u_pts);

      let y_max = 9.0 /. 4.0 *. (9.0 /. 4.0) in
      let len = int_of_float (Float.ceil (y_max /. intv)) + 1 in
      let y_pts = fa len in
      let v_pts = fa len in
      for i = 0 to len - 1 do
        y_pts.{i} <- float_of_int i *. intv;
        v_pts.{i} <- Float.sqrt (float_of_int i *. intv)
      done;

      az
        (add_gen_constr_pwl ~model ~name:(Some "gc2") ~x_var:1 ~y_var:3
           ~n_pts:len ~x_pts:y_pts ~y_pts:v_pts);

      az (optimize model);

      let x =
        eer "get_float_attr_array"
          (get_float_attr_array ~model ~name:"X" ~start:0 ~len:4)
      in
      pr "x = %f, u = %f\n" x.{0} x.{2};
      pr "y = %f, v = %f\n" x.{1} x.{3};

      let temp = Float.exp x.{0} +. (4. *. Float.sqrt x.{1}) -. 9. in
      let vio = max temp 0.0 in
      pr "Vio = %.0f\n" vio;

      az (reset_model ~model);

      let ind = to_i32a [| 0; 1 |] in
      az (del_gen_constrs ~model ~num_del:2 ~ind);

      az (update_model ~model);

      az
        (add_gen_constr_exp ~model ~name:(Some "gcf1") ~x_var:0 ~y_var:2
           ~options:None);

      az
        (add_gen_constr_pow ~model ~name:(Some "gcf2") ~x_var:1 ~y_var:3 ~a:0.5
           ~options:None);

      az (set_int_model_param ~model ~name:"FuncPieces" ~value:1);

      az (set_float_model_param ~model ~name:"FuncPieceLength" ~value:1e-3);

      az (optimize model);

      let x =
        eer "get_float_attr_array"
          (get_float_attr_array ~model ~name:"X" ~start:0 ~len:4)
      in
      pr "x = %f, u = %f\n" x.{0} x.{2};
      pr "y = %f, v = %f\n" x.{1} x.{3};

      let temp = Float.exp x.{0} +. (4. *. Float.sqrt x.{1}) -. 9. in
      let vio = max temp 0.0 in
      pr "Vio = %.0f\n" vio;

      let x =
        eer "get_float_attr_array"
          (get_float_attr_array ~model ~name:"X" ~start:0 ~len:4)
      in
      let t = max (x.{0} -. 0.01) 0.0 in
      az (set_float_attr_element ~model ~name:"LB" ~index:0 ~value:t);
      let t = max (x.{1} -. 0.01) 0.0 in
      az (set_float_attr_element ~model ~name:"LB" ~index:1 ~value:t);
      az
        (set_float_attr_element ~model ~name:"UB" ~index:0
           ~value:(x.{0} +. 0.01));
      az
        (set_float_attr_element ~model ~name:"UB" ~index:1
           ~value:(x.{1} +. 0.01));
      az (update_model ~model);
      az (reset_model ~model);
      az (set_float_model_param ~model ~name:"FuncPieceLength" ~value:1e-5);
      az (optimize model);

      let x =
        eer "get_float_attr_array"
          (get_float_attr_array ~model ~name:"X" ~start:0 ~len:4)
      in
      pr "x = %f, u = %f\n" x.{0} x.{2};
      pr "y = %f, v = %f\n" x.{1} x.{3};

      let temp = Float.exp x.{0} +. (4. *. Float.sqrt x.{1}) -. 9. in
      let vio = max temp 0.0 in
      pr "Vio = %.0f\n" vio

let () = main ()
