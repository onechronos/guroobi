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
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"piecewise.log");
      az (start_env env);

      (* Create an empty model *)
      let model =
        eer "new_model"
          (new_model ~env ~name:None ~num_vars:0 ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in
      let lb = 0.0 in
      let ub = 1.0 in
      az
        (add_var ~model ~num_nz:0 ~v_ind:None ~v_val:None ~obj:0.0 ~lb ~ub
           ~v_type:GRB.continuous ~var_name:(Some "x"));
      az
        (add_var ~model ~num_nz:0 ~v_ind:None ~v_val:None ~obj:0.0 ~lb ~ub
           ~v_type:GRB.continuous ~var_name:(Some "y"));
      az
        (add_var ~model ~num_nz:0 ~v_ind:None ~v_val:None ~obj:0.0 ~lb ~ub
           ~v_type:GRB.continuous ~var_name:(Some "z"));
      az
        (set_float_attr_element ~model ~name:GRB.dbl_attr_obj ~index:1
           ~value:(-1.0));

      let n_pts = 101 in
      let ptu = fa n_pts in
      let ptf = fa n_pts in
      let ptg = fa n_pts in
      for i = 0 to n_pts - 1 do
        ptu.{i} <-
          lb +. ((ub -. lb) *. float_of_int i /. float_of_int (n_pts - 1));
        ptf.{i} <- Float.exp (-1. *. ptu.{i});
        ptg.{i} <- (2. *. ptu.{i} *. ptu.{i}) -. (4. *. ptu.{i})
      done;

      az (set_pwl_obj ~model ~var:0 ~n_points:n_pts ~x:ptu ~y:ptf);
      az (set_pwl_obj ~model ~var:2 ~n_points:n_pts ~x:ptu ~y:ptg);

      let ind = to_i32a [| 0; 1; 2 |] in
      let value = to_fa [| 1.; 2.; 3. |] in
      az
        (add_constr ~model ~num_nz:3 ~var_index:ind ~nz:value
           ~sense:GRB.less_equal ~rhs:4.0 ~name:(Some "c0"));

      let ind = to_i32a [| 0; 1 |] in
      let value = to_fa [| 1.; 1. |] in
      az
        (add_constr ~model ~num_nz:2 ~var_index:ind ~nz:value
           ~sense:GRB.greater_equal ~rhs:1.0 ~name:(Some "c1"));

      az (optimize model);

      let ismip = eer "get_int_attr" (get_int_attr ~model ~name:"IsMIP") in
      let objval =
        eer "get_float_attr" (get_float_attr ~model ~name:"ObjVal")
      in
      let sol =
        eer "get_float_attr_array"
          (get_float_attr_array ~model ~name:"X" ~start:0 ~len:3)
      in
      pr "IsMIP: %d\n" ismip;
      pr "x %g\ny %g\nz %g\n" sol.{0} sol.{1} sol.{2};
      pr "Obj: %g\n" objval;
      pr "\n";

      for i = 0 to n_pts - 1 do
        ptf.{i} <- -.ptf.{i}
      done;

      az (set_pwl_obj ~model ~var:0 ~n_points:n_pts ~x:ptu ~y:ptf);

      az (optimize model);

      let ismip = eer "get_int_attr" (get_int_attr ~model ~name:"IsMIP") in
      let objval =
        eer "get_float_attr" (get_float_attr ~model ~name:"ObjVal")
      in
      let sol =
        eer "get_float_attr_array"
          (get_float_attr_array ~model ~name:"X" ~start:0 ~len:3)
      in
      pr "IsMIP: %d\n" ismip;
      pr "x %g\ny %g\nz %g\n" sol.{0} sol.{1} sol.{2};
      pr "Obj: %g\n" objval;
      pr "\n"

let () = main ()
