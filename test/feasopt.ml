open Guroobi
open Raw
open U
open Utils

let main () =
  (* Create environment *)
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 0
  | Ok () -> (
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"feasopt.log");
      az (start_env env);

      let model =
        eer "read_model"
          (match read_model ~env ~path:"data/qafiro.mps" with
          | FileNotFound ->
              pr "Error: unable to open input file\n";
              exit 1
          | Ok m -> Ok m
          | Error code -> Error code)
      in
      let feasmodel = copy_model ~model in
      match feasmodel with
      | None ->
          pr "Cannot copy model";
          exit 1
      | Some feasmodel ->
          let num_vars =
            eer "get_int_attr" (get_int_attr ~model ~name:"NumVars")
          in
          for j = 0 to num_vars - 1 do
            az (set_float_attr_element ~model ~name:"Obj" ~index:j ~value:0.0)
          done;

          let num_constrs =
            eer "get_int_attr" (get_int_attr ~model ~name:"NumConstrs")
          in
          for i = 0 to num_constrs - 1 do
            let sense =
              eer "get_char_attr_element"
                (get_char_attr_element ~model ~name:"Sense" ~index:i)
            in
            (if sense <> '>' then
               let cname =
                 eer "get_str_attr_element"
                   (get_str_attr_element ~model ~name:"ConstrName" ~index:i)
               in
               let v_name = sp "ArtN_%s" cname in
               let v_ind = to_i32a [| i |] in
               let v_val = to_fa [| -1. |] in
               az
                 (add_var ~model ~num_nz:1 ~v_ind:(Some v_ind)
                    ~v_val:(Some v_val) ~obj:1.0 ~lb:0.0 ~ub:GRB.infinity
                    ~v_type:GRB.continuous ~var_name:(Some v_name)));
            if sense <> '<' then
              let cname =
                eer "get_str_attr_element"
                  (get_str_attr_element ~model ~name:"ConstrName" ~index:i)
              in
              let v_name = sp "ArtP_%s" cname in
              let v_ind = to_i32a [| i |] in
              let v_val = to_fa [| 1. |] in
              az
                (add_var ~model ~num_nz:1 ~v_ind:(Some v_ind)
                   ~v_val:(Some v_val) ~obj:1.0 ~lb:0.0 ~ub:GRB.infinity
                   ~v_type:GRB.continuous ~var_name:(Some v_name))
          done;

          az (optimize model);
          az (write ~model ~path:"feasopt.lp");

          let status =
            eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
          in
          if status = GRB.optimal then
            let obj_val =
              eer "get_float_attr"
                (get_float_attr ~model ~name:GRB.dbl_attr_objval)
            in
            pr "Optimal objective: %.4e\n" obj_val
          else if status = GRB.inf_or_unbd then
            pr "Model is infeasible or unbounded\n"
          else pr "Optimization stopped early\n";

          let rhs_pen = to_fa (Array.make num_constrs 1.0) in

          az
            (feas_relax ~model:feasmodel ~relax_obj_type:GRB.feasrelax_linear
               ~min_relax:1 ~lb_pen:None ~ub_pen:None ~rhs_pen:(Some rhs_pen)
               ~feas_obf_p:None);

          az (optimize feasmodel);
          az (write ~model:feasmodel ~path:"feasopt1.lp");

          let status =
            eer "get_int_attr"
              (get_int_attr ~model:feasmodel ~name:GRB.int_attr_status)
          in
          if status = GRB.optimal then
            let obj_val =
              eer "get_float_attr"
                (get_float_attr ~model:feasmodel ~name:GRB.dbl_attr_objval)
            in
            pr "Optimal objective: %.4e\n" obj_val
          else if status = GRB.inf_or_unbd then
            pr "Model is infeasible or unbounded\n"
          else pr "Optimization stopped early\n")

let () = main ()
