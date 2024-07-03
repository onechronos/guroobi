open Guroobi
open Raw
open U

type var_t = { index : int; x : float }

let vcomp v1 v2 =
  let sol1 = abs_float v1.x in
  let sol2 = abs_float v2.x in
  let frac1 = abs_float (sol1 -. floor (sol1 +. 0.5)) in
  let frac2 = abs_float (sol2 -. floor (sol2 +. 0.5)) in
  if frac1 < frac2 then -1 else if frac1 > frac2 then 1 else 0

let main () =
  (* Create environment *)
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 0
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"fixanddive.log");
      az (start_env env);

      let model =
        eer "read_model"
          (match read_model ~env ~path:"data/stein9.mps" with
          | FileNotFound ->
              pr "Error: unable to open input file\n";
              exit 1
          | Ok m -> Ok m
          | Error code -> Error code)
      in
      let num_vars = eer "get_int_attr" (get_int_attr ~model ~name:"NumVars") in
      let num_int_vars =
        eer "get_int_attr" (get_int_attr ~model ~name:"NumIntVars")
      in
      let int_vars = Array.make num_int_vars 0 in
      let fractional = Array.make num_int_vars { index = 0; x = 0.0 } in
      let num_fractional = ref 0 in
      for j = 0 to num_vars - 1 do
        let v_type =
          eer "get_char_attr_element"
            (get_char_attr_element ~model ~name:"VType" ~index:j)
        in
        if v_type <> GRB.continuous then (
          int_vars.(!num_fractional) <- j;
          incr num_fractional;
          az
            (set_char_attr_element ~model ~name:"VType" ~index:j
               ~value:GRB.continuous))
      done;

      az (optimize model);

      let rec for_loop iter =
        let num_fractional = ref 0 in
        for j = 0 to num_int_vars - 1 do
          let sol =
            eer "get_float_attr_element"
              (get_float_attr_element ~model ~name:"X" ~index:int_vars.(j))
          in
          if Float.abs (sol -. Float.floor (sol +. 0.5)) > 1e-5 then (
            fractional.(!num_fractional) <- { index = int_vars.(j); x = sol };
            incr num_fractional)
        done;

        let obj = eer "get_float_attr" (get_float_attr ~model ~name:"ObjVal") in
        pr "Iteration %i, obj %f, fractional %i\n" iter obj !num_fractional;

        if !num_fractional = 0 then
          pr "Found feasible solution - objective %f\n" obj
        else
          let fractional_filled = Array.sub fractional 0 !num_fractional in
          Array.sort vcomp fractional_filled;
          let n_fix = max (!num_fractional / 4) 1 in
          for j = 0 to n_fix - 1 do
            let fix_val = Float.floor (fractional_filled.(j).x +. 0.5) in
            az
              (set_float_attr_element ~model ~name:"LB"
                 ~index:fractional_filled.(j).index ~value:fix_val);
            az
              (set_float_attr_element ~model ~name:"UB"
                 ~index:fractional_filled.(j).index ~value:fix_val);
            let vname =
              eer "get_str_attr_element"
                (get_str_attr_element ~model ~name:"VarName"
                   ~index:fractional_filled.(j).index)
            in
            pr "  Fix %s to %f ( rel %f )\n" vname fix_val
              fractional_filled.(j).x
          done;
          az (optimize model);

          let status =
            eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
          in
          if status == GRB.optimal then
            if iter < 999 then for_loop (iter + 1)
            else pr "Relaxation is infeasible\n"
      in
      for_loop 0

let () = main ()
