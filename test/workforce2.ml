open Guroobi
open Raw
open Utils
open U

let n_shifts = 14
let n_workers = 7

let shifts =
  [|
    "Mon1";
    "Tue2";
    "Wed3";
    "Thu4";
    "Fri5";
    "Sat6";
    "Sun7";
    "Mon8";
    "Tue9";
    "Wed10";
    "Thu11";
    "Fri12";
    "Sat13";
    "Sun14";
  |]

let workers = [| "Amy"; "Bob"; "Cathy"; "Dan"; "Ed"; "Fred"; "Gu" |]

let shift_requirements =
  [| 3.; 2.; 4.; 4.; 5.; 6.; 5.; 2.; 2.; 3.; 4.; 6.; 7.; 5. |]

let pay = [| 10.; 12.; 10.; 8.; 8.; 9.; 11. |]

let availability =
  [|
    [| 0.; 1.; 1.; 0.; 1.; 0.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1. |];
    [| 1.; 1.; 0.; 0.; 1.; 1.; 0.; 1.; 0.; 0.; 1.; 0.; 1.; 0. |];
    [| 0.; 0.; 1.; 1.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1. |];
    [| 0.; 1.; 1.; 0.; 1.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1.; 1. |];
    [| 1.; 1.; 1.; 1.; 1.; 0.; 1.; 1.; 1.; 0.; 1.; 0.; 1.; 1. |];
    [| 1.; 1.; 1.; 0.; 0.; 1.; 0.; 1.; 1.; 0.; 0.; 1.; 1.; 1. |];
    [| 1.; 1.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1. |];
  |]

let xcol w s = (n_shifts * w) + s

let main () =
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"workforce2.log");
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);

      az (start_env env);
      let model =
        eer "new_mode"
          (new_model ~env ~name:(Some "workforce2")
             ~num_vars:(n_workers * n_shifts) ~objective:None ~lower_bound:None
             ~upper_bound:None ~var_type:None ~var_name:None)
      in

      for w = 0 to n_workers - 1 do
        for s = 0 to n_shifts - 1 do
          let col = xcol w s in
          let vname = sp "%s.%s" workers.(w) shifts.(s) in
          az
            (set_float_attr_element ~model ~name:GRB.dbl_attr_ub ~index:col
               ~value:availability.(w).(s));
          az
            (set_float_attr_element ~model ~name:GRB.dbl_attr_obj ~index:col
               ~value:pay.(w));
          az
            (set_str_attr_element ~model ~name:GRB.str_attr_varname ~index:col
               ~value:vname)
        done
      done;

      az (set_int_attr ~model ~name:GRB.int_attr_modelsense ~value:GRB.minimize);

      let num_nz = n_shifts * n_workers in
      let sense = ca n_shifts in

      let compressed =
        { num_nz; xbeg = i32a num_nz; xind = i32a num_nz; xval = fa num_nz }
      in

      let idx = ref 0 in
      for s = 0 to n_shifts - 1 do
        compressed.xbeg.{s} <- Int32.of_int !idx;
        sense.{s} <- GRB.equal;
        for w = 0 to n_workers - 1 do
          compressed.xind.{!idx} <- Int32.of_int (xcol w s);
          compressed.xval.{!idx} <- 1.0;
          incr idx
        done
      done;
      assert (!idx = num_nz);
      let shift_requirements : fa =
        Bigarray.(Array1.of_array float64 c_layout shift_requirements)
      in
      az
        (add_constrs ~model ~num:n_shifts ~matrix:(Some compressed) ~sense
           ~rhs:shift_requirements ~name:(Some shifts));

      az (optimize model);
      let status =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
      in
      if status = GRB.unbounded then 
        pr "model unbounded\n"
      else if status = GRB.optimal then
        let obj_val =
          eer "get_float_attr" (get_float_attr ~model ~name:GRB.dbl_attr_objval)
        in
        pr "obj: %.4e\n" obj_val
      else if status <> GRB.inf_or_unbd && status <> GRB.infeasible then
        pr "optimization stopped early with status %d\n" status
      else (
        pr "The model is infeasible; computing IIS\n";
        let num_constraints =
          eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_numconstrs)
        in

        let removed = ref [] in
        let rec while_loop () =
          az (compute_iis model);
          pr "\nThe following constraint(s) cannot be satisfied:\n";
        
          let rec for_loop i =
            let iis =
              eer "get_int_attr_element"
                (get_int_attr_element ~model ~name:GRB.int_attr_iis_constr
                  ~index:i)
            in
            if iis = 0 && i < (num_constraints - 1) then
              for_loop (i + 1)
            else if iis <> 0 then (
              let constraint_name =
                eer "get_str_attr_element"
                  (get_str_attr_element ~model ~name:GRB.str_attr_constrname
                    ~index:i)
              in
              pr "%s\n" constraint_name;
              removed := constraint_name :: !removed;
              az (del_constrs ~model ~num_del:1 ~ind:(to_i32a [|i|]))
            )
          in
          for_loop 0;
        
          pr "\n";
        
          az (optimize model);
          let status =
            eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
          in
          if status = GRB.unbounded then
            pr "The model cannot be solved because it is unbounded\n"
          else if status = GRB.optimal then (
            pr "\nThe following constraints were removed to get a feasible LP:\n";
            for i = 0 to (List.length !removed) - 1 do
              pr "%s " (List.nth !removed i)
            done;
            pr "\n"
          ) else if status <> GRB.inf_or_unbd && status <> GRB.infeasible then
            pr "Optimization stopped early with status %d\n" status
          else
            while_loop ()
        in
        while_loop ()
      )
      
let () = main ()
