open Guroobi
open Raw
open Utils
open U

let n_shifts = 14
let n_workers = 8

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

let workers = [| "Amy"; "Bob"; "Cathy"; "Dan"; "Ed"; "Fred"; "Gu"; "Tobi" |]

let shift_requirements =
  [| 3.; 2.; 4.; 4.; 5.; 6.; 5.; 2.; 2.; 3.; 4.; 6.; 7.; 5. |]

let availability =
  [|
    [| 0.; 1.; 1.; 0.; 1.; 0.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1. |];
    [| 1.; 1.; 0.; 0.; 1.; 1.; 0.; 1.; 0.; 0.; 1.; 0.; 1.; 0. |];
    [| 0.; 0.; 1.; 1.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1. |];
    [| 0.; 1.; 1.; 0.; 1.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1.; 1. |];
    [| 1.; 1.; 1.; 1.; 1.; 0.; 1.; 1.; 1.; 0.; 1.; 0.; 1.; 1. |];
    [| 1.; 1.; 1.; 0.; 0.; 1.; 0.; 1.; 1.; 0.; 0.; 1.; 1.; 1. |];
    [| 0.; 1.; 1.; 1.; 0.; 1.; 1.; 0.; 1.; 1.; 1.; 0.; 1.; 1. |];
    [| 1.; 1.; 1.; 0.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1. |];
  |]

let xcol w s = (n_shifts * w) + s
let slack_col s = n_shifts * n_workers + s
let tot_slack_col = n_shifts * (n_workers + 1)
let tot_shifts_col w = n_shifts * (n_workers + 1) + 1 + w
let min_shift_col = (n_shifts+1)*(n_workers+1)
let max_shift_col = (n_shifts+1)*(n_workers+1)+1

let main () =
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"workforce5.log");
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);

      az (start_env env);
      let model =
        eer "new_model"
          (new_model ~env ~name:(Some "workforce5")
             ~num_vars:((n_workers+1) * (n_shifts+1) + 2) ~objective:None ~lower_bound:None
             ~upper_bound:None ~var_type:None ~var_name:None)
      in

      for w = 0 to n_workers - 1 do
        for s = 0 to n_shifts - 1 do
          let col = xcol w s in
          let vname = sp "%s.%s" workers.(w) shifts.(s) in
          az
            (set_char_attr_element ~model ~name:"VType" ~index:col
               ~value:GRB.binary);
          az
            (set_float_attr_element ~model ~name:"UB" ~index:col
               ~value:availability.(w).(s));
          az
            (set_str_attr_element ~model ~name:GRB.str_attr_varname ~index:col
               ~value:vname)
        done
      done;
      
      for s = 0 to n_shifts - 1 do
        let vname = sp "%sSlack" shifts.(s) in
        az (set_str_attr_element ~model ~name:"VarName" ~index:(slack_col s) ~value:vname);
      done;
      
      az (set_str_attr_element ~model ~name:"VarName" ~index:tot_slack_col ~value:"totSlack");

      for w = 0 to n_workers - 1 do
        let vname = sp "%sTotShifts" workers.(w) in
        az (set_str_attr_element ~model ~name:"VarName" ~index:(tot_shifts_col w) ~value:vname);
      done;

      az (set_str_attr_element ~model ~name:"VarName" ~index:min_shift_col ~value:"minShifts");
      az (set_str_attr_element ~model ~name:"VarName" ~index:max_shift_col ~value:"maxShifts");

      let sense = ca n_shifts in

      let compressed =
        { num_nz = (n_shifts * (n_workers + 1)); 
          xbeg = i32a n_shifts; 
          xind = i32a (n_shifts * (n_workers + 1)); 
          xval = fa (n_shifts * (n_workers + 1)) 
        }
      in

      let idx = ref 0 in
      for s = 0 to n_shifts - 1 do
        compressed.xbeg.{s} <- Int32.of_int !idx;
        sense.{s} <- GRB.equal;
        for w = 0 to n_workers - 1 do
          compressed.xind.{!idx} <- Int32.of_int (xcol w s);
          compressed.xval.{!idx} <- 1.0;
          incr idx
        done;
        compressed.xind.{!idx} <- Int32.of_int (slack_col s);
        compressed.xval.{!idx} <- 1.0;
        incr idx
      done;
      az
        (add_constrs ~model ~num:n_shifts ~matrix:(Some compressed) ~sense
           ~rhs:(to_fa shift_requirements) ~name:(Some shifts));

      let cind = i32a (n_shifts * (n_workers + 1)) in
      let cval = fa (n_shifts * (n_workers + 1)) in
      let idx = ref 0 in
      for s = 0 to n_shifts - 1 do
        cind.{!idx} <- Int32.of_int (slack_col s);
        cval.{!idx} <- 1.0;
        incr idx
      done;
      cind.{!idx} <- Int32.of_int tot_slack_col;
      cval.{!idx} <- -1.0;
      incr idx;
      az (add_constr ~model ~num_nz:!idx ~var_index:cind ~nz:cval ~sense:GRB.equal ~rhs:0.0 ~name:(Some "totSlack"));

      for w = 0 to n_workers - 1 do
        let idx = ref 0 in
        for s = 0 to n_shifts - 1 do
          cind.{!idx} <- Int32.of_int (xcol w s);
          cval.{!idx} <- 1.0;
          incr idx
        done;
        let cname = sp "totShifts%s" workers.(w) in
        cind.{!idx} <- Int32.of_int (tot_shifts_col w);
        cval.{!idx} <- -1.0;
        incr idx;
        az (add_constr ~model ~num_nz:!idx ~var_index:cind ~nz:cval ~sense:GRB.equal ~rhs:0.0 ~name:(Some cname))
      done;

      for w = 0 to n_workers - 1 do
        cind.{w} <- Int32.of_int (tot_shifts_col w)
      done;
      az (add_gen_constr_min ~model ~name:None ~res_var:min_shift_col 
        ~n_vars:n_workers ~vars:cind ~constant:GRB.infinity);
      az (add_gen_constr_max ~model ~name:None ~res_var:max_shift_col 
        ~n_vars:n_workers ~vars:cind ~constant:(-.GRB.infinity));

      az (set_int_attr ~model ~name:GRB.int_attr_modelsense ~value:GRB.minimize);

      cind.{0} <- Int32.of_int tot_slack_col;
      cval.{0} <- 1.0;
      az (set_objective_n ~model ~index:0 ~priority:2 ~weight:1.0 ~abs_tol:2.0 ~rel_tol:0.10
        ~name:(Some "TotalSlack") ~constant:0.0 ~num_nz:1 ~var_index:cind ~nz:cval);
      
      cind.{0} <- Int32.of_int max_shift_col;
      cval.{0} <- 1.0;
      cind.{1} <- Int32.of_int min_shift_col;
      cval.{1} <- -1.0;
      az (set_objective_n ~model ~index:1 ~priority:1 ~weight:1.0 ~abs_tol:0.0 ~rel_tol:0.0
        ~name:(Some "Fairness") ~constant:0.0 ~num_nz:2 ~var_index:cind ~nz:cval);

      az (write ~model ~path:"workforce5.lp");
      az (write ~model ~path:"workforce5.mps");
      
      az (optimize model);
      let status =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
      in
      if
        status = GRB.inf_or_unbd || status = GRB.infeasible
        || status = GRB.unbounded
      then 
        pr "the model cannot be solved because it is infeasible or unbounded\n"
      else if status != GRB.optimal then
        Printf.printf "optimization was stopped with status %d\n" status
      else (
        let sol =
          eer "get_float_attr_element" (get_float_attr_element ~model ~name:"X" ~index:tot_slack_col)
        in
        pr "\nTotal slack required: %f\n" sol;
        
        for w = 0 to n_workers - 1 do
          let sol =
            eer "get_float_attr_element" (get_float_attr_element ~model ~name:"X" ~index:(tot_shifts_col w))
          in
          pr "%s worked %f shifts\n" workers.(w) sol
        done;
        pr "\n"
      )
      
      
let () = main ()
