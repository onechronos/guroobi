open Gurobi
open Raw
open Utils

let main key_path =
  match Key.get key_path with
  | Error msg ->
    print_endline msg;
    exit 1
  | Ok { Key.name; app_name; expiration; v } ->
    let env =
      match empty_env () with
      | Error c ->
        pr "empty_env result: %d\n%!" c;
        exit 1
      | Ok env -> env
    in

    az (set_int_param env GRB.int_par_outputflag 0);

    az (set_str_param env "GURO_PAR_ISVNAME" name);
    az (set_str_param env "GURO_PAR_ISVAPPNAME" app_name);
    az (set_int_param env "GURO_PAR_ISVEXPIRATION" expiration);
    az (set_str_param env "GURO_PAR_ISVKEY" v);

    az (set_str_param env GRB.str_par_logfile "mip1.log");

    az (start_env env);

    let model =
      match new_model env (Some "mip1") 0 None None None None None with
      | Error c ->
        pr "new_model result: %d\n%!" c;
        exit 1
      | Ok model -> model
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

    az (add_vars model num_vars None (Some obj) None None (Some var_type) None);
    az (set_int_attr model GRB.int_attr_modelsense GRB.maximize);

    let num_nz = 3 in
    let c_ind = i32a num_nz in
    let c_val = fa num_nz in

    c_ind.{0} <- 0l;
    c_ind.{1} <- 1l;
    c_ind.{2} <- 2l;
    c_val.{0} <- 1.0;
    c_val.{1} <- 2.0;
    c_val.{2} <- 3.0;

    az (add_constr model 3 c_ind c_val GRB.less_equal 4.0 (Some "c0"));

    c_ind.{0} <- 0l;
    c_ind.{1} <- 1l;
    c_val.{0} <- 1.0;
    c_val.{1} <- 1.0;

    az (add_constr model 2 c_ind c_val GRB.greater_equal 1.0 (Some "c1"));
    az (optimize model);
    az (write model "mip1.lp");

    let status =
      match get_int_attr model GRB.int_attr_status with
      | Error code ->
        pr "error getting in attribute %s; code=%d\n%!" GRB.int_attr_status code;
        exit 1
      | Ok status -> status
    in

    if status = GRB.optimal then (
      let obj_val =
        match get_float_attr model GRB.dbl_attr_objval with
        | Error code ->
          pr "error getting attribute %s; code=%d\n%!" GRB.dbl_attr_objval code;
          exit 1
        | Ok obj_val -> obj_val
      in
      pr "obj: %.4e\n" obj_val;

      let sol =
        match get_float_attr_array model GRB.dbl_attr_x 0 3 with
        | Error code ->
          pr "error getting attribute %s; code=%d\n%!" GRB.dbl_attr_x code;
          exit 1
        | Ok sol -> sol
      in
      pr "x=%.0f\ny=%.0f\nz=%.0f\n" sol.{0} sol.{1} sol.{2})
    else if status = GRB.inf_or_unbd then
      pr "model is infeasible or unbounded\n"
    else pr "optimization stopped early\n"

let _ =
  let key_path_env_var = "GUROBI_ISV_KEY_PATH" in
  try
    let key_path = Unix.getenv key_path_env_var in
    main key_path
  with Not_found ->
    pr "environment variable %S not found\n%!" key_path_env_var;
    exit 1
