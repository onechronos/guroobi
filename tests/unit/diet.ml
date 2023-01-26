open Gurobi
open Raw
open Utils

let pr_name_and_value model j =
  match get_float_attr_element model GRB.dbl_attr_x j with
  | Error code ->
    pr "get x[%d] failed with code=%d\n%!" j code;
    exit 1
  | Ok x_j -> (
    if x_j > 0.0001 then
      match get_str_attr_element model GRB.str_attr_varname j with
      | Error code -> pr "get varname[%d] failed with code=%d\n%!" j code
      | Ok var_name -> pr "%s %f\n" var_name x_j)

let print_solution model n_categories n_foods =
  match get_int_attr model GRB.int_attr_status with
  | Error code ->
    pr "get status failed with code=%d\n%!" code;
    exit 1
  | Ok status ->
    if status = GRB.optimal then (
      match get_float_attr model GRB.dbl_attr_objval with
      | Error code ->
        pr "get objval failed with code=%d\n%!" code;
        exit 1
      | Ok obj ->
        pr "Cost: %f\n\nBuy:\n" obj;
        for j = 0 to n_foods - 1 do
          match get_float_attr_element model GRB.dbl_attr_x j with
          | Error code ->
            pr "get x[%d] failed with code=%d\n%!" j code;
            exit 1
          | Ok x_j -> (
            if x_j > 0.0001 then
              match get_str_attr_element model GRB.str_attr_varname j with
              | Error code ->
                pr "get varname[%d] failed with code=%d\n%!" j code
              | Ok var_name -> pr "%s %f\n" var_name x_j)
        done;
        pr "\nNutrition:\n";
        for j = 0 to n_categories - 1 do
          let i = n_foods + j in
          match get_float_attr_element model GRB.dbl_attr_x i with
          | Error code ->
            pr "get x[%d] failed with code=%d\n%!" i code;
            exit 1
          | Ok x_i -> (
            match get_str_attr_element model GRB.str_attr_varname i with
            | Error code -> pr "get varname[%d] failed with code=%d\n%!" i code
            | Ok var_name -> pr "%s %f\n" var_name x_i)
        done)
    else print_endline "no solution"

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

    az (start_env env);

    let categories = [| "calories"; "protein"; "fat"; "sodium" |] in
    let n_categories = Array.length categories in
    let min_nutrition = [| 1800.; 91.; 0.; 0. |] in
    let max_nutrition = [| 2200.; GRB.infinity; 65.; 1779. |] in

    let foods =
      [|
        "hamburger";
        "chicken";
        "hot dog";
        "fries";
        "macaroni";
        "pizza";
        "salad";
        "milk";
        "ice cream";
      |]
    in
    let n_foods = Array.length foods in

    let cost = [| 2.49; 2.89; 1.50; 1.89; 2.09; 1.99; 2.49; 0.89; 1.59 |] in

    let nutrition_values =
      [|
        [| 410.; 24.; 26.; 730. |];
        [| 420.; 32.; 10.; 1190. |];
        [| 560.; 20.; 32.; 1800. |];
        [| 380.; 4.; 19.; 270. |];
        [| 320.; 12.; 10.; 930. |];
        [| 320.; 15.; 12.; 820. |];
        [| 320.; 31.; 12.; 1230. |];
        [| 100.; 8.; 2.5; 125. |];
        [| 330.; 8.; 10.; 180. |];
      |]
    in

    let model =
      match
        new_model env "diet" (n_foods + n_categories) None None None None None
      with
      | Error c ->
        pr "new_model result: %d\n%!" c;
        exit 1
      | Ok model -> model
    in

    az (set_int_attr model GRB.int_attr_modelsense GRB.minimize);

    for j = 0 to n_foods - 1 do
      az (set_float_attr_element model GRB.dbl_attr_obj j cost.(j));
      az (set_str_attr_element model GRB.str_attr_varname j foods.(j))
    done;

    for j = 0 to n_categories - 1 do
      az
        (set_float_attr_element model GRB.dbl_attr_lb (j + n_foods)
           min_nutrition.(j));
      az
        (set_float_attr_element model GRB.dbl_attr_ub (j + n_foods)
           max_nutrition.(j));
      az
        (set_str_attr_element model GRB.str_attr_varname (j + n_foods)
           categories.(j))
    done;

    (* nutrition constraints *)
    let c_beg = i32a n_categories in
    let c_ind = i32a (n_categories * (n_foods + 1)) in
    let c_val = fa (n_categories * (n_foods + 1)) in
    let rhs = fa n_categories in
    let sense = ca n_categories in

    let idx = ref 0 in

    for i = 0 to n_categories - 1 do
      c_beg.{i} <- Int32.of_int !idx;
      rhs.{i} <- 0.0;
      sense.{i} <- GRB.equal;
      for j = 0 to n_foods - 1 do
        c_ind.{!idx} <- Int32.of_int j;
        c_val.{!idx} <- nutrition_values.(j).(i);
        incr idx
      done;
      c_ind.{!idx} <- Int32.of_int (n_foods + i);
      c_val.{!idx} <- -1.0;
      incr idx
    done;

    let csc = { num_nz = !idx; xbeg = c_beg; xind = c_ind; xval = c_val } in

    az (add_constrs model n_categories (Some csc) sense rhs (Some categories));
    az (optimize model);
    let num_nz = 2 in
    let c_ind = i32a num_nz in
    let c_val = fa 2 in
    c_ind.{0} <- 7l;
    c_val.{0} <- 1.0;
    c_ind.{1} <- 8l;
    c_val.{1} <- 1.0;

    az
      (add_constr model num_nz c_ind c_val GRB.less_equal 6.0
         (Some "limit_dairy"));
    print_solution model n_categories n_foods;

    az (optimize model);
    print_solution model n_categories n_foods

let _ =
  let key_path_env_var = "GUROBI_ISV_KEY_PATH" in
  try
    let key_path = Unix.getenv key_path_env_var in
    main key_path
  with Not_found ->
    pr "environment variable %S not found\n%!" key_path_env_var;
    exit 1
