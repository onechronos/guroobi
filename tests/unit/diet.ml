open Bigarray
open Gurobi
open Raw

let pr = Printf.printf
let fa n = Array1.create float64 c_layout n
let ca n = Array1.create char c_layout n
let i32a n = Array1.create int32 c_layout n
let inf = 1e100
let az v = assert (v = 0)

let main key_path =
  match Key.get key_path with
  | Error msg ->
    print_endline msg;
    exit 1
  | Ok { Key.name; app_name; expiration; v } ->
    print_endline "diet";

    let env =
      match empty_env () with
      | Error c ->
        pr "empty_env result: %d\n%!" c;
        exit 1
      | Ok env -> env
    in

    az (set_int_param env "OutputFlag" 1l);

    az (set_str_param env "GURO_PAR_ISVNAME" name);
    az (set_str_param env "GURO_PAR_ISVAPPNAME" app_name);
    az (set_int_param env "GURO_PAR_ISVEXPIRATION" (Int32.of_int expiration));
    az (set_str_param env "GURO_PAR_ISVKEY" v);

    az (start_env env);

    let categories = [| "calories"; "protein"; "fat"; "sodium" |] in
    let n_categories = Array.length categories in
    let min_nutrition = [| 1800.; 91.; 0.; 0. |] in
    let max_nutrition = [| 2200.; inf; 65.; 1779. |] in

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

    az (set_int_attr model "ModelSense" 1 (* TODO *));

    for j = 0 to n_foods - 1 do
      az (set_float_attr_element model "Obj" j cost.(j));
      az (set_str_attr_element model "VarName" j foods.(j))
    done;

    for j = 0 to n_categories - 1 do
      az (set_float_attr_element model "LB" (j + n_foods) min_nutrition.(j));
      az (set_float_attr_element model "UB" (j + n_foods) max_nutrition.(j))
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
      sense.{i} <- '=';
      for j = 0 to n_foods - 1 do
        c_ind.{!idx} <- Int32.of_int j;
        c_val.{!idx} <- nutrition_values.(j).(i);
        incr idx
      done;
      c_ind.{!idx} <- Int32.of_int (n_foods + i);
      c_val.{!idx} <- -1.0;
      incr idx
    done;

    az (add_constrs model n_categories !idx c_beg c_ind c_val sense rhs);
    az (optimize model);
    let num_nz = 2 in
    let c_ind = i32a num_nz in
    let c_val = fa 2 in
    c_ind.{0} <- 7l;
    c_val.{0} <- 1.0;
    c_ind.{1} <- 8l;
    c_val.{1} <- 1.0;

    az (add_constr model num_nz c_ind c_val '<' 6.0 (Some "limit_dairy"));

    let err = optimize model in
    ignore err;
    ()

let _ =
  let key_path = Sys.argv.(1) in
  main key_path
