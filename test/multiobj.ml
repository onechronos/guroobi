open Gurobi
open Raw
open Utils
open U

let ground_set_size = 20
let n_subsets = 4
let budget = 12

let set =
  [|
    [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 1; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 1; 1; 0; 0; 0; 1; 1; 1; 0; 0; 0; 1; 1; 1; 0; 0 |];
  |]

let set_obj_priority = [| 3; 2; 2; 1 |]
let set_obj_weight = [| 1.; 0.25; 1.25; 1.0 |]

let fa_of_array ia =
  let n = Array.length ia in
  let f = fa n in
  for i = 0 to n - 1 do
    f.{i} <- float ia.(i)
  done;
  f

let pr = Printf.printf
let sp = Printf.sprintf

let main () =
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
    print_endline msg;
    exit 1
  | Ok () ->
    az (set_int_param env GRB.int_par_outputflag 0);
    az (start_env env);

    let model =
      eer "new_model"
        (new_model env (Some "multiobj") ground_set_size None None None None
           None)
    in

    (* initialize decision variables for ground set: x[e] == 1 if element e is
       chosen for the covering. *)
    for e = 0 to ground_set_size - 1 do
      az (set_char_attr_element model GRB.char_attr_vtype e GRB.binary);
      let name = sp "El%d" e in
      az (set_str_attr_element model GRB.str_attr_varname e name)
    done;

    let cind = i32a ground_set_size in
    let cval = fa ground_set_size in

    (* constraint: limit total number of elements to be picked to be at most
       [budget] *)
    for e = 0 to ground_set_size - 1 do
      cind.{e} <- Int32.of_int e;
      cval.{e} <- 1.
    done;
    az
      (add_constr model ground_set_size cind cval GRB.less_equal (float budget)
         (Some "Budget"));

    (* set global sense for ALL objectives *)
    az (set_int_attr model GRB.int_attr_modelsense GRB.maximize);

    (* limit how many solutions to collect *)
    az (set_int_model_param model GRB.int_par_poolsolutions 100);

    for i = 0 to n_subsets - 1 do
      let name = sp "Set%d" (i + 1) in
      let set_i = fa_of_array set.(i) in
      az
        (set_objective_n model i set_obj_priority.(i) set_obj_weight.(i)
           (1.0 +. float i)
           0.01 (Some name) 0.0 ground_set_size cind set_i)
    done;

    az (optimize model);

    let status = eer "get_int_attr" (get_int_attr model GRB.int_attr_status) in
    if
      status = GRB.inf_or_unbd || status = GRB.infeasible
      || status = GRB.unbounded
    then (
      print_endline
        "the model cannot be solved because it is infeasible or unbounded";
      exit 1)
    else if status != GRB.optimal then (
      Printf.printf "optimization was stopped with status %d\n" status;
      exit 1)
    else
      let x =
        eer "get_float_attr_array"
          (get_float_attr_array model GRB.dbl_attr_x 0 ground_set_size)
      in
      pr "selected elements in best solution:\n";

      pr "\t";
      for e = 0 to ground_set_size - 1 do
        if x.{e} >= 0.9 then Printf.printf "El%d " e
      done;

      let n_solutions =
        eer "get_int_attr" (get_int_attr model GRB.int_attr_solcount)
      in
      pr "\nnumber of solutions found: %d\n" n_solutions;

      let n_solutions = min 10 n_solutions in
      pr "objective values for first %d solutions:\n" n_solutions;
      for i = 0 to n_subsets - 1 do
        az (set_int_model_param model GRB.int_par_objnumber i);
        pr "\tSet %d:" i;
        for e = 0 to n_solutions - 1 do
          az (set_int_model_param model GRB.int_par_solutionnumber e);
          let obj_n =
            eer "get_dbl_attr" (get_float_attr model GRB.dbl_attr_objnval)
          in
          pr " %6g" obj_n
        done;
        pr "\n"
      done

let () = main ()
