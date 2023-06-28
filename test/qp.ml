open Guroobi
open Raw
open Utils
open U

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
      eer "new_model" (new_model env (Some "qcp") 0 None None None None None)
    in

    (* variables and objective *)
    let num_vars = 3 in

    (let obj = fa num_vars in
     obj.{0} <- 1.;
     obj.{1} <- 0.;
     obj.{2} <- 0.;
     az (add_vars model num_vars None (Some obj) None None None None));

    (* maximize *)
    az (set_int_attr model GRB.int_attr_modelsense GRB.maximize);

    (* linear constraint: x + y + z = 1 *)
    (let num_nz = 3 in
     let c_ind = i32a num_nz in
     let c_val = fa num_nz in
     c_ind.{0} <- 0l;
     c_val.{0} <- 1.;
     c_ind.{1} <- 1l;
     c_val.{1} <- 1.;
     c_ind.{2} <- 2l;
     c_val.{2} <- 1.;

     az (add_constr model num_nz c_ind c_val GRB.equal 1.0 (Some "c0")));

    (* cone: x^2 + y^2 <= z^2 *)
    (let num_nz = 3 in
     let q_row = i32a num_nz in
     let q_col = i32a num_nz in
     let q_val = fa num_nz in
     q_row.{0} <- 0l;
     q_col.{0} <- 0l;
     q_val.{0} <- 1.;

     q_row.{1} <- 1l;
     q_col.{1} <- 1l;
     q_val.{1} <- 1.;

     q_row.{2} <- 2l;
     q_col.{2} <- 2l;
     q_val.{2} <- -1.;

     az
       (add_q_constr model None q_row q_col q_val GRB.less_equal 0. (Some "qc0")));

    (* rotated cone: x^2 <= yz *)
    (let num_nz = 2 in
     let q_row = i32a num_nz in
     let q_col = i32a num_nz in
     let q_val = fa num_nz in
     q_row.{0} <- 0l;
     q_col.{0} <- 0l;
     q_val.{0} <- 1.;

     q_row.{1} <- 1l;
     q_col.{1} <- 2l;
     q_val.{1} <- -1.;
     az
       (add_q_constr model None q_row q_col q_val GRB.less_equal 0. (Some "qc1")));

    az (optimize model);

    let status = eer "get_int_attr" (get_int_attr model GRB.int_attr_status) in
    if status = GRB.optimal then (
      let obj =
        eer "get_float_attr" (get_float_attr model GRB.dbl_attr_objval)
      in
      Printf.printf "objective: %.4e\n" obj;
      let sol =
        eer "get_float_attr_array"
          (get_float_attr_array model GRB.dbl_attr_x 0 num_vars)
      in
      Printf.printf "x=%.2f, y=%.2f, z=%.2f\n" sol.{0} sol.{1} sol.{2})
    else if status = GRB.inf_or_unbd then
      print_endline "model is infeasible or unbounded"
    else print_endline "optimization was stopped early"

let () = main ()
