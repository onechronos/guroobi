open Guroobi
open Raw
open Utils
open U

(* In this example we show the use of general constraints for modeling
 * some common expressions. We use as an example a SAT-problem where we
 * want to see if it is possible to satisfy at least four (or all) clauses
 * of the logical form
 *
 * L = (x0 or ~x1 or x2)  and (x1 or ~x2 or x3)  and
 *     (x2 or ~x3 or x0)  and (x3 or ~x0 or x1)  and
 *     (~x0 or ~x1 or x2) and (~x1 or ~x2 or x3) and
 *     (~x2 or ~x3 or x0) and (~x3 or ~x0 or x1)
 *
 * We do this by introducing two variables for each literal (itself and its
 * negated value), one variable for each clause, one variable indicating
 * whether we can satisfy at least four clauses, and one last variable to
 * identify the minimum of the clauses (so if it is one, we can satisfy all
 * clauses). Then we put these last two variables in the objective.
 * The objective function is therefore
 *
 * maximize Obj0 + Obj1
 *
 *  Obj0 = MIN(Clause1, ... , Clause8)
 *  Obj1 = 1 -> Clause1 + ... + Clause8 >= 4
 *
 * thus, the objective value will be two if and only if we can satisfy all
 * clauses; one if and only if at least four but not all clauses can be satisfied,
 * and zero otherwise.
 *)



let n_literals = 4
let n_clauses = 8
let n_obj = 2
let n_vars = (2 * n_literals + n_clauses + n_obj)
let lit n = n
let notlit n = n_literals + n
let cla n = 2 * n_literals + n
let obj n = 2 * n_literals + n_clauses + n

let clauses =
  [|
    [| (lit 0); (notlit 1); (lit 2) |];
    [| (lit 1); (notlit 2); (lit 3) |];
    [| (lit 2); (notlit 3); (lit 0) |];
    [| (lit 3); (notlit 0); (lit 1) |];
    [| (notlit 0); (notlit 1); (lit 2) |];
    [| (notlit 1); (notlit 2); (lit 3) |];
    [| (notlit 2); (notlit 3); (lit 0) |];
    [| (notlit 3); (notlit 0); (lit 1) |];
  |]

let main () =

  (* Create environment *)

  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"genconstr.log");
      az (start_env env);

      (* Create an empty model *)

      let model =
        eer "new_mode"
          (new_model ~env ~name:(Some "genconstr") ~num_vars:n_vars ~objective:None
             ~lower_bound:None ~upper_bound:None ~var_type:None ~var_name:None)
      in

      for i=0 to n_literals - 1 do
        let col = lit i in
        let buffer = sp "X%d" i in
        az (set_char_attr_element ~model ~name:"VType" ~index:col ~value:GRB.binary);
        az (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:buffer);

        let col = notlit i in
        let buffer = sp "notX%d" i in
        az (set_char_attr_element ~model ~name:"VType" ~index:col ~value:GRB.binary);
        az (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:buffer)
      done;

      for i=0 to n_clauses - 1 do
        let col = cla i in
        let buffer = sp "Clause%d" i in
        az (set_char_attr_element ~model ~name:"VType" ~index:col ~value:GRB.binary);
        az (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:buffer)
      done;

      for i=0 to n_obj - 1 do
        let col = obj i in
        let buffer = sp "Obj%d" i in
        az (set_char_attr_element ~model ~name:"VType" ~index:col ~value:GRB.binary);
        az (set_str_attr_element ~model ~name:"VarName" ~index:col ~value:buffer);
        az (set_float_attr_element ~model ~name:"Obj" ~index:col ~value:1.0)
      done;

      let cind = i32a n_vars in
      let cval = fa n_vars in
      for i=0 to n_literals - 1 do
        let buffer = sp "CNSTR_X%d" i in
        cind.{0} <- Int32.of_int (lit i);
        cind.{1} <- Int32.of_int (notlit i);
        cval.{0} <- 1.;
        cval.{1} <- 1.;
        az (add_constr ~model ~num_nz:2 ~var_index:cind ~nz:cval
          ~sense:GRB.equal ~rhs:1.0 ~name:(Some buffer))
      done;

      for i=0 to n_clauses - 1 do
        let buffer = sp "CNSTR_Clause%d" i in
        az (add_gen_constr_or ~model ~name:(Some buffer) ~res_var:(cla i) 
          ~n_vars:3 ~vars:(to_i32a clauses.(i)))
      done;

      for i=0 to n_clauses - 1 do
        cind.{i} <- Int32.of_int (cla i);
        cval.{i} <- 1.
      done;
      az (add_gen_constr_min ~model ~name:(Some "CNSTR_Obj0") ~res_var:(obj 0)
          ~n_vars:n_clauses ~vars:cind ~constant:GRB.infinity);
      az (add_gen_constr_indicator ~model ~name:(Some "CNSTR_Obj1") ~bin_var:(obj 1)
          ~bin_val:1 ~n_vars:n_clauses ~ind:(Some cind) ~value:(Some cval) ~sense:GRB.greater_equal ~rhs:4.0);



      az (set_int_attr ~model ~name:GRB.int_attr_modelsense ~value:GRB.maximize);

      az (write ~model ~path:"genconstr.lp");
      az (write ~model ~path:"genconstr.mps");

      az (optimize model);

      let status =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
      in
      if status = GRB.inf_or_unbd || status = GRB.infeasible || status = GRB.unbounded then
        pr "the model cannot be solved because it is infeasible or unbounded\n"
      else if status <> GRB.optimal then
        Printf.printf "optimization was stopped with status %d\n" status
      else (
        let obj_val =
          eer "get_float_attr" (get_float_attr ~model ~name:GRB.dbl_attr_objval)
        in
        if obj_val > 1.9 then
          pr "Logical expression is satisfiable\n"
        else if obj_val > 0.9 then
          pr "At least four clauses can be satisfied\n"
        else
          pr "At most three clauses may be satisfied\n" 
      )

let () = main ()
