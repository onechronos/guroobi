open Guroobi
open Raw
open Utils
open U

let sub_dim = 3
let dim = sub_dim * sub_dim

let main () =
  (* Create environment *)
  let env = eer "empty_env" (empty_env ()) in
  match Params.read_and_set env with
  | Error msg ->
      print_endline msg;
      exit 1
  | Ok () ->
      let filename = "data/sudoku1" in
      let in_channel =
        try open_in filename
        with Sys_error _ ->
          pr "Error: unable to open input file %s\n" filename;
          exit 1
      in
      let board = Array.make_matrix dim dim (-1) in
      for i = 0 to dim - 1 do
        try
          let inputline = input_line in_channel in
          if String.length inputline < 9 then (
            pr "Error: not enough board positions specified";
            exit 1);
          for j = 0 to dim - 1 do
            let char_value = Char.code inputline.[j] - Char.code '1' in
            if char_value < 0 || char_value >= dim then board.(i).(j) <- -1
            else board.(i).(j) <- char_value
          done
        with End_of_file ->
          pr "Error: unexpected end of file";
          exit 1
      done;

      close_in in_channel;

      let names = Array.make (dim * dim * dim) "" in
      let vtype = ca (dim * dim * dim) in
      let lb = fa (dim * dim * dim) in
      for i = 0 to dim - 1 do
        for j = 0 to dim - 1 do
          for v = 0 to dim - 1 do
            if board.(i).(j) = v then lb.{(i * dim * dim) + (j * dim) + v} <- 1.
            else lb.{(i * dim * dim) + (j * dim) + v} <- 0.;
            vtype.{(i * dim * dim) + (j * dim) + v} <- GRB.binary;
            names.((i * dim * dim) + (j * dim) + v) <-
              sp "x[%d,%d,%d]" i j (v + 1)
          done
        done
      done;

      az (set_int_param ~env ~name:GRB.int_par_outputflag ~value:0);
      az (set_str_param ~env ~name:GRB.str_par_logfile ~value:"sudoku.log");
      az (start_env env);

      let model =
        eer "new_model"
          (new_model ~env ~name:(Some "sudoku")
             ~num_vars:(dim * dim * dim)
             ~objective:None ~lower_bound:(Some lb) ~upper_bound:None
             ~var_type:(Some vtype) ~var_name:(Some names))
      in

      let ind = i32a dim in
      let value = fa dim in
      for i = 0 to dim - 1 do
        for j = 0 to dim - 1 do
          for v = 0 to dim - 1 do
            ind.{v} <- Int32.of_int ((i * dim * dim) + (j * dim) + v);
            value.{v} <- 1.0
          done;
          az
            (add_constr ~model ~num_nz:dim ~var_index:ind ~nz:value
               ~sense:GRB.equal ~rhs:1.0 ~name:None)
        done
      done;

      for v = 0 to dim - 1 do
        for j = 0 to dim - 1 do
          for i = 0 to dim - 1 do
            ind.{i} <- Int32.of_int ((i * dim * dim) + (j * dim) + v);
            value.{i} <- 1.0
          done;
          az
            (add_constr ~model ~num_nz:dim ~var_index:ind ~nz:value
               ~sense:GRB.equal ~rhs:1.0 ~name:None)
        done
      done;

      for v = 0 to dim - 1 do
        for i = 0 to dim - 1 do
          for j = 0 to dim - 1 do
            ind.{j} <- Int32.of_int ((i * dim * dim) + (j * dim) + v);
            value.{j} <- 1.0
          done;
          az
            (add_constr ~model ~num_nz:dim ~var_index:ind ~nz:value
               ~sense:GRB.equal ~rhs:1.0 ~name:None)
        done
      done;

      for v = 0 to dim - 1 do
        for ig = 0 to sub_dim - 1 do
          for jg = 0 to sub_dim - 1 do
            let count = ref 0 in
            for i = ig * sub_dim to ((ig + 1) * sub_dim) - 1 do
              for j = jg * sub_dim to ((jg + 1) * sub_dim) - 1 do
                ind.{!count} <- Int32.of_int ((i * dim * dim) + (j * dim) + v);
                value.{!count} <- 1.0;
                incr count
              done
            done;
            az
              (add_constr ~model ~num_nz:dim ~var_index:ind ~nz:value
                 ~sense:GRB.equal ~rhs:1.0 ~name:None)
          done
        done
      done;

      az (optimize model);

      az (write ~model ~path:"sudoku.lp");

      let status =
        eer "get_int_attr" (get_int_attr ~model ~name:GRB.int_attr_status)
      in
      pr "\nOptimization complete\n";
      if status = GRB.optimal then
        let obj_val =
          eer "get_float_attr" (get_float_attr ~model ~name:GRB.dbl_attr_objval)
        in
        pr "Optimal objective: %.4e\n" obj_val
      else if status = GRB.inf_or_unbd then
        pr "Model is infeasible or unbounded\n"
      else pr "optimization stopped early\n";
      pr "\n"

let () = main ()
