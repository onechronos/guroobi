type t = {
  int_params : (string * int) list;
  float_params : (string * float) list;
  string_params : (string * string) list;
}

exception ParserError of string

let read ?path () =
  try
    let gurobi_params_file_path =
      match path with Some p -> p | None -> Unix.getenv "GUROBI_PARAMS"
    in
    let j = Yojson.Safe.from_file gurobi_params_file_path in
    match j with
    | `Assoc key_value_list ->
      let vo key = List.assoc_opt key key_value_list in
      let int_params =
        match vo "int_params" with
        | None -> []
        | Some (`List i_list) ->
          List.rev_map
            (function
              | `List [ `String param; `Int i ] -> (param, i)
              | _ -> raise (ParserError gurobi_params_file_path))
            i_list
        | _ -> raise (ParserError gurobi_params_file_path)
      in
      let string_params =
        match vo "string_params" with
        | None -> []
        | Some (`List s_list) ->
          List.rev_map
            (function
              | `List [ `String param; `String s ] -> (param, s)
              | _ -> raise (ParserError gurobi_params_file_path))
            s_list
        | _ -> raise (ParserError gurobi_params_file_path)
      in
      let float_params =
        match vo "float_params" with
        | None -> []
        | Some (`List f_list) ->
          List.rev_map
            (function
              | `List [ `String param; `Float f ] -> (param, f)
              | _ -> raise (ParserError gurobi_params_file_path))
            f_list
        | _ -> raise (ParserError gurobi_params_file_path)
      in
      Ok { float_params; int_params; string_params }
    | _ -> Error "expecting a JSON record"
  with
  | Yojson.Json_error msg -> Error msg
  | Sys_error msg -> Error msg
  | ParserError path ->
    let s = Printf.sprintf "contents of file %S are invalid" path in
    Error s
  | Not_found ->
    (* environment variable pointing to a param configuration file not found;
       assume no params need to be set *)
    Ok { float_params = []; int_params = []; string_params = [] }

let read_and_set ?path env =
  match read ?path () with
  | Error s -> Error s
  | Ok t -> (
    let failures = [] in
    let failures =
      List.fold_left
        (fun failures (param, value) ->
          match Raw.set_int_param env param value with
          | 0 -> failures
          | _ -> param :: failures)
        failures t.int_params
    in
    let failures =
      List.fold_left
        (fun failures (param, value) ->
          match Raw.set_str_param env param value with
          | 0 -> failures
          | _ -> param :: failures)
        failures t.string_params
    in
    let failures =
      List.fold_left
        (fun failures (param, value) ->
          match Raw.set_float_param env param value with
          | 0 -> failures
          | _ -> param :: failures)
        failures t.float_params
    in
    match failures with
    | [] -> Ok ()
    | _ ->
      let failed_params_comma_sep = String.concat ", " failures in
      let msg =
        Printf.sprintf "failed to set following parameters: %s"
          failed_params_comma_sep
      in
      Error msg)
