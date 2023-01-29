(** load ISV key details from a JSON file *)

type key = {
  name : string;
  app_name : string;
  expiration : int;
  v : string;
}

let get path =
  try
    let j = Yojson.Safe.from_file path in
    match j with
    | `Assoc key_value_list -> (
      let vo key = List.assoc_opt key key_value_list in
      match (vo "name", vo "app_name", vo "expiration", vo "v") with
      | ( Some (`String name),
          Some (`String app_name),
          Some (`Int expiration),
          Some (`String v) ) ->
        Ok { name; app_name; expiration; v }
      | _ -> Error "parse error")
    | _ -> Error "expecting a JSON record"
  with Yojson.Json_error msg | Sys_error msg -> Error msg

let sp = Printf.sprintf
let az x = assert (x = 0)

let set env path =
  match get path with
  | Error msg -> Error msg
  | Ok { name; app_name; expiration; v } -> (
    let open Raw in
    (* temporarily disable logging, if set; to know how to restoring the logging
       variable to its original value, we need to get it first *)
    match get_int_param env GRB.int_par_outputflag with
    | Error code -> Error (sp "failed to get OutputFlag, code=%d" code)
    | Ok prev_output_flag ->
      (* disable logging *)
      let code = set_int_param env GRB.int_par_outputflag 0 in
      if code = 0 then (
        az (set_str_param env "GURO_PAR_ISVNAME" name);
        az (set_str_param env "GURO_PAR_ISVAPPNAME" app_name);
        az (set_int_param env "GURO_PAR_ISVEXPIRATION" expiration);
        az (set_str_param env "GURO_PAR_ISVKEY" v);

        (* restore output flag to its original value *)
        let code = set_int_param env GRB.int_par_outputflag prev_output_flag in
        if code = 0 then Ok ()
        else
          Error
            (sp "failed to restore OutputFlag to %d; code=%d" prev_output_flag
               code))
      else Error (sp "failed to set OutputFlag to zero; code=%d" code))
