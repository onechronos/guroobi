(** load ISV key details from a JSON file *)

let fo = List.find_opt

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
