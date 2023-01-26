(** parse the Gurobi C API include file, to produce a similar OCaml source file with corresponding values *)

(* in the include file we are seraching for lines with the following structure

   #define GRB_XYZ_PQR abc /* optional comment */

   where GRB_XYZ is a C preprocessor (cpp) variable, and where abc is either an
   integer, string or float.

   We translate that line into an OCaml ml source file as follows:

   {{{ (* optional comment *) let xyz_pqr = abc }}} Note that we lower-case the
   CPP *)

module IncludeFileParser = struct
  open Angstrom

  let whitespace = skip_while (function ' ' | '\t' -> true | _ -> false)
  let not_whitespace = take_while1 (function ' ' | '\t' -> false | _ -> true)
  let any = take_while (function _ -> true)

  let alphanum_and_underscore =
    take_while1 (function
      | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
      | _ -> false)

  let lex p = p <* whitespace

  (* keys have the form eg BLAH_ASDF_XYZ *)
  let key = alphanum_and_underscore

  (* we expect values to be valid, C integer, float and string literals, to also
     be valid OCaml literals *)
  let value = not_whitespace

  (* a C comment starts with '/*', and if present, we want to capture it *)
  let comment = whitespace *> string "/*" *> any

  let kv_c =
    let some x = Some x in
    lift3
      (fun k v c -> (k, v, c))
      (lex key) (lex value)
      (lex (option None (comment >>| some)))

  (* we strip away the GRB_ prefix, which is in every key *)
  let define_line = string "#define GRB_" *> kv_c

  let parse str =
    try
      match parse_string ~consume:All define_line str with
      | Ok v -> Some v
      | Error _msg -> None
    with Failure _ -> None
end

module SM = Map.Make (String)

(* here, we remove the trailing close comment string "*/". I can't figure out
   how to remove it with the parse *)
let remove_trailing_close_comment s =
  let len = String.length s in
  if len >= 2 then
    if s.[len - 2] = '*' && s.[len - 1] = '/' then String.sub s 0 (len - 2)
    else s
  else s

let () =
  (* input: C include file *)
  let input_path = Sys.argv.(1) in
  (* output: OCaml ml file *)
  let output_path = Sys.argv.(2) in

  match
    Bos.OS.File.fold_lines
      (fun kvc_list line ->
        match IncludeFileParser.parse line with
        | Some kvc -> kvc :: kvc_list
        | None -> kvc_list)
      [] (Fpath.v input_path)
  with
  | Error (`Msg msg) ->
    print_endline msg;
    exit 1
  | Ok kvc_list ->
    let ch = open_out output_path in
    let pr x = Printf.fprintf ch x in
    (* There may be some duplicated define directives in the include file. Since
       we nevertheless add them to the OCaml file, the OCaml compiler may
       complain about unused values, and dune's setting of treating warnings as
       errors will yield compilation failures. By adding this compilation
       attribute to the file, we override this treatment, so that warnings do
       not trigger compilation errors. *)
    pr "[@@@warnerror \"-32\"]\n";

    List.iter
      (fun (k, v, c_opt) ->
        (match c_opt with
        | None -> ()
        | Some comment ->
          let comment_no_trail = remove_trailing_close_comment comment in
          pr "(* %s *)\n" comment_no_trail);
        let k_low = String.lowercase_ascii k in
        pr "let %s = %s\n" k_low v)
      kvc_list;
    close_out ch
