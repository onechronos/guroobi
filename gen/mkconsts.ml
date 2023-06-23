(** parse the Gurobi C API include file, to produce a similar OCaml source file with corresponding values *)

(* in the include file we are seraching for lines with the following structure

   #define GRB_XYZ_PQR abc /* optional comment */

   where GRB_XYZ is a C preprocessor (cpp) variable, and where abc is either an
   integer, string or float.

   We translate that line into an OCaml ml source file as follows:

   {{{ (* optional comment *) let xyz_pqr = abc }}} Note that we lower-case the
   CPP *)

module PoundDefineLineParse = struct
  open Re.Pcre

  let define str =
    try
      let kv =
        Scanf.sscanf str "#define GRB_%s %s" (fun key value -> (key, value))
      in
      Some kv
    with Scanf.Scan_failure _ | End_of_file -> None

  (* here, we remove the trailing close comment string "*/". *)
  let remove_close_comment s =
    let len = String.length s in
    if len >= 2 then
      if s.[len - 2] = '*' && s.[len - 1] = '/' then String.sub s 0 (len - 2)
      else s
    else s

  let low = String.lowercase_ascii
  let open_comment_rex = regexp "/\\*"

  let parse line =
    match split ~rex:open_comment_rex line with
    | before_open_comment :: after_open_comment :: _ -> (
      match define before_open_comment with
      | None -> None
      | Some (k, v) ->
        let comment = remove_close_comment after_open_comment in
        Some (low k, v, Some comment))
    | [ no_comment ] -> (
      match define no_comment with
      | None -> None
      | Some (k, v) -> Some (low k, v, None))
    | [] -> None
end

(* identify parameters beginning with GRB_ERROR_; the values associated with
   these keys are those returned by API functions. we identify them here in
   order to build a map from value (an integer) to a string that can be used in
   error messages. *)
let error_map =
  let e = String.length "error_" in
  fun kvc_list ->
    List.fold_left
      (fun code_message_assoc (k, code_s, (_ : string option)) ->
        if String.length k > e && StringLabels.sub k ~pos:0 ~len:e = "error_"
        then
          let msg_ = StringLabels.sub ~pos:e ~len:(String.length k - e) k in
          (* replace underscores with spaces *)
          let msg = String.map (function '_' -> ' ' | c -> c) msg_ in
          let code = int_of_string code_s in
          (code, msg) :: code_message_assoc
        else code_message_assoc)
      [] kvc_list

let () =
  (* input: C include file *)
  let input_path = Sys.argv.(1) in
  (* output: OCaml ml file *)
  let output_path = Sys.argv.(2) in

  match
    Bos.OS.File.fold_lines
      (fun kvc_list line ->
        match PoundDefineLineParse.parse line with
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
    pr
      "(** names and values associated with Gurobi parameters and attributes \
       *)\n\n";

    (* There may be some duplicated define directives in the include file. Since
       we nevertheless add them to the OCaml file, the OCaml compiler may
       complain about unused values, and dune's setting of treating warnings as
       errors will yield compilation failures. By adding this compilation
       attribute to the file, we override this treatment, so that warnings do
       not trigger compilation errors. *)
    pr "[@@@warnerror \"-32\"]\n";

    (* print a map (or rather, association list) from error codes to error
       messages *)
    let code_error_msg_assoc = error_map kvc_list in
    pr "let code_error_msg_assoc = [\n";
    List.iter
      (fun (code, error_msg) -> pr "  %d, %S;\n" code error_msg)
      code_error_msg_assoc;
    pr "]\n";

    List.iter
      (fun (k, v, c_opt) ->
        (match c_opt with
        | None -> ()
        | Some comment -> pr "(* %s *)\n" comment);
        pr "let %s = %s\n" k v)
      kvc_list;
    close_out ch
