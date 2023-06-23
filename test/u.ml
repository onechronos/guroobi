let pr = Printf.printf
let sp = Printf.sprintf
let az v = assert (v = 0)

let ee fname code =
  (match Guroobi.Utils.string_of_error code with
  | None -> pr "%s failed with unknown error\n%!" fname
  | Some msg -> pr "%s failed with error: %s\n%!" fname msg);
  exit 1

let eer fname = function Ok v -> v | Error code -> ee fname code
