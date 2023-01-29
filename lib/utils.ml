open Bigarray

let fa n = Array1.create float64 c_layout n
let ca n = Array1.create char c_layout n
let i32a n = Array1.create int32 c_layout n
let string_of_error code = List.assoc_opt code GRB.code_error_msg_assoc
