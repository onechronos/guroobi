open Bigarray

let pr = Printf.printf
let fa n = Array1.create float64 c_layout n
let ca n = Array1.create char c_layout n
let i32a n = Array1.create int32 c_layout n
let az v = assert (v = 0)
