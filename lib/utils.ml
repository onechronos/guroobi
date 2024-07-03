open Bigarray

(** [fa n] creates a [float] bigarray whose length is [n] *)
let fa n = Array1.create float64 c_layout n

(** [to_fa arr] creates a [float] bigarray from float array [arr] *)
let to_fa arr = 
  let n = Array.length arr in
  let fa_arr = fa n in
  for i = 0 to n - 1 do
    fa_arr.{i} <- arr.(i)
  done;
  fa_arr

(** [ca n] creates a [char] bigarray whose length is [n] *)
let ca n = Array1.create char c_layout n

(** [to_ca arr] creates a [char] bigarray from char array [arr] *)
let to_ca arr = 
  let n = Array.length arr in
  let ca_arr = ca n in
  for i = 0 to n - 1 do
    ca_arr.{i} <- arr.(i)
  done;
  ca_arr

(** [i32a n] creates an [i32a] bigarray whose length is [n] *)
let i32a n = Array1.create int32 c_layout n

(** [to_i32a arr] creates an [i32a] bigarray from int array [arr] *)
let to_i32a arr = 
  let n = Array.length arr in
  let i32a_arr = i32a n in
  for i = 0 to n - 1 do
    i32a_arr.{i} <- (Int32.of_int arr.(i))
  done;
  i32a_arr

(** [string_of_error code] returns a string representation of the error [code],
    if known, and [None] otherwise *)
let string_of_error code = List.assoc_opt code GRB.code_error_msg_assoc
