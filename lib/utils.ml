open Bigarray

(** [fa n] creates a [float] bigarray whose length is [n] *)
let fa n = Array1.create float64 c_layout n

(** [ca n] creates a [char] bigarray whose length is [n] *)
let ca n = Array1.create char c_layout n

(** [i32a n] creates an [i32a] bigarray whose length is [n] *)
let i32a n = Array1.create int32 c_layout n

(** [string_of_error code] returns a string representation of the
    error [code], if known, and [None] otherwise *)
let string_of_error code = List.assoc_opt code GRB.code_error_msg_assoc
