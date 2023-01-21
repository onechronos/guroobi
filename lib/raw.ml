type t

external empty_env : unit -> (t, int) result = "gu_empty_env"

external set_int_param : t -> string -> int -> int = "gu_set_int_param"
external get_int_param : t -> string -> (int, int) result = "gu_get_int_param"

external set_str_param : t -> string -> string -> int = "gu_set_str_param"
external get_str_param : t -> string -> (string, int) result = "gu_get_str_param"

external set_float_param : t -> string -> float -> int = "gu_set_float_param"
external get_float_param : t -> string -> (float, int) result = "gu_get_float_param"
