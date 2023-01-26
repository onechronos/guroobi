open Bigarray

type fa = (float, float64_elt, c_layout) Array1.t
type ca = (char, int8_unsigned_elt, c_layout) Array1.t
type i32a = (int32, int32_elt, c_layout) Array1.t
type env

external empty_env : unit -> (env, int) result = "gu_empty_env"
external set_int_param : env -> string -> int -> int = "gu_set_int_param"
external get_int_param : env -> string -> (int, int) result = "gu_get_int_param"
external set_str_param : env -> string -> string -> int = "gu_set_str_param"

external get_str_param : env -> string -> (string, int) result
  = "gu_get_str_param"

external set_float_param : env -> string -> float -> int = "gu_set_float_param"

external get_float_param : env -> string -> (float, int) result
  = "gu_get_float_param"

external start_env : env -> int = "gu_start_env"

type model

external new_model :
  env ->
  string ->
  int ->
  fa option ->
  fa option ->
  fa option ->
  ca option ->
  string array option ->
  (model, int) result = "gu_new_model_bc" "gu_new_model"

external set_float_attr_element : model -> string -> int -> float -> int
  = "gu_set_float_attr_element"

external get_float_attr_element : model -> string -> int -> (float, int) result
  = "gu_get_float_attr_element"

external set_str_attr_element : model -> string -> int -> string -> int
  = "gu_set_str_attr_element"

external get_str_attr_element : model -> string -> int -> (string, int) result
  = "gu_get_str_attr_element"

external set_int_attr_element : model -> string -> int -> int -> int
  = "gu_set_int_attr_element"

external get_int_attr_element : model -> string -> int -> (int, int) result
  = "gu_get_int_attr_element"

external set_float_attr : model -> string -> float -> int = "gu_set_float_attr"

external get_float_attr : model -> string -> (float, int) result
  = "gu_get_float_attr"

external set_str_attr : model -> string -> string -> int = "gu_set_str_attr"

external get_str_attr : model -> string -> (string, int) result
  = "gu_get_str_attr"

external set_int_attr : model -> string -> int -> int = "gu_set_int_attr"
external get_int_attr : model -> string -> (int, int) result = "gu_get_int_attr"

type compressed = {
  num_nz : int;
  xbeg : i32a;
  xind : i32a;
  xval : fa;
}

external add_constrs :
  model -> int -> compressed option -> ca -> fa -> string array option -> int
  = "gu_add_constrs_bc" "gu_add_constrs"

external add_constr :
  model -> int -> i32a -> fa -> char -> float -> string option -> int
  = "gu_add_constr_bc" "gu_add_constr"

external add_vars :
  model ->
  int ->
  compressed option ->
  fa option ->
  fa option ->
  fa option ->
  ca option ->
  string array option ->
  int = "gu_add_vars_bc" "gu_add_vars"

external optimize : model -> int = "gu_optimize"
external write : model -> string -> int = "gu_write"
