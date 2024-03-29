(** Build and solve optimization models using Gurobi *)

(** This interface largely follows the
    {{:https://www.gurobi.com/documentation/current/refman/c_api_details.html}Gurobi's
    C API}, with a few notable exceptions. First, the function names
    use snake-case for legibility, and without the [GRB] prefix. So,
    for example, the C function [GRBgetintparam] is exposed in this
    interface as just [get_int_param]. Also, C functions names
    containing [dbl] (to indicate that one of the arguments is a C
    [double]) are translated to function names containing [float]. For
    example, the C function [GRBgetdblattrelement] is represented here
    as [get_float_attr_element]. The ordering of the arguments of the
    C interface is largly preserved.

    Note that all functions that take arrays or bigarrays ([float],
    [char], and [int32]) as arguments may raise [Invalid_argument err]
    as a result of array sizes that are inconsistent with other
    inputs. Similarly, the exception will be raised when the dimension
    of bigarrays is bigger than [1].
*)

open Bigarray

type fa = (float, float64_elt, c_layout) Array1.t
type ca = (char, int8_unsigned_elt, c_layout) Array1.t
type i32a = (int32, int32_elt, c_layout) Array1.t

type env
(** Gurobi enviroment *)

type model
(** Gurobi model *)

external empty_env : unit -> (env, int) result = "gu_empty_env"
external set_int_param : env -> string -> int -> int = "gu_set_int_param"

external set_int_model_param : model -> string -> int -> int
  = "gu_set_int_model_param"

external get_int_param : env -> string -> (int, int) result = "gu_get_int_param"

external get_int_model_param : model -> string -> (int, int) result
  = "gu_get_int_model_param"

external set_str_param : env -> string -> string -> int = "gu_set_str_param"

external set_str_model_param : model -> string -> string -> int
  = "gu_set_str_model_param"

external get_str_param : env -> string -> (string, int) result
  = "gu_get_str_param"

external get_str_model_param : model -> string -> (string, int) result
  = "gu_get_str_model_param"

external set_float_param : env -> string -> float -> int = "gu_set_float_param"

external set_float_model_param : model -> string -> float -> int
  = "gu_set_float_model_param"

external get_float_param : env -> string -> (float, int) result
  = "gu_get_float_param"

external get_float_model_param : env -> string -> (float, int) result
  = "gu_get_float_model_param"

external start_env : env -> int = "gu_start_env"

external new_model :
  env ->
  string option ->
  int ->
  fa option ->
  fa option ->
  fa option ->
  ca option ->
  string array option ->
  (model, int) result = "gu_new_model_bc" "gu_new_model"

type read_model_result =
  | FileNotFound
  | Ok of model
  | Error of int

external read_model : env -> string -> read_model_result = "gu_read_model"

external set_float_attr_element : model -> string -> int -> float -> int
  = "gu_set_float_attr_element"

external get_float_attr_element : model -> string -> int -> (float, int) result
  = "gu_get_float_attr_element"

external set_str_attr_element : model -> string -> int -> string -> int
  = "gu_set_str_attr_element"

external get_str_attr_element : model -> string -> int -> (string, int) result
  = "gu_get_str_attr_element"

external set_char_attr_element : model -> string -> int -> char -> int
  = "gu_set_char_attr_element"

external get_char_attr_element : model -> string -> int -> (char, int) result
  = "gu_get_char_attr_element"

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

external get_float_attr_array :
  model -> string -> int -> int -> (fa, int) result = "gu_get_float_attr_array"

external get_int_attr_array :
  model -> string -> int -> int -> (i32a, int) result = "gu_get_int_attr_array"

external get_char_attr_array : model -> string -> int -> int -> (ca, int) result
  = "gu_get_char_attr_array"

external get_str_attr_array :
  model -> string -> int -> int -> (string array, int) result
  = "gu_get_str_attr_array"

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

external add_q_constr :
  model ->
  (int * i32a * fa) option ->
  int ->
  i32a ->
  i32a ->
  fa ->
  char ->
  float ->
  string option ->
  int = "gu_add_q_constr_bc" "gu_add_q_constr"

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
external compute_iis : model -> int = "gu_compute_iis"

external set_objective_n :
  model ->
  int ->
  int ->
  float ->
  float ->
  float ->
  string option ->
  float ->
  int ->
  i32a ->
  fa ->
  int = "gu_set_objective_n_bc" "gu_set_objective_n"
