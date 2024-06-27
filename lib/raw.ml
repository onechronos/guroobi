(** Build and solve optimization models using Gurobi *)

(** This interface largely follows the
    {{:https://www.gurobi.com/documentation/current/refman/c_api_details.html}
      Gurobi's C API}, with a few notable exceptions. First, the function names
    use snake-case for legibility, and without the [GRB] prefix. So, for
    example, the C function [GRBgetintparam] is exposed in this interface as
    just [get_int_param]. Also, C functions names containing [dbl] (to indicate
    that one of the arguments is a C [double]) are translated to function names
    containing [float]. For example, the C function [GRBgetdblattrelement] is
    represented here as [get_float_attr_element]. The ordering of the arguments
    of the C interface is largly preserved.

    Note that all functions that take arrays or bigarrays ([float], [char], and
    [int32]) as arguments may raise [Invalid_argument err] as a result of array
    sizes that are inconsistent with other inputs. Similarly, the exception will
    be raised when the dimension of bigarrays is bigger than [1]. *)

open Bigarray

type fa = (float, float64_elt, c_layout) Array1.t
type ca = (char, int8_unsigned_elt, c_layout) Array1.t
type i32a = (int32, int32_elt, c_layout) Array1.t

type env
(** Gurobi enviroment *)

type model
(** Gurobi model *)

external empty_env : unit -> (env, int) result = "gu_empty_env"

external set_int_param : env:env -> name:string -> value:int -> int
  = "gu_set_int_param"

external set_int_model_param : model:model -> name:string -> value:int -> int
  = "gu_set_int_model_param"

external get_int_param : env:env -> name:string -> (int, int) result
  = "gu_get_int_param"

external get_int_model_param : model:model -> name:string -> (int, int) result
  = "gu_get_int_model_param"

external set_str_param : env:env -> name:string -> value:string -> int
  = "gu_set_str_param"

external set_str_model_param : model:model -> name:string -> value:string -> int
  = "gu_set_str_model_param"

external get_str_param : env:env -> name:string -> (string, int) result
  = "gu_get_str_param"

external get_str_model_param : env:model -> name:string -> (string, int) result
  = "gu_get_str_model_param"

external set_float_param : env:env -> name:string -> value:float -> int
  = "gu_set_float_param"

external set_float_model_param :
  model:model -> name:string -> value:float -> int = "gu_set_float_model_param"

external get_float_param : env:env -> name:string -> (float, int) result
  = "gu_get_float_param"

external get_float_model_param : env:env -> name:string -> (float, int) result
  = "gu_get_float_model_param"

external start_env : env -> int = "gu_start_env"

external new_model :
  env:env ->
  name:string option ->
  num_vars:int ->
  objective:fa option ->
  lower_bound:fa option ->
  upper_bound:fa option ->
  var_type:ca option ->
  var_name:string array option ->
  (model, int) result = "gu_new_model_bc" "gu_new_model"

type read_model_result = FileNotFound | Ok of model | Error of int

external read_model : env:env -> path:string -> read_model_result
  = "gu_read_model"

external set_float_attr_element :
  model:model -> name:string -> index:int -> value:float -> int
  = "gu_set_float_attr_element"

external get_float_attr_element :
  model:model -> name:string -> index:int -> (float, int) result
  = "gu_get_float_attr_element"

external set_str_attr_element :
  model:model -> name:string -> index:int -> value:string -> int
  = "gu_set_str_attr_element"

external get_str_attr_element :
  model:model -> name:string -> index:int -> (string, int) result
  = "gu_get_str_attr_element"

external set_char_attr_element :
  model:model -> name:string -> index:int -> value:char -> int
  = "gu_set_char_attr_element"

external get_char_attr_element :
  model:model -> name:string -> index:int -> (char, int) result
  = "gu_get_char_attr_element"

external set_int_attr_element :
  model:model -> name:string -> index:int -> value:int -> int
  = "gu_set_int_attr_element"

external get_int_attr_element :
  model:model -> name:string -> index:int -> (int, int) result
  = "gu_get_int_attr_element"

external set_float_attr : model:model -> name:string -> value:float -> int
  = "gu_set_float_attr"

external get_float_attr : model:model -> name:string -> (float, int) result
  = "gu_get_float_attr"

external set_str_attr : model:model -> name:string -> value:string -> int
  = "gu_set_str_attr"

external get_str_attr : model:model -> name:string -> (string, int) result
  = "gu_get_str_attr"

external set_int_attr : model:model -> name:string -> value:int -> int
  = "gu_set_int_attr"

external get_int_attr : model:model -> name:string -> (int, int) result
  = "gu_get_int_attr"

external set_float_attr_array :
  model:model -> name:string -> start:int -> len:int -> values:fa -> int
  = "gu_set_float_attr_array"

external get_float_attr_array :
  model:model -> name:string -> start:int -> len:int -> (fa, int) result
  = "gu_get_float_attr_array"

external set_int_attr_array :
  model:model -> name:string -> start:int -> len:int -> values:i32a -> int
  = "gu_set_int_attr_array"

external get_int_attr_array :
  mmodel:model -> name:string -> start:int -> len:int -> (i32a, int) result
  = "gu_get_int_attr_array"

external set_char_attr_array :
  model:model -> name:string -> start:int -> len:int -> values:ca -> int
  = "gu_set_char_attr_array"

external get_char_attr_array :
  model:model -> name:string -> start:int -> len:int -> (ca, int) result
  = "gu_get_char_attr_array"

external get_str_attr_array :
  model:model ->
  name:string ->
  start:int ->
  len:int ->
  (string array, int) result = "gu_get_str_attr_array"

type compressed = {
  num_nz : int;  (** length of [xind] and [xval] *)
  xbeg : i32a;
      (** [xbeg.{i}] is the value of the index into arrays [xind] and [xval]
          where nonzeros related to constraint [i] begin *)
  xind : i32a;
      (** [xind.{k}] is the value of variable index associated with a nonzero *)
  xval : fa;  (** [xval.{k}] is the value of a nonzero *)
}
(** compressed sparse row (CSR) or column (CSC) *)

external add_constrs :
  model:model ->
  num:int ->
  matrix:compressed option ->
  sense:ca ->
  rhs:fa ->
  name:string array option ->
  int = "gu_add_constrs_bc" "gu_add_constrs"

external del_constrs :
  model:model -> num_del:int -> ind:i32a -> int
  = "gu_del_constrs"

external add_constr :
  model:model ->
  num_nz:int ->
  var_index:i32a ->
  nz:fa ->
  sense:char ->
  rhs:float ->
  name:string option ->
  int = "gu_add_constr_bc" "gu_add_constr"

external add_q_constr :
  model:model ->
  linear:(int * i32a * fa) option ->
  q_num_nz:int ->
  q_row:i32a ->
  q_col:i32a ->
  q_val:fa ->
  sense:char ->
  rhs:float ->
  name:string option ->
  int = "gu_add_q_constr_bc" "gu_add_q_constr"

external add_gen_constr_min :
  model:model ->
  name:string option ->
  res_var:int ->
  n_vars:int ->
  vars:i32a ->
  constant:float ->
  int = "gu_add_gen_constr_min_bc" "gu_add_gen_constr_min"

external add_gen_constr_max :
  model:model ->
  name:string option ->
  res_var:int ->
  n_vars:int ->
  vars:i32a ->
  constant:float ->
  int = "gu_add_gen_constr_max_bc" "gu_add_gen_constr_max"

external add_gen_constr_and :
  model:model ->
  name:string option ->
  res_var:int ->
  n_vars:int ->
  vars:i32a ->
  int = "gu_add_gen_constr_and"

external add_gen_constr_or :
  model:model ->
  name:string option ->
  res_var:int ->
  n_vars:int ->
  vars:i32a ->
  int = "gu_add_gen_constr_or"

external add_gen_constr_indicator :
  model:model ->
  name:string option ->
  bin_var:int ->
  bin_val:int ->
  n_vars:int ->
  ind:i32a option ->
  value:fa option ->
  sense:char ->
  rhs:float ->
  int = "gu_add_gen_constr_indicator_bc" "gu_add_gen_constr_indicator"

  external feas_relax :
  model:model ->
  relax_obj_type:int ->
  min_relax:int ->
  lb_pen:fa option ->
  ub_pen:fa option ->
  rhs_pen:fa option ->
  feas_obf_p:fa option ->
  int = "gu_feas_relax_bc" "gu_feas_relax"

external add_var :
  model:model ->
  num_nz:int ->
  v_ind:i32a option ->
  v_val:fa option ->
  obj:float ->
  lb:float ->
  ub:float ->
  v_type:char ->
  var_name:string option ->
  int = "gu_add_var_bc" "gu_add_var"

external add_vars :
  model:model ->
  num_vars:int ->
  matrix:compressed option ->
  objective:fa option ->
  lower_bound:fa option ->
  upper_bound:fa option ->
  var_type:ca option ->
  name:string array option ->
  int = "gu_add_vars_bc" "gu_add_vars"

external chg_coeffs :
  model:model ->
  num_chgs:int ->
  c_ind:i32a ->
  v_ind:i32a ->
  value:fa ->
  int = "gu_chg_coeffs"

external add_q_p_terms :
  model:model ->
  num_qnz:int ->
  q_row:i32a ->
  q_col:i32a ->
  q_val:fa ->
  int = "gu_add_q_p_terms"

external optimize : model -> int = "gu_optimize"
external write : model:model -> path:string -> int = "gu_write"
external compute_iis : model -> int = "gu_compute_iis"

external set_objective_n :
  model:model ->
  index:int ->
  priority:int ->
  weight:float ->
  abs_tol:float ->
  rel_tol:float ->
  name:string option ->
  constant:float ->
  num_nz:int ->
  var_index:i32a ->
  nz:fa ->
  int = "gu_set_objective_n_bc" "gu_set_objective_n"
