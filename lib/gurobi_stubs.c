#define CAML_NAME_SPACE

/* OCaml's C FFI */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/misc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/bigarray.h>

/* Gurobi */
#include "gurobi_c.h"

/* standard C */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

// naming convention: Gurobi's functions consist of multiple words,
// concatenated without a space, resulting in unfortunate
// readibility. Here, we improve on that by separating the words with
// underscores. For example, we would wrap a Gurobi function
// GRBpickupthemilk with function gu_pick_up_the_milk.

#define env_val(v) (*((GRBenv **) Data_custom_val(v)))
#define model_val(v) (*((GRBmodel **) Data_custom_val(v)))

void gu_env_finalize(value v_env)
{
  GRBenv* env = env_val( v_env );
  GRBfreeenv( env );
}

void gu_model_finalize(value v_model)
{
  GRBmodel* model = model_val( v_model );
  GRBfreemodel( model );
}

static struct custom_operations env_ops = {
  "gurobi.env",
  gu_env_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations model_ops = {
  "gurobi.model",
  gu_model_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* corresponding to OCaml Bigarray type (float, float64_elt, c_layout) Array1.t */
double* get_fa( value a, int n ) {
  CAMLparam1( a );
  assert( Caml_ba_array_val(a)->num_dims == 1 );
  assert( Caml_ba_array_val(a)->dim[0] == n );
  assert( (Caml_ba_array_val(a)->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64 );
  return Caml_ba_data_val(a);
}

/* corresponding to OCaml Bigarray type (int, int32_elt, c_layout) Array1.t */
int* get_i32a( value a, int n ) {
  CAMLparam1( a );
  assert( Caml_ba_array_val(a)->num_dims == 1 );
  assert( Caml_ba_array_val(a)->dim[0] == n );
  assert( (Caml_ba_array_val(a)->flags & CAML_BA_KIND_MASK) == CAML_BA_INT32 );
  return Caml_ba_data_val(a);
}

/* corresponding to OCaml Bigarray type (char, int8_unsigned_elt, c_layout) Array1.t */
char* get_ca( value a, int n ) {
  CAMLparam1( a );
  assert( Caml_ba_array_val(a)->num_dims == 1 );
  assert( Caml_ba_array_val(a)->dim[0] == n );
  assert( (Caml_ba_array_val(a)->flags & CAML_BA_KIND_MASK) == CAML_BA_CHAR );
  return Caml_ba_data_val(a);
}

// from a value representing an OCaml array of strings, return a
// heap-allocated C array of null-terminated C-strings.
const char** get_sa( value v_sa )
{
  CAMLparam1( v_sa );
  CAMLlocal1( v_i );
  int n = Wosize_val( v_sa );
  const char** sa = malloc( sizeof(char*) * n );

  for (int i = 0; i < n; i++ ) {
    v_i = Field( v_sa, i );
    sa[i] = String_val( v_i );

  }
  return sa;
}

CAMLprim value gu_empty_env( value unit )
{
  CAMLparam1( unit /* unused */ );
  CAMLlocal2( v_res, v_env );

  GRBenv* env = NULL;
  int error = GRBemptyenv(&env);
  if ( error == 0 ) {
    v_env = caml_alloc_custom(&env_ops, sizeof(void*), 0, 1);
    env_val(v_env) = env;

    // Ok t
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, v_env );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}
  
// start environment
CAMLprim value gu_start_env( value v_env )
{
  CAMLparam1( v_env );
  GRBenv* env = env_val(v_env);
  int error = GRBstartenv( env );
  CAMLreturn( Val_int( error ) );
}

// set and get integer parameters
CAMLprim value gu_set_int_param( value v_env, value v_name, value v_i )
{
  CAMLparam3( v_env, v_name, v_i );
  GRBenv* env = env_val(v_env);
  const char* name = String_val(v_name);
  int i = Int_val(v_i);
  int error = GRBsetintparam( env, name, i );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_int_param( value v_env, value v_name )
{
  CAMLparam2( v_env, v_name );
  CAMLlocal1( v_res );
  GRBenv* gurobi = env_val(v_env);
  const char* name = String_val(v_name);
  int i;
  int error = GRBgetintparam( gurobi, name, &i );
  if ( error == 0 ) {
    // Ok i
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, Val_int(i) );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}
  
// set and get string parameters 
CAMLprim value gu_set_str_param( value v_env, value v_name, value v_i )
{
  CAMLparam3( v_env, v_name, v_i );
  GRBenv* env = env_val(v_env);
  const char* name = String_val(v_name);
  const char* s = String_val(v_i);
  int error = GRBsetstrparam( env, name, s );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_str_param( value v_env, value v_name )
{
  CAMLparam2( v_env, v_name );
  CAMLlocal2( v_res, v_s );
  GRBenv* env = env_val(v_env);
  const char* name = String_val(v_name);
  char s[GRB_MAX_STRLEN];
  int error = GRBgetstrparam( env, name, s );
  if ( error == 0 ) {
    v_s = caml_copy_string( s );

    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, v_s );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

// set and get floating-point  parameters
CAMLprim value gu_set_float_param( value v_env, value v_name, value v_f )
{
  CAMLparam3( v_env, v_name, v_f );
  GRBenv* gurobi = env_val(v_env);
  const char* name = String_val(v_name);
  double f = Double_val(v_f);
  int error = GRBsetdblparam( gurobi, name, f );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_float_param( value v_env, value v_name )
{
  CAMLparam2( v_env, v_name );
  CAMLlocal2( v_res, v_f );
  GRBenv* env = env_val(v_env);
  const char* name = String_val(v_name);
  double f;
  int error = GRBgetdblparam( env, name, &f );
  if ( error == 0 ) {
    v_f = caml_copy_double(f);
    // Ok f
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, v_f );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

// create a new model
CAMLprim value gu_new_model(
 value v_env,
 value v_name,
 value v_num_vars,
 value v_objective_opt,
 value v_lower_bound_opt,
 value v_upper_bound_opt,
 value v_var_type_opt,
 value v_var_names_opt
)

{
  CAMLparam5( v_env, v_name, v_num_vars, v_objective_opt, v_lower_bound_opt );
  CAMLxparam3( v_upper_bound_opt, v_var_type_opt, v_var_names_opt );
  CAMLlocal5( v_model, v_res, v_var_names, v_objective, v_lower_bound );
  CAMLlocal2( v_upper_bound, v_var_type );

  GRBenv* env = env_val( v_env );
  const char* name = String_val( v_name );
  int num_vars = Int_val( v_num_vars );

  // objective
  double* objective = NULL;
  if ( Is_some( v_objective_opt ) ) {
    v_objective = Some_val( v_objective_opt );
    objective = get_fa( v_objective, num_vars );
  }

  // lower bound
  double* lower_bound = NULL;
  if ( Is_some( v_lower_bound_opt ) ) {
    v_lower_bound = Some_val( v_lower_bound_opt );
    lower_bound = get_fa( v_lower_bound, num_vars );
  }

  // upper bound
  double* upper_bound = NULL;
  if ( Is_some( v_upper_bound_opt ) ) {
    v_upper_bound = Some_val( v_upper_bound_opt );
    upper_bound = get_fa( v_upper_bound, num_vars );
  }

  // var type
  char* var_type = NULL;
  if ( Is_some( v_var_type_opt ) ) {
    v_var_type = Some_val( v_var_type_opt );
    var_type = get_ca( v_var_type, num_vars );
  }

  // var names
  const char** var_names = NULL;
  if ( Is_some( v_var_names_opt ) ) {
    v_var_names = Some_val( v_var_names_opt );
    assert ( Wosize_val( v_var_names ) == num_vars );
    var_names = get_sa( v_var_names );
  }

  GRBmodel* model;

  int error = GRBnewmodel( env,
			   &model,
			   name,
			   num_vars,
			   objective,
			   lower_bound,
			   upper_bound,
			   var_type,
			   (char**)var_names );
  
  if ( Is_some( v_var_names ) ) {
    free( var_names );
  }

  if ( error == 0 ) {
    v_model = caml_alloc_custom(&model_ops, sizeof(void*), 0, 1);
    model_val(v_model) = model;

    // Ok model
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, v_model );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}


// create a new model (bytecode support)
CAMLprim value gu_new_model_bc(value* v_args, int arg_n )
{
  assert( arg_n == 8 );
  return gu_new_model( v_args[0],
		       v_args[1],
		       v_args[2],
		       v_args[3],
		       v_args[4],
		       v_args[5],
		       v_args[6],
		       v_args[7]
		     );
}

// set a float attribute in an implicit array of such attributes
CAMLprim value gu_set_float_attr_element(value v_model, value v_name, value v_element, value v_new_value )
{
  CAMLparam4( v_model, v_name, v_element, v_new_value );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  double new_value = Double_val(v_new_value);
  int error = GRBsetdblattrelement( model, name, element, new_value );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_float_attr_element(value v_model, value v_name, value v_element )
{
  CAMLparam3( v_model, v_name, v_element );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  double d;
  int error = GRBgetdblattrelement( model, name, element, &d );
  if ( error == 0 ) {
    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, caml_copy_double(d) );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

// set and get a string attribute in an implicit array of such attributes
CAMLprim value gu_set_str_attr_element(value v_model, value v_name, value v_element, value v_new_value )
{
  CAMLparam4( v_model, v_name, v_element, v_new_value );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  const char* new_value = String_val(v_new_value);
  int error = GRBsetstrattrelement( model, name, element, new_value );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_str_attr_element(value v_model, value v_name, value v_element )
{
  CAMLparam3( v_model, v_name, v_element );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  char* s;
  int error = GRBgetstrattrelement( model, name, element, &s );
  if ( error == 0 ) {
    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, caml_copy_string(s) );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

// set and get an int attribute in an implict array of such attributes
CAMLprim value gu_set_int_attr_element(value v_model, value v_name, value v_element, value v_new_value )
{
  CAMLparam4( v_model, v_name, v_element, v_new_value );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  int new_value = Int_val(v_new_value);
  int error = GRBsetintattrelement( model, name, element, new_value );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_int_attr_element(value v_model, value v_name, value v_element )
{
  CAMLparam3( v_model, v_name, v_element );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  int i;
  int error = GRBgetintattrelement( model, name, element, &i );
  if ( error == 0 ) {
    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, Val_int(i) );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

// set and get a float attribute
CAMLprim value gu_set_float_attr(value v_model, value v_name, value v_new_value )
{
  CAMLparam3( v_model, v_name, v_new_value );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  double new_value = Double_val(v_new_value);
  int error = GRBsetdblattr( model, name, new_value );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_float_attr( value v_model, value v_name )
{
  CAMLparam2( v_model, v_name );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  double d;
  int error = GRBgetdblattr( model, name, &d );
  if ( error == 0 ) {
    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, caml_copy_double(d) );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

// set and get a string attribute
CAMLprim value gu_set_str_attr(value v_model, value v_name, value v_new_value )
{
  CAMLparam3( v_model, v_name, v_new_value );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  const char* new_value = String_val(v_new_value);
  int error = GRBsetstrattr( model, name, new_value );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_str_attr( value v_model, value v_name )
{
  CAMLparam2( v_model, v_name );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  char* s;
  int error = GRBgetstrattr( model, name, &s );
  if ( error == 0 ) {
    v_s = caml_copy_string( s );

    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, v_s );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

// set and get an int attribute
CAMLprim value gu_set_int_attr(value v_model, value v_name, value v_new_value )
{
  CAMLparam3( v_model, v_name, v_new_value );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int new_value = Int_val(v_new_value);
  int error = GRBsetintattr( model, name, new_value );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_int_attr( value v_model, value v_name )
{
  CAMLparam2( v_model, v_name );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int i;
  int error = GRBgetintattr( model, name, &i );
  if ( error == 0 ) {
    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, Val_int(i) );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  CAMLreturn( v_res );
}

CAMLprim value gu_add_constrs(
 value v_model,
 value v_num_constraints,
 value v_num_nz,
 value v_c_beg,
 value v_c_ind,
 value v_c_val,
 value v_sense,
 value v_rhs,
 value v_constr_names_opt
)
{
  CAMLparam5( v_model, v_num_constraints, v_num_nz, v_c_beg, v_c_ind );
  CAMLxparam4( v_c_val, v_sense, v_rhs, v_constr_names_opt );
  CAMLlocal1( v_constr_names );
  GRBmodel* model = model_val( v_model );
  int num_constraints = Int_val( v_num_constraints );
  int num_nz = Int_val( v_num_nz );
  int* c_beg = get_i32a( v_c_beg, num_constraints );
  int* c_ind = get_i32a( v_c_ind, num_nz );
  double* c_val = get_fa( v_c_val, num_nz );
  char* sense = get_ca( v_sense, num_constraints );
  double* rhs = get_fa( v_rhs, num_constraints );

  const char** constr_names = NULL;
  if ( Is_some( v_constr_names_opt ) ) {
    v_constr_names = Some_val( v_constr_names_opt );
    assert ( Wosize_val( v_constr_names ) == num_constraints );
    constr_names = get_sa( v_constr_names );
  }

  int error = GRBaddconstrs( model,
			     num_constraints,
			     num_nz,
			     c_beg,
			     c_ind,
			     c_val,
			     sense,
			     rhs,
			     (char**)constr_names
	                   );

  if ( Is_some( v_constr_names ) ) {
    free( constr_names );
  }


  CAMLreturn( Val_int( error ) );
}
  

CAMLprim value gu_add_constrs_bc(value* v_args, int arg_n )
{
  assert( arg_n == 9 );
  return gu_add_constrs( v_args[0],
		         v_args[1],
  		         v_args[2],
		         v_args[3],
		         v_args[4],
		         v_args[5],
		         v_args[6],
		         v_args[7],
		         v_args[8]
			 );
}  

CAMLprim value gu_add_constr(
 value v_model,
 value v_num_nz,
 value v_c_ind,
 value v_c_val,
 value v_sense,
 value v_rhs,
 value v_name
)
{
  CAMLparam5( v_model, v_num_nz, v_c_ind, v_c_val, v_sense );
  CAMLxparam2( v_rhs, v_name );
  GRBmodel* model = model_val( v_model );
  int num_nz = Int_val( v_num_nz );
  int* c_ind = get_i32a( v_c_ind, num_nz );
  double* c_val = get_fa( v_c_val, num_nz );
  char sense = Int_val( v_sense );
  double rhs = Double_val( v_rhs );
  const char* name = NULL;
  if (Is_some( v_name )) {
    name = String_val( v_name );
  }
  int error = GRBaddconstr( model, num_nz, c_ind, c_val, sense, rhs, name );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_add_constr_bc(value* v_args, int arg_n )
{
  assert( arg_n == 7 );
  return gu_add_constr( v_args[0],
		        v_args[1],
  		        v_args[2],
		        v_args[3],
		        v_args[4],
		        v_args[5],
		        v_args[6]
		      );
}  

CAMLprim value gu_optimize( value v_model )
{
  CAMLparam1( v_model );
  GRBmodel* model = model_val( v_model );
  int error = GRBoptimize( model );
  CAMLreturn( Val_int( error ) );
}
