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
#include <string.h>
#include <unistd.h>

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
static double* get_fa( value a, int min_n ) {
  if ( (Caml_ba_array_val(a)->num_dims == 1) &&
       (Caml_ba_array_val(a)->dim[0] >= min_n) &&
       ((Caml_ba_array_val(a)->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64)
       ) {
    return Caml_ba_data_val(a);
  }
  else {
    return NULL;
  }
}

/* corresponding to OCaml Bigarray type (int, int32_elt, c_layout) Array1.t */
static int* get_i32a( value a, int min_n ) {
  if ( (Caml_ba_array_val(a)->num_dims == 1) &&
       (Caml_ba_array_val(a)->dim[0] >= min_n ) &&
       ((Caml_ba_array_val(a)->flags & CAML_BA_KIND_MASK) == CAML_BA_INT32)
       ) {
    return Caml_ba_data_val(a);
  }
  else {
    return NULL;
  }
}

/* corresponding to OCaml Bigarray type (char, int8_unsigned_elt, c_layout) Array1.t */
static char* get_ca( value a, int min_n ) {
  if ( (Caml_ba_array_val(a)->num_dims == 1 ) &&
       (Caml_ba_array_val(a)->dim[0] >= min_n) &&
       ((Caml_ba_array_val(a)->flags & CAML_BA_KIND_MASK) == CAML_BA_CHAR )
       ) {
    return Caml_ba_data_val(a);
  }
  else {
    return NULL;
  }
}

// from a value representing an OCaml array of strings, return a
// heap-allocated C array of null-terminated C-strings.
static const char** get_sa( value v_sa, int expected_n )
{
  int n = Wosize_val( v_sa );
  if ( n == expected_n ) {
    const char** sa = malloc( sizeof(char*) * n );

    for (int i = 0; i < n; i++ ) {
      value v_i = Field( v_sa, i );
      sa[i] = String_val( v_i );
    }
    return sa;
  }
  else {
    return NULL;
  }
}

CAMLprim value gu_empty_env( value unit )
{
  CAMLparam1( unit /* unused */ );
  CAMLlocal2( v_res, v_env );

  GRBenv* env = NULL;
  int error = GRBemptyenv(&env);
  if ( error == 0 ) {
    v_env = caml_alloc_custom(&env_ops, sizeof(GRBenv*), 0, 1);
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

CAMLprim value gu_set_int_model_param( value v_model, value v_name, value v_i )
{
  CAMLparam3( v_model, v_name, v_i );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int i = Int_val(v_i);
  GRBenv* env = GRBgetenv( model );
  assert( env != NULL );
  int error = GRBsetintparam( env, name, i );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_int_param( value v_env, value v_name )
{
  CAMLparam2( v_env, v_name );
  CAMLlocal1( v_res );
  GRBenv* env = env_val(v_env);
  const char* name = String_val(v_name);
  int i;
  int error = GRBgetintparam( env, name, &i );
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
  
CAMLprim value gu_get_int_model_param( value v_model, value v_name )
{
  CAMLparam2( v_model, v_name );
  CAMLlocal1( v_res );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int i;
  GRBenv* env = GRBgetenv( model );
  assert ( env != NULL );
  int error = GRBgetintparam( env, name, &i );
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

CAMLprim value gu_set_str_model_param( value v_model, value v_name, value v_i )
{
  CAMLparam3( v_model, v_name, v_i );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  const char* s = String_val(v_i);
  GRBenv* env = GRBgetenv( model );
  assert ( env != NULL );
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

CAMLprim value gu_get_str_model_param( value v_model, value v_name )
{
  CAMLparam2( v_model, v_name );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  char s[GRB_MAX_STRLEN];
  GRBenv* env = GRBgetenv( model );
  assert ( env != NULL );
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
  GRBenv* env = env_val(v_env);
  const char* name = String_val(v_name);
  double f = Double_val(v_f);
  int error = GRBsetdblparam( env, name, f );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_set_float_model_param( value v_model, value v_name, value v_f )
{
  CAMLparam3( v_model, v_name, v_f );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  double f = Double_val(v_f);
  GRBenv* env = GRBgetenv( model );
  assert( env != NULL );
  int error = GRBsetdblparam( env, name, f );
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

CAMLprim value gu_get_float_model_param( value v_model, value v_name )
{
  CAMLparam2( v_model, v_name );
  CAMLlocal2( v_res, v_f );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  double f;
  GRBenv* env = GRBgetenv( model );
  assert( env != NULL );
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

// get float attribute array
CAMLprim value gu_get_float_attr_array( value v_model, value v_name, value v_start, value v_len )
{
  CAMLparam4( v_model, v_name, v_start, v_len );
  CAMLlocal2( v_array, v_res );
  GRBmodel* model = model_val( v_model );
  const char* name = String_val( v_name );
  int start = Int_val( v_start );
  int len = Int_val( v_len );

  long ba_length = len - start;
  if ( ba_length <= 0 ) {
    caml_invalid_argument( "get_float_attr_array:(start,len)" );
  }
  else {
    v_array = caml_ba_alloc_dims(CAML_BA_FLOAT64 | CAML_BA_C_LAYOUT, 1, NULL, ba_length);
    double* array = Caml_ba_data_val(v_array);
    int error = GRBgetdblattrarray( model, name, start, len, array );

    if ( error == 0 ) {
      // Ok v_array
      v_res = caml_alloc(1, 0);
      Store_field( v_res, 0, v_array );
    }
    else {
      // Error code
      v_res = caml_alloc(1, 1);
      Store_field( v_res, 0, Val_int(error) );
    }
    CAMLreturn( v_res );
  }
}

// get int attribute array
CAMLprim value gu_get_int_attr_array( value v_model, value v_name, value v_start, value v_len )
{
  CAMLparam4( v_model, v_name, v_start, v_len );
  CAMLlocal2( v_array, v_res );
  GRBmodel* model = model_val( v_model );
  const char* name = String_val( v_name );
  int start = Int_val( v_start );
  int len = Int_val( v_len );

  long ba_length = len - start;
  if ( ba_length <= 0 ) {
    caml_invalid_argument( "get_int_attr_array:(start,len)" );
  }
  else {
    v_array = caml_ba_alloc_dims(CAML_BA_INT32 | CAML_BA_C_LAYOUT, 1, NULL, ba_length);
    int* array = Caml_ba_data_val(v_array);
    int error = GRBgetintattrarray( model, name, start, len, array );

    if ( error == 0 ) {
      // Ok v_array
      v_res = caml_alloc(1, 0);
      Store_field( v_res, 0, v_array );
    }
    else {
      // Error code
      v_res = caml_alloc(1, 1);
      Store_field( v_res, 0, Val_int(error) );
    }
    CAMLreturn( v_res );
  }
}

// get char attribute array
CAMLprim value gu_get_char_attr_array( value v_model, value v_name, value v_start, value v_len )
{
  CAMLparam4( v_model, v_name, v_start, v_len );
  CAMLlocal2( v_array, v_res );
  GRBmodel* model = model_val( v_model );
  const char* name = String_val( v_name );
  int start = Int_val( v_start );
  int len = Int_val( v_len );

  long ba_length = len - start;
  if ( ba_length <= 0 ) {
    caml_invalid_argument( "get_char_attr_array:(start,len)" );
  }
  else {
    v_array = caml_ba_alloc_dims(CAML_BA_CHAR | CAML_BA_C_LAYOUT, 1, NULL, ba_length);
    char* array = Caml_ba_data_val(v_array);
    int error = GRBgetcharattrarray( model, name, start, len, array );

    if ( error == 0 ) {
      // Ok v_array
      v_res = caml_alloc(1, 0);
      Store_field( v_res, 0, v_array );
    }
    else {
      // Error code
      v_res = caml_alloc(1, 1);
      Store_field( v_res, 0, Val_int(error) );
    }
    CAMLreturn( v_res );
  }
}

// get string attribute array
CAMLprim value gu_get_str_attr_array( value v_model, value v_name, value v_start, value v_len )
{
  CAMLparam4( v_model, v_name, v_start, v_len );
  CAMLlocal2( v_array, v_res );
  GRBmodel* model = model_val( v_model );
  const char* name = String_val( v_name );
  int start = Int_val( v_start );
  int len = Int_val( v_len );
  int c_len = len - start;
  char** array = malloc( c_len * sizeof(char*) );
  int error = GRBgetstrattrarray( model, name, start, len, array );
  if ( error == 0 ) {
    // Ok v_array
    v_array = caml_alloc( len, 0 );
    for (int i = 0; i < len; i++ ) {
      Store_field( v_array, i, caml_alloc_initialized_string( strlen(array[i]), array[i] ) );
    }
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, v_array );
  }
  else {
    // Error code
    v_res = caml_alloc(1, 1);
    Store_field( v_res, 0, Val_int(error) );
  }
  free(array);
  CAMLreturn( v_res );
}

// create a new model
CAMLprim value gu_new_model(
 value v_env,
 value v_name_opt,
 value v_num_vars,
 value v_objective_opt,
 value v_lower_bound_opt,
 value v_upper_bound_opt,
 value v_var_type_opt,
 value v_var_names_opt
)

{
  CAMLparam5( v_env, v_name_opt, v_num_vars, v_objective_opt, v_lower_bound_opt );
  CAMLxparam3( v_upper_bound_opt, v_var_type_opt, v_var_names_opt );
  CAMLlocal5( v_model, v_res, v_var_names, v_objective, v_lower_bound );
  CAMLlocal3( v_upper_bound, v_var_type, v_name );

  GRBenv* env = env_val( v_env );

  // name, optional
  const char* name = NULL;
  if ( Is_some( v_name_opt ) ) {
    v_name = Some_val( v_name_opt );
    name = String_val( v_name );
  }

  // number of variables
  int num_vars = Int_val( v_num_vars );

  // objective
  double* objective = NULL;
  if ( Is_some( v_objective_opt ) ) {
    v_objective = Some_val( v_objective_opt );
    objective = get_fa( v_objective, num_vars );
    if ( objective == NULL ) {
      caml_invalid_argument( "new_model:objective" );
    }
  }

  // lower bound
  double* lower_bound = NULL;
  if ( Is_some( v_lower_bound_opt ) ) {
    v_lower_bound = Some_val( v_lower_bound_opt );
    lower_bound = get_fa( v_lower_bound, num_vars );
    if ( lower_bound == NULL ) {
      caml_invalid_argument( "new_model:lower_bound" );
    }
  }

  // upper bound
  double* upper_bound = NULL;
  if ( Is_some( v_upper_bound_opt ) ) {
    v_upper_bound = Some_val( v_upper_bound_opt );
    upper_bound = get_fa( v_upper_bound, num_vars );
    if ( upper_bound == NULL ) {
      caml_invalid_argument( "new_model:upper_bound" );
    }
  }

  // var type
  char* var_type = NULL;
  if ( Is_some( v_var_type_opt ) ) {
    v_var_type = Some_val( v_var_type_opt );
    var_type = get_ca( v_var_type, num_vars );
    if ( var_type == NULL ) {
      caml_invalid_argument( "new_model:var_type" );
    }
  }

  // var names
  const char** var_names = NULL;
  if ( Is_some( v_var_names_opt ) ) {
    v_var_names = Some_val( v_var_names_opt );
    var_names = get_sa( v_var_names, num_vars );
    if ( var_names == NULL ) {
      caml_invalid_argument( "new_mode:var_names" );
    }
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
    v_model = caml_alloc_custom(&model_ops, sizeof(GRBmodel*), 0, 1);
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

// create a model from a file
CAMLprim value gu_read_model( value v_env, value v_path )
{
  CAMLparam2( v_env, v_path );
  CAMLlocal2( v_model, v_res );
  GRBenv* env = env_val( v_env );
  const char* path = String_val( v_path );

  // we have to check the existance of the file because GRBreadmodel()
  // does not do it, resulting in bus errors when the path is long
  if (access(path, F_OK) == 0) {
    // file exists
    GRBmodel* model = NULL;
    int error = GRBreadmodel( env, path, &model );
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

  }
  else {
    // file does not exist
    v_res = Val_int(0);
  }

  CAMLreturn( v_res );
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

// set and get a character attribute in an implicit array of such attributes
CAMLprim value gu_set_char_attr_element(value v_model, value v_name, value v_element, value v_new_value )
{
  CAMLparam4( v_model, v_name, v_element, v_new_value );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  char new_value = Int_val(v_new_value);
  int error = GRBsetcharattrelement( model, name, element, new_value );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_get_char_attr_element(value v_model, value v_name, value v_element )
{
  CAMLparam3( v_model, v_name, v_element );
  CAMLlocal2( v_res, v_s );
  GRBmodel* model = model_val(v_model);
  const char* name = String_val(v_name);
  int element = Int_val(v_element);
  char c;
  int error = GRBgetcharattrelement( model, name, element, &c );
  if ( error == 0 ) {
    // Ok s
    v_res = caml_alloc(1, 0);
    Store_field( v_res, 0, Val_int(c) );
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
 value v_compressed_opt,
 value v_sense,
 value v_rhs,
 value v_constr_names_opt
)
{
  CAMLparam5( v_model, v_num_constraints, v_compressed_opt, v_sense, v_rhs );
  CAMLxparam1( v_constr_names_opt );
  CAMLlocal5( v_num_nz, v_c_beg, v_c_ind, v_c_val, v_constr_names );
  CAMLlocal1( v_compressed );

  GRBmodel* model = model_val( v_model );
  int num_constraints = Int_val( v_num_constraints );

  int num_nz = 0;
  int* c_beg = NULL;
  int* c_ind = NULL;
  double* c_val = NULL;

  if ( Is_some( v_compressed_opt ) ) {
    v_compressed = Some_val( v_compressed_opt );
    v_num_nz = Field( v_compressed, 0 );
    v_c_beg = Field( v_compressed, 1 );
    v_c_ind = Field( v_compressed, 2 );
    v_c_val = Field( v_compressed, 3 );

    num_nz = Int_val( v_num_nz );
    c_beg = get_i32a( v_c_beg, num_constraints );
    if ( c_beg == NULL ) {
      caml_invalid_argument( "add_constrs:compressed.beg" );
    }
    c_ind = get_i32a( v_c_ind, num_nz );
    if ( c_ind == NULL ) {
      caml_invalid_argument( "add_constrs:compressed.ind" );
    }
    c_val = get_fa( v_c_val, num_nz );
    if ( c_val == NULL ) {
      caml_invalid_argument( "add_constrs:compressed.val" );
    }
  }

  char* sense = get_ca( v_sense, num_constraints );
  if ( sense == NULL ) {
    caml_invalid_argument( "add_constrs:sense" );
  }
  double* rhs = get_fa( v_rhs, num_constraints );
  if ( rhs == NULL ) {
    caml_invalid_argument( "add_constrs:rhs" );
  }

  const char** constr_names = NULL;
  if ( Is_some( v_constr_names_opt ) ) {
    v_constr_names = Some_val( v_constr_names_opt );
    constr_names = get_sa( v_constr_names, num_constraints );
    if ( constr_names == NULL ) {
      caml_invalid_argument( "add_constrs:constr_names" );
    }
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
  assert( arg_n == 6 );
  return gu_add_constrs( v_args[0],
		         v_args[1],
  		         v_args[2],
		         v_args[3],
		         v_args[4],
		         v_args[5]
			 );
}  

CAMLprim value gu_add_constr(
 value v_model,
 value v_num_nz,
 value v_c_ind,
 value v_c_val,
 value v_sense,
 value v_rhs,
 value v_name_opt
)
{
  CAMLparam5( v_model, v_num_nz, v_c_ind, v_c_val, v_sense );
  CAMLxparam2( v_rhs, v_name_opt );
  CAMLlocal1( v_name );

  GRBmodel* model = model_val( v_model );
  int num_nz = Int_val( v_num_nz );
  int* c_ind = get_i32a( v_c_ind, num_nz );
  if ( c_ind == NULL ) {
    caml_invalid_argument( "add_constr:ind" );
  }
  double* c_val = get_fa( v_c_val, num_nz );
  if ( c_val == NULL ) {
    caml_invalid_argument( "add_constr:val" );
  }
  char sense = Int_val( v_sense );
  double rhs = Double_val( v_rhs );

  const char* name = NULL;
  if (Is_some( v_name_opt )) {
    v_name = Some_val( v_name_opt );
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

CAMLprim value gu_add_q_constr(
 value v_model,
 value v_l_numnz_ind_val_triple_opt,
 value v_q_num_nz,
 value v_q_row,
 value v_q_col,
 value v_q_val,
 value v_sense,
 value v_rhs,
 value v_constr_name_opt
)
{
  CAMLparam5( v_model, v_l_numnz_ind_val_triple_opt, v_q_num_nz, v_q_row, v_q_col );
  CAMLxparam4( v_q_val, v_sense, v_rhs, v_constr_name_opt );
  CAMLlocal5( v_l_numnz_ind_val_triple, v_l_num_nz, v_l_ind, v_l_val, v_constr_name );

  GRBmodel* model = model_val( v_model );

  int l_num_nz = 0;
  int* l_ind = NULL;
  double* l_val = NULL;

  if (Is_some( v_l_numnz_ind_val_triple_opt )) {
    // we have a linear component to the quadratic constraint
    v_l_numnz_ind_val_triple = Some_val( v_l_numnz_ind_val_triple_opt );
    v_l_num_nz = Field( v_l_numnz_ind_val_triple, 0 );
    v_l_ind = Field( v_l_numnz_ind_val_triple, 1 );
    v_l_val = Field( v_l_numnz_ind_val_triple, 2 );

    // v_l_ind and v_l_val must have minimum length of l_num_nz
    l_num_nz = Int_val( v_l_num_nz );
    l_ind = get_i32a( v_l_ind, l_num_nz );
    if ( l_ind == NULL ) {
      caml_invalid_argument( "add_q_constr::l_ind" );
    }
    l_val = get_fa( v_l_val, l_num_nz );
    if ( l_val == NULL ) {
      caml_invalid_argument( "add_q_constr::l_val" );
    }
  }

  // v_q_row, v_q_col, and v_q_val must have minimum length q_num_nz
  int q_num_nz = Int_val( v_q_num_nz );
  int* q_row = get_i32a( v_q_row, q_num_nz );
  if ( q_row == NULL ) {
    caml_invalid_argument( "add_q_constr::q_row" );
  }
  int* q_col = get_i32a( v_q_col, q_num_nz );
  if ( q_col == NULL ) {
    caml_invalid_argument( "add_q_constr::q_col" );
  }
  double* q_val = get_fa( v_q_val, q_num_nz );
  if ( q_val == NULL ) {
    caml_invalid_argument( "add_q_constr::q_val" );
  }

  char sense = Int_val( v_sense );
  double rhs = Double_val( v_rhs );

  const char* constr_name = NULL;
  if (Is_some( v_constr_name_opt )) {
    v_constr_name = Some_val( v_constr_name_opt );
    constr_name = String_val( v_constr_name );
  }

  int error = GRBaddqconstr( model,
			     l_num_nz,
			     l_ind,
			     l_val,
			     q_num_nz,
			     q_row,
			     q_col,
			     q_val,
			     sense,
			     rhs,
			     constr_name );
			     
  CAMLreturn( Val_int( error ) );

}

CAMLprim value gu_add_q_constr_bc(value* v_args, int arg_n )
{
  assert( arg_n == 9 );
  return gu_add_q_constr( v_args[0],
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

CAMLprim value gu_add_vars(
 value v_model,
 value v_num_vars,
 value v_compressed_opt,
 value v_obj_opt,
 value v_lower_bound_opt,
 value v_upper_bound_opt,
 value v_var_type_opt,
 value v_var_names_opt
)
{
  CAMLparam5( v_model, v_num_vars, v_compressed_opt, v_obj_opt, v_lower_bound_opt );
  CAMLxparam3( v_upper_bound_opt, v_var_type_opt, v_var_names_opt );
  CAMLlocal5( v_compressed, v_obj, v_lower_bound, v_upper_bound, v_var_type );
  CAMLlocal5( v_var_names, v_num_nz, v_v_beg, v_v_ind, v_v_val );

  GRBmodel* model = model_val( v_model );
  int num_vars = Int_val( v_num_vars );

  int num_nz = 0;
  int* v_beg = NULL;
  int* v_ind = NULL;
  double* v_val = NULL;

  if ( Is_some( v_compressed_opt ) ) {
    v_compressed = Some_val( v_compressed_opt );
    v_num_nz = Field( v_compressed, 0 );
    v_v_beg = Field( v_compressed, 1 );
    v_v_ind = Field( v_compressed, 2 );
    v_v_val = Field( v_compressed, 3 );

    num_nz = Int_val( v_num_nz );
    v_beg = get_i32a( v_v_beg, num_vars );
    if ( v_beg == NULL ) {
      caml_invalid_argument( "add_vars:compressed.beg" );
    }
    v_ind = get_i32a( v_v_ind, num_nz );
    if ( v_ind == NULL ) {
      caml_invalid_argument( "add_vars:compressed.ind" );
    }
    v_val = get_fa( v_v_val, num_nz );
    if ( v_val == NULL ) {
      caml_invalid_argument( "add_vars:compressed.val" );
    }
  }

  // objective
  double* obj = NULL;
  if ( Is_some( v_obj_opt ) ) {
    v_obj = Some_val( v_obj_opt );
    obj = get_fa( v_obj, num_vars );
    if ( obj == NULL ) {
      caml_invalid_argument( "add_vars:objective" );
    }
  }

  // lower bound
  double* lower_bound = NULL;
  if ( Is_some( v_lower_bound_opt ) ) {
    v_lower_bound = Some_val( v_lower_bound_opt );
    lower_bound = get_fa( v_lower_bound, num_vars );
    if ( lower_bound == NULL ) {
      caml_invalid_argument( "add_vars:lower_bound" );
    }
  }

  // upper bound
  double* upper_bound = NULL;
  if ( Is_some( v_upper_bound_opt ) ) {
    v_upper_bound = Some_val( v_upper_bound_opt );
    upper_bound = get_fa( v_upper_bound, num_vars );
    if ( upper_bound == NULL ) {
      caml_invalid_argument( "add_vars:upper_bound" );
    }
  }

  // var type
  const char* var_type = NULL;
  if ( Is_some( v_var_type_opt ) ) {
    v_var_type = Some_val( v_var_type_opt );
    var_type = get_ca( v_var_type, num_vars );
    if ( var_type == NULL ) {
      caml_invalid_argument( "add_vars:var_type" );
    }
  }

  // var names
  const char** var_names = NULL;
  if ( Is_some( v_var_names_opt ) ) {
    v_var_names = Some_val( v_var_names_opt );
    var_names = get_sa( v_var_names, num_vars );
    if ( var_names == NULL ) {
      caml_invalid_argument( "add_vars:var_names" );
    }
  }

  int error = GRBaddvars( model,
			  num_vars,
			  num_nz,
			  v_beg,
			  v_ind,
			  v_val,
			  obj,
			  lower_bound,
			  upper_bound,
			  (char*)var_type,
			  (char**)var_names );

  if ( Is_some( v_var_names ) ) {
    free( var_names );
  }

  CAMLreturn( Val_int( error ) );

}

CAMLprim value gu_chg_coeffs(
 value v_model,
 value v_num_chgs,
 value v_c_ind,
 value v_v_ind,
 value v_val
)
{
  CAMLparam5( v_model, v_num_chgs, v_c_ind, v_v_ind, v_val );

  GRBmodel* model = model_val( v_model );
  int num_chgs = Int_val( v_num_chgs );
  int* c_ind = get_i32a( v_c_ind, num_chgs );
  if ( c_ind == NULL ) {
    caml_invalid_argument( "chg_coeffs:cind" );
  }
  int* v_ind = get_i32a( v_v_ind, num_chgs );
  if ( v_ind == NULL ) {
    caml_invalid_argument( "chg_coeffs:vind" );
  }
  double* val = get_fa( v_val, num_chgs );
  if ( val == NULL ) {
    caml_invalid_argument( "chg_coeffs:val" );
  }

  int error = GRBchgcoeffs( model, num_chgs, c_ind, v_ind, val );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_add_q_p_terms(
 value v_model,
 value v_num_qnz,
 value v_q_row,
 value v_q_col,
 value v_q_val
)
{
  CAMLparam5( v_model, v_num_qnz, v_q_row, v_q_col, v_q_val );

  GRBmodel* model = model_val( v_model );
  int num_qnz = Int_val( v_num_qnz );
  int* q_row = get_i32a( v_q_row, num_qnz );
  if ( q_row == NULL ) {
    caml_invalid_argument( "add_q_p_terms:qrow" );
  }
  int* q_col = get_i32a( v_q_col, num_qnz );
  if ( q_col == NULL ) {
    caml_invalid_argument( "add_q_p_terms:qcol" );
  }
  double* q_val = get_fa( v_q_val, num_qnz );
  if ( q_val == NULL ) {
    caml_invalid_argument( "add_q_p_terms:qval" );
  }

  int error = GRBaddqpterms( model, num_qnz, q_row, q_col, q_val );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_add_vars_bc(value* v_args, int arg_n )
{
  assert( arg_n == 8 );
  return gu_add_vars( v_args[0],
		      v_args[1],
		      v_args[2],
		      v_args[3],
		      v_args[4],
		      v_args[5],
		      v_args[6],
		      v_args[7]
		      );
}

CAMLprim value gu_optimize( value v_model )
{
  CAMLparam1( v_model );
  GRBmodel* model = model_val( v_model );
  int error = GRBoptimize( model );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_write( value v_model, value v_path )
{
  CAMLparam2( v_model, v_path );
  GRBmodel* model = model_val( v_model );
  const char* path = String_val( v_path );
  int error = GRBwrite( model, path );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_compute_iis( value v_model )
{
  CAMLparam1( v_model );
  GRBmodel* model = model_val( v_model );
  int error = GRBcomputeIIS( model );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_set_objective_n(
  value v_model,
  value v_index,
  value	v_priority,
  value v_weight,
  value	v_abstol,
  value v_reltol,
  value v_name_opt,
  value v_constant,
  value	v_nnz,
  value	v_ind,
  value	v_val
)
{
  CAMLparam5( v_model, v_index, v_priority, v_weight, v_abstol );
  CAMLxparam5( v_reltol, v_name_opt, v_constant, v_nnz, v_ind );
  CAMLxparam1( v_val );
  CAMLlocal1( v_name );

  GRBmodel* model = model_val( v_model );
  int index = Int_val( v_index );
  int priority = Int_val( v_priority );
  double weight = Double_val( v_weight );
  double abstol = Double_val( v_abstol );
  double reltol = Double_val( v_reltol );

  // name, optional
  const char* name = NULL;
  if ( Is_some( v_name_opt ) ) {
    v_name = Some_val( v_name_opt );
    name = String_val( v_name );
  }

  double constant = Double_val( v_constant );
  int nnz = Int_val( v_nnz );
  int* ind = get_i32a( v_ind, nnz );
  if ( ind == NULL ) {
    caml_invalid_argument( "set_objective_n:<ind>" );
  }
  double* val = get_fa( v_val, nnz );
  if ( val == NULL ) {
    caml_invalid_argument( "set_objective_n:<val>" );
  }

  int error = GRBsetobjectiven(model,
			       index,
			       priority,
			       weight,
			       abstol,
			       reltol,
			       name,
			       constant,
			       nnz,
			       ind,
			       val );
  CAMLreturn( Val_int( error ) );
}

CAMLprim value gu_set_objective_n_bc(value* v_args, int arg_n )
{
  assert( arg_n == 11 );
  return gu_set_objective_n( v_args[0],
			     v_args[1],
			     v_args[2],
			     v_args[3],
			     v_args[4],
			     v_args[5],
			     v_args[6],
			     v_args[7],
			     v_args[8],
			     v_args[9],
			     v_args[10]
			     );
}

