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
#include <assert.h>
#include <stdbool.h>

#define env_val(v) (*((GRBenv **) Data_custom_val(v)))

void gu_finalize(value v_env) {
  GRBenv* env = env_val( v_env );
  GRBfreeenv( env );
}

static struct custom_operations env_ops = {
  "gurobi.env",
  gu_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

// naming convention: Gurobi's functions consist of multiple words,
// concatenated without a space, resulting in unfortunate
// readibility. Here, we improve on that by separating the words with
// underscores. For example, we would wrap a Gurobi function
// GRBpickupthemilk with function gu_pick_up_the_milk.

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

