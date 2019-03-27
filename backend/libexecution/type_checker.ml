open Core_kernel
open Libcommon
open Types
open Types.RuntimeT

type type_error =
  { expected : tipe
  ; actual : tipe }

let unify ~(type_env : user_tipe list) (expected : tipe) (value : dval) :
    (unit, type_error) Result.t =
  Ok ()


let check_function_call
    ~(type_env : user_tipe list) (fn : fn) (args : dval_map) :
    (unit, type_error list) Result.t =
  Ok ()
