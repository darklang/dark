open Core_kernel
open Libcommon
open Types
open Types.RuntimeT

type type_error =
  { expected : tipe
  ; actual : tipe }

val check_function_call :
     user_tipes:user_tipe list
  -> fn
  -> dval_map
  -> (unit, type_error list) Result.t
