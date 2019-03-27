open Core_kernel
open Libcommon
open Types
open Types.RuntimeT

type type_error =
  { expected : tipe
  ; actual : tipe }

val unify :
  type_env:user_tipe list -> tipe -> dval -> (unit, type_error) Result.t

val check_function_call :
  type_env:user_tipe list -> fn -> dval_map -> (unit, type_error list) Result.t
