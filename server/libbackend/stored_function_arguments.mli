open Core
open Libexecution


val store :
  Types.RuntimeT.user_fn_desc ->
  Types.RuntimeT.dval_map ->
  unit

val load :
  Types.RuntimeT.user_fn_desc ->
  (Types.RuntimeT.dval_map * Time.t) list


