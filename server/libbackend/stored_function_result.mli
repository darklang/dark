open Core
open Libexecution


val store :
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load :
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  (Types.RuntimeT.dval * Time.t) option
