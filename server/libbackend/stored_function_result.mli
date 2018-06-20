open Core
open Libexecution


val store :
  Uuidm.t * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load :
  Uuidm.t * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  (Types.RuntimeT.dval * Time.t) option
