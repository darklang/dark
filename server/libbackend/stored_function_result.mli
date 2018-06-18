open Core
open Libexecution


val store :
  Uuidm.t * string * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load :
  Uuidm.t * string * Types.tlid * string * Types.id ->
  Types.RuntimeT.dval list ->
  (Types.RuntimeT.dval * Time.t) option
