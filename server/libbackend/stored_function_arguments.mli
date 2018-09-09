open Core
open Libexecution


val store :
  canvas_id:Uuidm.t ->
  trace_id:Uuidm.t ->
  Types.tlid ->
  Types.RuntimeT.dval_map ->
  unit

val load :
  canvas_id:Uuidm.t ->
  Types.tlid ->
  (Types.RuntimeT.dval_map * Time.t) list


