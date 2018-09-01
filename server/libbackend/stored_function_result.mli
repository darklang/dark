open Core
open Libexecution


val store :
  canvas_id:Uuidm.t ->
  trace_id:Uuidm.t ->
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load :
  canvas_id:Uuidm.t ->
  trace_id:Uuidm.t ->
  Types.tlid ->
  (Types.RuntimeT.dval Analysis_types.function_result) list
