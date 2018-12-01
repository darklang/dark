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
  Analysis_types.function_result list

(* Trim the events for an entire canvas, removing events from before the time,
 * unless listed in keep.
 *)
val trim_results :
  canvas_id:Uuidm.t ->
  keep:(Analysis_types.traceid list) ->
  unit ->
  unit

