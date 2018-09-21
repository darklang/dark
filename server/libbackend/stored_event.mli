open Core
open Libexecution

(* space, path, modifier *)
type event_desc = string * string * string [@@deriving show]
type four_oh_four = event_desc [@@deriving show, yojson]

(* We store a set of events for each host. The events may or may not
 * belong to a toplevel. We provide a list in advance so that they can
 * be partitioned effectively *)
val store_event :
  trace_id:Uuidm.t ->
  canvas_id:Uuidm.t ->
  event_desc ->
  Types.RuntimeT.dval ->
  unit

val load_events :
  canvas_id:Uuidm.t ->
  event_desc ->
  (Uuidm.t * Types.RuntimeT.dval) list

val list_events :
  canvas_id:Uuidm.t ->
  unit ->
  event_desc list

val clear_all_events :
  canvas_id:Uuidm.t ->
  unit ->
  unit

val get_all_recent_canvas_traceids :
  Uuidm.t ->
  Analysis_types.traceid list

(* Trim the events for an entire canvas, removing events from before the time,
 * unless listed in keep.
 *)
val trim_events :
  canvas_id:Uuidm.t ->
  keep:(Analysis_types.traceid list) ->
  unit ->
  unit

