open Core
open Libexecution

(* space, path, modifier *)
type event_desc = string * string * string [@@deriving show]
type four_oh_four = event_desc * Types.RuntimeT.dval list [@@deriving show]

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

val clear_events :
  canvas_id:Uuidm.t ->
  unit ->
  unit

val four_oh_four_to_yojson : four_oh_four -> Yojson.Safe.json
