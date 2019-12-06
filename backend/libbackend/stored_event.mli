open Core
open Libexecution

(* space, path, modifier *)
type event_desc = string * string * string [@@deriving show, yojson]

type event_record =
  string * string * string * Types.RuntimeT.time * Analysis_types.traceid
[@@deriving show, yojson]

type four_oh_four = event_record [@@deriving show, yojson]

(* We store a set of events for each host. The events may or may not
 * belong to a toplevel. We provide a list in advance so that they can
 * be partitioned effectively. Returns the DB-assigned event timestamp. *)
val store_event :
     trace_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> event_desc
  -> Types.RuntimeT.dval
  -> Types.RuntimeT.time

val load_event_for_trace :
     canvas_id:Uuidm.t
  -> Uuidm.t
  -> (string * Types.RuntimeT.time * Types.RuntimeT.dval) option

val load_events :
     canvas_id:Uuidm.t
  -> event_desc
  -> (string * Uuidm.t * Types.RuntimeT.time * Types.RuntimeT.dval) list

val load_event_ids : canvas_id:Uuidm.t -> event_desc -> (Uuidm.t * string) list

val list_events :
     limit:[`All | `Since of Types.RuntimeT.time]
  -> canvas_id:Uuidm.t
  -> unit
  -> event_record list

val clear_all_events : canvas_id:Uuidm.t -> unit -> unit

(* Trim the events for an entire canvas, removing events from before the time,
 * unless listed in keep.
 *)
val trim_events : unit -> int
