open Core
open Libexecution

(* space, path, modifier *)
type event_desc = string * string * string [@@deriving show]
type four_oh_four = event_desc * Types.RuntimeT.dval list [@@deriving show]

(* We store a set of events for each host. The events may or may not
 * belong to a toplevel. We provide a list in advance so that they can
 * be partitioned effectively *)
val store_event : Uuidm.t -> event_desc -> Types.RuntimeT.dval -> unit
val load_events : Uuidm.t -> event_desc -> Types.RuntimeT.dval list
val list_events : Uuidm.t -> event_desc list
val clear_events : Uuidm.t -> unit

val four_oh_four_to_yojson : four_oh_four -> Yojson.Safe.json
