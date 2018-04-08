open Core

(* space, path, modifier *)
type event_desc = string * string * string [@@deriving show]
type four_oh_four = event_desc * Types.RuntimeT.dval list [@@deriving show]

val store_event : string -> event_desc -> Types.RuntimeT.dval -> unit
val load_events : string -> event_desc -> Types.RuntimeT.dval list
val list_events : string -> event_desc list

val four_oh_four_to_yojson : four_oh_four -> Yojson.Safe.json
