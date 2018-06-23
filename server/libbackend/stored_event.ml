open Core_kernel
open Libexecution

module RTT = Types.RuntimeT

module Dbp = Dbprim

type event_desc = string * string * string
                [@@deriving show, yojson]
type four_oh_four = (event_desc * Types.RuntimeT.dval list)
                  [@@deriving show]

(* ------------------------- *)
(* Event data *)
(* ------------------------- *)
let store_event (canvas_id: Uuidm.t) ((module_, path, modifier): event_desc) (event: RTT.dval) : unit =
  Db.run_sql2
    ~name:"stored_event.store_event"
    "INSERT INTO stored_events
     (canvas_id, module, path, modifier, timestamp, value)
     VALUES ($1, $2, $3, $4, CURRENT_TIMESTAMP, $5)"
    ~params:[ Uuid canvas_id
            ; String module_
            ; String path
            ; String modifier
            ; DvalJson event]

let list_events (canvas_id: Uuidm.t) : event_desc list =
  Db.fetch
    ~name:"list_events"
    "SELECT DISTINCT module, path, modifier FROM stored_events
     WHERE canvas_id = $1"
    ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(function
      | [module_; path; modifier] -> (module_, path, modifier)
      | _ -> Exception.internal "Bad DB format for stored_events")

let load_events (canvas_id: Uuidm.t) ((module_, path, modifier): event_desc) : RTT.dval list =
  Db.fetch
    ~name:"load_events"
    "SELECT value, timestamp FROM stored_events
    WHERE canvas_id = $1
      AND module = $2
      AND path = $3
      AND modifier = $4
    LIMIT 20"
    ~params:[ Uuid canvas_id
            ; String module_
            ; String path
            ; String modifier]
  |> List.map ~f:(function
      | [dval; _ts] -> Dval.dval_of_json_string dval
      | _ -> Exception.internal "Bad DB format for stored_events")

let clear_events (canvas_id: Uuidm.t) : unit =
  Db.run_sql2
    ~name:"stored_event.clear_events"
    "DELETE FROM stored_events
     WHERE canvas_id = $1"
    ~params:[ Uuid canvas_id]

let four_oh_four_to_yojson (((space, path, modifier), dvals) : four_oh_four) : Yojson.Safe.json =
  `List [ `String space
        ; `String path
        ; `String modifier
        ; `List (List.map ~f:Dval.dval_to_yojson dvals)
        ]

