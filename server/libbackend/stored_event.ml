open Core

module RTT = Types.RuntimeT

module Dbp = Dbprim

type event_desc = string * string * string
                [@@deriving show, yojson]
type four_oh_four = (event_desc * Types.RuntimeT.dval list)
                  [@@deriving show]

(* ------------------------- *)
(* Event data *)
(* ------------------------- *)
let store_event (canvas_id: Uuid.t) _  ((module_, path, modifier): event_desc) (event: RTT.dval) : unit =
  Printf.sprintf
    "INSERT INTO stored_events
    (canvas_id, module, path, modifier, timestamp, value)
    VALUES (%s, %s, %s, %s, CURRENT_TIMESTAMP, %s)"
    (Dbp.uuid canvas_id)
    (Dbp.string module_)
    (Dbp.string path)
    (Dbp.string modifier)
    (Dbp.dvaljson event)
  |> Db.run_sql

let list_events (canvas_id: Uuid.t) _  : event_desc list =
  Printf.sprintf
    "SELECT DISTINCT module, path, modifier FROM stored_events
     WHERE canvas_id = %s"
    (Dbp.uuid canvas_id)
  |> Db.fetch_via_sql
  |> List.map ~f:(function
      | [module_; path; modifier] -> (module_, path, modifier)
      | _ -> Exception.internal "Bad DB format for stored_events")

let load_events (canvas_id: Uuid.t) _ ((module_, path, modifier): event_desc) : RTT.dval list =
  Printf.sprintf
    "SELECT value, timestamp FROM stored_events
    WHERE canvas_id = %s
      AND module = %s
      AND path = %s
      AND modifier = %s
    LIMIT 20"
    (Dbp.uuid canvas_id)
    (Dbp.string module_)
    (Dbp.string path)
    (Dbp.string modifier)
  |> Db.fetch_via_sql
  |> List.map ~f:(function
      | [dval; _ts] -> Dval.dval_of_json_string dval
      | _ -> Exception.internal "Bad DB format for stored_events")

let clear_events (canvas_id: Uuid.t) _ : unit =
   Printf.sprintf
    "DELETE FROM stored_events
     WHERE canvas_id = %s"
    (Dbp.uuid canvas_id)
  |> Db.run_sql

let four_oh_four_to_yojson (((space, path, modifier), dvals) : four_oh_four) : Yojson.Safe.json =
  `List [ `String space
        ; `String path
        ; `String modifier
        ; `List (List.map ~f:Dval.dval_to_yojson dvals)
        ]

let store_event = store_event
let load_events = load_events
let list_events = list_events
let clear_events = clear_events
