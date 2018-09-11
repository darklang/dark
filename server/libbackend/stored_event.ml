open Core_kernel
open Libexecution

module RTT = Types.RuntimeT

type event_desc = string * string * string
                [@@deriving show, yojson]
type four_oh_four = event_desc
                  [@@deriving show, yojson]

(* ------------------------- *)
(* Event data *)
(* ------------------------- *)
let store_event ~(trace_id: Uuidm.t) ~(canvas_id: Uuidm.t) ((module_, path, modifier): event_desc) (event: RTT.dval) : unit =
  Db.run
    ~name:"stored_event.store_event"
    "INSERT INTO stored_events
     (canvas_id, trace_id, module, path, modifier, timestamp, value)
     VALUES ($1, $2, $3, $4, $5, CURRENT_TIMESTAMP, $6)"
    ~params:[ Uuid canvas_id
            ; Uuid trace_id
            ; String module_
            ; String path
            ; String modifier
            ; DvalJson event]

let list_events ~(canvas_id: Uuidm.t) () : event_desc list =
  Db.fetch
    ~name:"list_events"
    "SELECT DISTINCT module, path, modifier FROM stored_events
     WHERE canvas_id = $1"
    ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(function
      | [module_; path; modifier] -> (module_, path, modifier)
      | _ -> Exception.internal "Bad DB format for stored_events")

let load_events ~(canvas_id: Uuidm.t) ((module_, path, modifier): event_desc) : (Uuidm.t * RTT.dval) list =
  Db.fetch
    ~name:"load_events"
    "SELECT value, timestamp, trace_id FROM stored_events
    WHERE canvas_id = $1
      AND module = $2
      AND path = $3
      AND modifier = $4
    ORDER BY timestamp DESC
    LIMIT 10"
    ~params:[ Uuid canvas_id
            ; String module_
            ; String path
            ; String modifier]
  |> List.map ~f:(function
      | [dval; _ts; trace_id] ->
        let trace_id =
          if trace_id = ""
          then Util.create_uuid ()
          else Util.uuid_of_string trace_id
        in
        (trace_id, Dval.dval_of_json_string dval)
      | _ -> Exception.internal "Bad DB format for stored_events")

let clear_events ~(canvas_id: Uuidm.t) () : unit =
  Db.run
    ~name:"stored_event.clear_events"
    "DELETE FROM stored_events
     WHERE canvas_id = $1"
    ~params:[ Uuid canvas_id]


