open Core_kernel
open Libexecution
module RTT = Types.RuntimeT

type event_desc = string * string * string [@@deriving show, yojson]

type event_record = string * string * string * RTT.time
[@@deriving show, yojson]

type four_oh_four = event_record [@@deriving show, yojson]

(* ------------------------- *)
(* Event data *)
(* ------------------------- *)
let store_event
    ~(trace_id : Uuidm.t)
    ~(canvas_id : Uuidm.t)
    ((module_, path, modifier) : event_desc)
    (event : RTT.dval) : RTT.time =
  Db.fetch_one
    ~name:"stored_event.store_event"
    "INSERT INTO stored_events_v2
     (canvas_id, trace_id, module, path, modifier, timestamp, value)
     VALUES ($1, $2, $3, $4, $5, CURRENT_TIMESTAMP, $6)
     RETURNING timestamp"
    ~params:
      [ Uuid canvas_id
      ; Uuid trace_id
      ; String module_
      ; String path
      ; String modifier
      ; DvalJson event ]
  |> List.hd_exn
  |> Util.date_of_isostring


let list_events
    ~(limit : [`All | `Since of RTT.time]) ~(canvas_id : Uuidm.t) () :
    event_record list =
  let timestamp_constraint =
    match limit with
    | `All ->
        ""
    | `Since since ->
        "AND timestamp > " ^ Db.escape (Time since)
  in
  let sql =
    "SELECT
         DISTINCT ON (module, path, modifier)
         module, path, modifier, timestamp
       FROM stored_events_v2
       WHERE canvas_id = $1"
    ^ timestamp_constraint
  in
  Db.fetch sql ~name:"list_events" ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(function
         | [module_; path; modifier; timestamp] ->
             (module_, path, modifier, Util.date_of_isostring timestamp)
         | out ->
             Exception.internal "Bad DB format for stored_events" )


let load_events
    ~(canvas_id : Uuidm.t) ((module_, route, modifier) : event_desc) :
    (string * Uuidm.t * RTT.dval) list =
  let route = Http.route_to_postgres_pattern route in
  Db.fetch
    ~name:"load_events"
    "SELECT path, value, timestamp, trace_id FROM stored_events_v2
    WHERE canvas_id = $1
      AND module = $2
      AND path LIKE $3
      AND modifier = $4
    ORDER BY timestamp DESC
    LIMIT 10"
    ~params:[Uuid canvas_id; String module_; String route; String modifier]
  |> List.map ~f:(function
         | [request_path; dval; _ts; trace_id] ->
             let trace_id = Util.uuid_of_string trace_id in
             (request_path, trace_id, Dval.unsafe_dval_of_json_string dval)
         | _ ->
             Exception.internal "Bad DB format for stored_events" )


let clear_all_events ~(canvas_id : Uuidm.t) () : unit =
  Db.run
    ~name:"stored_event.clear_events"
    "DELETE FROM stored_events_v2
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]


let get_recent_event_traceids ~(canvas_id : Uuidm.t) event_rec =
  let module_, path, modifier, _ = event_rec in
  Db.fetch
    ~name:"stored_event.get_recent_traces"
    "SELECT trace_id FROM stored_events_v2
     WHERE canvas_id = $1
       AND module = $2
       AND path = $3
       AND modifier = $4
     ORDER BY timestamp DESC
     LIMIT 10"
    ~params:[Uuid canvas_id; String module_; String path; String modifier]
  |> List.filter_map ~f:(function
         | [trace_id] ->
             if trace_id = ""
             then None
             else Some (Util.uuid_of_string trace_id)
         | _ ->
             Exception.internal "Bad DB format for stored_events" )


let get_all_recent_canvas_traceids (canvas_id : Uuidm.t) =
  list_events ~limit:`All ~canvas_id ()
  |> List.map ~f:(get_recent_event_traceids ~canvas_id)
  |> List.concat


let trim_events ~(canvas_id : Uuidm.t) ~(keep : Analysis_types.traceid list) ()
    =
  Db.run
    ~name:"stored_event.trim_events"
    "DELETE FROM stored_events_v2
     WHERE canvas_id = $1
       AND timestamp < CURRENT_TIMESTAMP
       AND NOT (trace_id = ANY (string_to_array($2, $3)::uuid[]))"
    ~subject:(Uuidm.to_string canvas_id)
    ~params:
      [ Uuid canvas_id
      ; List (List.map ~f:(fun u -> Db.Uuid u) keep)
      ; String Db.array_separator ]
