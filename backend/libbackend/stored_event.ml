open Core_kernel
open Libexecution
module RTT = Types.RuntimeT
open Libcommon

type event_desc = string * string * string [@@deriving show, yojson]

type event_record =
  string * string * string * RTT.time * Analysis_types.traceid
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
      ; RoundtrippableDval event ]
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
    (* Note we just grab the first one in the group because the ergonomics
     * of SELECT DISTINCT ON is much easier than the complex GROUP BY
     * with row_partition/row_num counting to express "give me the
     * first N in the group" which is probably more what we want
     * from a product POV.
     *
     * Also we _could_ order by timestamp desc here to get the more
     * recent events if we desire in the future *)
    "SELECT DISTINCT ON (module, path, modifier)
     module, path, modifier, timestamp, trace_id
     FROM stored_events_v2
     WHERE canvas_id = $1"
    ^ timestamp_constraint
  in
  Db.fetch sql ~name:"list_events" ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(function
         | [module_; path; modifier; timestamp; trace_id] ->
             let trace_id =
               trace_id
               |> Uuidm.of_string
               |> Option.value_exn
                    ~message:("Bad UUID from stored_events: " ^ trace_id)
             in
             ( module_
             , path
             , modifier
             , Util.date_of_isostring timestamp
             , trace_id )
         | out ->
             Exception.internal "Bad DB format for stored_events" )


let load_events
    ~(canvas_id : Uuidm.t) ((module_, route, modifier) : event_desc) :
    (string * Uuidm.t * RTT.time * RTT.dval) list =
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
         | [request_path; dval; ts; trace_id] ->
             let trace_id = Util.uuid_of_string trace_id in
             ( request_path
             , trace_id
             , Util.date_of_isostring ts
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal "Bad DB format for load_events" )


let load_event_for_trace ~(canvas_id : Uuidm.t) (trace_id : Uuidm.t) :
    (string * RTT.time * RTT.dval) option =
  Db.fetch
    ~name:"load_event_for_trace"
    "SELECT path, value, timestamp FROM stored_events_v2
    WHERE canvas_id = $1
      AND trace_id = $2
    LIMIT 1"
    ~params:[Uuid canvas_id; Uuid trace_id]
  |> List.hd
  |> Option.map ~f:(function
         | [request_path; dval; timestamp] ->
             ( request_path
             , Util.date_of_isostring timestamp
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal "Bad DB format for load_event_for_trace" )


let load_event_ids
    ~(canvas_id : Uuidm.t) ((module_, route, modifier) : event_desc) :
    Uuidm.t list =
  let route =
    (* Only munge the route for HTTP events, as they have wildcards, whereas
     * background events are completely concrete.
     *
     * `split_uri_path` inside `Http.route_to_postgres_pattern` doesn't like that background
     * events don't have leading slashes. *)
    if String.Caseless.equal module_ "HTTP"
    then Http.route_to_postgres_pattern route
    else
      (* https://www.postgresql.org/docs/9.6/functions-matching.html *)
      route |> Util.string_replace "%" "\\%" |> Util.string_replace "_" "\\_"
  in
  Db.fetch
    ~name:"load_events"
    "SELECT trace_id FROM stored_events_v2
    WHERE canvas_id = $1
      AND module = $2
      AND path LIKE $3
      AND modifier = $4
    ORDER BY timestamp DESC
    LIMIT 10"
    ~params:[Uuid canvas_id; String module_; String route; String modifier]
  |> List.map ~f:(function
         | [trace_id] ->
             Util.uuid_of_string trace_id
         | _ ->
             Exception.internal "Bad DB format for stored_events" )


let clear_all_events ~(canvas_id : Uuidm.t) () : unit =
  Db.run
    ~name:"stored_event.clear_events"
    "DELETE FROM stored_events_v2
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]


let get_recent_event_traceids ~(canvas_id : Uuidm.t) event_rec =
  let module_, path, modifier, _, _ = event_rec in
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


(* see comment on Stored_event.trim_results for why this query *)
let trim_events () : int =
  Db.delete
    ~name:"stored_event.trim_events"
    "DELETE FROM stored_events_v2
    WHERE trace_id IN (
      SELECT trace_id FROM (
        SELECT row_number()
        OVER (PARTITION BY canvas_id, module, path, modifier ORDER BY timestamp
desc) as rownum, t.trace_id
        FROM stored_events_v2 t
        WHERE timestamp < (NOW() - interval '1 week')
        LIMIT 10000) as u
      WHERE rownum > 10
      LIMIT 10000
    )"
    ~params:[]
