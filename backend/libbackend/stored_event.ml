open Core_kernel
open Libexecution
module RTT = Types.RuntimeT
open Libcommon

type event_desc = string * string * string [@@deriving show, yojson]

type event_record = string * string * string * RTT.time * Analysis_types.traceid
[@@deriving show, yojson]

type four_oh_four = event_record [@@deriving show, yojson]

let event_subject module_ path modifier = module_ ^ "_" ^ path ^ "_" ^ modifier

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
    ~subject:(event_subject module_ path modifier)
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


let list_events ~(limit : [`All | `Since of RTT.time]) ~(canvas_id : Uuidm.t) ()
    : event_record list =
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
             Exception.internal "Bad DB format for stored_events")


let load_events ~(canvas_id : Uuidm.t) ((module_, route, modifier) : event_desc)
    : (string * Uuidm.t * RTT.time * RTT.dval) list =
  let route = Http.route_to_postgres_pattern route in
  Db.fetch
    ~name:"load_events"
    ~subject:(event_subject module_ route modifier)
    "SELECT path, value, timestamp, trace_id FROM stored_events_v2
    WHERE canvas_id = $1
      AND module = $2
      AND path LIKE $3
      AND modifier = $4
    ORDER BY timestamp DESC
    LIMIT 10"
    (* the number in the LIMIT is shared with Analysis.mergeTraces on the client *)
    ~params:[Uuid canvas_id; String module_; String route; String modifier]
  |> List.map ~f:(function
         | [request_path; dval; ts; trace_id] ->
             let trace_id = Util.uuid_of_string trace_id in
             ( request_path
             , trace_id
             , Util.date_of_isostring ts
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal "Bad DB format for load_events")


let load_event_for_trace ~(canvas_id : Uuidm.t) (trace_id : Uuidm.t) :
    (string * RTT.time * RTT.dval) option =
  Db.fetch
    ~name:"load_event_for_trace"
    ~subject:(Uuidm.to_string trace_id)
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
             Exception.internal "Bad DB format for load_event_for_trace")


let load_event_ids
    ~(canvas_id : Uuidm.t) ((module_, route, modifier) : event_desc) :
    (Uuidm.t * string) list =
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
    ~subject:(event_subject module_ route modifier)
    "SELECT trace_id, path FROM stored_events_v2
    WHERE canvas_id = $1
      AND module = $2
      AND path LIKE $3
      AND modifier = $4
    ORDER BY timestamp DESC
    LIMIT 10"
    ~params:[Uuid canvas_id; String module_; String route; String modifier]
  |> List.map ~f:(function
         | [trace_id; path] ->
             (Util.uuid_of_string trace_id, path)
         | _ ->
             Exception.internal "Bad DB format for stored_events")


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
    ~subject:(event_subject module_ path modifier)
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
             if trace_id = "" then None else Some (Util.uuid_of_string trace_id)
         | _ ->
             Exception.internal "Bad DB format for stored_events")


type trim_events_action =
  | Count
  | Delete

let trim_events_for_handler
    ~(span : Libcommon.Telemetry.Span.t)
    ?(action : trim_events_action = Count)
    ~(limit : int)
    ~(module_ : string)
    ~(modifier : string)
    ~(path : string)
    ~(canvas_name : string)
    ~(canvas_id : Uuidm.t) : int =
  Telemetry.with_span span "trim_events_for_handler" (fun span ->
      let db_fn trim_events_action =
        match action with Count -> Db.fetch_count | Delete -> Db.delete
      in
      let action_str =
        match action with Count -> "SELECT count(*)" | Delete -> "DELETE"
      in
      Telemetry.Span.set_attrs
        span
        [ ("limit", `Int limit)
        ; ("module", `String module_)
        ; ("modifier", `String modifier)
        ; ("path", `String path)
        ; ("canvas_name", `String canvas_name)
        ; ("canvas_id", `String (canvas_id |> Uuidm.to_string))
        ; ("action", `String action_str) ] ;
      let count =
        try
          (db_fn action)
            ~name:"gc"
            (* the WHERE conditions in the final query, prior to
             * 'trace_id IN (... to_delete)', are logically redundant with the
             * to_delete subquery, but they improve performance by allowing the
             * query to make use of the index. *)
            (Printf.sprintf
               "WITH last_ten AS (
                SELECT trace_id
                FROM stored_events_v2
                WHERE module = $1
                AND modifier = $2
                AND path = $3
                AND canvas_id = $4
                AND timestamp < (NOW() - interval '1 week') LIMIT 10
              ),
              to_delete AS (SELECT trace_id FROM stored_events_v2
                WHERE module = $1
                AND modifier = $2
                AND path = $3
                AND canvas_id = $4
                AND timestamp < (NOW() - interval '1 week')
                AND trace_id NOT IN (SELECT trace_id FROM last_ten)
                LIMIT $5)
              %s FROM stored_events_v2
                WHERE module = $1
                AND modifier = $2
                AND path = $3
                AND canvas_id = $4
                AND timestamp < (NOW() - interval '1 week')
                AND trace_id IN (SELECT trace_id FROM to_delete);"
               action_str)
            ~params:
              [ Db.String module_
              ; Db.String modifier
              ; Db.String path
              ; Db.Uuid canvas_id
              ; Db.Int limit ]
        with Exception.DarkException e ->
          Log.erroR
            "db error"
            ~params:
              [ ( "err"
                , e
                  |> Exception.exception_data_to_yojson
                  |> Yojson.Safe.to_string ) ] ;
          Exception.reraise (Exception.DarkException e)
      in
      Telemetry.Span.set_attr span "row_count" (`Int count) ;
      count)


(** trim_events removes all stored_events_v2 records older than a week, leaving
 * at minimum 10 records for each unique handler on a canvas regardless of age.
 *
 * Returns the number of rows deleted.
 *
 * CAVEAT: in order to keep our DB from bursting into flames given a large
 * number of records to cleanup, we cap the maximum number of records deleted
 * per call to 10_000.
 *
 * See also
 * - Stored_function_result.trim_results
 * - Stored_function_arguments.trum_arguments
 * which are nearly identical queries on different tables *)
let trim_events () : int =
  Db.delete
    ~name:"stored_event.trim_events"
    "WITH indexed_events AS (
       SELECT trace_id, row_number() OVER (
         PARTITION BY canvas_id, module, path, modifier
         ORDER BY timestamp DESC
       ) as rownum
       FROM stored_events_v2
       WHERE timestamp < (NOW() - interval '1 week')
    )
    DELETE FROM stored_events_v2 WHERE trace_id IN (
      SELECT trace_id FROM indexed_events
      WHERE rownum > 10
      LIMIT 10000
    )"
    ~params:[]


(** trim_events_for_canvas is like trim_events but for a single canvas.
 *
 * All the comments and warnings there apply. Please read them. *)
let trim_events_for_canvas
    ~(span : Telemetry.Span.t)
    ?(action : trim_events_action = Count)
    (canvas_id : Uuidm.t)
    (canvas_name : string)
    (limit : int) : int =
  Telemetry.with_span span "trim_events_for_canvas" (fun span ->
      let handlers =
        Telemetry.with_span
          span
          "get_handlers_for_canvas"
          ~attrs:[("canvas_name", `String canvas_name)]
          (fun span ->
            ( try
                (* modifier, module, name are IS NOT NULL here because the
                 * equivalent fields in stored_events_v2 (modifier, module, path)
                 * are all marked NOT NULL. In production, that cuts us down from
                 * 102k rows (42k distinct) to 27k rows (and about the same #
                 * distinct) *)
                Db.fetch
                  ~name:"get_handlers_for_gc"
                  "SELECT module, modifier, name
                   FROM toplevel_oplists
                   WHERE canvas_id = $1
                   AND modifier IS NOT NULL
                   AND module IS NOT NULL
                   AND name IS NOT NULL;"
                  ~params:[Db.Uuid canvas_id]
              with Exception.DarkException e ->
                Log.erroR
                  "db error"
                  ~params:
                    [ ( "err"
                      , e
                        |> Exception.exception_data_to_yojson
                        |> Yojson.Safe.to_string ) ] ;
                Exception.reraise (Exception.DarkException e) )
            |> List.map ~f:(function
                   | [module_; modifier; path] ->
                       (module_, modifier, path)
                   | xs ->
                       Log.erroR
                         "wrong shape"
                         ~params:[("result", xs |> String.concat ~sep:",")] ;
                       Exception.internal "Wrong shape in get_handlers_for_db"))
      in
      let row_count : int =
        handlers
        |> List.map ~f:(fun (module_, modifier, path) ->
               trim_events_for_handler
                 ~span
                 ~action
                 ~limit
                 ~module_
                 ~modifier
                 ~path
                 ~canvas_name
                 ~canvas_id)
        |> Tc.List.sum
      in
      Telemetry.Span.set_attrs
        span
        [ ("handler_count", `Int (handlers |> List.length))
        ; ("row_count", `Int row_count)
        ; ("canvas_name", `String canvas_name)
        ; ("canvas_id", `String (canvas_id |> Uuidm.to_string)) ] ;
      row_count)
