(** Stored events. These are the "input values" for handler traces, containing
  * the `request` or `event` for a trace.
  *
  * We keep traces around for a week, and also keep the last 10 regardless of age.
  *
  * Traces are also used for 404s - which are just traces for which a handler doesn't exist.
  * Note that traces are stored for routes (technically for an `event_desc`), not for handlers.
  * There is a GC process to clean these up.
  *)

open Core_kernel
open Libexecution
module RTT = Types.RuntimeT
open Libcommon
module Db = Libbackend_basics.Db

type event_desc = string * string * string [@@deriving show, yojson]

type event_record = string * string * string * RTT.time * Analysis_types.traceid
[@@deriving show, yojson]

type four_oh_four = event_record [@@deriving show, yojson]

let event_subject module_ path modifier = module_ ^ "_" ^ path ^ "_" ^ modifier

(* ------------------------- *)
(* Event data *)
(* ------------------------- *)

(* Note that this returns munged version of the name, that are designed for
 * pattern matching using postgres' LIKE syntax. *)
let get_handlers_for_canvas (canvas_id : Uuidm.t) :
    (Types.tlid * event_desc) list =
  Db.fetch
    ~name:"get_handlers_for_canvas"
    "SELECT tlid, module, name, modifier FROM toplevel_oplists
            WHERE canvas_id = $1
              AND module IS NOT NULL
              AND name IS NOT NULL
              AND modifier IS NOT NULL
              AND tipe = 'handler'::toplevel_type"
    ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(function
         | [tlid; modu; n; modi] ->
             (Types.id_of_string tlid, (modu, n, modi))
         | _ ->
             Exception.internal "Bad DB format for get_handlers_for_canvas")


let throttled =
  Uuidm.of_string "730b77ce-f505-49a8-80c5-8cabb481d60d" |> Option.value_exn


(* ------------------------- *)
(* Event data *)
(* ------------------------- *)
let store_event
    ~(trace_id : Uuidm.t)
    ~(canvas_id : Uuidm.t)
    ?(timestamp : Time.t = Time.now ())
    ((module_, path, modifier) : event_desc)
    (event : RTT.dval) : RTT.time =
  if canvas_id = throttled
  then timestamp
  else
    Db.fetch_one
      ~name:"stored_event.store_event"
      ~subject:(event_subject module_ path modifier)
      "INSERT INTO stored_events_v2
      (canvas_id, trace_id, module, path, modifier, timestamp, value)
      VALUES ($1, $2, $3, $4, $5, $6, $7)
      RETURNING timestamp"
      ~params:
        [ Uuid canvas_id
        ; Uuid trace_id
        ; String module_
        ; String path
        ; String modifier
        ; Time timestamp
        ; RoundtrippableDval event ]
    |> List.hd_exn
    |> Util.date_of_isostring


let list_events
    ~(limit : [`All | `After of RTT.time | `Before of RTT.time])
    ~(canvas_id : Uuidm.t)
    () : event_record list =
  let timestamp_constraint =
    match limit with
    | `All ->
        ""
    | `After ts ->
        "AND timestamp > " ^ Db.escape (Time ts)
    | `Before ts ->
        "AND timestamp < " ^ Db.escape (Time ts)
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


let list_event_descs
    ~(limit : [`All | `After of RTT.time | `Before of RTT.time])
    ~(canvas_id : Uuidm.t)
    () : event_desc list =
  let timestamp_constraint =
    match limit with
    | `All ->
        ""
    | `After ts ->
        "AND timestamp > " ^ Db.escape (Time ts)
    | `Before ts ->
        "AND timestamp < " ^ Db.escape (Time ts)
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
     module, path, modifier
     FROM stored_events_v2
     WHERE canvas_id = $1"
    ^ timestamp_constraint
  in
  Db.fetch sql ~name:"list_events" ~params:[Db.Uuid canvas_id]
  |> List.map ~f:(function
         | [module_; path; modifier] ->
             (module_, path, modifier)
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


let munge_path_for_postgres module_ path =
  (* Only munge the route for HTTP events, as they have wildcards, whereas
   * background events are completely concrete.
   *
   * `split_uri_path` inside `Http.route_to_postgres_pattern` doesn't like that background
   * events don't have leading slashes. *)
  if String.Caseless.equal module_ "HTTP"
  then Http.route_to_postgres_pattern path
  else
    (* https://www.postgresql.org/docs/9.6/functions-matching.html *)
    path |> Util.string_replace "%" "\\%" |> Util.string_replace "_" "\\_"


let load_event_ids
    ~(canvas_id : Uuidm.t) ((module_, route, modifier) : event_desc) :
    (Uuidm.t * string) list =
  let route = munge_path_for_postgres module_ route in
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


let get_404s ~limit (canvas_id : Uuidm.t) : four_oh_four list =
  let events = list_events ~limit ~canvas_id () in
  let handlers = get_handlers_for_canvas canvas_id in
  let match_event h event : bool =
    let space, request_path, modifier, _ts, _ = event in
    let h_space, h_name, h_modifier = h in
    Http.request_path_matches_route ~route:h_name request_path
    && h_modifier = modifier
    && h_space = space
  in
  events
  |> List.filter ~f:(fun e ->
         not (List.exists handlers ~f:(fun (_tlid, h) -> match_event h e)))


(* Cut back version of get_404s which hits the index. *)
let get_404_descs ~limit (canvas_id : Uuidm.t) : event_desc list =
  let events = list_event_descs ~limit ~canvas_id () in
  let handlers = get_handlers_for_canvas canvas_id in
  let match_event h event : bool =
    let space, request_path, modifier = event in
    let h_space, h_name, h_modifier = h in
    let path_matches =
      if h_space = "HTTP"
      then Http.request_path_matches_route ~route:h_name request_path
      else h_name = request_path
    in
    path_matches && h_modifier = modifier && h_space = space
  in
  events
  |> List.filter ~f:(fun e ->
         not (List.exists handlers ~f:(fun (_tlid, h) -> match_event h e)))


let clear_all_events ~(canvas_id : Uuidm.t) () : unit =
  Db.run
    ~name:"stored_event.clear_events"
    "DELETE FROM stored_events_v2
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]


(* ------------------------- *)
(* Garbage collection  *)
(* ------------------------- *)
type trim_events_action =
  | Count
  | Delete

let action_to_string action =
  match action with Count -> "SELECT count(*)" | Delete -> "DELETE"


let db_fn action =
  match action with Count -> Db.fetch_count | Delete -> Db.delete


let repeat_while_hitting_limit
    ~action ~(span : Telemetry.Span.t) ~(limit : int) ~(f : unit -> int) : int =
  (* We want to keep the limit small to avoid hurting the DB too
   * much. But that doesn't do a great job of deleting this data
   * over time. So repeat lots of times so long as we're still
   * deleting the limit. *)
  match action with
  | Delete ->
      let total_rows = ref 0 in
      let repeat = ref true in
      let iterations = ref 0 in
      while !repeat && !iterations < 100 do
        Telemetry.with_span
          span
          "iteration"
          ~attrs:[("iteration", `Int !iterations)]
          (fun span ->
            let deleted_count = f () in
            total_rows := !total_rows + deleted_count ;
            iterations := !iterations + 1 ;
            repeat := deleted_count = limit)
      done ;
      !total_rows
  | Count ->
      f ()


type trim_events_canvases =
  | All
  | JustOne of string

let trim_events_for_handler
    ~(span : Libcommon.Telemetry.Span.t)
    ~(action : trim_events_action)
    ~(limit : int)
    ~(module_ : string)
    ~(path : string)
    ~(modifier : string)
    ~(canvas_name : string)
    ~(canvas_id : Uuidm.t) : int =
  let action_str = action_to_string action in
  Telemetry.with_span span "trim_events_for_handler" (fun span ->
      Telemetry.Span.set_attrs
        span
        [ ("limit", `Int limit)
        ; ("module", `String module_)
        ; ("path", `String path)
        ; ("modifier", `String modifier)
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
                  AND path LIKE $2
                  AND modifier = $3
                  AND canvas_id = $4
                  AND timestamp < (NOW() - interval '1 week')
                  LIMIT 10),
              to_delete AS (
                SELECT trace_id FROM stored_events_v2
                  WHERE module = $1
                    AND path LIKE $2
                    AND modifier = $3
                    AND canvas_id = $4
                    AND timestamp < (NOW() - interval '1 week')
                    AND trace_id NOT IN (SELECT trace_id FROM last_ten)
                    LIMIT $5)
              %s FROM stored_events_v2
                WHERE module = $1
                  AND path LIKE $2
                  AND modifier = $3
                  AND canvas_id = $4
                  AND timestamp < (NOW() - interval '1 week')
                  AND trace_id IN (SELECT trace_id FROM to_delete);"
               action_str)
            ~params:
              [ Db.String module_
              ; Db.String path
              ; Db.String modifier
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


(** Remove all the stored_events that are older than a week, and then keeping
 * the latest from the last week if there is one. *)
let trim_404s
    ~(span : Telemetry.Span.t)
    ~(action : trim_events_action)
    (canvas_id : Uuidm.t)
    (canvas_name : string)
    (limit : int) : int =
  let action_str = action_to_string action in
  Telemetry.with_span
    span
    ~attrs:
      [ ("canvas_name", `String canvas_name)
      ; ("canvas_id", `String (canvas_id |> Uuidm.to_string))
      ; ("limit", `Int limit) ]
    "trim_404s"
    (fun span ->
      get_404_descs ~limit:`All canvas_id
      |> List.map ~f:(fun (module_, path, modifier) ->
             repeat_while_hitting_limit ~span ~action ~limit ~f:(fun () ->
                 Telemetry.with_span
                   span
                   "trim_404"
                   ~attrs:
                     [ ("module", `String module_)
                     ; ("path", `String path)
                     ; ("modifier", `String modifier)
                     ; ("action", `String action_str) ]
                   (fun span ->
                     let row_count : int =
                       Telemetry.Span.set_attrs
                         span
                         [ ("limit", `Int limit)
                         ; ("module", `String module_)
                         ; ("path", `String path)
                         ; ("modifier", `String modifier)
                         ; ("canvas_name", `String canvas_name)
                         ; ("canvas_id", `String (canvas_id |> Uuidm.to_string))
                         ; ("action", `String action_str) ] ;
                       try
                         (* postgres doesn't allow a limit in a DELETE so
                          * `to_delete` is the workaround. *)
                         (* We don't use LIKE here because we're trimming the exact 404. *)
                         (db_fn action)
                           ~name:"gc_404s"
                           (Printf.sprintf
                              "WITH latest_trace AS
                              (SELECT trace_id from stored_events_v2
                               WHERE module = $1
                                 AND path = $2
                                 AND modifier = $3
                                 AND canvas_id = $4
                                 ORDER BY timestamp DESC
                                 LIMIT 1),
                            to_delete AS (
                              SELECT trace_id FROM stored_events_v2
                              WHERE module = $1
                                AND path = $2
                                AND modifier = $3
                                AND canvas_id = $4
                                AND trace_id NOT IN (SELECT trace_id FROM latest_trace)
                                LIMIT $5)
                            %s FROM stored_events_V2
                              WHERE module = $1
                                AND path = $2
                                AND modifier = $3
                                AND canvas_id = $4
                                AND trace_id in (SELECT trace_id FROM to_delete)"
                              action_str)
                           ~params:
                             [ Db.String module_
                             ; Db.String path
                             ; Db.String modifier
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
                     Telemetry.Span.set_attrs
                       span
                       [("row_count", `Int row_count)] ;
                     row_count)))
      |> Tc.List.sum)


(** trim_events_for_canvas removes all stored_events_v2 records older than a week, leaving
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

let trim_events_for_canvas
    ~(span : Telemetry.Span.t)
    ~(action : trim_events_action)
    (canvas_id : Uuidm.t)
    (canvas_name : string)
    (limit : int) : int =
  Telemetry.with_span span "trim_events_for_canvas" (fun span ->
      let handlers =
        Telemetry.with_span
          span
          "get_handlers_for_canvas"
          ~attrs:[("canvas_name", `String canvas_name)]
          (fun span -> get_handlers_for_canvas canvas_id)
      in
      let row_count : int =
        handlers
        |> List.map ~f:(fun (tlid_, (module_, path, modifier)) ->
               repeat_while_hitting_limit ~span ~action ~limit ~f:(fun () ->
                   trim_events_for_handler
                     ~span
                     ~action
                     ~limit
                     ~module_
                     ~path
                     ~modifier
                     ~canvas_name
                     ~canvas_id))
        |> Tc.List.sum
      in

      let f404_count = trim_404s ~span ~action canvas_id canvas_name limit in
      Telemetry.Span.set_attrs
        span
        [ ("handler_count", `Int (handlers |> List.length))
        ; ("row_count", `Int (row_count + f404_count))
        ; ("canvas_name", `String canvas_name)
        ; ("canvas_id", `String (canvas_id |> Uuidm.to_string)) ] ;
      row_count + f404_count)
