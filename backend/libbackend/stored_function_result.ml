open Core_kernel
open Libexecution
open Analysis_types
open Types
module RTT = Types.RuntimeT
open Libcommon
module Db = Libbackend_basics.Db

(* ------------------------- *)
(* External *)
(* ------------------------- *)

let store_v2
    ~canvas_id ~trace_id (tlid, fnname, id) (arglist : RTT.dval list) result =
  if canvas_id = Stored_event.throttled
  then ()
  else
    Db.run
      ~name:"stored_function_result.store"
      "INSERT INTO function_results_v2
      (canvas_id, trace_id, tlid, fnname, id, hash, hash_version, timestamp, value)
      VALUES ($1, $2, $3, $4, $5, $6, $7, CURRENT_TIMESTAMP, $8)"
      ~params:
        [ Uuid canvas_id
        ; Uuid trace_id
        ; ID tlid
        ; String fnname
        ; ID id
        ; String (Dval.hash Dval.current_hash_version arglist)
        ; Int Dval.current_hash_version
        ; RoundtrippableDval result ]


let store_v3
    ~canvas_id ~trace_id (tlid, fnname, id) (arglist : RTT.dval list) result =
  if canvas_id = Stored_event.throttled
  then ()
  else
    Db.run
      ~name:"stored_function_result.store"
      "INSERT INTO function_results_v3
      (canvas_id, trace_id, tlid, fnname, id, hash, hash_version, timestamp, value)
      VALUES ($1, $2, $3, $4, $5, $6, $7, CURRENT_TIMESTAMP, $8)"
      ~params:
        [ Uuid canvas_id
        ; Uuid trace_id
        ; ID tlid
        ; String fnname
        ; ID id
        ; String (Dval.hash Dval.current_hash_version arglist)
        ; Int Dval.current_hash_version
        ; RoundtrippableDval result ]


let store = store_v2

let load_v2 ~canvas_id ~trace_id tlid : function_result list =
  (* Right now, we don't allow the user to see multiple results when a function
   * is called in a loop. But, there's a lot of data when functions are called
   * in a loop, so avoid massive responses. *)
  Db.fetch
    ~name:"sfr_load"
    "SELECT
       DISTINCT ON (fnname, id, hash, hash_version)
       fnname, id, hash, hash_version, value, timestamp
     FROM function_results_v2
     WHERE canvas_id = $1
       AND trace_id = $2
       AND tlid = $3
     ORDER BY fnname, id, hash, hash_version, timestamp DESC"
    ~params:[Db.Uuid canvas_id; Db.Uuid trace_id; Db.ID tlid]
  |> List.map ~f:(function
         | [fnname; id; hash; hash_version; dval; ts] ->
             ( fnname
             , id_of_string id
             , hash
               (* hash_version is nullable, nulls come back as empty string *)
             , (match hash_version with "" -> 0 | hv -> hv |> int_of_string)
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions_results.load")


let load_v3 ~canvas_id ~trace_id tlid : function_result list =
  (* Right now, we don't allow the user to see multiple results when a function
   * is called in a loop. But, there's a lot of data when functions are called
   * in a loop, so avoid massive responses. *)
  Db.fetch
    ~name:"sfr_load"
    "SELECT
       DISTINCT ON (fnname, id, hash, hash_version)
       fnname, id, hash, hash_version, value, timestamp
     FROM function_results_v3
     WHERE canvas_id = $1
       AND trace_id = $2
       AND tlid = $3
     ORDER BY fnname, id, hash, hash_version, timestamp DESC"
    ~params:[Db.Uuid canvas_id; Db.Uuid trace_id; Db.ID tlid]
  |> List.map ~f:(function
         | [fnname; id; hash; hash_version; dval; ts] ->
             ( fnname
             , id_of_string id
             , hash
               (* hash_version is nullable, nulls come back as empty string *)
             , (match hash_version with "" -> 0 | hv -> hv |> int_of_string)
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions_results.load")


let load ~canvas_id ~trace_id tlid : function_result list =
  (* DISABLED FOR NOW UNTIL THE MIGRATION IS DONE *)
  (* let v3_results = load_v3 ~canvas_id ~trace_id tlid in *)
  (* if List.length v3_results >= 10 then v3_results else *)
  load_v2 ~canvas_id ~trace_id tlid


(** trim_results_for_canvas is like trim_results but for a single canvas.
 *
 * All the comments and warnings there apply. Please read them. *)
type trim_results_action = Stored_event.trim_events_action

(* Works for both handlers and functions *)
let trim_toplevel
    span
    (action : trim_results_action)
    ~canvas_id
    ~canvas_name
    ~tlid
    ~before
    ~traces
    ~limit
    ~(typ : string) =
  let db_fn trim_events_action =
    match action with Count -> Db.fetch_count | Delete -> Db.delete
  in
  let action_str =
    match action with Count -> "SELECT count(*)" | Delete -> "DELETE"
  in
  Telemetry.Span.set_attrs
    span
    [ ("limit", `Int limit)
    ; ("canvas_id", `String (canvas_id |> Uuidm.to_string))
    ; ("canvas_name", `String canvas_name)
    ; ("tlid", `String (string_of_id tlid))
    ; ("type", `String typ)
    ; ("action", `String action_str) ] ;
  let count =
    (db_fn action)
      ~name:"gc_function_results_for_handler"
      (* can't do limit on delete, so use nested clause. Chose the
       * fields to match the idx_function_results_v2_most_recent index
       * *)
      (Printf.sprintf
         "WITH to_delete AS (SELECT canvas_id, trace_id, tlid, timestamp
                                     FROM function_results_v2
                                    WHERE canvas_id = $1
                                      AND tlid = $2
                                      AND timestamp < $3
                                      AND (NOT trace_id = ANY (string_to_array($4, $5)::uuid[]))
                                    LIMIT $6)
                %s FROM function_results_v2
                  WHERE (canvas_id, trace_id, tlid, timestamp) IN
                    (SELECT canvas_id, trace_id, tlid, timestamp
                     FROM to_delete);"
         action_str)
      ~params:
        [ Db.Uuid canvas_id
        ; Db.ID tlid
        ; Db.Time before
        ; traces |> List.map ~f:(fun t -> Db.Uuid t) |> Db.List
        ; String Db.array_separator
        ; Db.Int limit ]
  in

  Telemetry.Span.set_attr span "row_count" (`Int count) ;
  count


let trim_results_for_handler
    (span : Libcommon.Telemetry.Span.t)
    (action : trim_results_action)
    ~(before : Time.t)
    ~(limit : int)
    ~(tlid : tlid)
    ~(canvas_name : string)
    ~(module_ : string)
    ~(path : string)
    ~(modifier : string)
    (canvas_id : Uuidm.t) : int =
  Telemetry.with_span span "trim_results_for_handler" (fun span ->
      Telemetry.Span.set_attrs
        span
        [ ("module", `String module_)
        ; ("path", `String path)
        ; ("modifier", `String modifier) ] ;
      let valid_traces =
        Db.fetch
          ~name:"valid_traces"
          "SELECT DISTINCT trace_id
           FROM stored_events_v2
           WHERE canvas_id = $1
             AND module = $2
             AND path LIKE $3
             AND modifier = $4
             AND timestamp < $5
           LIMIT 1000"
          ~params:
            [ Db.Uuid canvas_id
            ; Db.String module_
            ; Db.String path
            ; Db.String modifier
            ; Db.Time before ]
        |> List.map ~f:(fun uuid ->
               uuid |> List.hd_exn |> Uuidm.of_string |> Option.value_exn)
      in
      let count =
        (* Each handler should only have 10 traces newer than a week *)
        if List.length valid_traces >= 1000
        then (
          Log.erroR "db error" ~params:[("err", "Too many traces")] ;
          Telemetry.Span.set_attr span "too_many" (`Bool true) ;
          0 )
        else
          trim_toplevel
            span
            action
            ~canvas_id
            ~canvas_name
            ~typ:"handler"
            ~tlid
            ~before
            ~traces:valid_traces
            ~limit
      in
      Telemetry.Span.set_attr span "row_count" (`Int count) ;
      count)


(* TODO: The index is set up trace_id first, not tlid first. That means we
 * should delete by trace, not by handler *)
let trim_results_for_handlers
    (span : Libcommon.Telemetry.Span.t)
    (action : trim_results_action)
    ~(before : Time.t)
    ~(limit : int)
    ~(canvas_name : string)
    (canvas_id : Uuidm.t) : int * int =
  let handlers = Stored_event.get_handlers_for_canvas canvas_id in
  let row_count =
    handlers
    |> List.map ~f:(fun (tlid, (module_, path, modifier)) ->
           Stored_event.repeat_while_hitting_limit
             ~span
             ~action
             ~limit
             ~f:(fun () ->
               trim_results_for_handler
                 span
                 action
                 ~before
                 ~tlid
                 ~module_
                 ~path
                 ~modifier
                 ~canvas_name
                 ~limit
                 canvas_id))
    |> Tc.List.sum
  in
  (List.length handlers, row_count)


let trim_results_for_function
    (span : Libcommon.Telemetry.Span.t)
    (action : trim_results_action)
    ~(before : Time.t)
    ~(traces : Uuidm.t list)
    ~(limit : int)
    ~(canvas_name : string)
    ~(tlid : id)
    (canvas_id : Uuidm.t) : int =
  Telemetry.with_span span "trim_results_for_function" (fun span ->
      trim_toplevel
        span
        action
        ~typ:"function"
        ~canvas_id
        ~canvas_name
        ~tlid
        ~before
        ~traces
        ~limit)


let get_canvas_functions ~span ~canvas_name ~canvas_id () : tlid list =
  Telemetry.with_span
    span
    "get_functions_for_canvas"
    ~attrs:[("canvas_name", `String canvas_name)]
    (fun span ->
      Db.fetch
        ~name:"get_functions_for_gc"
        "SELECT tlid
                FROM toplevel_oplists
                WHERE canvas_id = $1
                AND tipe = 'user_function';"
        ~params:[Db.Uuid canvas_id]
      |> List.map ~f:(fun tlid -> tlid |> List.hd_exn |> id_of_string))


let get_all_handler_traces ~span ~canvas_name ~canvas_id ~before () :
    Uuidm.t list =
  Telemetry.with_span
    span
    "get_all_handler_traces"
    ~attrs:[("canvas_name", `String canvas_name)]
    (fun span ->
      Db.fetch
        ~name:"valid_traces"
        "SELECT DISTINCT trace_id
         FROM stored_events_v2
         WHERE canvas_id = $1
           AND timestamp < $2
         LIMIT 1000"
        ~params:[Db.Uuid canvas_id; Db.Time before]
      |> List.map ~f:(fun uuid ->
             uuid |> List.hd_exn |> Uuidm.of_string |> Option.value_exn))


let trim_results_for_functions
    (span : Libcommon.Telemetry.Span.t)
    (action : trim_results_action)
    ~(before : Time.t)
    ~(limit : int)
    ~(canvas_name : string)
    (canvas_id : Uuidm.t) : int * int =
  (* Unlike handlers, a function can have traces that it didn't create, as it
   * can be called by both handlers and other functions' default traces. As a
   * result, we need the global set of traces in the entire canvas to be able
   * to trim this effectively. *)
  (* We have a set of global traces, which we hope have already been trimmed to
   * a reasonable size. *)
  let handler_traces =
    get_all_handler_traces ~span ~canvas_name ~canvas_id ~before ()
  in
  let all_functions = get_canvas_functions ~span ~canvas_name ~canvas_id () in
  let all_function_traces =
    let traceid_of_tlid (tlid : tlid) : Uuidm.t =
      (* Copied from analysis.ml *)
      Uuidm.v5 Uuidm.nil (string_of_id tlid)
    in
    all_functions |> List.map ~f:traceid_of_tlid
  in
  let all_traces = handler_traces @ all_function_traces in
  let row_count =
    all_functions
    |> List.map ~f:(fun tlid ->
           Stored_event.repeat_while_hitting_limit
             ~span
             ~action
             ~limit
             ~f:(fun () ->
               trim_results_for_function
                 span
                 action
                 ~before
                 ~traces:all_traces
                 ~tlid
                 ~canvas_name
                 ~limit
                 canvas_id))
    |> Tc.List.sum
  in
  (List.length all_functions, row_count)


let trim_results_for_canvas
    (span : Libcommon.Telemetry.Span.t)
    (action : trim_results_action)
    ~(limit : int)
    ~(canvas_name : string)
    (canvas_id : Uuidm.t) : int =
  Telemetry.with_span span "trim_results_for_canvas" (fun span ->
      try
        if Tc.List.member
             canvas_id
             [ Uuidm.of_string "730b77ce-f505-49a8-80c5-8cabb481d60d"
               |> Option.value_exn ]
        then (
          Log.infO
            ~params:[("canvas_id", Uuidm.to_string canvas_id)]
            "skipping large canvas" ;
          0 )
        else
          (* We pick an exact time so that it's synced, and pick a time where we
           can avoid being affected by current operations coming in. This could be
           tuned better. *)
          let before = Time.sub (Time.now ()) (Time.Span.of_day 7.0) in
          let handler_count, handler_row_count =
            trim_results_for_handlers
              span
              action
              ~before
              ~limit
              ~canvas_name
              canvas_id
          in
          let function_count, function_row_count =
            trim_results_for_functions
              span
              action
              ~before
              ~limit
              ~canvas_name
              canvas_id
          in
          let row_count = function_row_count + handler_row_count in
          Telemetry.Span.set_attrs
            span
            [ ("handler_count", `Int handler_count)
            ; ("function_count", `Int function_count)
            ; ("function_row_count", `Int function_row_count)
            ; ("handler_row_count", `Int handler_row_count)
            ; ("row_count", `Int row_count)
            ; ("canvas_name", `String canvas_name)
            ; ("canvas_id", `String (canvas_id |> Uuidm.to_string)) ] ;
          row_count
      with Exception.DarkException e ->
        Log.erroR
          "error trimming stored_function_results"
          ~params:
            [ ("canvas_name", canvas_name)
            ; ( "err"
              , e |> Exception.exception_data_to_yojson |> Yojson.Safe.to_string
              ) ] ;
        Exception.reraise (Exception.DarkException e))
