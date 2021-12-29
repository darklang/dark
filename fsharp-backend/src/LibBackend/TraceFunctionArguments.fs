module LibBackend.TraceFunctionArguments

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

module AT = LibExecution.AnalysisTypes
module RT = LibExecution.RuntimeTypes
module DvalRepr = LibExecution.DvalRepr

// -------------------------
// External *)
// -------------------------

let store
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlid : tlid)
  (args : RT.DvalMap)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    Sql.query
      "INSERT INTO function_arguments
      (canvas_id, trace_id, tlid, timestamp, arguments_json)
      VALUES (@canvasID, @traceID, @tlid, CURRENT_TIMESTAMP, @args)"
    |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                        "traceID", Sql.uuid traceID
                        "tlid", Sql.tlid tlid
                        ("args",
                         Sql.string (
                           DvalRepr.toInternalRoundtrippableV0 (RT.DObj args)
                         )) ]
    |> Sql.executeStatementAsync


let loadForAnalysis
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlid : tlid)
  : Task<Option<AT.InputVars * System.DateTime>> =
  // We need to alias the subquery (here aliased as `q`) because Postgres
  // requires inner SELECTs to be aliased.
  Sql.query
    "SELECT arguments_json, timestamp FROM (
       SELECT DISTINCT ON (trace_id) trace_id, timestamp, arguments_json
       FROM function_arguments
       WHERE canvas_id = @canvasID
         AND tlid = @tlid
         AND trace_id = @traceID
       ORDER BY trace_id, timestamp DESC
     ) AS q
     ORDER BY timestamp DESC
     LIMIT 1"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "tlid", Sql.id tlid
                      "traceID", Sql.uuid traceID ]
  |> Sql.executeRowOptionAsync (fun read ->
    (read.string "arguments_json"
     |> DvalRepr.ofInternalRoundtrippableV0
     |> fun dv -> RT.Dval.toPairs dv, read.dateTime "timestamp"))


let loadTraceIDs (canvasID : CanvasID) (tlid : tlid) : Task<List<AT.TraceID>> =
  // We need to alias the subquery (here aliased as `q`) because Postgres
  // requires inner SELECTs to be aliased.
  Sql.query
    "SELECT trace_id FROM (
       SELECT DISTINCT ON (trace_id) trace_id, timestamp
       FROM function_arguments
       WHERE canvas_id = @canvasID
         AND tlid = @tlid
       ORDER BY trace_id, timestamp DESC
     ) AS q
     ORDER BY timestamp DESC
     LIMIT 10"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlid", Sql.id tlid ]
  |> Sql.executeAsync (fun read -> read.uuid "trace_id")

// type trim_arguments_action = Stored_event.trim_events_action
//
// let trim_arguments_for_handler
//     (span : Libcommon.Telemetry.Span.t)
//     (action : trim_arguments_action)
//     ~(limit : int)
//     ~(canvas_name : string)
//     ~(tlid : string)
//     (canvas_id : Uuidm.t) : int =
//   Telemetry.with_span span "trim_arguments_for_handler" (fun span ->
//       let db_fn trim_events_action =
//         match action with Count -> Db.fetch_count | Delete -> Db.delete
//       in
//       let action_str =
//         match action with Count -> "SELECT count(*)" | Delete -> "DELETE"
//       in
//       Telemetry.Span.set_attrs
//         span
//         [ ("limit", `Int limit)
//         ; ("canvas_id", `String (canvas_id |> Uuidm.to_string))
//         ; ("canvas_name", `String canvas_name)
//         ; ("tlid", `String tlid)
//         ; ("action", `String action_str) ] ;
//       let count =
//         try
//           (db_fn action)
//             ~name:"gc_function_arguments"
//             (Printf.sprintf
//                "WITH last_ten AS (
//                   SELECT canvas_id, tlid, trace_id
//                   FROM function_arguments
//                   WHERE canvas_id = $1
//                     AND tlid = $2
//                     AND timestamp < (NOW() - interval '1 week') LIMIT 10),
//               to_delete AS (
//                 SELECT canvas_id, tlid, trace_id
//                   FROM function_arguments
//                   WHERE canvas_id = $1
//                     AND tlid = $2
//                     AND timestamp < (NOW() - interval '1 week')
//                   LIMIT $3)
//               %s FROM function_arguments
//                 WHERE canvas_id = $1
//                   AND tlid = $2
//                   AND timestamp < (NOW() - interval '1 week')
//                   AND (canvas_id, tlid, trace_id) NOT IN (SELECT canvas_id, tlid, trace_id FROM last_ten)
//                   AND (canvas_id, tlid, trace_id) IN (SELECT canvas_id, tlid, trace_id FROM to_delete);"
//                action_str)
//             ~params:[Db.Uuid canvas_id; Db.String tlid; Db.Int limit]
//         with Exception.DarkException e ->
//           Log.erroR
//             "db error"
//             ~params:
//               [ ( "err"
//                 , e
//                   |> Exception.exception_data_to_yojson
//                   |> Yojson.Safe.to_string ) ] ;
//           Exception.reraise (Exception.DarkException e)
//       in
//       Telemetry.Span.set_attr span "row_count" (`Int count) ;
//       count)
//
//
// (** trim_arguments_for_canvas is like trim_arguments_for_canvas but for a single canvas.
//  *
//  * All the comments and warnings there apply. Please read them. *)
// let trim_arguments_for_canvas
//     (span : Libcommon.Telemetry.Span.t)
//     (action : trim_arguments_action)
//     ~(limit : int)
//     ~(canvas_name : string)
//     (canvas_id : Uuidm.t) : int =
//   Telemetry.with_span span "trim_arguments_for_canvas" (fun span ->
//       let handlers =
//         Telemetry.with_span
//           span
//           "get_user_functions_for_canvas"
//           ~attrs:[("canvas_name", `String canvas_name)]
//           (fun span ->
//             ( try
//                 Db.fetch
//                   ~name:"get_user_functions_for_gc"
//                   "SELECT tlid
//                    FROM toplevel_oplists
//                    WHERE canvas_id = $1
//                    AND tipe = 'user_function';"
//                   ~params:[Db.Uuid canvas_id]
//               with Exception.DarkException e ->
//                 Log.erroR
//                   "db error"
//                   ~params:
//                     [ ( "err"
//                       , e
//                         |> Exception.exception_data_to_yojson
//                         |> Yojson.Safe.to_string ) ] ;
//                 Exception.reraise (Exception.DarkException e) )
//             (* List.hd_exn - we're only returning one field from this query *)
//             |> List.map ~f:(fun tlid -> tlid |> List.hd_exn))
//       in
//       let row_count : int =
//         handlers
//         |> List.map ~f:(fun tlid ->
//                trim_arguments_for_handler
//                  span
//                  action
//                  ~tlid
//                  ~canvas_name
//                  ~limit
//                  canvas_id)
//         |> Tc.List.sum
//       in
//       Telemetry.Span.set_attrs
//         span
//         [ ("handler_count", `Int (handlers |> List.length))
//         ; ("row_count", `Int row_count)
//         ; ("canvas_name", `String canvas_name)
//         ; ("canvas_id", `String (canvas_id |> Uuidm.to_string)) ] ;
//       row_count)
//
