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
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable

// -------------------------
// External
// -------------------------

type FunctionArgumentStore = tlid * RT.DvalMap * NodaTime.Instant

let store
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  (tlid : tlid)
  (args : RT.DvalMap)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    Sql.query
      "INSERT INTO trace_old_function_arguments_v0
      (canvas_id, trace_id, tlid, timestamp, arguments_json)
      VALUES (@canvasID, @traceID, @tlid, CURRENT_TIMESTAMP, @args)"
    |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                        "traceID", Sql.traceID traceID
                        "tlid", Sql.tlid tlid
                        ("args",
                         Sql.string (
                           DvalReprInternalRoundtrippable.toJsonV0 (RT.DDict args)
                         )) ]
    |> Sql.executeStatementAsync

let storeMany
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  (functionArguments : ResizeArray<FunctionArgumentStore>)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    let transactionData =
      functionArguments
      |> ResizeArray.map (fun (tlid, args, timestamp) ->
        [ "canvasID", Sql.uuid canvasID
          "traceID", Sql.traceID traceID
          "tlid", Sql.tlid tlid
          "timestamp", Sql.instantWithTimeZone timestamp
          ("args",
           Sql.string (DvalReprInternalRoundtrippable.toJsonV0 (RT.DDict args))) ])
      |> ResizeArray.toList

    LibService.DBConnection.connect ()
    |> Sql.executeTransactionAsync [ ("INSERT INTO trace_old_function_arguments_v0
             (canvas_id, trace_id, tlid, timestamp, arguments_json)
           VALUES
             (@canvasID, @traceID, @tlid, @timestamp, @args)",
                                      transactionData) ]
    |> Task.map ignore<List<int>>



let loadForAnalysis
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  (tlid : tlid)
  : Task<Option<AT.InputVars * NodaTime.Instant>> =
  task {
    // We need to alias the subquery (here aliased as `q`) because Postgres
    // requires inner SELECTs to be aliased.
    let! result =
      Sql.query
        "SELECT arguments_json, timestamp FROM (
                        SELECT DISTINCT ON (trace_id) trace_id, timestamp, arguments_json
                        FROM trace_old_function_arguments_v0
                        WHERE canvas_id = @canvasID
                          AND tlid = @tlid
                          AND trace_id = @traceID
                        ORDER BY trace_id, timestamp DESC
                      ) AS q
                      ORDER BY timestamp DESC
                      LIMIT 1"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "tlid", Sql.id tlid
                          "traceID", Sql.traceID traceID ]
      |> Sql.executeRowOptionAsync (fun read ->
        (read.string "arguments_json", read.instant "timestamp"))
    match result with
    | None -> return None
    | Some (arguments_json, timestamp) ->
      let arguments = DvalReprInternalRoundtrippable.parseJsonV0 arguments_json
      let arguments =
        arguments
        |> RT.Dval.toPairs
        |> Exception.unwrapResultInternal [ "dval", arguments ]
      return Some(arguments, timestamp)
  }

let loadTraceIDs (canvasID : CanvasID) (tlid : tlid) : Task<List<AT.TraceID.T>> =
  // We need to alias the subquery (here aliased as `q`) because Postgres
  // requires inner SELECTs to be aliased.
  Sql.query
    "SELECT trace_id FROM (
       SELECT DISTINCT ON (trace_id) trace_id, timestamp
       FROM trace_old_function_arguments_v0
       WHERE canvas_id = @canvasID
         AND tlid = @tlid
       ORDER BY trace_id, timestamp DESC
     ) AS q
     ORDER BY timestamp DESC
     LIMIT 10"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlid", Sql.id tlid ]
  |> Sql.executeAsync (fun read -> read.traceID "trace_id")
