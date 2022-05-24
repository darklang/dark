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
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated

// -------------------------
// External
// -------------------------

type FunctionArgumentStore = tlid * RT.DvalMap * NodaTime.Instant

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
                           DvalReprInternalDeprecated.toInternalRoundtrippableV0 (
                             RT.DObj args
                           )
                         )) ]
    |> Sql.executeStatementAsync

let storeMany
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (functionArguments : ResizeArray<FunctionArgumentStore>)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    let transactionData =
      functionArguments
      |> ResizeArray.map (fun (tlid, args, timestamp) ->
        [ "canvasID", Sql.uuid canvasID
          "traceID", Sql.uuid traceID
          "tlid", Sql.tlid tlid
          "timestamp", Sql.instantWithTimeZone timestamp
          ("args",
           Sql.string (
             DvalReprInternalDeprecated.toInternalRoundtrippableV0 (RT.DObj args)
           )) ])
      |> ResizeArray.toList

    LibService.DBConnection.connect ()
    |> Sql.executeTransactionAsync [ ("INSERT INTO function_arguments
             (canvas_id, trace_id, tlid, timestamp, arguments_json)
           VALUES
             (@canvasID, @traceID, @tlid, @timestamp, @args)",
                                      transactionData) ]
    |> Task.map ignore<List<int>>



let loadForAnalysis
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlid : tlid)
  : Task<Option<AT.InputVars * NodaTime.Instant>> =
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
    (read.string "arguments_json", read.instant "timestamp"))
  |> Task.map (
    Option.map (fun (arguments_json, timestamp) ->
      let arguments =
        arguments_json
        |> DvalReprInternalDeprecated.ofInternalRoundtrippableV0
        |> fun dv ->
             RT.Dval.toPairs dv |> Exception.unwrapResultInternal [ "dval", dv ]
      (arguments, timestamp))
  )


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
