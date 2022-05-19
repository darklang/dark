module LibBackend.TraceFunctionResults

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

// The data we save to store this
type FunctionResultKey = tlid * RT.FQFnName.T * id * string

type FunctionResultValue = RT.Dval * NodaTime.Instant

let store
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  ((tlid, fnDesc, id) : tlid * RT.FQFnName.T * id)
  (arglist : List<RT.Dval>)
  (result : RT.Dval)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    Sql.query
      "INSERT INTO function_results_v3
      (canvas_id, trace_id, tlid, fnname, id, hash, hash_version, timestamp, value)
      VALUES (@canvasID, @traceID, @tlid, @fnName, @id, @hash, @hashVersion, CURRENT_TIMESTAMP, @value)"
    |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                        "traceID", Sql.uuid traceID
                        "tlid", Sql.tlid tlid
                        "fnName", fnDesc |> RT.FQFnName.toString |> Sql.string
                        "id", Sql.id id
                        ("hash",
                         arglist
                         |> DvalReprInternalDeprecated.hash
                              DvalReprInternalDeprecated.currentHashVersion
                         |> Sql.string)

                        "hashVersion",
                        Sql.int DvalReprInternalDeprecated.currentHashVersion
                        ("value",
                         result
                         |> DvalReprInternalDeprecated.toInternalRoundtrippableV0
                         |> Sql.string) ]
    |> Sql.executeStatementAsync

// CLEANUP store these in Cloud Storage instead of the DB
let storeMany
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (functionResults : Dictionary.T<FunctionResultKey, FunctionResultValue>)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    let transactionData =
      functionResults
      |> Dictionary.toList
      |> List.map (fun ((tlid, fnDesc, id, hash), (result, timestamp)) ->
        [ "canvasID", Sql.uuid canvasID
          "traceID", Sql.uuid traceID
          "tlid", Sql.tlid tlid
          "fnName", fnDesc |> RT.FQFnName.toString |> Sql.string
          "id", Sql.id id
          "timestamp", Sql.instantWithTimeZone timestamp
          "hash", Sql.string hash
          "hashVersion", Sql.int DvalReprInternalDeprecated.currentHashVersion
          ("value",
           result
           |> DvalReprInternalDeprecated.toInternalRoundtrippableV0
           |> Sql.string) ])
    LibService.DBConnection.connect ()
    |> Sql.executeTransactionAsync [ "INSERT INTO function_results_v3
          (canvas_id, trace_id, tlid, fnname, id, hash, hash_version, timestamp, value)
          VALUES (@canvasID, @traceID, @tlid, @fnName, @id, @hash, @hashVersion, @timestamp, @value)",
                                     transactionData ]
    |> Task.map ignore<List<int>>



let load
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlid : tlid)
  : Task<List<AT.FunctionResult>> =
  // Right now, we don't allow the user to see multiple results when a function
  // is called in a loop. But, there's a lot of data when functions are called
  // in a loop, so avoid massive responses.
  Sql.query
    "SELECT
       DISTINCT ON (fnname, id, hash, hash_version)
       fnname, id, hash, hash_version, value, timestamp
     FROM function_results_v3
     WHERE canvas_id = @canvasID
       AND trace_id = @traceID
       AND tlid = @tlid
     ORDER BY fnname, id, hash, hash_version, timestamp DESC"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "traceID", Sql.uuid traceID
                      "tlid", Sql.id tlid ]
  |> Sql.executeAsync (fun read ->
    (read.string "fnname",
     read.tlid "id",
     read.string "hash",
     read.intOrNone "hash_version" |> Option.unwrap 0,
     read.string "value" |> DvalReprInternalDeprecated.ofInternalRoundtrippableV0))
