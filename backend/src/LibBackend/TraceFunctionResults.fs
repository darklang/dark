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
module ReprHash = LibExecution.DvalReprInternalHash
module ReprRoundtrippable = LibExecution.DvalReprInternalRoundtrippable

// -------------------------
// External
// -------------------------

// The data we save to store this
type FunctionResultKey = tlid * RT.FQFnName.T * id * string

type FunctionResultValue = RT.Dval * NodaTime.Instant

let store
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  ((tlid, fnDesc, callerID) : tlid * RT.FQFnName.T * id)
  (arglist : List<RT.Dval>)
  (result : RT.Dval)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    Sql.query
      "INSERT INTO trace_old_function_results_v0
      (canvas_id, trace_id, tlid, fnname, caller_id, hash, hash_version, timestamp, value)
      VALUES (@canvasID, @traceID, @tlid, @fnName, @callerID, @hash, @hashVersion, CURRENT_TIMESTAMP, @value)"
    |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                        "traceID", Sql.traceID traceID
                        "tlid", Sql.tlid tlid
                        "fnName", fnDesc |> RT.FQFnName.toString |> Sql.string
                        "caller_id", Sql.id callerID
                        ("hash",
                         arglist
                         |> ReprHash.hash ReprHash.currentHashVersion
                         |> Sql.string)
                        "hashVersion", Sql.int ReprHash.currentHashVersion
                        "value", result |> ReprRoundtrippable.toJsonV0 |> Sql.string ]
    |> Sql.executeStatementAsync

// CLEANUP store these in Cloud Storage instead of the DB
let storeMany
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  (functionResults : Dictionary.T<FunctionResultKey, FunctionResultValue>)
  : Task<unit> =
  if canvasID = TraceInputs.throttled then
    Task.FromResult()
  else
    let transactionData =
      functionResults
      |> Dictionary.toList
      |> List.map (fun ((tlid, fnDesc, callerID, hash), (result, timestamp)) ->
        [ "canvasID", Sql.uuid canvasID
          "traceID", Sql.traceID traceID
          "tlid", Sql.tlid tlid
          "fnName", fnDesc |> RT.FQFnName.toString |> Sql.string
          "callerID", Sql.id callerID
          "timestamp", Sql.instantWithTimeZone timestamp
          "hash", Sql.string hash
          "hashVersion", Sql.int ReprHash.currentHashVersion
          "value", result |> ReprRoundtrippable.toJsonV0 |> Sql.string ])
    LibService.DBConnection.connect ()
    |> Sql.executeTransactionAsync [ "INSERT INTO trace_old_function_results_v0
          (canvas_id, trace_id, tlid, fnname, caller_id, hash, hash_version, timestamp, value)
          VALUES (@canvasID, @traceID, @tlid, @fnName, @callerID, @hash, @hashVersion, @timestamp, @value)",
                                     transactionData ]
    |> Task.map ignore<List<int>>



let load
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  (tlid : tlid)
  : Task<List<AT.FunctionResult>> =
  task {
    // Right now, we don't allow the user to see multiple results when a function
    // is called in a loop. But, there's a lot of data when functions are called
    // in a loop, so avoid massive responses.
    let! results =
      Sql.query
        "SELECT
          DISTINCT ON (fnname, caller_id, hash, hash_version)
          fnname, caller_id, hash, hash_version, value, timestamp
        FROM trace_old_function_results_v0
        WHERE canvas_id = @canvasID
          AND trace_id = @traceID
          AND tlid = @tlid
        ORDER BY fnname, caller_id, hash, hash_version, timestamp DESC"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "traceID", Sql.traceID traceID
                          "tlid", Sql.tlid tlid ]
      |> Sql.executeAsync (fun read ->
        (read.string "fnname",
         read.id "caller_id",
         read.string "hash",
         read.intOrNone "hash_version" |> Option.unwrap 0,
         read.string "value"))
    return
      results
      |> List.map (fun (fnname, callerID, hash, hashVersion, value) ->
        (fnname, callerID, hash, hashVersion, ReprRoundtrippable.parseJsonV0 value))
  }
