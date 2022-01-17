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
module DvalReprInternal = LibExecution.DvalReprInternal

// -------------------------
// External
// -------------------------

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
                        "fnName", Sql.string (string fnDesc)
                        "id", Sql.id id
                        ("hash",
                         arglist
                         |> DvalReprInternal.hash DvalReprInternal.currentHashVersion
                         |> Sql.string)

                        "hashVersion", Sql.int DvalReprInternal.currentHashVersion
                        ("value",
                         result
                         |> DvalReprInternal.toInternalRoundtrippableV0
                         |> Sql.string) ]
    |> Sql.executeStatementAsync

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
     read.string "value" |> DvalReprInternal.ofInternalRoundtrippableV0))
