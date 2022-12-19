/// Functions and types for storing traces in Google Cloud Storage.
module LibBackend.TraceCloudStorage

// Design and implementation via https://github.com/darklang/dark/issues/3954
//
// The high level intent of this implementation was to move trace data out of the
// database and instead keep it in Cloud Storage. The DB had grown to 10TB, 99.7% of
// it being trace storage.
//
// Another major goal was to remove our GC code that spent a lot of the time
// querying the database to see what to delete, and then very expensively and slowly
// deleting it. This code locked up the DB for users with a lot of traces. By
// contrast, GCS has Object Lifecycle Management which does parts of this
// automatically.
//
// This differs from the old GC as that was stored in 3 tables, one for inputs, one
// for function_results, and one for function arguments. Instead, we track an entire
// trace as one document, and do not track function arguments at all (relying
// instead on analysis to provide this information)
//
// High level design:
// - a single trace is collected by an execution. The trace contains:
//   - input of the root handler called
//   - function_results with the same data as currently available
// - we store this in Google Cloud Storage
//   - format `{canvasID}/{traceID}`
//   - set a custom time on it
//   - compress the data
// - we store metadata about who uses the trace in a new table in our DB
//   - or canvasID, traceID, timestamp, tlid
//   - we need to find last 10 traces for a tlid
//   - this data can be deleted when the trace is deleted or when the canvas is deleted
//
// - we allow updating traces
//   - For the execute handler button, we replace the entire trace
//     - We might store a hash so the client can check if it's changed
//     - Also send a push notification
//     - This would also wipe out other layers
//   - For the execute button, all we're doing is adding more function results.
//     - We support extending a trace with "layers" to the storage with the same
//       prefix for example, for the trace at {canvasID}/{traceID} add more function
//       results at {canvasID}/{traceID}/{timestamp}/{randomNumber} when fetched, it
//       can be layered on top to overwrite other function results
//   - For the execute_function button, we are again just adding more functions
//     results, so these are also just layers
//
// - 404s
//   - store them using {canvasID}/404s/{timestamp}
//   - store path such that we can request it in one request
//   - store just the input
//   - when converted to a trace, delete and rewrite it the normal way
//
// - it's garbage collected as follows:
//   - the bucket has an Object Lifecycle policy which deletes traces after X days
//     from when the last custom date was set on it
//   - when a trace is created, it gets todays custom date
//   - with some frequency, let's say daily, we go through all valid canvases and
//     toplevels, and get their last 10 traces. We mark those with the new custom date
//     metadata
//   - when GCS deletes them, we receive a pubsub notification. We then use this to
//     delete the trace/tlid connection metadata, and the layers

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

// Only do storage here. Anything else needs utility functions that go in
// LibDarkInternal.

type RoundTrippableDval =
  LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval

type FunctionArgHash = string
type FnName = string
type InputVars = List<string * RoundTrippableDval>

type Result = NodaTime.Instant * RoundTrippableDval

let roundtrippableToDval (dval : RoundTrippableDval) : RT.Dval =
  LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.toRT dval

let dvalToRoundtrippable (dval : RT.Dval) : RoundTrippableDval =
  LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.fromRT dval


// type FunctionResultKey = tlid * RT.FQFnName.T * id * FunctionArgHash

let currentStorageVersion = 0

type CloudStorageFormat =
  { storageFormatVersion : int
    input : InputVars
    timestamp : NodaTime.Instant
    functionArguments : seq<(tlid * InputVars)>
    functionResults : seq<tlid * id * FnName * FunctionArgHash * RoundTrippableDval> }

let bucketName = Config.traceStorageBucketName

type StorageClientBuilder = Google.Cloud.Storage.V1.StorageClientBuilder

let client =
  lazy
    (task {
      let builder =
        match Config.traceStorageCredentials with
        | None ->
          StorageClientBuilder(
            BaseUri = Config.traceStorageBaseUri,
            UnauthenticatedAccess = true
          )
        | Some cred -> StorageClientBuilder(JsonCredentials = cred)
      return! builder.BuildAsync()
    })

let isTraceInCloudStorage (canvasID : CanvasID) (traceID : AT.TraceID) : Task<bool> =
  Sql.query
    "SELECT EXISTS (SELECT 1 FROM traces_v0 WHERE canvas_id = @canvasID AND trace_id = @traceID)"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "traceID", Sql.uuid traceID ]
  |> Sql.executeRowAsync (fun read -> read.bool "exists")


let listMostRecentTraceIDsForTLIDs
  (canvasID : CanvasID)
  (tlids : list<tlid>)
  : Task<List<tlid * AT.TraceID>> =
  Sql.query
    "SELECT tlid, trace_id
     FROM (
       SELECT
         tlid, trace_id,
         ROW_NUMBER() OVER (PARTITION BY tlid ORDER BY timestamp DESC) as row_num
       FROM traces_v0
       WHERE tlid = ANY(@tlids::bigint[])
         AND canvas_id = @canvasID
     ) t
     WHERE row_num <= 10"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
  |> Sql.executeAsync (fun read -> (read.tlid "tlid", read.uuid "trace_id"))

let storeTraceTLIDs
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (tlids : list<tlid>)
  : Task<unit> =
  let tlids = tlids |> set |> Set.toList
  Sql.query
    "INSERT INTO traces_v0
     (canvas_id, trace_id, tlid, timestamp)
     VALUES (@canvasID, @traceID, UNNEST(@tlids::bigint[]), CURRENT_TIMESTAMP)"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "traceID", Sql.uuid traceID
                      "tlids", Sql.idArray tlids ]
  |> Sql.executeStatementAsync

let getTraceData (canvasID : CanvasID) (traceID : AT.TraceID) : Task<AT.Trace> =
  task {
    let! client = client.Force()
    let ms = new System.IO.MemoryStream()
    do! client.DownloadObjectAsync(bucketName, $"{canvasID}/{traceID}/0", ms)

    let cloudStorageData =
      ms.ToArray()
      |> UTF8.ofBytesUnsafe
      |> Json.Vanilla.deserialize<CloudStorageFormat>

    let parseDval =
      LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.toRT

    let traceData : AT.TraceData =
      { input = cloudStorageData.input |> List.map (Tuple2.mapSecond parseDval)
        timestamp = cloudStorageData.timestamp
        function_results =
          cloudStorageData.functionResults
          |> Seq.map (fun (tlid, id, fnName, argHash, dval) ->
            (fnName,
             id,
             argHash,
             cloudStorageData.storageFormatVersion,
             parseDval dval) : AT.FunctionResult)
          |> List.ofSeq }

    return (traceID, traceData)
  }


module Test =
  let listAllTraceIDs (canvasID : CanvasID) : Task<List<AT.TraceID>> =
    Sql.query
      "SELECT trace_id
       FROM traces_v0
      WHERE canvas_id = @canvas_id
   ORDER BY timestamp DESC"
    |> Sql.parameters [ "canvas_id", Sql.uuid canvasID ]
    |> Sql.executeAsync (fun read -> read.uuid "trace_id")




let storeToCloudStorage
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (timestamp : NodaTime.Instant)
  (touchedTLIDs : List<tlid>)
  (inputVars : List<string * RT.Dval>)
  (functionArguments : ResizeArray<TraceFunctionArguments.FunctionArgumentStore>)
  (functionResults : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue>)
  : Task<unit> =
  task {
    let functionResults =
      functionResults
      |> Dictionary.toList
      |> List.map (fun ((tlid, fnName, id, hash), (dval, _)) ->
        // TODO do we really want to parse and unparse fnName?
        tlid, id, RT.FQFnName.toString fnName, hash, dvalToRoundtrippable dval)

    let functionArguments =
      functionArguments
      |> ResizeArray.toList
      |> List.map (fun (tlid, inputVars, _) ->
        (tlid,
         inputVars
         |> Map.toList
         |> List.map (fun (name, dval) -> (name, dvalToRoundtrippable dval))))

    let inputVars =
      inputVars |> List.map (fun (name, dval) -> (name, dvalToRoundtrippable dval))
    let data =
      { input = inputVars
        functionResults = functionResults
        functionArguments = functionArguments
        storageFormatVersion = currentStorageVersion
        timestamp = timestamp }
    let stream = new System.IO.MemoryStream()
    do! Json.Vanilla.serializeToStream (stream, data)


    // Store to CloudStorage
    let! client = client.Force()
    let name = $"{canvasID}/{traceID}/0"
    let storageTask =
      client.UploadObjectAsync(
        bucketName,
        name,
        "application/json",
        stream,
        null,
        System.Threading.CancellationToken(),
        null
      )

    // Store to the DB
    let dbTask = storeTraceTLIDs canvasID traceID touchedTLIDs

    // Wait for both to be done in parallel. Exceptions from either will be thrown here
    do! Task.WhenAll [ storageTask :> Task; dbTask ]

    return ()
  }
