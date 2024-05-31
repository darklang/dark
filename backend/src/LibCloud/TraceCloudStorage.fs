/// Functions and types for storing traces in Google Cloud Storage.
module LibCloud.TraceCloudStorage

// Design and implementation via https://github.com/darklang/dark/issues/3954
//
// The high level intent of this implementation was to move trace data out of the
// database and instead keep it in Cloud Storage. The DB had grown to 10TB, 99.7% of
// it being trace storage.
//
// Another major goal was to remove our GC code that spent a lot of the time querying
// the database to see what to delete, and then very expensively and slowly deleting
// it. This code locked up the DB when we had users with a lot of traces. The
// approach we took instead was to leverage Cloud Storage's Object Lifecycle
// Management to automatically tell us when a certain object gets old.
//
// This differs from the old trace implementation which was stored in 3 tables, one
// for inputs, one for function_results, and one for function_arguments. Instead, we
// track an entire trace as one document (in the future we hope to not track function
// arguments at all, relying instead on analysis to provide this information)


// High level design:
// - a single trace is collected by an execution. The trace contains:
//   - input of the root handler called
//   - functionResults with the same data as currently available
// - we store this in Google Cloud Storage
//   - format `{canvasID}/{tlid}/{traceID}`
//   - main trace is called 0
//   - other layers will have a different suffix
//   - traceIDs now include timestamps (similar to ULID) so listing out
//   `{canvasID}/{tlid}` will give us the most recent traces always.
// - we store metadata about functions called during the trace in `traces_v0`
//   - this data can be deleted when the trace is deleted or when the canvas is deleted
//
// - we allow updating traces (not implemented yet)
//   - For the execute handler button, we replace the entire trace
//     - We might store a hash so the client can check if it's changed
//     - Also send a push notification
//     - This would also wipe out other layers
//   - For the execute button, all we're doing is adding more function results.
//     - We support extending a trace with "layers" to the storage with the same
//       prefix for example, for the trace at {canvasID}/{traceID} add more function
//       results at `{canvasID}/{traceID}/{timestamp}/{randomNumber}` when fetched,
//       it can be layered on top to overwrite other function results
//   - For the execute_function button, we are again just adding more functions
//     results, so these are also just layers
//
// - 404s (not implemented yet)
//   - store them using `{canvasID}/404s/{traceID}`
//   - store path such that we can request it in one request
//   - store just the input
//   - when converted to a trace, delete and rewrite it the normal way
//
// - garbage collection (not implemented yet)
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
open System.IO
open System.IO.Compression

open Npgsql.FSharp
open Npgsql
open Db

open Prelude

module AT = LibExecution.AnalysisTypes
module RT = LibExecution.RuntimeTypes

// Only do storage here.
// Anything else needs utility functions that go in BuiltinDarkInternal.

/// The formats that we store in Cloud Storage need to be roundtrippable
/// and relatively stable. Route all data through this module.
[<RequireQualifiedAccess>]
module Roundtrippable =
  type Dval = LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval

  let toRT (dval : Dval) : RT.Dval =
    LibExecution.DvalReprInternalRoundtrippable.FormatV0.toRT dval

  let fromRT (dval : RT.Dval) : Dval =
    LibExecution.DvalReprInternalRoundtrippable.FormatV0.fromRT dval


type FunctionArgHash = string
type FnName = string
type InputVars = List<string * Roundtrippable.Dval>

type Result = NodaTime.Instant * Roundtrippable.Dval



let currentStorageVersion = 0

// TRACINGTODO
// I suppose the `tlid` here is of the ... fn?
// I'm not sure what the `id` corresponds to - maybe the exprId? of the fn call?
// and where does the `string` key come from?
type FunctionResultKey = tlid * RT.FQFnName.FQFnName * id * string
type FunctionResultValue = RT.Dval * NodaTime.Instant


type CloudStorageFormat =
  { storageFormatVersion : int
    input : InputVars
    functionResults :
      seq<tlid * id * FnName * int * FunctionArgHash * Roundtrippable.Dval> }

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

let rootTLIDFor (canvasID : CanvasID) (traceID : AT.TraceID.T) : Task<Option<tlid>> =
  Sql.query
    "SELECT root_tlid
    FROM traces_v0
    WHERE canvas_id = @canvasID
      AND trace_id = @traceID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "traceID", Sql.traceID traceID ]
  |> Sql.executeRowOptionAsync (fun read -> read.tlid "root_tlid")

let objectName
  (canvasID : CanvasID)
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  (fileName : string)
  : string =
  $"{canvasID}/{rootTLID}/{traceID}/{fileName}"

let storeTraceTLIDs
  (canvasID : CanvasID)
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  (callgraphTLIDs : list<tlid>)
  : Task<unit> =
  Sql.query
    "INSERT INTO traces_v0
      (id, canvas_id, trace_id, root_tlid, callgraph_tlids)
    VALUES
      (@id, @canvasID, @traceID, @rootTLID, @callgraphTLIDs::bigint[])"
  |> Sql.parameters
    [ "id", System.Guid.NewGuid() |> Sql.uuid
      "canvasID", Sql.uuid canvasID
      "traceID", Sql.traceID traceID
      "rootTLID", Sql.tlid rootTLID
      "callgraphTLIDs", Sql.idArray callgraphTLIDs ]
  |> Sql.executeStatementAsync

let listMostRecentTraceIDsForTLIDs
  (canvasID : CanvasID)
  (tlids : list<tlid>)
  : Task<List<tlid * AT.TraceID.T>> =
  Sql.query
    "SELECT callgraph_tlids, trace_id
    FROM (
      SELECT
        callgraph_tlids, trace_id,
        ROW_NUMBER() OVER (PARTITION BY root_tlid ORDER BY trace_id ASC) as row_num
      FROM traces_v0
      WHERE root_tlid = ANY(@tlids::bigint[])
        AND canvas_id = @canvasID
    ) t
    WHERE row_num <= 10"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
  |> Sql.executeAsync (fun read ->
    (read.traceID "trace_id", read.idArray "callgraph_tlids"))
  |> Task.map (fun results ->
    results
    |> List.map (fun (traceID, callgraphTLIDs) ->
      // Don't need the rootTLID as it will also be in the callgraphTLIDs
      callgraphTLIDs |> List.map (fun tlid -> (tlid, traceID)))
    |> List.flatten
    // We've ruined the sort order by splatting against callgraphTLIDs, so sort
    // again. These will be in order of most recent first just by sorting on the
    // traceID
    |> List.sortBy (fun (_, traceID) -> traceID))

let getTraceData
  (canvasID : CanvasID)
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  : Task<AT.Trace> =
  task {
    let! client = client.Force()
    let name = objectName canvasID rootTLID traceID "0"

    // Download compressed data
    use compressedStream = new MemoryStream()
    let! (_ : Google.Apis.Storage.v1.Data.Object) =
      client.DownloadObjectAsync(bucketName, name, compressedStream)
    do! compressedStream.FlushAsync()
    compressedStream.Position <- 0L

    // Decompress - BrotliStream takes the compressed stream as its input, so the
    // data needs to be available and we can't stream through it (that is, we need
    // the separate compressedStream, which we don't need on upload)
    use brotliStream = new BrotliStream(compressedStream, CompressionMode.Decompress)
    use outputStream = new MemoryStream()
    do! brotliStream.CopyToAsync(outputStream)
    do! brotliStream.FlushAsync()
    do! outputStream.FlushAsync()

    // Deserialize
    let cloudStorageData =
      outputStream.ToArray()
      |> UTF8.ofBytesUnsafe
      |> Json.Vanilla.deserialize<CloudStorageFormat>

    let traceData : AT.TraceData =
      { input =
          cloudStorageData.input |> List.map (Tuple2.mapSecond Roundtrippable.toRT)
        functionResults =
          cloudStorageData.functionResults
          |> Seq.map (fun (_tlid, id, fnName, hashVersion, argHash, dval) ->
            (fnName, id, argHash, hashVersion, Roundtrippable.toRT dval)
            : AT.FunctionResult)
          |> List.ofSeq }

    return (traceID, traceData)
  }


let storeToCloudStorage
  (canvasID : CanvasID)
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  (touchedTLIDs : List<tlid>)
  (inputVars : List<string * RT.Dval>)
  (functionResults : Dictionary.T<FunctionResultKey, FunctionResultValue>)
  : Task<unit> =
  task {
    let functionResults =
      functionResults
      |> Dictionary.toList
      |> List.map (fun ((tlid, fnName, id, hash), (dval, _)) ->
        // TODO do we really want to parse and unparse fnName?
        tlid,
        id,
        RT.FQFnName.toString fnName,
        LibExecution.DvalReprInternalHash.currentHashVersion,
        hash,
        Roundtrippable.fromRT dval)

    let inputVars =
      inputVars |> List.map (fun (name, dval) -> (name, Roundtrippable.fromRT dval))
    let data =
      { input = inputVars
        functionResults = functionResults
        storageFormatVersion = currentStorageVersion }

    // Serialize and Compress in one step
    use stream = new MemoryStream()
    use brotliStream = new BrotliStream(stream, CompressionMode.Compress)
    do! Json.Vanilla.serializeToStream (brotliStream, data)
    do! brotliStream.FlushAsync()
    do! stream.FlushAsync()
    stream.Position <- 0L

    // Create the object and metadata
    let object =
      new Google.Apis.Storage.v1.Data.Object(
        Name = objectName canvasID rootTLID traceID "0",
        ContentType = "application/json",
        ContentEncoding = "br",
        Metadata =
          Map
            [ "storage_format_version", string currentStorageVersion
              "hash_version",
              string LibExecution.DvalReprInternalHash.currentHashVersion ],
        Bucket = bucketName
      )


    // Upload to CloudStorage
    let! client = client.Force()
    let storageTask =
      client.UploadObjectAsync(
        object,
        stream,
        null,
        System.Threading.CancellationToken(),
        null
      )

    // Store to the DB
    let dbTask = storeTraceTLIDs canvasID rootTLID traceID touchedTLIDs

    // Wait for both to be done in parallel. Exceptions from either will be thrown here
    do! Task.WhenAll [ storageTask :> Task; dbTask ]

    return ()
  }

type Bucket = Google.Apis.Storage.v1.Data.Bucket

let init () : Task<unit> =
  task {
    printTime "Initing TraceCloudStorage"
    let! client = client.Force()
    if Config.traceStorageCreateBucket then
      try
        // if it exists, don't recreate it
        let! (_ : Bucket) = client.GetBucketAsync(bucketName)
        return ()
      with _ ->
        // create bucket
        let! (_ : Bucket) =
          client.CreateBucketAsync("some-project-id", bucketName, null)
        return ()
    printTime " Inited TraceCloudStorage"
  }


module Test =
  let listAllTraceIDs (canvasID : CanvasID) : Task<List<AT.TraceID.T>> =
    Sql.query
      "SELECT trace_id
       FROM traces_v0
      WHERE canvas_id = @canvasID
      ORDER BY trace_id ASC"
    |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
    |> Sql.executeAsync (fun read -> read.traceID "trace_id")
