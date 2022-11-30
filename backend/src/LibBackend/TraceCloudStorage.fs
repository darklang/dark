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

type RoundTrippableDval = RT.Dval
type FunctionArgHash = string
type FnName = string
type InputVars = List<string * RoundTrippableDval>

type Result = NodaTime.Instant * RoundTrippableDval

type CloudStorageFormat =
  { storageVersion : int // maybe in metadata instead
    hashVersion : int
    input : InputVars
    timestamp : NodaTime.Instant
    functionResults : Map<tlid, Map<id, FnName * FunctionArgHash * RoundTrippableDval>> }

// let client = Google.Cloud.Storage.StorageClientImpl.Create()

let roundtrippableToDval (rt : RoundTrippableDval) : RT.Dval = rt

let bucketName = Config.traceStorageBucketName

// Require TLIDs rather than having unbounded search
let listMostRecentTraceIDs
  (canvasID : CanvasID)
  (tlid : tlid)
  : Task<List<AT.TraceID>> =
  Sql.query
    "SELECT trace_id
       FROM traces_v0
      WHERE canvas_id = @canvas_id
        AND tlid = @tlid
   ORDER BY timestamp DESC
      LIMIT 10"
  |> Sql.parameters [ "canvas_id", Sql.uuid canvasID; "tlid", Sql.tlid tlid ]
  |> Sql.executeAsync (fun read -> read.uuid "trace_id")


module Test =
  let listAllTraceIDs (canvasID : CanvasID) : Task<List<AT.TraceID>> =
    Sql.query
      "SELECT trace_id
       FROM traces_v0
      WHERE canvas_id = @canvas_id
   ORDER BY timestamp DESC"
    |> Sql.parameters [ "canvas_id", Sql.uuid canvasID ]
    |> Sql.executeAsync (fun read -> read.uuid "trace_id")

  let fetchTraceData
    (canvasID : CanvasID)
    (traceID : AT.TraceID)
    : Task<CloudStorageFormat> =
    // TODO
    Task.FromResult(Unchecked.defaultof<CloudStorageFormat>)



let storeToCloudStorage
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (touchedTLIDs : List<tlid>)
  (storedInput : RT.Dval)
  (functionResults : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue>)
  : Task<unit> =
  task {
    // construct the data
    let data =
      { hashVersion = 0 // TODO
        input = [] // TODO
        functionResults = Map [] // TODO
        storageVersion = 0 // TODO
        timestamp = NodaTime.Instant.now () // TODO
      }
    let data = Json.Vanilla.serialize data

    // Save it to cloud storage



    // save the tlid data to the DB
    return ()
  }
