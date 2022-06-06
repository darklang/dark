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
//   - canvasID, traceID, timestamp, list<tlid>
//     - or maybe just canvasID, traceID, timestamp, tlid
//   - we need to find last 10 traces for a tlid
//   - we also need to be able to add tlids for later executions in a trace
//   - this data can be deleted when the trace is deleted or when the canvas is deleted
//
// - it's garbage collected as follows:
//   - the bucket has an Object Lifecycle policy which deletes traces after X days
//     from when the last custom date was set on it
//   - when a trace is created, it gets todays custom date
//   - with some frequency, let's say daily, we go through all valid canvases and
//     toplevels, and get their last 10 traces. We mark those with the new custom date
//     metadata
//   - when GCS deletes them, we receive a pubsub notification. We then use this to
//     delete the trace/tlid connection metadata.

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


let x = 5
