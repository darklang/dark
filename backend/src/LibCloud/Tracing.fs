/// Tracing for real execution
module LibCloud.Tracing

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module Exe = LibExecution.Execution
module DvalReprInternalHash = LibExecution.DvalReprInternalHash

module LD = LibService.LaunchDarkly
module Rollbar = LibService.Rollbar
module Telemetry = LibService.Telemetry

/// Tracing can go overboard, so use a per-handler feature flag to control it. If
/// sampling is disabled for a canvas, no traces will be recorded to be saved to the
/// DBs, but tlids will still be recorded as they are needed by APIs.
module TraceSamplingRule =
  type T =
    | SampleNone
    | SampleAll
    /// Sample one every `n`
    | SampleOneIn of n : int
    | SampleAllWithTelemetry

  let parseRule (ruleString : string) : Result<T, string> =
    match ruleString with
    | "sample-none" -> Ok SampleNone
    | "sample-all" -> Ok SampleAll
    | "sample-all-with-telemetry" -> Ok SampleAllWithTelemetry
    | _ ->
      try
        let prefix = "sample-one-in-"
        if String.startsWith prefix ruleString then
          let number = ruleString |> String.dropLeft (String.length prefix) |> int
          Ok(SampleOneIn number)
        else
          Error "Invalid sample"
      with _ ->
        Error "Exception thrown"

  /// Fetch the traceSamplingRule from the feature flag, and parse it. If parsing
  /// fails, returns SampleNone.
  let ruleForHandler (canvasID : CanvasID) (tlid : tlid) : T =
    let ruleString = LD.traceSamplingRule canvasID tlid
    Telemetry.addTag "trace_sampling_rule" ruleString
    match parseRule ruleString with
    | Error msg ->
      Rollbar.sendError
        $"Invalid traceSamplingRule: {msg}"
        [ "ruleString", ruleString; "canvasID", canvasID; "tlid", tlid ]
      SampleNone
    | Ok rule -> rule



/// Simplified version of the TraceSamplingRule. TraceSamplingRule is what's stored
/// in LaunchDarkly. This resolves the one-in-x option into DoTrace or DontTrace
module TracingConfig =
  type T =
    | DoTrace
    | DontTrace
    | TraceWithTelemetry

  let fromRule (rule : TraceSamplingRule.T) (traceID : AT.TraceID.T) : T =
    match rule with
    | TraceSamplingRule.SampleAll -> DoTrace
    | TraceSamplingRule.SampleNone -> DontTrace
    | TraceSamplingRule.SampleAllWithTelemetry -> TraceWithTelemetry
    | TraceSamplingRule.SampleOneIn freq ->
      // Use the traceID as an existing source of entropy.
      let random =
        (AT.TraceID.toUUID traceID).ToByteArray() |> System.BitConverter.ToInt64
      if random % (int64 freq) = 0L then DoTrace else DontTrace

  let forHandler (canvasID : CanvasID) (tlid : tlid) (traceID : AT.TraceID.T) : T =
    let samplingRule = TraceSamplingRule.ruleForHandler canvasID tlid
    fromRule samplingRule traceID

  let shouldTrace (config : T) =
    match config with
    | DoTrace
    | TraceWithTelemetry -> true
    | DontTrace -> false



module TraceResults =
  type T =
    { tlids : HashSet.HashSet<tlid>
      functionResults :
        Dictionary.T<TraceCloudStorage.FunctionResultKey, TraceCloudStorage.FunctionResultValue> }

  let empty () : T =
    { tlids = HashSet.empty (); functionResults = Dictionary.empty () }



/// Collections of functions and values used during a single execution
type T =
  {
    /// Store the tracing input, if enabled
    /// TRACINGTODO
    /// Q: What's the `string` here? Edit: it's the `varname` as passed by LibCloudExecution.executeHandler
    storeTraceInput : PT.Handler.HandlerDesc -> string -> RT.Dval -> unit

    /// Store the trace results calculated over the execution, if enabled
    storeTraceResults : unit -> unit

    /// The functions to run tracing during execution
    executionTracing : RT.Tracing.Tracing

    /// Results of the execution
    results : TraceResults.T
    enabled : bool
  }


//TRACINGTODO bring thi sback
// let createCloudStorageTracer
//   (canvasID : CanvasID)
//   (rootTLID : tlid) // TODO: this should probably take in some entrypoint rather than an opaque rootTLID
//   (traceID : AT.TraceID.T)
//   : T =
//   // Any real execution needs to track the touched TLIDs in order to send traces to pusher
//   let touchedTLIDs, traceTLIDFn = Exe.traceTLIDs ()
//   let results = { TraceResults.empty () with tlids = touchedTLIDs }
//   let mutable storedInput : (string * RT.Dval) = ("", RT.DUnit)

//   let initialCallStack = RT.CallStack.fromEntryPoint entryPoint
//   { enabled = true
//     results = results
//     executionTracing =
//       { (Exe.noTracing initialCallStack) with
//           storeFnResult =
//             (fun (source, name) args result ->
//               let tlid, id = Option.defaultValue (0UL, 0UL) source
//               let hash =
//                 args
//                 |> DvalReprInternalHash.hash DvalReprInternalHash.currentHashVersion
//               Dictionary.add
//                 (tlid, name, id, hash)
//                 (result, NodaTime.Instant.now ())
//                 results.functionResults)
//           traceExecutionPoint = traceTLIDFn }
//     storeTraceResults =
//       fun () ->
//         LibService.FireAndForget.fireAndForgetTask
//           "store-to-cloud-storage"
//           (fun () ->
//             TraceCloudStorage.storeToCloudStorage
//               canvasID
//               rootTLID
//               traceID
//               (HashSet.toList touchedTLIDs)
//               [ storedInput ]
//               results.functionResults)
//     storeTraceInput = fun _ name input -> storedInput <- (name, input) }


// let createTelemetryTracer
//   (canvasID : CanvasID)
//   (rootTLID : tlid)
//   (traceID : AT.TraceID.T)
//   : T =
//   let result = createCloudStorageTracer canvasID rootTLID traceID
//   let standardTracing = result.executionTracing
//   { result with
//       executionTracing =
//         { standardTracing with
//             storeFnResult =
//               (fun (source, name) args result ->
//                 let stringifiedName =
//                   LibExecution.RuntimeTypes.FQFnName.toString name
//                 let tlid, id = Option.defaultValue (0UL, 0UL) source
//                 let hash =
//                   args
//                   |> DvalReprInternalHash.hash
//                     DvalReprInternalHash.currentHashVersion
//                 Telemetry.addEvent
//                   $"function result for {name}"
//                   [ "fnName", stringifiedName
//                     "tlid", tlid
//                     "id", id
//                     "argCount", NEList.length args
//                     "hash", hash
//                     "resultType",
//                     LibExecution.DvalReprDeveloper.toTypeName result :> obj ]
//                 standardTracing.storeFnResult (source, name) args result)
//             traceExecutionPoint =
//               fun tlid ->
//                 Telemetry.addEvent $"called {tlid}" [ "tlid", tlid ]
//                 standardTracing.traceExecutionPoint tlid } }

let createNonTracer (_canvasID : CanvasID) (_traceID : AT.TraceID.T) : T =
  // Any real execution needs to track the touched TLIDs in order to send traces to pusher
  let results = TraceResults.empty ()
  { enabled = false
    results = results
    executionTracing = LibExecution.Execution.noTracing
    storeTraceResults = fun () -> ()
    storeTraceInput = fun _ _ _ -> () }


let create (canvasID : CanvasID) (rootTLID : tlid) (traceID : AT.TraceID.T) : T =
  let config = TracingConfig.forHandler canvasID rootTLID traceID
  match config with
  // TRACINGTODO
  | TracingConfig.DoTrace // -> createCloudStorageTracer canvasID rootTLID traceID
  | TracingConfig.TraceWithTelemetry // -> createTelemetryTracer canvasID rootTLID traceID
  | TracingConfig.DontTrace -> createNonTracer canvasID traceID
