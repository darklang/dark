/// Tracing for real execution
module LibBackend.Tracing

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module AT = LibExecution.AnalysisTypes
module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
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
    | SampleOneIn of int
    | SampleAllWithTelemetry
    | SampleAllToCloudStorage

  let parseRule (ruleString : string) : Result<T, string> =
    match ruleString with
    | "sample-none" -> Ok SampleNone
    | "sample-all" -> Ok SampleAll
    | "sample-all-to-cloud-storage" -> Ok SampleAllToCloudStorage
    | "sample-all-with-telemetry" -> Ok SampleAllWithTelemetry
    | _ ->
      try
        let prefix = "sample-one-in-"
        if String.startsWith prefix ruleString then
          let number = ruleString |> String.dropLeft (String.length prefix) |> int
          Ok(SampleOneIn number)
        else
          Error "Invalid sample"
      with
      | _ -> Error "Exception thrown"

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
    | TraceToCloudStorage

  let fromRule (rule : TraceSamplingRule.T) (traceID : AT.TraceID.T) : T =
    match rule with
    | TraceSamplingRule.SampleAll -> DoTrace
    | TraceSamplingRule.SampleNone -> DontTrace
    | TraceSamplingRule.SampleAllWithTelemetry -> TraceWithTelemetry
    | TraceSamplingRule.SampleAllToCloudStorage -> TraceToCloudStorage
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
    | TraceToCloudStorage
    | TraceWithTelemetry -> true
    | DontTrace -> false



module TraceResults =
  type T =
    { tlids : HashSet.T<tlid>
      functionResults : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue>
      functionArguments : ResizeArray<TraceFunctionArguments.FunctionArgumentStore> }

  let empty () : T =
    { tlids = HashSet.empty ()
      functionResults = Dictionary.empty ()
      functionArguments = ResizeArray.empty () }



/// Collections of functions and values used during a single execution
type T =
  { /// Store the tracing input, if enabled
    storeTraceInput : HandlerDesc -> string -> RT.Dval -> unit

    /// Store the trace results calculated over the execution, if enabled
    storeTraceResults : unit -> unit

    /// The functions to run tracing during execution
    executionTracing : RT.Tracing

    /// Results of the execution
    results : TraceResults.T
    enabled : bool }


let createStandardTracer (canvasID : CanvasID) (traceID : AT.TraceID.T) : T =
  // Any real execution needs to track the touched TLIDs in order to send traces to pusher
  let touchedTLIDs, traceTLIDFn = Exe.traceTLIDs ()
  let results = { TraceResults.empty () with tlids = touchedTLIDs }
  { enabled = true
    results = results
    executionTracing =
      { Exe.noTracing RT.Real with
          storeFnResult =
            (fun (tlid, name, id) args result ->
              let hash =
                args
                |> DvalReprInternalHash.hash DvalReprInternalHash.currentHashVersion
              Dictionary.add
                (tlid, name, id, hash)
                (result, NodaTime.Instant.now ())
                results.functionResults)
          storeFnArguments =
            (fun tlid args ->
              ResizeArray.append
                (tlid, args, NodaTime.Instant.now ())
                results.functionArguments)
          traceTLID = traceTLIDFn }
    storeTraceResults =
      (fun () ->
        LibService.FireAndForget.fireAndForgetTask "traceResultHook" (fun () ->
          task {
            do!
              TraceFunctionArguments.storeMany
                canvasID
                traceID
                results.functionArguments
            do!
              TraceFunctionResults.storeMany canvasID traceID results.functionResults
          }))
    storeTraceInput =
      (fun desc _ input ->
        LibService.FireAndForget.fireAndForgetTask "traceResultHook" (fun () ->
          task {
            let! (_timestamp : NodaTime.Instant) =
              TraceInputs.storeEvent canvasID traceID desc input
            return ()
          })) }

let createCloudStorageTracer
  (canvasID : CanvasID)
  (rootTLID : tlid)
  (traceID : AT.TraceID.T)
  : T =
  // Any real execution needs to track the touched TLIDs in order to send traces to pusher
  let touchedTLIDs, traceTLIDFn = Exe.traceTLIDs ()
  let results = { TraceResults.empty () with tlids = touchedTLIDs }
  let mutable storedInput : (string * RT.Dval) = ("", RT.DUnit)
  { enabled = true
    results = results
    executionTracing =
      { Exe.noTracing RT.Real with
          storeFnResult =
            (fun (tlid, name, id) args result ->
              let hash =
                args
                |> DvalReprInternalHash.hash DvalReprInternalHash.currentHashVersion
              Dictionary.add
                (tlid, name, id, hash)
                (result, NodaTime.Instant.now ())
                results.functionResults)
          storeFnArguments =
            (fun tlid args ->
              ResizeArray.append
                (tlid, args, NodaTime.Instant.now ())
                results.functionArguments)
          traceTLID = traceTLIDFn }
    storeTraceResults =
      fun () ->
        LibService.FireAndForget.fireAndForgetTask
          "store-to-cloud-storage"
          (fun () ->
            TraceCloudStorage.storeToCloudStorage
              canvasID
              rootTLID
              traceID
              (HashSet.toList touchedTLIDs)
              [ storedInput ]
              results.functionArguments
              results.functionResults)
    storeTraceInput = fun _ name input -> storedInput <- (name, input) }


let createTelemetryTracer (canvasID : CanvasID) (traceID : AT.TraceID.T) : T =
  let result = createStandardTracer canvasID traceID
  let standardTracing = result.executionTracing
  { result with
      executionTracing =
        { standardTracing with
            storeFnResult =
              (fun (tlid, name, id) args result ->
                let stringifiedName =
                  LibExecution.RuntimeTypes.FQFnName.toString name
                let hash =
                  args
                  |> DvalReprInternalHash.hash
                       DvalReprInternalHash.currentHashVersion
                Telemetry.addEvent
                  $"function result for {name}"
                  [ "fnName", stringifiedName
                    "tlid", tlid
                    "id", id
                    "argCount", List.length args
                    "hash", hash
                    "resultType",
                    LibExecution.DvalReprDeveloper.dvalTypeName result :> obj ]
                standardTracing.storeFnResult (tlid, name, id) args result)
            storeFnArguments =
              (fun tlid args ->
                Telemetry.addEvent
                  $"function arguments for {tlid}"
                  [ "tlid", tlid; "id", id; "argCount", Map.count args ]
                standardTracing.storeFnArguments tlid args)
            traceTLID =
              fun tlid ->
                Telemetry.addEvent $"called {tlid}" [ "tlid", tlid ]
                standardTracing.traceTLID tlid } }

let createNonTracer (_canvasID : CanvasID) (_traceID : AT.TraceID.T) : T =
  // Any real execution needs to track the touched TLIDs in order to send traces to pusher
  let results = TraceResults.empty ()
  { enabled = false
    results = results
    executionTracing = LibExecution.Execution.noTracing RT.Real
    storeTraceResults = fun () -> ()
    storeTraceInput = fun _ _ _ -> () }


let create (c : Canvas.Meta) (rootTLID : tlid) (traceID : AT.TraceID.T) : T =
  let config = TracingConfig.forHandler c.id rootTLID traceID
  match config with
  | TracingConfig.DoTrace -> createStandardTracer c.id traceID
  | TracingConfig.TraceWithTelemetry -> createTelemetryTracer c.id traceID
  | TracingConfig.DontTrace -> createNonTracer c.id traceID
  | TracingConfig.TraceToCloudStorage ->
    createCloudStorageTracer c.id rootTLID traceID

module Test =
  let saveTraceResult
    (canvasID : CanvasID)
    (traceID : AT.TraceID.T)
    (results : TraceResults.T)
    : Task<unit> =
    task {
      do! TraceFunctionArguments.storeMany canvasID traceID results.functionArguments
      do! TraceFunctionResults.storeMany canvasID traceID results.functionResults
      return ()
    }
