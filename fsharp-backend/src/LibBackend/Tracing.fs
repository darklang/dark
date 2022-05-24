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
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated

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
      with
      | _ -> Error "Exception thrown"

  /// Fetch the traceSamplingRule from the feature flag, and parse it. If parsing
  /// fails, returns SampleNone.
  let ruleForHandler (canvasName : CanvasName.T) (tlid : tlid) : T =
    let ruleString = LD.traceSamplingRule canvasName tlid
    Telemetry.addTag "trace_sampling_rule" ruleString
    match parseRule ruleString with
    | Error msg ->
      Rollbar.sendError
        $"Invalid traceSamplingRule: {msg}"
        [ "ruleString", ruleString; "canvasName", canvasName; "tlid", tlid ]
      SampleNone
    | Ok rule -> rule

module TraceResults =
  type T =
    { tlids : HashSet.T<tlid>
      functionResults : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue>
      functionArguments : ResizeArray<TraceFunctionArguments.FunctionArgumentStore> }

  let empty () : T =
    { tlids = HashSet.empty ()
      functionResults = Dictionary.empty ()
      functionArguments = ResizeArray.empty () }



/// Simplified version of the TraceSamplingRule. TraceSamplingRule is what's stored
/// in LaunchDarkly. This resolves the one-in-x option into DoTrace or DontTrace
module TracingConfig =
  type T =
    | DoTrace
    | DontTrace
    | TraceWithTelemetry

  let fromRule (rule : TraceSamplingRule.T) (traceID : AT.TraceID) : T =
    match rule with
    | TraceSamplingRule.SampleAll -> DoTrace
    | TraceSamplingRule.SampleNone -> DontTrace
    | TraceSamplingRule.SampleAllWithTelemetry -> TraceWithTelemetry
    | TraceSamplingRule.SampleOneIn freq ->
      // Use the traceID as an existing source of entropy.
      let random = traceID.ToByteArray() |> System.BitConverter.ToInt64
      if random % (int64 freq) = 0L then DoTrace else DontTrace

  let forHandler
    (canvasName : CanvasName.T)
    (tlid : tlid)
    (traceID : AT.TraceID)
    : T =
    let samplingRule = TraceSamplingRule.ruleForHandler canvasName tlid
    fromRule samplingRule traceID

  let shouldTrace (config : T) =
    match config with
    | DoTrace
    | TraceWithTelemetry -> true
    | DontTrace -> false


let createStandardTracer () : TraceResults.T * RT.Tracing =
  // Any real execution needs to track the touched TLIDs in order to send traces to pusher
  let touchedTLIDs, traceTLIDFn = Exe.traceTLIDs ()

  let results = { TraceResults.empty () with tlids = touchedTLIDs }

  let tracing =
    { Exe.noTracing RT.Real with
        storeFnResult =
          (fun (tlid, name, id) args result ->
            let hash =
              args
              |> DvalReprInternalDeprecated.hash
                   DvalReprInternalDeprecated.currentHashVersion
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
  (results, tracing)

let createTelemetryTracer () : TraceResults.T * RT.Tracing =
  let results, standardTracing = createStandardTracer ()

  let tracing =
    { standardTracing with
        storeFnResult =
          (fun (tlid, name, id) args result ->
            let stringifiedName = LibExecution.RuntimeTypes.FQFnName.toString name
            let hash =
              args
              |> DvalReprInternalDeprecated.hash
                   DvalReprInternalDeprecated.currentHashVersion
            LibService.Telemetry.addEvent
              $"function result for {name}"
              [ "fnName", stringifiedName
                "tlid", tlid
                "id", id
                "argCount", List.length args
                "hash", hash
                "resultType",
                result
                |> RT.Dval.toType
                |> LibExecution.DvalReprExternal.typeToDeveloperReprV0
                :> obj ]
            standardTracing.storeFnResult (tlid, name, id) args result)
        storeFnArguments =
          (fun tlid args ->
            LibService.Telemetry.addEvent
              $"function arguments for {tlid}"
              [ "tlid", tlid; "id", id; "argCount", Map.count args ]
            standardTracing.storeFnArguments tlid args)
        traceTLID =
          fun tlid ->
            LibService.Telemetry.addEvent $"called {tlid}" [ "tlid", tlid ]
            standardTracing.traceTLID tlid }
  (results, tracing)

let createNonTracer () : TraceResults.T * RT.Tracing =
  (TraceResults.empty (), LibExecution.Execution.noTracing RT.Real)

/// Collections of functions and values used during a single execution
type T =
  {
    /// Store the tracing input, if enabled
    storeInput : HandlerDesc -> RT.Dval -> unit

    /// Store the trace results calculated over the execution, if enabled
    storeTraceResults : unit -> unit

    /// The functions to run tracing during execution
    executionTracing : RT.Tracing

    /// Results of the execution
    results : TraceResults.T
    enabled : bool }

/// Store trace input in the background
let storeTraceInput
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (desc : string * string * string)
  (input : RT.Dval)
  : unit =
  LibService.FireAndForget.fireAndForgetTask "traceResultHook" (fun () ->
    task {
      let! (_timestamp : NodaTime.Instant) =
        TraceInputs.storeEvent canvasID traceID desc input
      return ()
    })

// Store trace results once the request is done
let storeTraceResults
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (results : TraceResults.T)
  : unit =
  LibService.FireAndForget.fireAndForgetTask "traceResultHook" (fun () ->
    task {
      do! TraceFunctionArguments.storeMany canvasID traceID results.functionArguments
      do! TraceFunctionResults.storeMany canvasID traceID results.functionResults
    })

let create (c : Canvas.Meta) (tlid : tlid) (traceID : AT.TraceID) : T =
  let canvasID = c.id
  let config = TracingConfig.forHandler c.name tlid traceID
  let traceResults, executionTracing =
    match config with
    | TracingConfig.DoTrace -> createStandardTracer ()
    | TracingConfig.TraceWithTelemetry -> createTelemetryTracer ()
    | TracingConfig.DontTrace -> createNonTracer ()
  let enabled = TracingConfig.shouldTrace config
  let storeInput = if enabled then storeTraceInput c.id traceID else fun _ _ -> ()
  let storeResults =
    if enabled then
      fun () -> storeTraceResults c.id traceID traceResults
    else
      fun _ -> ()
  { enabled = enabled
    results = traceResults
    executionTracing = executionTracing
    storeInput = storeInput
    storeTraceResults = storeResults }


module Test =
  let saveTraceResult
    (canvasID : CanvasID)
    (traceID : AT.TraceID)
    (results : TraceResults.T)
    : Task<unit> =
    task {
      do! TraceFunctionArguments.storeMany canvasID traceID results.functionArguments
      do! TraceFunctionResults.storeMany canvasID traceID results.functionResults
      return ()
    }
