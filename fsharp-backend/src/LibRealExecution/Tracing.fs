/// Tracing for real execution
module LibRealExecution.Tracing

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

open LibBackend

type TraceResults =
  { tlids : HashSet.T<tlid>
    functionResults : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue>
    functionArguments : ResizeArray<TraceFunctionArguments.FunctionArgumentStore> }

let createStandardTracer () : TraceResults * RT.Tracing =
  // Any real execution needs to track the touched TLIDs in order to send traces to pusher
  let touchedTLIDs, traceTLIDFn = Exe.traceTLIDs ()

  let savedFunctionResult : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue> =
    Dictionary.empty ()

  let savedFunctionArguments : ResizeArray<TraceFunctionArguments.FunctionArgumentStore> =
    ResizeArray.empty ()

  let state =
    { tlids = touchedTLIDs
      functionResults = savedFunctionResult
      functionArguments = savedFunctionArguments }

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
              state.functionResults)
        storeFnArguments =
          (fun tlid args ->
            ResizeArray.append
              (tlid, args, NodaTime.Instant.now ())
              state.functionArguments)
        traceTLID = traceTLIDFn }
  (state, tracing)

/// Store trace input in the background
let storeTraceInput
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (desc : string * string * string)
  (executionID : ExecutionID)
  (input : RT.Dval)
  : unit =
  LibService.FireAndForget.fireAndForgetTask
    executionID
    "traceResultHook"
    (fun () ->
      task {
        let! (_timestamp : NodaTime.Instant) =
          TraceInputs.storeEvent canvasID traceID desc input
        return ()
      })



// Store trace results once the request is done
let storeTraceCompletion
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (executionID : ExecutionID)
  (results : TraceResults)
  : unit =
  LibService.FireAndForget.fireAndForgetTask
    executionID
    "traceResultHook"
    (fun () ->
      task {
        do!
          TraceFunctionArguments.storeMany canvasID traceID results.functionArguments
        do! TraceFunctionResults.storeMany canvasID traceID results.functionResults
      })

module Test =
  let saveTraceResult
    (canvasID : CanvasID)
    (traceID : AT.TraceID)
    (results : TraceResults)
    : Task<unit> =
    task {
      do! TraceFunctionArguments.storeMany canvasID traceID results.functionArguments
      do! TraceFunctionResults.storeMany canvasID traceID results.functionResults
      return ()
    }
