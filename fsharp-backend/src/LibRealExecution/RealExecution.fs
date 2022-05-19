module LibRealExecution.RealExecution

// For executing code with the appropriate production "real" execution, setting
// traces, stdlib, etc, appropriately. Used by most of the executables.

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

let stdlibFns : Map<RT.FQFnName.T, RT.BuiltInFn> =
  LibExecutionStdLib.StdLib.fns @ BackendOnlyStdLib.StdLib.fns
  |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)

let packageFns : Lazy<Task<Map<RT.FQFnName.T, RT.Package.Fn>>> =
  lazy
    (task {
      let! packages = PackageManager.allFunctions ()

      return
        packages
        |> List.map (fun (f : PT.Package.Fn) ->
          (f.name |> PT2RT.FQFnName.PackageFnName.toRT |> RT.FQFnName.Package,
           PT2RT.Package.toRT f))
        |> Map.ofList
    })

let libraries : Lazy<Task<RT.Libraries>> =
  lazy
    (task {
      let! packageFns = Lazy.force packageFns
      // TODO: this keeps a cached version so we're not loading them all the time.
      // Of course, this won't be up to date if we add more functions. This should be
      // some sort of LRU cache.
      return { stdlib = stdlibFns; packageFns = packageFns }
    })

type TraceResult =
  { tlids : HashSet.T<tlid>
    functionResults : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue>
    functionArguments : ResizeArray<TraceFunctionArguments.FunctionArgumentStore> }


let createState
  (executionID : ExecutionID)
  (traceID : AT.TraceID)
  (tlid : tlid)
  (program : RT.ProgramContext)
  : Task<RT.ExecutionState * TraceResult> =
  task {
    // Any real execution needs to track the touched TLIDs in order to send traces to pusher
    let touchedTLIDs, traceTLIDFn = Exe.traceTLIDs ()
    HashSet.add tlid touchedTLIDs

    let savedFunctionResult : Dictionary.T<TraceFunctionResults.FunctionResultKey, TraceFunctionResults.FunctionResultValue> =
      Dictionary.empty ()

    let savedFunctionArguments : ResizeArray<TraceFunctionArguments.FunctionArgumentStore> =
      ResizeArray.empty ()


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
                savedFunctionResult)
          storeFnArguments =
            (fun tlid args ->
              ResizeArray.append
                (tlid, args, NodaTime.Instant.now ())
                savedFunctionArguments)
          traceTLID = traceTLIDFn }

    let! libraries = Lazy.force libraries

    let username () =
      (Account.ownerNameFromCanvasName program.canvasName).toUserName ()

    let extraMetadata (state : RT.ExecutionState) : Metadata =
      [ "tlid", tlid
        "trace_id", traceID
        "touched_tlids", touchedTLIDs
        "executing_fn_name", state.executingFnName
        "callstack", state.callstack
        "canvas", program.canvasName
        "username", username ()
        "canvas_id", program.canvasID
        "account_id", program.accountID ]

    let notify (state : RT.ExecutionState) (msg : string) (metadata : Metadata) =
      let metadata = extraMetadata state @ metadata
      LibService.Rollbar.notify state.executionID msg metadata

    let sendException (state : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
      let metadata = extraMetadata state @ metadata
      let person : LibService.Rollbar.Person =
        Some { id = program.accountID; username = Some(username ()) }
      LibService.Rollbar.sendException state.executionID person metadata exn


    return
      (Exe.createState
        executionID
        libraries
        tracing
        sendException
        notify
        tlid
        program,
       { tlids = touchedTLIDs
         functionResults = savedFunctionResult
         functionArguments = savedFunctionArguments })
  }

// Store trace - Do not resolve task, send this into the ether
let traceInputHook
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (executionID : ExecutionID)
  (eventDesc : string * string * string)
  (request : RT.Dval)
  : unit =
  LibService.FireAndForget.fireAndForgetTask executionID "store-event" (fun () ->
    TraceInputs.storeEvent canvasID traceID eventDesc request)

// Store trace results once the request is done
let traceResultHook
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (executionID : ExecutionID)
  (result : TraceResult)
  : unit =
  LibService.FireAndForget.fireAndForgetTask
    executionID
    "traceResultHook"
    (fun () ->
      task {
        do!
          TraceFunctionArguments.storeMany canvasID traceID result.functionArguments
        do! TraceFunctionResults.storeMany canvasID traceID result.functionResults
        // Send to Pusher
        Pusher.pushNewTraceID
          executionID
          canvasID
          traceID
          (HashSet.toList result.tlids)
      })

module Test =
  let saveTraceResult
    (canvasID : CanvasID)
    (traceID : AT.TraceID)
    (result : TraceResult)
    : Task<unit> =
    task {
      do! TraceFunctionArguments.storeMany canvasID traceID result.functionArguments
      do! TraceFunctionResults.storeMany canvasID traceID result.functionResults
      return ()
    }




/// Ensure library is ready to be called. Throws if it cannot initialize.
let init () : Task<unit> =
  task {
    let! (_ : RT.Libraries) = Lazy.force libraries
    return ()
  }
