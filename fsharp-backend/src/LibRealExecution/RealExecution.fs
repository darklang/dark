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

let createState
  (traceID : AT.TraceID)
  (tlid : tlid)
  (program : RT.ProgramContext)
  : Task<RT.ExecutionState * Tracing.TraceResults> =
  task {
    let (tracingState, tracing) = Tracing.createStandardTracer ()
    let! libraries = Lazy.force libraries

    let username () =
      (Account.ownerNameFromCanvasName program.canvasName).toUserName ()

    let extraMetadata (state : RT.ExecutionState) : Metadata =
      [ "tlid", tlid
        "trace_id", traceID
        "touched_tlids", tracingState.tlids
        "executing_fn_name", state.executingFnName
        "callstack", state.callstack
        "canvas", program.canvasName
        "username", username ()
        "canvas_id", program.canvasID
        "account_id", program.accountID ]

    let notify (state : RT.ExecutionState) (msg : string) (metadata : Metadata) =
      let metadata = extraMetadata state @ metadata
      LibService.Rollbar.notify msg metadata

    let sendException (state : RT.ExecutionState) (metadata : Metadata) (exn : exn) =
      let metadata = extraMetadata state @ metadata
      let person : LibService.Rollbar.Person =
        Some { id = program.accountID; username = Some(username ()) }
      LibService.Rollbar.sendException person metadata exn

    return
      (Exe.createState libraries tracing sendException notify tlid program,
       tracingState)
  }

type ExecutionReason =
  /// The first time a trace is executed. This means more data should be stored and
  /// more users notified.
  | InitialExecution of (string * string * string) * RT.Dval

  /// A reexecution is a trace that already exists, being amended with new values
  | ReExecution

let executeExpr
  (c : Canvas.T)
  (tlid : tlid)
  (traceID : AT.TraceID)
  (inputVars : Map<string, RT.Dval>)
  (executionType : ExecutionReason)
  (expr : RT.Expr)
  : Task<RT.Dval * Tracing.TraceResults> =
  task {

    match executionType with
    | InitialExecution (eventDesc, inputVar) ->
      Tracing.storeTraceInput c.meta.id traceID eventDesc inputVar
    | ReExecution -> ()

    let! (state, traceResults) = createState traceID tlid (Canvas.toProgram c)
    HashSet.add tlid traceResults.tlids

    let! result = Exe.executeExpr state inputVars expr

    Tracing.storeTraceCompletion c.meta.id traceID traceResults

    match executionType with
    | ReExecution -> ()
    | InitialExecution _ ->
      Pusher.pushNewTraceID c.meta.id traceID (HashSet.toList traceResults.tlids)

    return (result, traceResults)
  }

let executeFunction
  (c : Canvas.T)
  (callerID : tlid)
  (traceID : AT.TraceID)
  (name : RT.FQFnName.T)
  (args : List<RT.Dval>)
  : Task<RT.Dval * Tracing.TraceResults> =
  task {
    let! (state, traceResults) = createState traceID callerID (Canvas.toProgram c)

    let! result = Exe.executeFunction state callerID name args

    Tracing.storeTraceCompletion c.meta.id traceID traceResults

    return result, traceResults
  }


/// Ensure library is ready to be called. Throws if it cannot initialize.
let init () : Task<unit> =
  task {
    let! (_ : RT.Libraries) = Lazy.force libraries
    return ()
  }
