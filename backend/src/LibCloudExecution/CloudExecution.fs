/// For executing code with the appropriate production "Dark cloud" execution,
/// setting traces, stdlib, etc, appropriately.
/// Used by cloud services (bwdserver, etc.)
module LibCloudExecution.CloudExecution

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module AT = LibExecution.AnalysisTypes
module Exe = LibExecution.Execution
module PackageIDs = LibExecution.PackageIDs

open LibCloud


let pmRT = LibPackageManager.PackageManager.rt
let pmPT = LibPackageManager.PackageManager.pt

let builtins : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins HttpClient.configuration
      BuiltinPM.Builtin.builtins pmPT
      BuiltinCloudExecution.Builtin.builtins
      BuiltinDarkInternal.Builtin.builtins ]
    []


let createState
  (traceID : AT.TraceID.T)
  (program : RT.Program)
  (tracing : RT.Tracing.Tracing)
  : Task<RT.ExecutionState> =
  task {
    let extraMetadata (state : RT.ExecutionState) (vm : RT.VMState) : Ply<Metadata> =
      uply {
        let callStack = Exe.callStackFromVM vm
        let epToString ep =
          match ep with
          | None -> Ply "None -- empty CallStack"
          | Some ep -> Exe.executionPointToString state ep

        let! entrypoint = epToString (RT.CallStack.entrypoint callStack)
        let! lastCalled = epToString (RT.CallStack.last callStack)

        return
          [ ("entrypoint", entrypoint)
            ("lastCalled", lastCalled)
            ("traceID", traceID)
            ("canvasID", program.canvasID) ]
      }

    let notify
      (state : RT.ExecutionState)
      (vm : RT.VMState)
      (msg : string)
      (metadata : Metadata)
      =
      uply {
        let! extra = extraMetadata state vm
        let metadata = extra @ metadata
        print $"[notify] {msg}"
        metadata |> List.iter (fun (k, v) -> print $"  {k}: {v}")
      }

    let sendException
      (state : RT.ExecutionState)
      (vm : RT.VMState)
      (metadata : Metadata)
      (exn : exn)
      =
      uply {
        let! extra = extraMetadata state vm
        let metadata = extra @ metadata
        printException "[exception]" metadata exn
      }

    return Exe.createState builtins pmRT tracing sendException notify program
  }

type ExecutionReason =
  /// The first time a trace is executed. This means more data should be stored and
  /// more users notified.
  | InitialExecution of PT.Handler.HandlerDesc * varname : string * RT.Dval

  /// A reexecution is a trace that already exists, being amended with new values
  | ReExecution

/// Execute handler.
/// This could be
/// - the first execution, which will
///   - have an ExecutionReason of InitialExecution
///   - initialize traces
/// - or ReExecution, which will
///   - update existing traces
let executeHandler
  (h : PT.Handler.T)
  (program : RT.Program)
  (traceID : AT.TraceID.T)
  (inputVars : Map<string, RT.Dval>)
  (reason : ExecutionReason)
  : Task<RT.Dval * Tracing.TraceResults.T> =
  task {
    let tracing = Tracing.create program.canvasID h.tlid traceID

    // Store the inputs of the trace (i.e. the arguments to the handler)
    match reason with
    | InitialExecution(desc, varname, inputVar) ->
      tracing.storeTraceInput desc varname inputVar
    | ReExecution -> ()

    let! state = createState traceID program tracing.executionTracing

    // TODO we shouldn't PT2RT here -- we should fetch from the PM instead

    let instrs = PT2RT.Handler.toRT inputVars h.ast

    HashSet.add h.tlid tracing.results.tlids
    let! result = Exe.executeToplevel state h.tlid instrs

    let callStackString = Exe.callStackString state

    let error (msg : string) : RT.Dval =
      let typeName = RT.FQTypeName.fqPackage PackageIDs.Type.Stdlib.Http.response

      let fields =
        [ ("statusCode", RT.DInt64 500)
          ("headers", [] |> Dval.list (RT.KTTuple(VT.string, VT.string, [])))
          ("body", msg |> UTF8.toBytes |> Dval.byteArrayToDvalList) ]

      RT.DRecord(typeName, typeName, [], Map fields)

    // CLEANUP This is a temporary hack to make it easier to work on local dev
    // servers. We should restrict this to dev mode only
    let! result =
      task {
        match result with
        | Ok result -> return result
        | Error(originalRTE, originalCallStack) ->
          let! originalCallStack = callStackString originalCallStack

          match! Exe.runtimeErrorToString state originalRTE with
          | Ok(RT.DString msg) ->
            let msg = $"Error: {msg}\n\nSource: {originalCallStack}"
            return error msg
          | Ok result -> return result
          | Error(firstErrorRTE, firstErrorCallStack) ->
            let! firstErrorCallStack = callStackString firstErrorCallStack
            match! Exe.runtimeErrorToString state firstErrorRTE with
            | Ok(RT.DString msg) ->
              return
                error (
                  $"An error occured trying to print a runtime error."
                  + $"\n\nThe formatting error occurred in {firstErrorCallStack}. The error was:\n{msg}"
                  + $"\n\nThe original error is ({originalCallStack}) {originalRTE}"
                )
            | Ok result -> return result
            | Error(secondErrorRTE, secondErrorCallStack) ->
              let! secondErrorCallStack = callStackString secondErrorCallStack
              return
                error (
                  $"Two errors occured trying to print a runtime error."
                  + $"\n\nThe 2nd formatting error occurred in {secondErrorCallStack}. The error was:\n{secondErrorRTE}"
                  + $"\n\nThe first formatting error occurred in {firstErrorCallStack}. The error was:\n{firstErrorRTE}"
                  + $"\n\nThe original error is ({originalCallStack}) {originalRTE}"
                )
      }

    tracing.storeTraceResults ()

    return (result, tracing.results)
  }

/// We call this reexecuteFunction because it always runs in an existing trace.
let reexecuteFunction
  (canvasID : CanvasID)
  (program : RT.Program)
  (traceID : AT.TraceID.T)
  (rootTLID : tlid)
  (name : RT.FQFnName.FQFnName)
  (typeArgs : List<RT.TypeReference>)
  (args : NEList<RT.Dval>)
  : Task<RT.ExecutionResult * Tracing.TraceResults.T> =
  task {
    let tracing = Tracing.create canvasID rootTLID traceID
    let! state = createState traceID program tracing.executionTracing
    let! result = Exe.executeFunction state name typeArgs args
    tracing.storeTraceResults ()
    return result, tracing.results
  }


/// Ensure library is ready to be called. Throws if it cannot initialize.
let init () : Task<unit> =
  task {
    do! pmRT.init
    return ()
  }
