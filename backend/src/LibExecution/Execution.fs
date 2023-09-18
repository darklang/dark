module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = RuntimeTypes
module AT = AnalysisTypes

let traceNoDvals : RT.TraceDval = fun _ _ _ -> ()
let traceNoTLIDs : RT.TraceTLID = fun _ -> ()
let loadNoFnResults : RT.LoadFnResult = fun _ _ -> None
let storeNoFnResults : RT.StoreFnResult = fun _ _ _ -> ()

let noTracing (realOrPreview : RT.RealOrPreview) : RT.Tracing =
  { traceDval = traceNoDvals
    traceTLID = traceNoTLIDs
    loadFnResult = loadNoFnResults
    storeFnResult = storeNoFnResults
    realOrPreview = realOrPreview }

let noTestContext : RT.TestContext =
  { sideEffectCount = 0

    exceptionReports = []
    expectedExceptionCount = 0
    postTestExecutionHook = fun _ _ -> () }

let createState
  (builtIns : RT.BuiltIns)
  (packageManager : RT.PackageManager)
  (tracing : RT.Tracing)
  (reportException : RT.ExceptionReporter)
  (notify : RT.Notifier)
  (tlid : tlid)
  (program : RT.Program)
  : RT.ExecutionState =
  { builtIns = builtIns
    packageManager = packageManager
    tracing = tracing
    program = program
    test = noTestContext
    reportException = reportException
    notify = notify
    tlid = tlid
    callstack = Set.empty
    onExecutionPath = true
    executingFnName = None }

let executeExpr
  (state : RT.ExecutionState)
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  : Task<RT.ExecutionResult> =
  task {
    try
      let symtable = Interpreter.withGlobals state inputVars
      let typeSymbolTable = Map.empty
      let! result = Interpreter.eval state typeSymbolTable symtable expr
      // Does nothing in non-tests
      state.test.postTestExecutionHook state.test result
      return Ok result
    with RT.RuntimeErrorException(source, rte) ->
      return Error(source, rte)
  }


let executeFunction
  (state : RT.ExecutionState)
  (callerID : id)
  (name : RT.FnName.FnName)
  (typeArgs : List<RT.TypeReference>)
  (args : NEList<RT.Dval>)
  : Task<RT.ExecutionResult> =
  task {
    try
      let typeSymbolTable = Map.empty
      let! result =
        Interpreter.callFn state typeSymbolTable callerID name typeArgs args
      // Does nothing in non-tests
      state.test.postTestExecutionHook state.test result
      return Ok result
    with RT.RuntimeErrorException(source, rte) ->
      return Error(source, rte)
  }

let runtimeErrorToString
  (state : RT.ExecutionState)
  (rte : RT.RuntimeError)
  : Task<Result<RT.Dval, RT.DvalSource * RT.RuntimeError>> =
  task {
    let fnName =
      RT.FnName.fqPackage
        "Darklang"
        [ "LanguageTools"; "RuntimeErrors"; "Error" ]
        "toString"
        0
    let args = NEList.singleton (RT.RuntimeError.toDT rte)
    return! executeFunction state 8UL fnName [] args
  }

/// Return a function to trace TLIDs (add it to state via
/// state.tracing.traceTLID), and a mutable set which updates when the
/// traceFn is used
let traceTLIDs () : HashSet.HashSet<tlid> * RT.TraceTLID =
  let touchedTLIDs = HashSet.empty ()
  let traceTLID tlid : unit = HashSet.add tlid touchedTLIDs
  (touchedTLIDs, traceTLID)

/// Return a function to trace Dvals (add it to state via
/// state.tracing.traceDval), and a mutable dictionary which updates when the
/// traceFn is used
let traceDvals () : Dictionary.T<id, AT.ExecutionResult> * RT.TraceDval =
  let results = Dictionary.empty ()

  let trace onExecutionPath (id : id) (dval : RT.Dval) : unit =
    let result =
      (if onExecutionPath then AT.ExecutedResult dval else AT.NonExecutedResult dval)

    // Overwrites if present, which is what we want
    results[id] <- result

  (results, trace)
