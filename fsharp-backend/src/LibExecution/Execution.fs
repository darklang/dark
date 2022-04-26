module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

module RT = RuntimeTypes
module AT = AnalysisTypes

let traceNoDvals : RT.TraceDval = fun _ _ _ -> ()
let traceNoTLIDs : RT.TraceTLID = fun _ -> ()
let loadNoFnResults : RT.LoadFnResult = fun _ _ -> None
let storeNoFnResults : RT.StoreFnResult = fun _ _ _ -> task { return () }
let loadNoFnArguments : RT.LoadFnArguments = fun _ -> []
let storeNoFnArguments : RT.StoreFnArguments = fun _ _ -> task { return () }

let noTracing (realOrPreview : RT.RealOrPreview) : RT.Tracing =
  { traceDval = traceNoDvals
    traceTLID = traceNoTLIDs
    loadFnResult = loadNoFnResults
    storeFnResult = storeNoFnResults
    loadFnArguments = loadNoFnArguments
    storeFnArguments = storeNoFnArguments
    realOrPreview = realOrPreview }

let noTestContext : RT.TestContext =
  { sideEffectCount = 0
    exceptionReports = []
    expectedExceptionCount = 0
    postTestExecutionHook = fun _ _ -> () }

let createState
  (executionID : ExecutionID)
  (libraries : RT.Libraries)
  (tracing : RT.Tracing)
  (reportException : RT.ExceptionReporter)
  (notify : RT.Notifier)
  (tlid : tlid)
  (program : RT.ProgramContext)
  : RT.ExecutionState =
  { libraries = libraries
    tracing = tracing
    program = program
    test = noTestContext
    reportException = reportException
    notify = notify
    executionID = executionID
    tlid = tlid
    callstack = Set.empty
    onExecutionPath = true
    executingFnName = None }

let executeExpr
  (state : RT.ExecutionState)
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  : Task<RT.Dval> =
  task {
    let symtable = Interpreter.withGlobals state inputVars
    printfn "Expr to evaluate %A" expr
    let! result = Interpreter.eval state symtable expr
    // Does nothing in non-tests
    state.test.postTestExecutionHook state.test result
    return result
  }

let executeHandler
  (state : RT.ExecutionState)
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  : Task<RT.Dval> =
  executeExpr state inputVars expr


let executeFunction
  (state : RT.ExecutionState)
  (callerID : tlid)
  (args : List<RT.Dval>)
  (name : RT.FQFnName.T)
  : Task<RT.Dval> =
  task {
    let! result = Interpreter.callFn state callerID name args RT.NotInPipe RT.NoRail
    // Does nothing in non-tests
    state.test.postTestExecutionHook state.test result
    return result
  }


/// Return a function to trace TLIDs (add it to state via
/// state.tracing.traceTLID), and a mutable set which updates when the
/// traceFn is used
let traceTLIDs () : HashSet.T<tlid> * RT.TraceTLID =
  let touchedTLIDs = HashSet.empty ()
  let traceTLID tlid : unit = HashSet.add tlid touchedTLIDs
  (touchedTLIDs, traceTLID)

let updateTracing
  (fn : RT.Tracing -> RT.Tracing)
  (state : RT.ExecutionState)
  : RT.ExecutionState =
  { state with tracing = fn state.tracing }



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
