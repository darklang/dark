module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
type HashSet<'k> = System.Collections.Generic.HashSet<'k>

open Prelude
open Tablecloth

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

let createState
  (libraries : RT.Libraries)
  (tracing : RT.Tracing)
  (tlid : tlid)
  (program : RT.ProgramContext)
  : RT.ExecutionState =
  { libraries = libraries
    tracing = tracing
    program = program
    tlid = tlid
    callstack = Set.empty
    onExecutionPath = true
    executingFnName = None }

let executeExpr
  (state : RT.ExecutionState)
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  : Task<RT.Dval> =
  let symtable = Interpreter.withGlobals state inputVars
  Interpreter.eval state symtable expr |> TaskOrValue.toTask

let executeFunction
  (state : RT.ExecutionState)
  (callerID : tlid)
  (args : List<RT.Dval>)
  (name : RT.FQFnName.T)
  : Task<RT.Dval> =
  Interpreter.callFn state name callerID args RT.NoRail RT.NotInPipe
  |> TaskOrValue.toTask



// Return a function to trace TLIDs (add it to state via
// state.tracing.traceTLID), and a mutable set which updates when the traceFn
// is used
let traceTLIDs () : HashSet<tlid> * RT.TraceTLID =
  let touchedTLIDs = HashSet()

  let traceTLID tlid : unit =
    let (_set : HashSet<tlid>) = HashSet.add tlid touchedTLIDs in ()

  (touchedTLIDs, traceTLID)

let updateTraceTLID
  (traceTLID : RT.TraceTLID)
  (state : RT.ExecutionState)
  : RT.ExecutionState =
  { state with tracing = { state.tracing with traceTLID = traceTLID } }



// Return a function to trace Dvals (add it to state via
// state.tracing.traceDval), and a mutable dictionary which updates when the
// traceFn is used
let traceDvals () : Dictionary<id, AT.ExecutionResult> * RT.TraceDval =
  let results = Dictionary()

  let trace onExecutionPath (id : id) (dval : RT.Dval) =
    let result =
      (if onExecutionPath then AT.ExecutedResult dval else AT.NonExecutedResult dval)

    results.Add(id, result)

  (results, trace)

let updateTraceDval
  (traceDval : RT.TraceDval)
  (state : RT.ExecutionState)
  : RT.ExecutionState =
  { state with tracing = { state.tracing with traceDval = traceDval } }
