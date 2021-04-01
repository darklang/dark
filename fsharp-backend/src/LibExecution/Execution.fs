module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
type HashSet<'k> = System.Collections.Generic.HashSet<'k>

open Prelude
open Tablecloth

module RT = RuntimeTypes
module AT = AnalysisTypes
module PReq = ParsedRequest

// --------------------
// For ExecutionState
// --------------------

let loadNoResults _ _ = None

let storeNoResults _ _ _ = task { return () }
let loadNoArguments _ = []
let storeNoArguments _ _ = task { return () }

// --------------------
// Execution
// --------------------

let createState
  (accountID : UserID)
  (canvasID : CanvasID)
  (tlid : tlid)
  (functions : Map<RT.FQFnName.T, RT.BuiltInFn>)
  (packageFns : Map<RT.FQFnName.T, RT.Package.Fn>)
  (dbs : Map<string, RT.DB.T>)
  (userFns : Map<string, RT.UserFunction.T>)
  (userTypes : Map<string * int, RT.UserType.T>)
  (secrets : List<RT.Secret.T>)
  (loadFnResult : RT.LoadFnResult)
  (storeFnResult : RT.StoreFnResult)
  (loadFnArguments : RT.LoadFnArguments)
  (storeFnArguments : RT.StoreFnArguments)
  : RT.ExecutionState =
  { tlid = tlid
    functions = functions
    callstack = Set.empty
    accountID = accountID
    canvasID = canvasID
    userFns = userFns
    userTypes = userTypes
    packageFns = packageFns
    dbs = dbs
    secrets = secrets
    trace = (fun on_execution_path _ _ -> ())
    traceTLID = fun _ -> ()
    onExecutionPath = true
    context = RT.Real
    executingFnName = None
    loadFnResult = loadFnResult
    loadFnArguments = loadFnArguments
    storeFnResult = storeFnResult
    storeFnArguments = storeFnArguments }

let run
  (state : RT.ExecutionState)
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  //     ?(parent = (None : Span.t option))
  : Task<RT.Dval> =
  // FSTODO: get the list of tlids some other way
  // let tlidStore = new System.Collections.Generic.HashSet<tlid>()
  // let traceTLID (tlid : tlid) = tlidStore.Add tlid |> ignore
  // let state = { state with traceTLID = traceTLID }
  let symtable = Interpreter.withGlobals state inputVars
  Interpreter.eval state symtable expr |> TaskOrValue.toTask


// FSTODO
//   match parent with
//   | None ->
//       f ()
//   | Some parent ->
//       Telemetry.with_span parent "execute_handler" (fun _parent -> f ())


let executeFunction
  (state : RT.ExecutionState)
  (callerID : tlid)
  (args : List<RT.Dval>)
  (name : string)
  : Task<RT.Dval * List<tlid>> =
  task {
    let touchedTLIDs = HashSet()

    let traceTLID tlid =
      let (_set : HashSet<tlid>) = HashSet.add tlid touchedTLIDs in ()

    let state = { state with traceTLID = traceTLID }

    let! result =
      Interpreter.callFn
        state
        (RT.FQFnName.User name)
        callerID
        args
        RT.NoRail
        RT.NotInPipe
      |> TaskOrValue.toTask

    return (result, HashSet.toList touchedTLIDs)
  }


// --------------------
// Execution
// --------------------
let analyseExpr
  (state : RT.ExecutionState)
  (loadFnResults : RT.LoadFnResult)
  (loadFnArguments : RT.LoadFnArguments)
  (inputVars : RT.DvalMap)
  (ast : RT.Expr)
  : Task<AT.AnalysisResults> =
  task {
    let results = Dictionary()

    let trace onExecutionPath (id : id) (dval : RT.Dval) =
      let result =
        (if onExecutionPath then
           AT.ExecutedResult dval
         else
           AT.NonExecutedResult dval)

      results.Add(id, result)

    let state : RT.ExecutionState =
      { state with
          context = RT.Preview
          trace = trace
          loadFnResult = loadFnResults
          loadFnArguments = loadFnArguments
          storeFnResult = storeNoResults
          storeFnArguments = storeNoArguments }

    let symtable = Interpreter.withGlobals state inputVars
    let! (_ : RT.Dval) = (Interpreter.eval state symtable ast) |> TaskOrValue.toTask
    return results
  }
