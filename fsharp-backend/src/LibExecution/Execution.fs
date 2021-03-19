module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

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
//
//
// let execute_function
//     ~tlid
//     ~execution_id
//     ~trace_id
//     ~dbs
//     ~user_fns
//     ~user_tipes
//     ~package_fns
//     ~secrets
//     ~account_id
//     ~canvas_id
//     ~caller_id
//     ~args
//     ?(store_fn_result = store_no_results)
//     ?(store_fn_arguments = store_no_arguments)
//     fnname =
//   let tlid_store = TLIDTable.create () in
//   let trace_tlid tlid = Hashtbl.set tlid_store ~key:tlid ~data:true in
//   let state : exec_state =
//     { tlid
//     ; callstack = Tc.StrSet.empty
//     ; account_id
//     ; canvas_id
//     ; user_fns
//     ; user_tipes
//     ; package_fns
//     ; dbs
//     ; secrets
//     ; trace = (fun ~on_execution_path _ _ -> ())
//     ; trace_tlid
//     ; on_execution_path = true
//     ; exec =
//         (fun ~state _ _ -> Exception.internal "invalid state.exec function")
//     ; context = Real
//     ; execution_id
//     ; fail_fn = None
//     ; executing_fnname = fnname
//     ; load_fn_result = load_no_results
//     ; load_fn_arguments = load_no_arguments
//     ; store_fn_result
//     ; store_fn_arguments }
//   in
//   (* Note the order of the next couple of lines. We *must* actually execute the fn
//    * before we grab the touched_tlids from the store. You might think that you could
//    * write (execute_fn ..., TLIDTable.keys ...) as a tuple. you would be incorrect,
//    * as this is undefined behaviour according to the OCaml specification
//    *
//    * http://caml.inria.fr/pub/docs/manual-ocaml/expr.html#sss:expr-products *)
//   let result = Ast.execute_fn state fnname caller_id args in
//   let touched_tlids = TLIDTable.keys tlid_store in
//   (result, touched_tlids)


// --------------------
// Execution
// --------------------
let analyseExpr
  (accountID : UserID)
  (canvasID : CanvasID)
  (tlid : tlid)
  (inputVars : RT.DvalMap)
  (packageFns : Map<RT.FQFnName.T, RT.Package.Fn>)
  (dbs : Map<string, RT.DB.T>)
  (userFns : Map<string, RT.UserFunction.T>)
  (userTypes : Map<string * int, RT.UserType.T>)
  (secrets : List<RT.Secret.T>)
  (ast : RT.Expr)
  : AT.AnalysisResults =
  let results = Dictionary()

  let trace onExecutionPath (id : id) (dval : RT.Dval) =
    let result =
      (if onExecutionPath then AT.ExecutedResult dval else AT.NonExecutedResult dval)

    results.Add(id, result)

  let state : RT.ExecutionState =
    { functions = Map.empty
      tlid = tlid
      callstack = Set.empty
      accountID = accountID
      canvasID = canvasID
      userFns = userFns
      userTypes = userTypes
      packageFns = packageFns
      dbs = dbs
      secrets = secrets
      trace = trace
      traceTLID = fun _ -> ()
      onExecutionPath = true
      context = RT.Preview
      executingFnName = None
      loadFnResult = loadNoResults
      loadFnArguments = loadNoArguments
      storeFnResult = storeNoResults
      storeFnArguments = storeNoArguments }

  let symtable = Interpreter.withGlobals state inputVars
  let _ = Interpreter.eval state symtable ast
  results
