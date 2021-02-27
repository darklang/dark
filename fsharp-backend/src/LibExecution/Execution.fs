module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = RuntimeTypes
module AT = AnalysisTypes
module PReq = ParsedRequest

// --------------------
// For exec_state
// --------------------
// let load_no_results _ _ = None
//
// let store_no_results _ _ _ = ()
//
// let load_no_arguments _ = []
//
// let store_no_arguments _ _ = ()
//
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
  (userTypes : Map<string, RT.UserType.T>)
  (secrets : List<RT.Secret.T>)
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
    executingFnName = ""
  // loadFnResult = ""
  // loadFnArguments = ""
  // storeFnResult = ""
  // storeFnArguments = ""
  }

let run
  (state : RT.ExecutionState)
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  //     ?(parent = (None : Span.t option))
//     ?(load_fn_result = load_no_results)
//     ?(load_fn_arguments = load_no_arguments)
//     ?(store_fn_result = store_no_results)
//     ?(store_fn_arguments = store_no_arguments)
  : Task<RT.Dval> =
  // FSTODO: get the list of tlids some other way
  // let tlidStore = new System.Collections.Generic.HashSet<tlid>()
  // let traceTLID (tlid : tlid) = tlidStore.Add tlid |> ignore
  // let state = { state with traceTLID = traceTLID }
  let symtable = Interpreter.withGlobals state inputVars
  Interpreter.eval state symtable expr |> TaskOrValue.toTask
// loadFnResult = ""
// loadFnArguments = ""
// storeFnResult = ""
// storeFnArguments = ""



let runHttp
  (state : RT.ExecutionState)
  (url : string)
  (body : byte [])
  (inputVars : RT.Symtable)
  (expr : RT.Expr)
  //     ?(parent = (None : Span.t option))
//     ?(load_fn_result = load_no_results)
//     ?(load_fn_arguments = load_no_arguments)
//     ?(store_fn_result = store_no_results)
//     ?(store_fn_arguments = store_no_arguments)
  : Task<RT.Dval> =
  task {
    // FSTODO: get the list of tlids some other way
    // let tlidStore = new System.Collections.Generic.HashSet<tlid>()
    // let traceTLID (tlid : tlid) = tlidStore.Add tlid |> ignore
    // let state = { state with traceTLID = traceTLID }
    let symtable = Interpreter.withGlobals state inputVars

    let! result =
      Interpreter.applyFnVal
        state
        (RT.Expr.toID expr)
        (RT.FnName(RT.FQFnName.stdlibName "Http" "middleware" 0))
        [ RT.DStr url
          RT.DBytes body
          RT.DObj Map.empty
          RT.DFnVal(
            RT.Lambda
              { parameters = [ gid (), "request" ]
                symtable = symtable
                body = expr }
          ) ]
        RT.NotInPipe
        RT.NoRail
      |> TaskOrValue.toTask

    match result with
    | RT.DErrorRail (RT.DOption None)
    | RT.DErrorRail (RT.DResult (Error _)) ->
        return
          (RT.DHttpResponse(
            RT.Response(404, [ "Content-length", "9"; "server", "darklang" ]),
            RT.DBytes(toBytes "Not found")
          ))
    | RT.DErrorRail _ ->
        return
          (RT.DHttpResponse(
            RT.Response(500, [ "Content-length", "32"; "server", "darklang" ]),
            RT.DBytes(toBytes "Invalid conversion from errorrail")
          ))
    | dv -> return dv
  // loadFnResult = ""
  // loadFnArguments = ""
  // storeFnResult = ""
  // storeFnArguments = ""
  }


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
//
//
// (* -------------------- *)
// (* Execution *)
// (* -------------------- *)
// let analyse_ast
//     ~tlid
//     ~execution_id
//     ~input_vars
//     ~dbs
//     ~user_fns
//     ~user_tipes
//     ~package_fns
//     ~secrets
//     ~account_id
//     ~canvas_id
//     ?(load_fn_result = load_no_results)
//     ?(load_fn_arguments = load_no_arguments)
//     (ast : fluid_expr) : analysis =
//   let value_store = IDTable.create () in
//   let trace ~on_execution_path id dval =
//     Hashtbl.set
//       value_store
//       ~key:id
//       ~data:
//         ( if on_execution_path
//         then ExecutedResult dval
//         else NonExecutedResult dval )
//   in
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
//     ; trace
//     ; trace_tlid = (fun _ -> ())
//     ; on_execution_path = true
//     ; exec =
//         (fun ~state _ _ -> Exception.internal "invalid state.exec function")
//     ; context = Preview
//     ; execution_id
//     ; fail_fn = None
//     ; executing_fnname = ""
//     ; load_fn_result
//     ; load_fn_arguments
//     ; store_fn_result = store_no_results
//     ; store_fn_arguments = store_no_arguments }
//   in
//   let _ = Ast.execute_ast ~state ~input_vars ast in
//   value_store
//
