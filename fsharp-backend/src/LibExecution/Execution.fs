module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = RuntimeTypes
module AT = AnalysisTypes
module PReq = ParsedRequest

// (* -------------------- *)
// (* For exec_state *)
// (* -------------------- *)
// let load_no_results _ _ = None
//
// let store_no_results _ _ _ = ()
//
// let load_no_arguments _ = []
//
// let store_no_arguments _ _ = ()
//
// (* -------------------- *)
// (* Execution *)
// (* -------------------- *)
// let execute_handler
//     ~tlid
//     ~execution_id
//     ~input_vars
//     ~(dbs : RuntimeT.DbT.db list)
//     ~user_fns
//     ~user_tipes
//     ~package_fns
//     ~secrets
//     ~account_id
//     ~canvas_id
//     ?(parent = (None : Span.t option))
//     ?(load_fn_result = load_no_results)
//     ?(load_fn_arguments = load_no_arguments)
//     ?(store_fn_result = store_no_results)
//     ?(store_fn_arguments = store_no_arguments)
//     (h : HandlerT.handler) : dval * tlid list =
//   let f unit =
//     let tlid_store = TLIDTable.create () in
//     let trace_tlid tlid = Hashtbl.set tlid_store ~key:tlid ~data:true in
//     let state : exec_state =
//       { tlid
//       ; callstack = Tc.StrSet.empty
//       ; account_id
//       ; canvas_id
//       ; user_fns
//       ; user_tipes
//       ; package_fns
//       ; dbs
//       ; secrets
//       ; trace = (fun ~on_execution_path _ _ -> ())
//       ; trace_tlid
//       ; on_execution_path = true
//       ; exec =
//           (fun ~state _ _ -> Exception.internal "invalid state.exec function")
//       ; context = Real
//       ; execution_id
//       ; fail_fn = None
//       ; executing_fnname = ""
//       ; load_fn_result
//       ; load_fn_arguments
//       ; store_fn_result
//       ; store_fn_arguments }
//     in
//     let result = Ast.execute_ast ~state ~input_vars h.ast in
//     let tlids = TLIDTable.keys tlid_store in
//     match result with
//     | DErrorRail (DOption OptNothing) | DErrorRail (DResult (ResError _)) ->
//         (DResp (Response (404, []), Dval.dstr_of_string_exn "Not found"), tlids)
//     | DErrorRail _ ->
//         ( DResp
//             ( Response (500, [])
//             , Dval.dstr_of_string_exn "Invalid conversion from errorrail" )
//         , tlids )
//     | dv ->
//         (dv, tlids)
//   in
//   (* Wrapping here b/c we call execute_handler in a lot of places, I'm not yet
//    * ready to instrument all of them *)
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

let run
  (tlid : tlid)
  (vars : List<string * RT.Dval>)
  (fns : List<RT.BuiltInFn>)
  (e : RT.Expr)
  : Task<RT.Dval> =
  task {
    let functions = fns |> List.map (fun fn -> (fn.name, fn)) |> Map
    let state : RT.ExecutionState = { functions = functions; tlid = tlid }
    let symtable = Map.ofList vars

    let result = Interpreter.eval state symtable e

    match result with
    | Prelude.Task t -> return! t
    | Prelude.Value v -> return v
  }

open RT.Shortcuts

let runHttp
  (tlid : tlid)
  (url : string)
  (vars : RT.DvalMap)
  (body : byte array)
  (fns : List<RT.BuiltInFn>)
  (e : RT.Expr)
  : Task<RT.Dval> =
  task {
    let functions = fns |> List.map (fun fn -> (fn.name, fn)) |> Map
    let state : RT.ExecutionState = { functions = functions; tlid = tlid }

    let result =
      Interpreter.applyFnVal
        state
        (RT.Expr.toID e)
        (RT.FnName(RT.FQFnName.stdlibName "Http" "middleware" 0))
        [ RT.DStr url
          RT.DBytes body
          RT.DObj Map.empty
          RT.DFnVal(
            RT.Lambda
              { parameters = [ gid (), "request" ]; symtable = vars; body = e }
          ) ]
        RT.NotInPipe
        RT.NoRail


    match result with
    | Prelude.Task t ->
        let! t = t
        printfn $"result in runHttp is a task {t}"
        return t

    | Prelude.Value v ->
        printfn $"result is runHttp is a value {v}"
        return v
  }
