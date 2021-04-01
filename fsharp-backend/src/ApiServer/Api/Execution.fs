module ApiServer.Execution

// Execution API endpoints

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Tablecloth

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibBackend.OCamlInterop.OCamlTypes
module ORT = LibBackend.OCamlInterop.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibBackend.OCamlInterop.Convert

module Canvas = LibBackend.Canvas
module Auth = LibBackend.Authorization
module Exe = LibExecution.Execution
module TraceFunctionArguments = LibBackend.TraceFunctionArguments
module TraceFunctionResults = LibBackend.TraceFunctionResults
module DvalRepr = LibExecution.DvalRepr


module ExecuteFunction =
  let fns =
    lazy
      (LibExecution.StdLib.StdLib.fns @ LibBackend.StdLib.StdLib.fns
       |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name))

  type Params =
    { tlid : tlid
      trace_id : AT.TraceID
      caller_id : id
      args : ORT.dval list
      fnname : string }

  type T =
    { result : ORT.dval
      hash : string
      hashVersion : int
      touched_tlids : tlid list
      unlocked_dbs : tlid list }

  let execute (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! body = ctx.BindModelAsync<Params>()
      t "loadCanvasInfo"

      let! c = Canvas.loadTLIDsWithContext canvasInfo [ body.tlid ]
      let c = Result.unwrapUnsafe c
      t "load-canvas"

      let dbs =
        c.dbs
        |> Map.values
        |> List.map (fun db -> (db.name, PT.DB.toRuntimeType db))
        |> Map.ofList

      let userFns =
        c.userFunctions
        |> Map.values
        |> List.map (fun f -> (f.name, PT.UserFunction.toRuntimeType f))
        |> Map.ofList

      let userTypes =
        c.userTypes
        |> Map.values
        |> List.map (fun t -> ((t.name, t.version), PT.UserType.toRuntimeType t))
        |> Map.ofList

      let secrets =
        (c.secrets |> Map.map (fun pt -> pt.toRuntimeType ()) |> Map.values)

      let args = List.map Convert.ocamlDval2rt body.args
      let! packageFns = Lazy.force LibBackend.PackageManager.cachedForExecution

      let storeFnResult = TraceFunctionResults.store canvasInfo.id body.trace_id
      let storeFnArguments = TraceFunctionArguments.store canvasInfo.id body.trace_id

      let state =
        Exe.createState
          canvasInfo.owner
          canvasInfo.id
          body.tlid
          (Lazy.force fns)
          packageFns
          dbs
          userFns
          userTypes
          secrets
          Exe.loadNoResults
          storeFnResult
          Exe.loadNoArguments
          storeFnArguments

      t "load-execution-state"

      let fnname = body.fnname |> PT.FQFnName.parse

      let! (result, tlids) = Exe.executeFunction state body.caller_id args fnname

      t "execute-function"

      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id
      t "get-unlocked"

      let hashVersion = DvalRepr.currentHashVersion
      let hash = DvalRepr.hash hashVersion args

      let result =
        { result = Convert.rt2ocamlDval result
          hash = hash
          hashVersion = hashVersion
          touched_tlids = tlids
          unlocked_dbs = unlocked }

      t "create-result"
      return result
    }

let endpoints : Endpoint list =
  let h = Middleware.apiHandler

  [ POST [ routef "/api/%s/execute_function" (h ExecuteFunction.execute Auth.Read) ] ]
// type trigger_handler_rpc_params =
//   { tlid : tlid
//   ; trace_id : RuntimeT.uuid
//   ; input : input_vars }


// | `POST, ["api"; canvas; "trigger_handler"] ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers
//           (trigger_handler ~execution_id parent canvas body))
// type trigger_handler_rpc_result = {touched_tlids : tlid list}
//
// let to_trigger_handler_rpc_result touched_tlids : string =
//   {touched_tlids}
//   |> trigger_handler_rpc_result_to_yojson
//   |> Yojson.Safe.to_string ~std:true
//
