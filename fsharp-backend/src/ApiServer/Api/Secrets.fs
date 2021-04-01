module ApiServer.Secrets

// API endpoints for Secrets

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module PT = LibBackend.ProgramTypes
module OT = LibBackend.OCamlInterop.OCamlTypes
module ORT = LibBackend.OCamlInterop.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibBackend.OCamlInterop.Convert

// type insert_secret_params = RuntimeT.secret
//
// type secrets_list_results = {secrets : RuntimeT.secret list}


let endpoints : Endpoint list =
  let h = Middleware.apiHandler
  let oh = Middleware.apiOptionHandler

  [ POST [] ]
// routef "/api/%s/execute_function" (h ExecuteFunction.execute Auth.Read)
// | `POST, ["api"; canvas; "insert_secret"] ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers
//           (insert_secret ~execution_id parent canvas body))
