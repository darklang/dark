module ApiServer.F404s

// Functions and API endpoints for the API

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Tablecloth

module Auth = LibBackend.Authorization
module TI = LibBackend.TraceInputs

module Get404s =

  type T = { f404s : List<TI.F404> }

  let get404s (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      t "loadCanvasInfo"

      let! f404s = TI.getRecent404s canvasInfo.id
      t "getRecent404s"
      return { f404s = f404s }
    }

let endpoints : Endpoint list =
  let h = Middleware.apiHandler

  [ POST [ routef "/api/%s/get_404s" (h Get404s.get404s Auth.Read) ] ]
// | `POST, ["api"; canvas; "delete_404"] ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers (delete_404 ~execution_id parent canvas body))


// type new_404_push = SE.four_oh_four

// type get_404s_result = {f404s : fofs}
//
// let to_get_404s_result (f404s : fofs) : string =
//   {f404s} |> get_404s_result_to_yojson |> Yojson.Safe.to_string ~std:true
