module ApiServer.Workers

// API endpoints for Workers

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

module Stats = LibBackend.Stats
module Auth = LibBackend.Authorization

// type worker_schedule_update_rpc_params =
//   { name : string
//   ; schedule : string }

module WorkerStats =
  type Params = { tlid : tlid }

  type T = { count : int }

  let getStats (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! args = ctx.BindModelAsync<Params>()
      t "read-api"

      let! result = Stats.workerStats canvasInfo.id args.tlid
      t "analyse-worker-stats"

      return { count = result }
    }

let endpoints : Endpoint list =
  let h = Middleware.apiHandler

  [ POST [ routef "/api/%s/get_worker_stats" (h WorkerStats.getStats Auth.Read) ] ]

// | `POST, ["api"; canvas; "worker_schedule"] ->
//     when_can_edit ~canvas (fun _ ->
//         wrap_editor_api_headers
//           (worker_schedule ~execution_id parent canvas body))
