/// API endpoints related to 404s
module ApiServer.F404s

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module TI = LibBackend.TraceInputs
module CTApi = ClientTypes.Api
module Telemetry = LibService.Telemetry

module List =
  /// Endpoint to fetch a list of recent 404s
  let get (ctx : HttpContext) : Task<CTApi.F404.List.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx

      t.next "get-recent-404s"
      let! f404s = TI.getRecent404s canvasInfo.id
      return { f404s = f404s }
    }

module Delete =
  /// Endpoint to delete a 404
  let delete (ctx : HttpContext) : Task<CTApi.F404.Delete.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<CTApi.F404.Delete.Request>()
      Telemetry.addTags [ "space", p.space; "path", p.path; "modifier", p.modifier ]

      t.next "delete-404"
      do! TI.delete404s canvasInfo.id p.space p.path p.modifier

      return { result = "deleted" }
    }
