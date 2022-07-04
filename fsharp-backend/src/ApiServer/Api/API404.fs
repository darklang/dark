/// API endpoints related to 404s
module ApiServer.F404s

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module TI = LibBackend.TraceInputs
module Telemetry = LibService.Telemetry

module List =

  type T = { f404s : List<TI.F404> }

  /// Endpoint to fetch a list of recent 404s
  let get (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx

      t.next "get-recent-404s"
      let! f404s = TI.getRecent404s canvasInfo.id
      return { f404s = f404s }
    }

module Delete =
  type T = { result : string }

  type Params = { space : string; path : string; modifier : string }

  /// Endpoint to delete a 404
  let delete (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTags [ "space", p.space; "path", p.path; "modifier", p.modifier ]

      t.next "delete-404"
      do! TI.delete404s canvasInfo.id p.space p.path p.modifier

      return { result = "deleted" }
    }
