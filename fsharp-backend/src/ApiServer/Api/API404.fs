module ApiServer.F404s

// Functions and API endpoints for the API

open Microsoft.AspNetCore.Http
open Giraffe

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Tablecloth

module TI = LibBackend.TraceInputs

module List =

  type T = { f404s : List<TI.F404> }

  let get (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      t "read-api"

      let! f404s = TI.getRecent404s canvasInfo.id
      t "get-recent-404s"
      return { f404s = f404s }
    }

module Delete =
  type T = { result : string }
  type Params = { space : string; path : string; modifier : string }

  let delete (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! p = ctx.BindModelAsync<Params>()
      t "read-api"

      do! TI.delete404s canvasInfo.id p.space p.path p.modifier
      t "delete-404"

      return { result = "deleted" }
    }
