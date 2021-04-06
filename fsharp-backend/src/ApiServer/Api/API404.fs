module ApiServer.F404s

// Functions and API endpoints for the API

open Microsoft.AspNetCore.Http
open Giraffe

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open Tablecloth

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
