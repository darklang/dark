/// API endpoints related to toplevels
module ApiServer.Toplevels

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module Canvas = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module Telemetry = LibService.Telemetry

module Delete =
  type T = { result : string }
  type Params = { tlid : tlid }

  /// Endpoint to delete a toplevel
  let delete (ctx : HttpContext) : Task<Option<T>> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTags [ "tlid", p.tlid ]

      t.next "load-toplevel"
      let! c =
        Canvas.loadFrom Serialize.IncludeDeletedToplevels canvasInfo [ p.tlid ]

      t.next "delete-toplevel"
      if Map.containsKey p.tlid c.deletedHandlers
         || Map.containsKey p.tlid c.deletedDBs
         || Map.containsKey p.tlid c.deletedUserTypes
         || Map.containsKey p.tlid c.deletedUserFunctions then
        do! Canvas.deleteToplevelForever canvasInfo p.tlid
        return Some { result = "deleted" }
      else
        return None
    }
