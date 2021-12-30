module ApiServer.DBs

// DB-related API endpoints

open Microsoft.AspNetCore.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth
open Http

module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert

module Stats = LibBackend.Stats
module Canvas = LibBackend.Canvas
module RT = LibExecution.RuntimeTypes
module TI = LibBackend.TraceInputs
module Telemetry = LibService.Telemetry

module Unlocked =
  type T = { unlocked_dbs : tlid list }

  let get (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx

      t.next "getUnlocked"
      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id

      return { unlocked_dbs = unlocked }
    }

module DBStats =
  type Params = { tlids : tlid list }
  type Stat = { count : int; example : Option<ORT.dval * string> }
  type T = Map<tlid, Stat>

  let getStats (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTags [ "tlids", p.tlids ]

      t.next "load-canvas"
      let! c = Canvas.loadAllDBs canvasInfo

      t.next "load-db-stats"
      let! result = Stats.dbStats c p.tlids

      t.next "write-api"
      // CLEANUP, this is shimming an RT.Dval into an ORT.dval. Nightmare.
      let (result : T) =
        Map.map
          (fun (s : Stats.DBStat) ->
            { count = s.count
              example =
                Option.map (fun (dv, s) -> (Convert.rt2ocamlDval dv, s)) s.example })
          result

      return result
    }
