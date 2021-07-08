module ApiServer.DBs

// DB-related API endpoints

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert

module Stats = LibBackend.Stats
module Canvas = LibBackend.Canvas
module RT = LibExecution.RuntimeTypes
module TI = LibBackend.TraceInputs

module Unlocked =
  type T = { unlocked_dbs : tlid list }

  let get (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      t "loadCanvasInfo"

      let! unlocked = LibBackend.UserDB.unlocked canvasInfo.owner canvasInfo.id
      t "getUnlocked"
      return { unlocked_dbs = unlocked }
    }

module DBStats =
  type Params = { tlids : tlid list }
  type Stat = { count : int; example : Option<ORT.dval * string> }
  type T = Map<tlid, Stat>

  let getStats (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! p = ctx.BindModelAsync<Params>()
      t "read-api"

      let! c = Canvas.loadAllDBs canvasInfo |> Task.map Result.unwrapUnsafe
      t "load-canvas"

      let! result = Stats.dbStats c p.tlids
      t "load-db-stats"

      // CLEANUP, this is shimming an RT.Dval into an ORT.dval. Nightmare.
      let (result : T) =
        Map.map
          (fun (s : Stats.DBStat) ->
            { count = s.count
              example =
                Option.map (fun (dv, s) -> (Convert.rt2ocamlDval dv, s)) s.example })
          result

      t "write-api"

      return result
    }
