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

module PT = LibBackend.ProgramTypes
module OT = LibBackend.OCamlInterop.OCamlTypes
module ORT = LibBackend.OCamlInterop.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibBackend.OCamlInterop.Convert

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
      let! args = ctx.BindModelAsync<Params>()
      t "readApiTLIDs"

      let! c = Canvas.loadAllDBs canvasInfo |> Task.map Result.unwrapUnsafe
      t "loadSavedOps"

      let! result = Stats.dbStats c args.tlids

      // CLEANUP, this is shimming an RT.Dval into an ORT.dval. Nightmare.
      let (result : T) =
        Map.map
          (fun (s : Stats.DBStat) ->
            { count = s.count
              example =
                Option.map (fun (dv, s) -> (Convert.rt2ocamlDval dv, s)) s.example })
          result

      t "analyse-db-stats"

      return result
    }
