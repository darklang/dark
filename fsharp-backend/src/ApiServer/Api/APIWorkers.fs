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
module EQ = LibBackend.EventQueue

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

module Scheduler =
  type Params = { name : string; schedule : string }
  type T = EQ.WorkerStates.T

  let updateSchedule (ctx : HttpContext) : Task<T> =
    task {
      let t = Middleware.startTimer ctx
      let canvasInfo = Middleware.loadCanvasInfo ctx
      let! args = ctx.BindModelAsync<Params>()
      t "read-api"

      match args.schedule with
      | "pause" -> do! EQ.pauseWorker canvasInfo.id args.name
      | "run" -> do! EQ.unpauseWorker canvasInfo.id args.name
      | _ -> failwith "Invalid schedule"

      t "schedule-worker"

      let! ws = EQ.getWorkerSchedules canvasInfo.id
      t "get-worker-schedule"

      // CLEANUP: perhaps this update should go closer where it happens, in
      // case it doesn't happen in an API call.
      LibBackend.Pusher.pushWorkerStates canvasInfo.id ws

      return ws
    }
