module ApiServer.Workers

// API endpoints for Workers

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
module EQ = LibBackend.EventQueue

module WorkerStats =
  type Params = { tlid : tlid }

  type T = { count : int }

  let getStats (ctx : HttpContext) : Task<T> =
    task {
      let t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()

      t.next "analyse-worker-stats"
      let! result = Stats.workerStats canvasInfo.id p.tlid

      t.stop ()
      return { count = result }
    }

module Scheduler =
  type Params = { name : string; schedule : string }
  type T = EQ.WorkerStates.T

  let updateSchedule (ctx : HttpContext) : Task<T> =
    task {
      let t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()

      t.next "schedule-worker"
      match p.schedule with
      | "pause" -> do! EQ.pauseWorker canvasInfo.id p.name
      | "run" -> do! EQ.unpauseWorker canvasInfo.id p.name
      | _ -> failwith "Invalid schedule"


      t.next "get-worker-schedule"
      let! ws = EQ.getWorkerSchedules canvasInfo.id

      t.next "update-pusher"
      // TODO: perhaps this update should go closer where it happens, in
      // case it doesn't happen in an API call.
      let executionID = loadExecutionID ctx
      LibBackend.Pusher.pushWorkerStates executionID canvasInfo.id ws

      t.stop ()
      return ws
    }
