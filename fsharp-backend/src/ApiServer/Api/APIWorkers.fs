/// API endpoints for Workers
module ApiServer.Workers

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

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
module Telemetry = LibService.Telemetry

module SchedulingRules = LibBackend.QueueSchedulingRules

module WorkerStats =
  type Params = { tlid : tlid }

  type T = { count : int }

  /// API endpoint to get statistical data related to a Worker
  let getStats (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTag "tlid" p.tlid

      t.next "analyse-worker-stats"
      let! result = Stats.workerStats canvasInfo.id p.tlid
      return { count = result }
    }

module Scheduler =
  type Params = { name : string; schedule : string }
  type T = SchedulingRules.WorkerStates.T

  /// API endpoint to update the Schedule of a Worker
  let updateSchedule (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadJsonAsync<Params>()
      Telemetry.addTags [ "name", p.name; "schedule", p.schedule ]

      t.next "schedule-worker"
      match p.schedule with
      | "pause" -> do! SchedulingRules.pauseWorker canvasInfo.id p.name
      | "run" -> do! SchedulingRules.unpauseWorker canvasInfo.id p.name
      | _ -> Exception.raiseEditor "Invalid schedule"


      t.next "get-worker-schedule"
      let! ws = SchedulingRules.getWorkerSchedules canvasInfo.id

      t.next "update-pusher"
      // TODO: perhaps this update should go closer where it happens, in
      // case it doesn't happen in an API call.
      let executionID = loadExecutionID ctx
      LibBackend.Pusher.pushWorkerStates executionID canvasInfo.id ws

      return ws
    }
