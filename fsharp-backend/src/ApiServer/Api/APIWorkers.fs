/// API endpoints for Workers
module ApiServer.Workers

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module Stats = LibBackend.Stats
module EQ = LibBackend.EventQueueV2
module SchedulingRules = LibBackend.QueueSchedulingRules
module Telemetry = LibService.Telemetry

module CTApi = ClientTypes.Api

module WorkerStats =
  /// API endpoint to get statistical data related to a Worker
  let getStats (ctx : HttpContext) : Task<CTApi.Workers.WorkerStats.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<CTApi.Workers.WorkerStats.Request>()
      Telemetry.addTag "tlid" p.tlid

      t.next "analyse-worker-stats"
      let! result = Stats.workerStats canvasInfo.id p.tlid
      return { count = result }
    }

module Scheduler =
  /// API endpoint to update the Schedule of a Worker
  let updateSchedule (ctx : HttpContext) : Task<CTApi.Workers.Scheduler.Response> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx
      let! p = ctx.ReadVanillaJsonAsync<CTApi.Workers.Scheduler.Request>()
      Telemetry.addTags [ "name", p.name; "schedule", p.schedule ]

      t.next "schedule-worker"
      match p.schedule with
      | "pause" -> do! EQ.pauseWorker canvasInfo.id p.name
      | "run" -> do! EQ.unpauseWorker canvasInfo.id p.name
      | _ -> Exception.raiseEditor "Invalid schedule"


      t.next "get-worker-schedule"
      let! ws = SchedulingRules.getWorkerSchedules canvasInfo.id

      t.next "update-pusher"
      // TODO: perhaps this update should go closer where it happens, in
      // case it doesn't happen in an API call.
      LibBackend.Pusher.push
        ClientTypes2BackendTypes.Pusher.eventSerializer
        canvasInfo.id
        (LibBackend.Pusher.UpdateWorkerStates ws)
        None

      let response : CTApi.Workers.Scheduler.Response =
        ClientTypes2BackendTypes.Worker.WorkerStates.toCT ws

      return response
    }
