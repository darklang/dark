module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry

let shutdown = ref false

let run () : Task<unit> =
  task {
    Telemetry.createRoot "CronChecker.run"
    while not shutdown.Value do
      do! LibBackend.Cron.checkAndScheduleWorkForAllCrons ()
      do! Task.Delay 1000
      return ()
  }


[<EntryPoint>]
let main _ : int =
  try
    print "Starting CronChecker"
    LibService.Init.init "CronChecker"
    LibService.Telemetry.Console.loadTelemetry "CronChecker"
    LibExecution.Init.init "CronChecker"
    LibExecutionStdLib.Init.init "CronChecker"
    LibBackend.Init.init "CronChecker"
    BackendOnlyStdLib.Init.init "CronChecker"
    LibRealExecution.Init.init "CronChecker"
    // StopTaking things
    LibService.Kubernetes.runKubernetesServer
      "CronChecker"
      LibService.Config.croncheckerKubernetesPort
      (fun () -> shutdown.Value <- true)
    if false then
      // LibBackend.Config.triggerQueueWorkers then
      (run ()).Result
    else
      Telemetry.addEvent "Pointing at prodclone; will not trigger crons" []
    0
  with
  | e ->
    LibService.Rollbar.lastDitchBlocking
      "Error running CronChecker"
      (ExecutionID "cronchecker")
      []
      e
    -1
