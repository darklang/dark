module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry

let shutdown = ref false

let run () : Task<unit> =
  Telemetry.addEvent "called run" []
  task {
    Telemetry.addEvent "start task" [ "shutdown", shutdown.Value ]
    use span = Telemetry.child "CronChecker.run" []
    while not shutdown.Value do
      Telemetry.addEvent "running checkAndSchedule" [ "shutdown", shutdown.Value ]
      do! LibBackend.Cron.checkAndScheduleWorkForAllCrons ()
      Telemetry.addEvent "finished checkAndSchedule" [ "shutdown", shutdown.Value ]
      do! Task.Delay 1000
      Telemetry.addEvent "delay finished" [ "shutdown", shutdown.Value ]
    return ()
  }


[<EntryPoint>]
let main _ : int =
  try
    print "Starting CronChecker"
    LibService.Init.init "CronChecker"
    Telemetry.Console.loadTelemetry "CronChecker"
    LibExecution.Init.init "CronChecker"
    LibExecutionStdLib.Init.init "CronChecker"
    LibBackend.Init.init "CronChecker"
    BackendOnlyStdLib.Init.init "CronChecker"
    LibRealExecution.Init.init "CronChecker"
    // StopTaking things
    LibService.Kubernetes.runKubernetesServer
      "CronChecker"
      LibService.Config.croncheckerKubernetesPort
      (fun () ->
        Telemetry.addEvent "Shutting down" []
        shutdown.Value <- true)
    if true then // FSTODO for now enable everywhere and do the trigger check much deeper
      // LibBackend.Config.triggerCrons then
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
