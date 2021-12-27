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
    while not shutdown.Value do
      use span = Telemetry.createRoot "CronChecker.run"
      do! LibBackend.Cron.checkAndScheduleWorkForAllCrons ()
      do! Task.Delay 1000
    return ()
  }


[<EntryPoint>]
let main _ : int =
  try
    print "Starting CronChecker"
    LibService.Init.init "CronChecker"
    LibExecution.Init.init "CronChecker"
    LibExecutionStdLib.Init.init "CronChecker"
    LibBackend.Init.init "CronChecker"
    BackendOnlyStdLib.Init.init "CronChecker"
    LibRealExecution.Init.init "CronChecker"

    Telemetry.Console.loadTelemetry "CronChecker" Telemetry.DontTraceDBQueries
    // we need to stop running if we're told to stop by k8s
    LibService.Kubernetes.runKubernetesServer
      "CronChecker"
      LibService.Config.croncheckerKubernetesPort
      (fun () ->
        Telemetry.addEvent "Shutting down" []
        shutdown.Value <- true)
    |> ignore<Task>

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
