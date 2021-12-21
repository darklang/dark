module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module Execution = LibExecution.Execution

module Telemetry = LibService.Telemetry
module Span = Telemetry.Span

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
    LibExecution.Init.init "CronChecker"
    LibExecutionStdLib.Init.init "CronChecker"
    LibBackend.Init.init "CronChecker"
    BackendOnlyStdLib.Init.init "CronChecker"
    LibRealExecution.Init.init "CronChecker"
    LibService.Kubernetes.runKubernetesServer
      "CronChecker"
      LibService.Config.croncheckerKubernetesPort
      (fun () -> shutdown := true)
    if false then
      // LibBackend.Config.triggerQueueWorkers then
      (run ()).Result
    else
      Telemetry.createRoot "Pointing at prodclone; will not trigger crons"
    0
  with
  | e ->
    LibService.Rollbar.lastDitchBlocking
      "Error running CronChecker"
      (ExecutionID "cronchecker")
      []
      e
    -1
