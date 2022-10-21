module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let mutable shouldShutdown = false

let run () : Task<unit> =
  task {
    while not shouldShutdown do
      try
        use (span : Telemetry.Span.T) = Telemetry.createRoot "CronChecker.run"
        do! LibBackend.Cron.checkAndScheduleWorkForAllCrons ()
      with
      | e ->
        // If there's an exception, alert and continue
        Rollbar.sendException None [] e
      do! Task.Delay LibBackend.Config.pauseBetweenCronsInMs
    return ()
  }


[<EntryPoint>]
let main _ : int =
  try
    let name = "CronChecker"
    print "Starting CronChecker"
    Prelude.init ()
    LibService.Init.init name
    LibExecution.Init.init ()
    Telemetry.Console.loadTelemetry name Telemetry.DontTraceDBQueries
    //? ClientTypes.Init.init name
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result

    // This fn is called if k8s tells us to stop
    let shutdownCallback () =
      Telemetry.addEvent "Shutting down" []
      shouldShutdown <- true

    // Set up healthchecks and shutdown with k8s
    let port = LibService.Config.croncheckerKubernetesPort
    LibService.Kubernetes.runKubernetesServer name [] port shutdownCallback
    |> ignore<Task>

    if LibBackend.Config.triggerCrons then
      (run ()).Result
    else
      Telemetry.addEvent "Pointing at prodclone; will not trigger crons" []
    LibService.Init.shutdown name
    0
  with
  | e -> Rollbar.lastDitchBlockAndPage "Error starting cronchecker" e
