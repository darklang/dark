module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let shutdown = ref false

let run () : Task<unit> =
  task {
    while not shutdown.Value do
      try
        use span = Telemetry.createRoot "CronChecker.run"
        do! LibBackend.Cron.checkAndScheduleWorkForAllCrons ()
      with
      | e ->
        // If there's an exception, alert and continue
        Rollbar.sendException (ExecutionID "cronchecker") Rollbar.emptyPerson [] e
      do! Task.Delay LibBackend.Config.pauseBetweenCronsInMs
    return ()
  }


[<EntryPoint>]
let main _ : int =
  try
    print "Starting CronChecker"
    LibService.Init.init "CronChecker"
    LibExecution.Init.init "CronChecker"
    LibExecutionStdLib.Init.init "CronChecker"
    (LibBackend.Init.init "CronChecker" false).Result
    LibRealExecution.Init.init "CronChecker"

    Telemetry.Console.loadTelemetry "CronChecker" Telemetry.DontTraceDBQueries

    // we need to stop running if we're told to stop by k8s
    LibService.Kubernetes.runKubernetesServer
      "CronChecker"
      []
      LibService.Config.croncheckerKubernetesPort
      (fun () ->
        Telemetry.addEvent "Shutting down" []
        shutdown.Value <- true)
    |> ignore<Task>


    // Don't start until the DB is available. Otherwise we'll just spin off
    // exceptions in a loop.
    LibBackend.Init.waitForDB().Result

    if LibBackend.Config.triggerCrons then
      (run ()).Result
    else
      Telemetry.addEvent "Pointing at prodclone; will not trigger crons" []
    0
  with
  | e -> LibService.Rollbar.lastDitchBlockAndPage "Error starting cronchecker" e
