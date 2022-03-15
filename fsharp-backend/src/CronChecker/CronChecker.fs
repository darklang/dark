module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let shouldShutdown = ref false

let run () : Task<unit> =
  task {
    while not shouldShutdown.Value do
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

    // This fn is called if k8s tells us to stop
    let shutdownCallback () =
      Telemetry.addEvent "Shutting down" []
      shouldShutdown.Value <- true

    LibService.Kubernetes.runKubernetesServer
      "CronChecker"
      []
      LibService.Config.croncheckerKubernetesPort
      shutdownCallback
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
  | e -> Rollbar.lastDitchBlockAndPage "Error starting cronchecker" e
