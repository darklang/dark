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
        use (_span : Telemetry.Span.T) = Telemetry.createRoot "CronChecker.run"
        do! LibBackend.Cron.checkAndScheduleWorkForAllCrons ()
      with e ->
        // If there's an exception, alert and continue
        Rollbar.sendException None [] e
      do! Task.Delay LibBackend.Config.pauseBetweenCronsInMs
    return ()
  }

let initSerializers () =
  Json.Vanilla.allow<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
    "RoundtrippableSerializationFormatV0.Dval"
  Json.Vanilla.allow<LibBackend.Queue.NotificationData> "eventqueue storage"
  Json.Vanilla.allow<LibService.Rollbar.HoneycombJson> "Rollbar"

[<EntryPoint>]
let main _ : int =
  try
    let name = "CronChecker"
    print "Starting CronChecker"
    initSerializers ()
    LibService.Init.init name
    Telemetry.Console.loadTelemetry name Telemetry.DontTraceDBQueries
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result

    // This fn is called if k8s tells us to stop
    let shutdownCallback () =
      Telemetry.addEvent "Shutting down" []
      shouldShutdown <- true

    // Set up healthchecks and shutdown with k8s
    let port = LibService.Config.croncheckerKubernetesPort
    let healthchecks = []
    LibService.Kubernetes.runKubernetesServer name healthchecks port shutdownCallback
    |> ignore<Task>

    if LibBackend.Config.triggerCrons then
      (run ()).Result
    else
      Telemetry.addEvent "Pointing at prodclone; will not trigger crons" []
    LibService.Init.shutdown name
    0
  with e ->
    Rollbar.lastDitchBlockAndPage "Error starting cronchecker" e
