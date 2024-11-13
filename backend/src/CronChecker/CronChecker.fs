module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let mutable shouldShutdown = false

let run () : Task<unit> =
  task {
    while not shouldShutdown do
      try
        use (_span : Telemetry.Span.T) = Telemetry.createRoot "CronChecker.run"
        do! LibCloud.Cron.checkAndScheduleWorkForAllCrons ()
        ()
      with e ->
        // If there's an exception, alert and continue
        Rollbar.sendException None [] e
      do! Task.Delay LibCloud.Config.pauseBetweenCronsInMs
    return ()
  }

let initSerializers () =
  Json.Vanilla.allow<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
    "RoundtrippableSerializationFormatV0.Dval"
  Json.Vanilla.allow<LibCloud.Queue.NotificationData> "eventqueue storage"
  Json.Vanilla.allow<LibService.Rollbar.HoneycombJson> "Rollbar"

[<EntryPoint>]
let main _ : int =
  try
    let name = "CronChecker"
    print "Starting CronChecker"
    initSerializers ()
    LibService.Init.init name
    Telemetry.Console.loadTelemetry name Telemetry.DontTraceDBQueries
    (LibCloud.Init.init LibCloud.Init.WaitForDB name).Result

    // This fn is called if k8s tells us to stop
    let shutdownCallback () =
      Telemetry.addEvent "Shutting down" []
      shouldShutdown <- true

    // Set up healthchecks and shutdown with k8s
    let port = LibService.Config.croncheckerKubernetesPort
    let healthchecks = []
    LibService.Kubernetes.runKubernetesServer name healthchecks port shutdownCallback
    |> ignore<Task>

    if LibCloud.Config.triggerCrons then
      (run ()).Result
    else
      Telemetry.addEvent "Pointing at prodclone; will not trigger crons" []
    LibService.Init.shutdown name
    0
  with e ->
    Rollbar.lastDitchBlockAndPage "Error starting cronchecker" e
