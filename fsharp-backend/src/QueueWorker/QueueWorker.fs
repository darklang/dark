module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

type Instant = NodaTime.Instant

open Prelude
open Prelude.Tablecloth
open Tablecloth
open LibBackend.Db

module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module EQ = LibBackend.EventQueueV2
module TI = LibBackend.TraceInputs
module Execution = LibExecution.Execution
module Pusher = LibBackend.Pusher // FSTODO: we should push events, right?
module RealExecution = LibRealExecution.RealExecution
module Canvas = LibBackend.Canvas
module DvalReprExternal = LibExecution.DvalReprExternal

module LD = LibService.LaunchDarkly
module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let shutdown = ref false

let executeEvent
  (c : Canvas.T)
  (h : PT.Handler.T)
  (traceID : AT.TraceID)
  (e : EQ.T)
  : Task<RT.Dval> =
  task {
    let executionID = Telemetry.executionID ()

    let! (state, traceResult) =
      RealExecution.createState executionID traceID h.tlid (Canvas.toProgram c)

    let symtable = Map.ofList [ ("event", e.value) ]
    let ast = PT2RT.Expr.toRT h.ast
    let! result = Execution.executeHandler state symtable ast
    HashSet.add h.tlid traceResult.tlids
    RealExecution.traceResultHook c.meta.id traceID executionID traceResult
    return result
  }

type ShouldRetry =
  | Retry of int // How many seconds to wait before trying again
  | NoRetry

/// The algorithm here is described in docs/production/eventsV2.md. The algorithm
/// below is annotated with names from chart.
/// Returns the number of events it executed (0 or 1)
let dequeueAndProcess () : Task<int> =
  task {
    use _span = Telemetry.child "dequeueAndProcess" []
    let resultType (dv : RT.Dval) : string =
      dv |> RT.Dval.toType |> DvalReprExternal.typeToDeveloperReprV0

    // Receive Notification - if there's an exception here, we don't have a job so no
    // cleanup required.
    let! notification = EQ.dequeue ()

    // Function used to quit this event
    let stop (reason : string) (retry : ShouldRetry) : Task<int> =
      task {
        print $"stop: {reason} {notification}"
        Telemetry.addTags [ "queue.completion_reason", reason
                            "queue.success", false
                            "queue.retrying", retry <> NoRetry ]
        match retry with
        | Retry delay -> return! EQ.requeueEvent notification delay
        | NoRetry -> return! EQ.acknowledgeEvent notification
        return 0 // no events executed
      }

    let! meta = Canvas.getMetaFromID notification.data.canvasID
    Telemetry.addTags [ "canvas_name", meta.name ]

    // -------
    // EventLoad
    // -------
    match! EQ.loadEvent notification.data.canvasID notification.data.id with
    | None -> return! stop "EventMissing" NoRetry
    | Some event -> // EventPresent
      Telemetry.addTags [ "event.handler.name", event.name
                          "event.handler.modifier", event.modifier
                          "event.handler.module", event.module'
                          "event.retries", event.retries
                          "event.value.type", (event.value |> resultType :> obj)
                          "event.delayUntil", event.delayUntil
                          "event.lockedAt", event.lockedAt
                          "event.enqueuedAt", event.enqueuedAt ]

      // -------
      // DelayCheck
      // -------
      let timeLeft = event.delayUntil - Instant.now ()
      if timeLeft.TotalSeconds > 0 then
        return! stop "DelayNotYet" (Retry(int timeLeft.TotalSeconds))
      else // DelayNone

        // -------
        // LockCheck
        // -------
        let secondsLeft =
          match event.lockedAt with
          | Some lockedAt -> // LockExpired
            let expiryTime = lockedAt.Plus(NodaTime.Duration.FromMinutes 5.0)
            let timeLeft = expiryTime - Instant.now ()
            int timeLeft.TotalSeconds
          | None -> 0 // LockNone
        if secondsLeft > 0 then
          return! stop "IsLocked" (Retry secondsLeft)
        else // LockNone/LockExpired

          // -------
          // RuleCheck
          // -------
          match! EQ.getRule meta.id event with
          | Some rule ->
            // Drop the notification - we'll requeue it if someone unpauses
            Telemetry.addTags [ "queue.rule.type", rule.ruleType
                                "queue.rule.id", rule.id ]
            return! stop $"RuleCheckPaused/Blocked" NoRetry
          | None -> // RuleNone

            // -------
            // LockClaim
            // -------
            match! EQ.claimLock event notification with
            | Error msg -> return! stop $"LockClaimFailed: {msg}" (Retry 5)
            | Ok () -> // LockClaimed

              // -------
              // Process
              // -------
              let traceID = System.Guid.NewGuid()
              let desc = (event.module', event.name, event.modifier)
              let! c =
                Canvas.loadForEventV2 meta event.module' event.name event.modifier
              // CLEANUP switch events and scheduling rules to use TLIDs instead of eventDescs
              let h =
                c.handlers
                |> Map.values
                |> List.filter (fun h ->
                  Some desc = PTParser.Handler.Spec.toEventDesc h.spec)
                |> List.head
              match h with
              | None ->
                // If an event gets put in the queue and there's no handler for it,
                // they're probably emiting to a handler they haven't created yet.
                // In this case, all they need to build is the trace. So just drop
                // this event immediately.
                let! (_ : Instant) = TI.storeEvent meta.id traceID desc event.value
                do! EQ.deleteEvent event
                return! stop "MissingHandler" NoRetry
              | Some h ->

                // If we acknowledge the event here, and the machine goes down, the
                // event is lost forever. So instead give ourselves enough time to
                // run the job and then acknowledge completion after.
                do! EQ.extendDeadline notification

                // FSTODO Set a time limit of 3m

                let! (_ : Instant) = TI.storeEvent c.meta.id traceID desc event.value
                let! result = executeEvent c h traceID event
                Telemetry.addTag "resultType" (resultType result)

                // -------
                // Delete
                // -------
                do! EQ.deleteEvent event
                do! EQ.acknowledgeEvent notification

                // -------
                // End
                // -------
                return 1
  }

let run () : Task<unit> =
  task {
    while not shutdown.Value do
      try
        use _span = Telemetry.createRoot "QueueWorker.run"
        let! count = dequeueAndProcess ()
        if count > 0 then return () else return! Task.Delay 1000
      with
      | e ->
        // No matter where else we catch it, this is essential or else the loop won't
        // continue
        Rollbar.sendException
          (Telemetry.executionID ())
          Rollbar.emptyPerson
          []
          (PageableException("Unhandled exception bubbled to run", [], e))
  }


[<EntryPoint>]
let main _ : int =
  try
    let name = "QueueWorker"
    print "Starting QueueWorker"
    LibService.Init.init name
    Telemetry.Console.loadTelemetry name Telemetry.DontTraceDBQueries
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    (LibRealExecution.Init.init name).Result

    // Called if k8s tells us to stop
    let shutdownCallback () =
      Telemetry.addEvent "shutting down" []
      shutdown.Value <- true

    // Set up healthchecks and shutdown with k8s
    let port = LibService.Config.queueWorkerKubernetesPort
    let healthChecks = [ LibBackend.Init.legacyServerCheck ]
    LibService.Kubernetes.runKubernetesServer name healthChecks port shutdownCallback
    |> ignore<Task>

    if LibBackend.Config.triggerQueueWorkers then
      (run ()).Result
    else
      Telemetry.createRoot "Pointing at prodclone; will not dequeue"
      |> ignore<Telemetry.Span.T>
    LibService.Init.flush name
    0

  with
  | e -> Rollbar.lastDitchBlockAndPage "Error running Queueworker" e
