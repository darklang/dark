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
module Pusher = LibBackend.Pusher
module RealExecution = LibRealExecution.RealExecution
module Canvas = LibBackend.Canvas
module DvalReprExternal = LibExecution.DvalReprExternal

module LD = LibService.LaunchDarkly
module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

let mutable shouldShutdown = false

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
  | Retry of NodaTime.Duration
  | NoRetry

let mutable cpuUsage : float = 0.0
let mutable memoryUsage : int64 = 0L

/// Background thread tracking current CPU and memory usage. We intend to use this to
/// decide whether to schedule more workers, but for now let's just track what the
/// numbers say
let cpuThread : unit =
  let proc = System.Diagnostics.Process.GetCurrentProcess()
  let threadFunc () =
    while not shouldShutdown do
      // Measure CPU usage over a time period
      // From https://medium.com/@jackwild/getting-cpu-usage-in-net-core-7ef825831b8b
      let startTime = System.DateTime.UtcNow
      let startCpuUsage = proc.TotalProcessorTime

      System.Threading.Thread.Sleep 1000

      let endTime = System.DateTime.UtcNow
      let endCpuUsage = proc.TotalProcessorTime
      let cpuUsedMs = (endCpuUsage - startCpuUsage).TotalMilliseconds
      let totalMsPassed = (endTime - startTime).TotalMilliseconds
      let cpuUsageTotal =
        cpuUsedMs / (float System.Environment.ProcessorCount * totalMsPassed)
      cpuUsage <- cpuUsageTotal * 100.0
      memoryUsage <- proc.PrivateMemorySize64
  let thread = System.Threading.Thread(System.Threading.ThreadStart(threadFunc))
  thread.IsBackground <- true
  thread.Start()



/// The algorithm here is described in the chart in docs/eventsV2.md. The algorithm
/// below is annotated with names from chart. `dequeueAndProcess` will block until it
/// receives a notification. Returns a Result containing the notification and the
/// event on success, and just the notification and failure reason on failure. Should
/// not throw on error.
let processNotification
  (notification : EQ.Notification)
  : Task<Result<EQ.T * EQ.Notification, string * EQ.Notification>> =
  task {
    use _span = Telemetry.createRoot "process"
    Telemetry.addTags [ "process.cpu.percentage", cpuUsage
                        "process.memory.private_bytes", memoryUsage ]
    let resultType (dv : RT.Dval) : string =
      dv |> RT.Dval.toType |> DvalReprExternal.typeToDeveloperReprV0

    // Function used to quit this event
    let stop
      (reason : string)
      (retry : ShouldRetry)
      : Task<Result<_, string * EQ.Notification>> =
      task {
        Telemetry.addTags [ "queue.completion_reason", reason
                            "queue.success", false
                            "queue.retrying", retry <> NoRetry ]
        match retry with
        | Retry delay -> return! EQ.requeueEvent notification delay
        | NoRetry -> return! EQ.acknowledgeEvent notification
        return Error(reason, notification) // no events executed
      }

    // -------
    // EventLoad
    // -------
    match! EQ.loadEvent notification.data.canvasID notification.data.id with
    | None -> return! stop "EventMissing" NoRetry
    | Some event -> // EventPresent
      Telemetry.addTags [ "event.handler.name", event.name
                          "event.handler.modifier", event.modifier
                          "event.handler.module", event.module'
                          "event.value.type", (event.value |> resultType :> obj)
                          "event.locked_at", event.lockedAt
                          "event.enqueued_at", event.enqueuedAt ]

      // -------
      // LockCheck
      // -------
      let timeLeft =
        match event.lockedAt with
        | Some lockedAt -> // LockExpired
          let expiryTime = lockedAt.Plus(NodaTime.Duration.FromMinutes 5.0)
          // Date math is hard so let's spell it out. `timeLeft` measures how long is
          // left until the lock expires. If there is time left until the lock
          // expires, `timeLeft` is positive. So
          //
          // `timeLeft = expiryTime - now`
          //
          // as that way there is positive `timeLeft` if `expiryTime` is later than
          // `now`.
          expiryTime - Instant.now ()
        | None -> NodaTime.Duration.FromSeconds 0.0 // LockNone
      if timeLeft.TotalSeconds > 0 then
        // RETRY but it means something else is running it so doesn't matter
        return! stop "IsLocked" (Retry timeLeft)
      else // LockNone/LockExpired

        // -------
        // RuleCheck
        // -------
        match! EQ.getRule notification.data.canvasID event with
        | Some rule ->
          // Drop the notification - we'll requeue it if someone unpauses
          Telemetry.addTags [ "queue.rule.type", rule.ruleType
                              "queue.rule.id", rule.id ]
          return! stop "RuleCheckPaused/Blocked" NoRetry
        | None -> // RuleNone

          // -------
          // DeliveryCheck
          // Note that this happens after all the other checks, as we might have
          // multiple notifications for the same event and we don't want to delete
          // one that is being executed or isn't ready. We stop after 4 retries here
          // because the retries might happen for a reason that isn't strictly
          // retries, such as lockedAt.
          // -------
          if notification.deliveryAttempt >= 5 then
            // DeliveryTooManyRetries
            do! EQ.deleteEvent event
            return! stop "DeliveryTooMany" NoRetry
          else // DeliveryPermitted

            // -------
            // LockClaim
            // -------
            match! EQ.claimLock event notification with
            | Error msg ->
              // Someone else just claimed the lock!
              let retryTime = NodaTime.Duration.FromSeconds 300.0
              return! stop $"LockClaimFailed: {msg}" (Retry retryTime)
            | Ok () -> // LockClaimed

              // -------
              // Process
              // -------
              let! canvas =
                Exception.taskCatch (fun () ->
                  task {
                    let! meta = Canvas.getMetaFromID notification.data.canvasID
                    return!
                      Canvas.loadForEventV2
                        meta
                        event.module'
                        event.name
                        event.modifier
                  })
              match canvas with
              | None ->
                do! EQ.deleteEvent event
                return! stop "MissingCanvas" NoRetry
              | Some c ->
                let traceID = System.Guid.NewGuid()
                let desc = (event.module', event.name, event.modifier)
                Telemetry.addTags [ "canvas_name", c.meta.name; "trace_id", traceID ]


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
                  let! timestamp = TI.storeEvent c.meta.id traceID desc event.value
                  Pusher.pushNew404
                    (Telemetry.executionID ())
                    c.meta.id
                    (event.module', event.name, event.modifier, timestamp, traceID)
                  do! EQ.deleteEvent event
                  return! stop "MissingHandler" NoRetry
                | Some h ->

                  // If we acknowledge the event here, and the machine goes down,
                  // PubSub will retry this once the ack deadline runs out
                  do! EQ.extendDeadline notification

                  // CLEANUP Set a time limit of 3m
                  try
                    let! (_ : Instant) =
                      TI.storeEvent c.meta.id traceID desc event.value
                    let! result = executeEvent c h traceID event
                    Telemetry.addTags [ "result_type", resultType result
                                        "queue.success", true
                                        "queue.completion_reason", "completed" ]
                    // ExecutesToCompletion

                    // -------
                    // Delete
                    // -------
                    do! EQ.deleteEvent event
                    do! EQ.acknowledgeEvent notification

                    // -------
                    // End
                    // -------
                    return Ok(event, notification)
                  with
                  | _ ->
                    // This automatically increments the deliveryAttempt, so it might
                    // be deleted at the next iteration.
                    let timeLeft = NodaTime.Duration.FromSeconds 301.0
                    return! stop "RetryAllowed" (Retry timeLeft)
  }

/// Run in the background, using the semaphore to track completion
let runInBackground
  (semaphore : System.Threading.SemaphoreSlim)
  (notification : EQ.Notification)
  : unit =
  backgroundTask {
    try
      let! (_ : Result<_, _>) = processNotification notification
      return ()
    finally
      semaphore.Release() |> ignore<int>
  }
  |> ignore<Task<unit>>

let run () : Task<unit> =
  task {
    use _span = Telemetry.createRoot "dequeueAndProcess"

    // Our goal here is to concurrently run a number of events dictated by the amount
    // of memory and CPU available, adding more until we hit a threshold. Furthermore
    // we want to limit this with a feature flag to we don't overdo it.  We use a
    // semaphore to count the number in use - it's updated automatically when an
    // event finishes.
    let initialCount = 10000 // just be a high number
    let semaphore = new System.Threading.SemaphoreSlim(initialCount)


    // Only use the semaphore to count the threads that are done
    let maxEventsFn = LD.queueMaxConcurrentEventsPerWorker
    while not shouldShutdown do
      try
        if initialCount - semaphore.CurrentCount >= maxEventsFn () then
          do! Task.Delay(LD.queueDelayBetweenPullsInMillis ())
        else
          // FSTODO: include memory and CPU usage checks in here
          match! EQ.dequeue () with
          | Some notification ->
            // We claim a semaphore here instead of in the background thread, as
            // otherwise we might miscount if it hasn't started yet by the next
            // iteration
            do! semaphore.WaitAsync()
            runInBackground semaphore notification
          | None -> do! Task.Delay(LD.queueDelayBetweenPullsInMillis ())
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
    Telemetry.Console.loadTelemetry name Telemetry.TraceDBQueries
    (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    (LibRealExecution.Init.init name).Result

    // Called if k8s tells us to stop
    let shutdownCallback () =
      Telemetry.addEvent "shutting down" []
      shouldShutdown <- true

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

    (LibBackend.Init.shutdown name).Result
    LibService.Init.shutdown name
    0

  with
  | e -> Rollbar.lastDitchBlockAndPage "Error running Queueworker" e
