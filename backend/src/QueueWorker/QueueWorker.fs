module QueueWorker

open System.Threading.Tasks
open FSharp.Control.Tasks

type Instant = NodaTime.Instant

open Prelude

module PTParser = LibExecution.ProgramTypesParser
module AT = LibExecution.AnalysisTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module EQ = LibCloud.Queue
module Pusher = LibCloud.Pusher
module CloudExecution = LibCloudExecution.CloudExecution
module Canvas = LibCloud.Canvas
//module DvalReprDeveloper = LibExecution.DvalReprDeveloper

module LD = LibService.LaunchDarkly
module Telemetry = LibService.Telemetry
module Rollbar = LibService.Rollbar

module CTPusher = LibClientTypes.Pusher

open LibCloud.Db


let mutable shouldShutdown = false

type ShouldRetry =
  | Retry of NodaTime.Duration
  | NoRetry


// /// The algorithm here is described in the chart in `docs/eventsV2.md`.
// /// The code below is annotated with names from chart.
// ///
// /// Notes:
// /// - `dequeueAndProcess` will block until it receives a notification.
// /// - Returns a Result containing the notification and the
// ///   event on success, and just the notification and failure reason on failure.
// /// - Should not throw on error.
// let processNotification
//   (notification : EQ.Notification)
//   : Task<Result<EQ.T * EQ.Notification, string * EQ.Notification>> =
//   task {
//     use _span = Telemetry.createRoot "process"
//     Telemetry.addTags
//       [ "event.time_in_queue_ms", notification.timeInQueue.TotalMilliseconds
//         "event.id", notification.data.id
//         "event.canvas_id", notification.data.canvasID
//         "event.delivery_attempt", notification.deliveryAttempt
//         "event.pubsub.ack_id", notification.pubSubAckID
//         "event.pubsub.message_id", notification.pubSubMessageID ]

//     // Function used to quit this event
//     let stop
//       (reason : string)
//       (retry : ShouldRetry)
//       : Task<Result<_, string * EQ.Notification>> =
//       task {
//         Telemetry.addTags
//           [ "queue.completion_reason", reason
//             "queue.success", false
//             "queue.retrying", retry <> NoRetry ]
//         match retry with
//         | Retry delay -> return! EQ.requeueEvent notification delay
//         | NoRetry -> return! EQ.acknowledgeEvent notification
//         return Error(reason, notification) // no events executed
//       }

//     // -------
//     // EventLoad
//     // -------
//     match! EQ.loadEvent notification.data.canvasID notification.data.id with
//     | None -> return! stop "EventMissing" NoRetry
//     | Some event -> // EventPresent
//       Telemetry.addTags
//         [ "event.handler.name", event.name
//           "event.handler.modifier", event.modifier
//           "event.handler.module", event.module'
//           "event.value.type", (event.value |> DvalReprDeveloper.toTypeName :> obj)
//           "event.locked_at", event.lockedAt
//           "event.enqueued_at", event.enqueuedAt ]


//       // -------
//       // LockCheck
//       // -------
//       let timeLeft =
//         match event.lockedAt with
//         | Some lockedAt -> // LockExpired
//           let expiryTime = lockedAt.Plus(NodaTime.Duration.FromMinutes 5.0)
//           // Date math is hard so let's spell it out. `timeLeft` measures how long is
//           // left until the lock expires. If there is time left until the lock
//           // expires, `timeLeft` is positive. So
//           //
//           // `timeLeft = expiryTime - now`
//           //
//           // as that way there is positive `timeLeft` if `expiryTime` is later than
//           // `now`.
//           expiryTime - Instant.now ()
//         | None -> NodaTime.Duration.FromSeconds 0.0 // LockNone

//       if timeLeft.TotalSeconds > 0 then
//         // RETRY but it means something else is running it so doesn't matter
//         return! stop "IsLocked" (Retry timeLeft)
//       else // LockNone/LockExpired

//         // -------
//         // RuleCheck
//         // -------
//         match! EQ.getRule notification.data.canvasID event with
//         | Some rule ->
//           // Drop the notification - we'll requeue it if someone unpauses
//           Telemetry.addTags
//             [ "queue.rule.type", rule.ruleType; "queue.rule.id", rule.id ]
//           return! stop "RuleCheckPaused/Blocked" NoRetry
//         | None -> // RuleNone
//           // -------
//           // DeliveryCheck
//           // Note that this happens after all the other checks, as we might have
//           // multiple notifications for the same event and we don't want to delete
//           // one that is being executed or isn't ready. We stop after 4 retries here
//           // because the retries might happen for a reason that isn't strictly
//           // retries, such as lockedAt.
//           // -------
//           if notification.deliveryAttempt >= 5 then
//             // DeliveryTooManyRetries
//             do! EQ.deleteEvent event
//             return! stop "DeliveryTooMany" NoRetry
//           else // DeliveryPermitted

//             // -------
//             // LockClaim
//             // -------
//             match! EQ.claimLock event notification with
//             | Error msg ->
//               // Someone else just claimed the lock!
//               let retryTime = NodaTime.Duration.FromSeconds 300.0
//               return! stop $"LockClaimFailed: {msg}" (Retry retryTime)
//             | Ok() -> // LockClaimed

//               // -------
//               // Process
//               // -------
//               let! canvas =
//                 Exception.taskCatch (fun () ->
//                   task {
//                     return!
//                       Canvas.loadForEvent
//                         notification.data.canvasID
//                         event.module'
//                         event.name
//                         event.modifier
//                   })
//               match canvas with
//               | None ->
//                 do! EQ.deleteEvent event
//                 return! stop "MissingCanvas" NoRetry
//               | Some c ->
//                 let traceID = AT.TraceID.create ()
//                 let desc = (event.module', event.name, event.modifier)
//                 Telemetry.addTags [ "canvasID", c.id; "trace_id", traceID ]


//                 // CLEANUP switch events and scheduling rules to use TLIDs instead of eventDescs
//                 let h =
//                   c.handlers
//                   |> Map.values
//                   |> List.filter (fun h ->
//                     Some desc = PTParser.Handler.Spec.toEventDesc h.spec)
//                   |> List.head

//                 match h with
//                 | None ->
//                   // If an event gets put in the queue and there's no handler for it,
//                   // they're probably emiting to a handler they haven't created yet.
//                   // In this case, all they need to build is the trace. So just drop
//                   // this event immediately.

//                   // TODO: reenable using CloudStorage
//                   // let! timestamp = TI.storeEvent c.id traceID desc event.value
//                   // Pusher.push
//                   //   LibClientTypesToCloudTypes.Pusher.eventSerializer
//                   //   c.id
//                   //   (Pusher.New404(
//                   //     event.module',
//                   //     event.name,
//                   //     event.modifier,
//                   //     timestamp,
//                   //     traceID
//                   //   ))
//                   //   None

//                   do! EQ.deleteEvent event
//                   return! stop "MissingHandler" NoRetry
//                 | Some h ->

//                   // If we acknowledge the event here, and the machine goes down,
//                   // PubSub will retry this once the ack deadline runs out
//                   do! EQ.extendDeadline notification

//                   // CLEANUP Set a time limit of 3m
//                   try
//                     let! program = Canvas.toProgram c
//                     let! (result, traceResults) =
//                       CloudExecution.executeHandler
//                         LibClientTypesToCloudTypes.Pusher.eventSerializer
//                         (PT2RT.Handler.toRT h)
//                         program
//                         traceID
//                         (Map [ "event", event.value ])
//                         (CloudExecution.InitialExecution(
//                           EQ.toEventDesc event,
//                           "event",
//                           event.value
//                         ))

//                     Telemetry.addTags
//                       [ "result_type", DvalReprDeveloper.toTypeName result
//                         "queue.success", true
//                         "executed_tlids", HashSet.toList traceResults.tlids
//                         "queue.completion_reason", "completed" ]
//                     // ExecutesToCompletion

//                     // -------
//                     // Delete
//                     // -------
//                     do! EQ.deleteEvent event
//                     do! EQ.acknowledgeEvent notification

//                     // -------
//                     // End
//                     // -------
//                     return Ok(event, notification)
//                   with _ ->
//                     // This automatically increments the deliveryAttempt, so it might
//                     // be deleted at the next iteration.
//                     let timeLeft = NodaTime.Duration.FromSeconds 301.0
//                     return! stop "RetryAllowed" (Retry timeLeft)
//   }

/// Run in the background, using the semaphore to track completion
let runInBackground
  (semaphore : System.Threading.SemaphoreSlim)
  //(notification : EQ.Notification)
  : unit =
  // Ensure we get a lock before the background task starts. We should always get a
  // lock here, but if something goes awry it's better that we wait rather than fetch
  // more events to run.
  semaphore.Wait()
  backgroundTask {
    try
      //let! (_ : Result<_, _>) = processNotification notification
      return ()
    finally
      semaphore.Release() |> ignore<int>
  }
  |> ignore<Task<unit>>

let run () : Task<unit> =
  task {
    use _span = Telemetry.createRoot "dequeueAndProcess"
    // Our goal here is to concurrently run a number of events dictated by the amount
    // of memory and CPU available, adding more until we hit a threshold.
    // Unfortunately, I think that will involve talking to /proc, which is doable but
    // some work. We also want to limit this with a feature flag to we don't overdo
    // it. We use a semaphore to count the number in use - it's updated
    // automatically when an event finishes.

    // We use a high number because we don't actually know the number of slots: it's
    // decided somewhat dynamically by a feature flag. So just pick a high number,
    // and then use the semaphore to count the events in progress.
    let initialCount = 100000 // just be a high number
    let _semaphore = new System.Threading.SemaphoreSlim(initialCount)

    let _maxEventsFn = LD.queueMaxConcurrentEventsPerWorker
    while not shouldShutdown do
      let _timeout = System.TimeSpan.FromSeconds 5
      try
        // // TODO: include memory and CPU usage checks in here
        // let runningCount = initialCount - semaphore.CurrentCount
        // let remainingSlots = maxEventsFn () - runningCount
        // if remainingSlots > 0 then
        //   // let! notifications = EQ.dequeue timeout remainingSlots
        //   // if notifications = [] then
        //   //   do! Task.Delay(LD.queueDelayBetweenPullsInMillis ())
        //   // else
        //     List.iter (runInBackground semaphore) //notifications
        // else
        do! Task.Delay(LD.queueDelayBetweenPullsInMillis ())

      with e ->
        // No matter where else we catch it, this is essential or else the loop won't
        // continue
        let e =
          (Exception.PageableException("Unhandled exception bubbled to run", [], e))
        Rollbar.sendException None [] e
  }


// Generally speaking, this should be the same list as BwdServer's
let initSerializers () =
  // universally-serializable types

  // one-off types used internally
  // Json.Vanilla.allow<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
  //   "RoundtrippableSerializationFormatV0.Dval"
  Json.Vanilla.allow<LibExecution.ProgramTypes.Toplevel.T> "Canvas.loadJsonFromDisk"
  //Json.Vanilla.allow<LibCloud.Queue.NotificationData> "eventqueue storage"
  // Json.Vanilla.allow<LibCloud.TraceCloudStorage.CloudStorageFormat>
  //   "TraceCloudStorageFormat"
  Json.Vanilla.allow<LibService.Rollbar.HoneycombJson> "Rollbar"

  // for Pusher.com payloads
  Json.Vanilla.allow<CTPusher.Payload.NewTrace> "Pusher"
  Json.Vanilla.allow<CTPusher.Payload.New404> "Pusher"
// Json.Vanilla.allow<CTPusher.Payload.AddOpV1> "Pusher"
// Json.Vanilla.allow<CTPusher.Payload.AddOpV1PayloadTooBig> "Pusher" // this is so-far unused
// Json.Vanilla.allow<CTPusher.Payload.UpdateWorkerStates> "Pusher"


[<EntryPoint>]
let main _ : int =
  try
    let name = "QueueWorker"
    printTime "Starting QueueWorker"
    initSerializers ()
    LibService.Init.init name
    Telemetry.Console.loadTelemetry name Telemetry.TraceDBQueries
    (LibCloud.Init.init LibCloud.Init.WaitForDB name).Result
    (LibCloudExecution.Init.init name).Result

    // Called if k8s tells us to stop
    let shutdownCallback () =
      Telemetry.addEvent "shutting down" []
      shouldShutdown <- true

    // Set up healthchecks and shutdown with k8s
    let port = LibService.Config.queueWorkerKubernetesPort
    let healthChecks = [] //Canvas.healthCheck ]
    LibService.Kubernetes.runKubernetesServer name healthChecks port shutdownCallback
    |> ignore<Task>

    if LibCloud.Config.triggerQueueWorkers then
      (run ()).Result
    else
      Telemetry.createRoot "Pointing at prodclone; will not dequeue"
      |> ignore<Telemetry.Span.T>

    LibService.Init.shutdown name
    0

  with e ->
    Rollbar.lastDitchBlockAndPage "Error running Queueworker" e
