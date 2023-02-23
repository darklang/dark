/// QueueWorkers (including Crons). See [the docs](/docs/eventsV2.md).
module LibBackend.EventQueueV2

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Google.Cloud.PubSub.V1
open Grpc.Auth

type Instant = NodaTime.Instant

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry

module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module SchedulingRules = QueueSchedulingRules

module TI = TraceInputs

module LD = LibService.LaunchDarkly

/// -----------------
/// Types
/// -----------------

type EventID = System.Guid

/// Notifications are sent by PubSub to say that now would be a good time to try this
/// event. We only load events in response to notifications.

type NotificationData = { id : EventID; canvasID : CanvasID }

type Notification =
  { data : NotificationData
    pubSubMessageID : string
    /// The first delivery has value 1
    deliveryAttempt : int
    timeInQueue : System.TimeSpan
    pubSubAckID : string }

/// Events are stored in the DB and are the source of truth for when and how an event
/// should be executed. When they are complete, they are deleted.
type T =
  { id : EventID
    canvasID : CanvasID
    module' : string
    name : string
    modifier : string
    value : RT.Dval
    lockedAt : Option<Instant>
    enqueuedAt : Instant }

let toEventDesc t = (t.module', t.name, t.modifier)

/// -----------------
/// Database
/// The events_v2 is the source of truth for the queue
/// -----------------

let createEvent
  (canvasID : CanvasID)
  (id : EventID)
  (module' : string)
  (name : string)
  (modifier : string)
  (value : RT.Dval)
  : Task<unit> =
  Sql.query
    "INSERT INTO events_v2
       (id, canvas_id, module, name, modifier, value,
        enqueued_at, locked_at)
     VALUES
       (@id, @canvasID, @module, @name, @modifier, @value,
        CURRENT_TIMESTAMP, NULL)"
  |> Sql.parameters [ "id", Sql.uuid id
                      "canvasID", Sql.uuid canvasID
                      "module", Sql.string module'
                      "name", Sql.string name
                      "modifier", Sql.string modifier
                      "value",
                      Sql.string (DvalReprInternalRoundtrippable.toJsonV0 value) ]
  |> Sql.executeStatementAsync

let loadEvent (canvasID : CanvasID) (id : EventID) : Task<Option<T>> =
  Sql.query
    "SELECT module, name, modifier, enqueued_at, locked_at, value
     FROM events_v2
     WHERE id = @eventID
       AND canvas_id = @canvasID"
  |> Sql.parameters [ "eventId", Sql.uuid id; "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowOptionAsync (fun read ->
    { id = id
      canvasID = canvasID
      module' = read.string "module"
      name = read.string "name"
      modifier = read.string "modifier"
      enqueuedAt = read.instant "enqueued_at"
      lockedAt = read.instantOrNone "locked_at"
      value = read.string "value" |> DvalReprInternalRoundtrippable.parseJsonV0 })

let loadEventIDs
  (canvasID : CanvasID)
  ((module', name, modifier) : HandlerDesc)
  : Task<List<EventID>> =
  Sql.query
    "SELECT id
     FROM events_v2
     WHERE module = @module
       AND name = @name
       AND modifier = @modifier
       AND canvas_id = @canvasID
       LIMIT 1000" // don't go overboard
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "module", Sql.string module'
                      "name", Sql.string name
                      "modifier", Sql.string modifier ]
  |> Sql.executeAsync (fun read -> read.uuid "id")

module Test =
  let loadEvents
    (canvasID : CanvasID)
    ((module', name, modifier) : HandlerDesc)
    : Task<List<RT.Dval>> =
    Sql.query
      "SELECT value
        FROM events_v2
        WHERE module = @module
          AND name = @name
          AND modifier = @modifier
          AND canvas_id = @canvasID
          LIMIT 1000" // don't go overboard
    |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                        "module", Sql.string module'
                        "name", Sql.string name
                        "modifier", Sql.string modifier ]
    |> Sql.executeAsync (fun read ->
      read.string "value" |> DvalReprInternalRoundtrippable.parseJsonV0)



let deleteEvent (event : T) : Task<unit> =
  Sql.query "DELETE FROM events_v2 WHERE id = @eventID AND canvas_id = @canvasID"
  |> Sql.parameters [ "eventID", Sql.uuid event.id
                      "canvasID", Sql.uuid event.canvasID ]
  |> Sql.executeStatementAsync

/// Claim the lock by setting the lockedAt field. Must have already determined in
/// queue logic that this is OK to do. The update checks the old value and this
/// function will return Error without updating the DB if it does not see the
/// expected value.
let claimLock (event : T) (_n : Notification) : Task<Result<unit, string>> =
  task {
    let! rowCount =
      Sql.query
        $"UPDATE events_v2
            SET locked_at = CURRENT_TIMESTAMP
          WHERE id = @eventID
            AND canvas_id = @canvasID
            AND locked_at IS NOT DISTINCT FROM @currentLockedAt"
      // IS NOT DISTINCT FROM is like `=`, but it allows a null value
      |> Sql.parameters [ "eventID", Sql.uuid event.id
                          "canvasID", Sql.uuid event.canvasID
                          "currentLockedAt",
                          Sql.instantWithTimeZoneOrNone event.lockedAt ]
      |> Sql.executeNonQueryAsync
    if rowCount = 1 then return Ok()
    else if rowCount = 0 then return Error "LockNotClaimed"
    else return Error $"LockError: Invalid count: {rowCount}"
  }

/// -----------------
/// PubSub
/// -----------------


let topicName = TopicName(Config.queuePubSubProjectID, Config.queuePubSubTopicName)

let subscriptionName =
  SubscriptionName(Config.queuePubSubProjectID, Config.queuePubSubSubscriptionName)


let credentials : Option<Grpc.Core.ChannelCredentials> =
  Config.queuePubSubCredentials
  |> Option.map Google.Apis.Auth.OAuth2.GoogleCredential.FromJson
  |> Option.map (fun c -> c.ToChannelCredentials())

// PublisherClient and SubscriberClient have deprecated constructors that take
// Channels as arguments. However, it's not clear what we're supposed to use instead.
#nowarn "44"

let publisher : Lazy<Task<PublisherServiceApiClient>> =
  lazy
    (task {
      let! client =
        // If we have no credentials, then assume we must be trying to use the
        // PubSub Emulator for local/test development
        match credentials with
        | None ->
          PublisherServiceApiClientBuilder(
            EmulatorDetection = Google.Api.Gax.EmulatorDetection.EmulatorOrProduction
          )
            .BuildAsync()
        | Some credentials ->
          task {
            let endpoint = PublisherServiceApiClient.DefaultEndpoint
            let channel = Grpc.Core.Channel(endpoint, credentials)
            let grpcClient = Publisher.PublisherClient(channel)
            let settings = PublisherServiceApiSettings()
            return PublisherServiceApiClientImpl(grpcClient, settings)
          }


      // Ensure the topic is created locally
      if Config.queuePubSubCreateTopic then
        let! topicFound =
          task {
            try
              let! _ = client.GetTopicAsync(topicName)
              return true
            with
            | _ -> return false
          }
        if not topicFound then
          let! (_ : Topic) = client.CreateTopicAsync(topicName)
          ()


      return client
    })



let subscriber : Lazy<Task<SubscriberServiceApiClient>> =
  lazy
    (task {
      let! (_ : PublisherServiceApiClient) = publisher.Force()

      // Ensure subscription is created locally
      let! client =
        match credentials with
        | None ->
          SubscriberServiceApiClientBuilder(
            EmulatorDetection = Google.Api.Gax.EmulatorDetection.EmulatorOrProduction
          )
            .BuildAsync()
        | Some credentials ->
          task {
            let endpoint = SubscriberServiceApiClient.DefaultEndpoint
            let channel = Grpc.Core.Channel(endpoint, credentials)
            let grpcClient = Subscriber.SubscriberClient(channel)
            let settings = SubscriberServiceApiSettings()
            return SubscriberServiceApiClientImpl(grpcClient, settings)
          }


      if Config.queuePubSubCreateTopic then
        let! subFound =
          task {
            try
              let! _ = client.GetSubscriptionAsync(subscriptionName)
              return true
            with
            | _ -> return false
          }
        if not subFound then
          let! (_ : Subscription) =
            client.CreateSubscriptionAsync(
              subscriptionName,
              topicName,
              pushConfig = null,
              ackDeadlineSeconds = 60
            )
          ()
      return client
    })

/// Gets as many messages as allowed the next available notification in the queue, or None if it doesn't find
/// one within a timeout
let dequeue (count : int) : Task<List<Notification>> =
  task {
    let! subscriber = subscriber.Force()
    // We set a deadline in case we get the shutdown signal. We want to go back to
    // the outer loop if we're told to shutdown.
    let expiration =
      Google.Api.Gax.Expiration.FromTimeout(System.TimeSpan.FromSeconds 5)
    let callSettings = Google.Api.Gax.Grpc.CallSettings.FromExpiration expiration
    let! envelopes =
      task {
        try
          let! response =
            subscriber.PullAsync(
              subscriptionName,
              callSettings = callSettings,
              maxMessages = count
            )
          return response.ReceivedMessages |> Seq.toList
        with
        // We set the deadline above, and then if it didn't find anything it throws a
        // DeadlineExceeded Exception
        | :? Grpc.Core.RpcException as e when
          e.StatusCode = Grpc.Core.StatusCode.DeadlineExceeded
          ->
          return []
      }
    return
      envelopes
      |> List.map (fun (envelope : ReceivedMessage) ->
        let message = envelope.Message
        let timeInQueue = System.DateTime.Now - (message.PublishTime.ToDateTime())
        let data =
          message.Data.ToByteArray()
          |> UTF8.ofBytesUnsafe
          |> Json.Vanilla.deserialize<NotificationData>
        let deliveryAttempt =
          let da = message.GetDeliveryAttempt()
          if da.HasValue then Some da.Value else None
        { data = data
          deliveryAttempt = Option.defaultValue 1 deliveryAttempt
          timeInQueue = timeInQueue
          pubSubMessageID = message.MessageId
          pubSubAckID = envelope.AckId })
  }



let createNotifications (canvasID : CanvasID) (ids : List<EventID>) : Task<unit> =
  task {
    if ids <> [] then
      let! publisher = publisher.Force()
      let messages =
        ids
        |> List.map (fun id ->
          { id = id; canvasID = canvasID }
          |> Json.Vanilla.serialize
          |> Google.Protobuf.ByteString.CopyFromUtf8
          |> fun contents -> PubsubMessage(Data = contents))
      let! response = publisher.PublishAsync(topicName, messages)
      Telemetry.addTag "event.pubsub_ids" response.MessageIds
    else
      // Don't send an empty list of messages to pubsub
      Telemetry.addTag "event.pubsub_ids" []
    return ()
  }

let enqueue
  (canvasID : CanvasID)
  (module' : string)
  (name : string)
  (modifier : string)
  (value : RT.Dval)
  : Task<unit> =
  task {
    use _ =
      Telemetry.child
        "enqueue"
        [ "canvas_id", canvasID
          "handler.module", module'
          "handler.name", name
          "handler.modifier", modifier ]
    // save the event
    let id = System.Guid.NewGuid()
    do! createEvent canvasID id module' name modifier value
    do! createNotifications canvasID [ id ]
    return ()
  }

/// Tell PubSub that it can try to deliver this again, waiting [delay] seconds to do
/// so. This expiration of the ack is called NACK in the PubSub docs, and it
/// increments the deliveryAttempt counter
let requeueEvent (n : Notification) (delay : NodaTime.Duration) : Task<unit> =
  task {
    Telemetry.addTag "queue.requeue_delay_ms" delay.TotalMilliseconds
    let! subscriber = subscriber.Force()
    let delay = min 600 (int delay.TotalSeconds)
    let delay = max 0 delay
    Telemetry.addTag "queue.requeue_delay_actual_ms" (delay * 1000)
    return!
      subscriber.ModifyAckDeadlineAsync(
        subscriptionName,
        [ n.pubSubAckID ],
        int delay
      )
  }

/// Tell PubSub not to try again for 5 minutes
let extendDeadline (n : Notification) : Task<unit> =
  task {
    let! subscriber = subscriber.Force()
    return!
      subscriber.ModifyAckDeadlineAsync(
        subscriptionName,
        [ n.pubSubAckID ],
        LD.queueAllowedExecutionTimeInSeconds ()
      )
  }

/// Tell PubSub that we have handled this event. This drops the event.
let acknowledgeEvent (n : Notification) : Task<unit> =
  task {
    let! subscriber = subscriber.Force()
    return! subscriber.AcknowledgeAsync(subscriptionName, [ n.pubSubAckID ])
  }


let getRule
  (canvasID : CanvasID)
  (event : T)
  : Task<Option<SchedulingRules.SchedulingRule.T>> =
  task {
    // Rules seem to ignore modifiers which is fine as they shouldn't have meaning here
    let! rules = SchedulingRules.getSchedulingRules canvasID
    let rule =
      rules
      |> List.filter (fun r ->
        (r.eventSpace, r.handlerName) = (event.module', event.name))
      |> List.head
    return rule
  }

let requeueSavedEvents (canvasID : CanvasID) (handlerName : string) : Task<unit> =
  task {
    let! ids = loadEventIDs canvasID ("WORKER", handlerName, "_")
    return! createNotifications canvasID ids
  }

let init () : Task<unit> =
  task {
    let! (_ : PublisherServiceApiClient) = publisher.Force()
    let! (_ : SubscriberServiceApiClient) = subscriber.Force()
    return ()
  }


// DARK INTERNAL FN
let blockWorker = SchedulingRules.addSchedulingRule "block"

// DARK INTERNAL FN
let unblockWorker (canvasID : CanvasID) (handlerName : string) : Task<unit> =
  task {
    do! SchedulingRules.removeSchedulingRule "block" canvasID handlerName
    return! requeueSavedEvents canvasID handlerName
  }

let pauseWorker : CanvasID -> string -> Task<unit> =
  SchedulingRules.addSchedulingRule "pause"

let unpauseWorker (canvasID : CanvasID) (handlerName : string) : Task<unit> =
  task {
    do! SchedulingRules.removeSchedulingRule "pause" canvasID handlerName
    return! requeueSavedEvents canvasID handlerName
  }
