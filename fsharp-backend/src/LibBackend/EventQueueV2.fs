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

module DvalReprInternal = LibExecution.DvalReprInternal
module DvalReprExternal = LibExecution.DvalReprExternal
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Execution = LibExecution.Execution
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes

module TI = TraceInputs

module LD = LibService.LaunchDarkly

/// -----------------
/// Types
/// -----------------

type EventID = System.Guid

/// Notifications are sent by PubSub to say that now would be a good time to try this
/// event. We only load events in response to notifications.
type NotificationData = { id : EventID; canvasID : CanvasID }
type Notification = { data : NotificationData; pubSubID : string }

/// Events are stored in the DB and are the source of truth for when and how an event
/// should be executed. When they are complete, they are deleted.
type T =
  { id : EventID
    canvasID : CanvasID
    module' : string
    name : string
    modifier : string
    retries : int
    value : RT.Dval
    delayUntil : Instant
    lockedAt : Option<Instant>
    enqueuedAt : Instant }


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
  print $"createEvent {id}"
  Sql.query
    "INSERT INTO events_v2
       (id, canvas_id, module, name, modifier, value,
        delay_until, enqueued_at, retries, locked_at)
     VALUES
       (@id, @canvasID, @module, @name, @modifier, @value,
        CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, NULL)"
  |> Sql.parameters [ "id", Sql.uuid id
                      "canvasID", Sql.uuid canvasID
                      "module", Sql.string module'
                      "name", Sql.string name
                      "modifier", Sql.string modifier
                      "value",
                      Sql.string (DvalReprInternal.toInternalRoundtrippableV0 value) ]
  |> Sql.executeStatementAsync


/// -----------------
/// Database
/// The events_v2 is the source of truth for the queue
/// -----------------
let loadEvent (canvasID : CanvasID) (id : EventID) : Task<Option<T>> =
  print $"loadEvent {id}"
  Sql.query
    "SELECT module, name, modifier,
            delay_until, enqueued_at, retries, locked_at,
            value
     FROM events_v2
     WHERE id = @eventID
       AND canvas_id = @canvasID"
  |> Sql.parameters [ "eventId", Sql.uuid id; "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowOptionAsync (fun read ->
    let e =
      { id = id
        canvasID = canvasID
        module' = read.string "module"
        name = read.string "name"
        modifier = read.string "modifier"
        delayUntil = read.instant "delay_until"
        enqueuedAt = read.instant "enqueued_at"
        retries = read.int "retries"
        lockedAt = read.instantOrNone "locked_at"
        // FSTODO: what's the right format to encode these with?
        value = read.string "value" |> DvalReprInternal.ofInternalRoundtrippableV0 }
    Telemetry.addTags [ ("queue_delay", Instant.now().Minus(e.enqueuedAt))
                        ("module", e.module')
                        ("name", e.name)
                        ("modifier", e.modifier)
                        ("enqueued_at", e.enqueuedAt)
                        ("delay_until", e.delayUntil)
                        ("retries", e.retries)
                        ("locked_at", e.lockedAt) ]
    e)

let deleteEvent (event : T) : Task<unit> =
  print $"deletedEvent {event.id}"
  Sql.query "DELETE FROM events_v2 WHERE id = @eventID AND canvas_id = @canvasID"
  |> Sql.parameters [ "eventID", Sql.uuid event.id
                      "canvasID", Sql.uuid event.canvasID ]
  |> Sql.executeStatementAsync

let claimLock (event : T) (n : Notification) : Task<Result<unit, string>> =
  task {
    print $"claimLock {n}"
    let! rowCount =
      Sql.query
        $"UPDATE events_v2
            SET locked_at = CURRENT_TIMESTAMP
          WHERE id = @eventID
            AND canvas_id = @canvasID
            AND locked_at IS NOT DISTINCT FROM @currentLockedAt"
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

// PublisherClient and Subscriber client have deprecated forms that the Channels as
// arguments. No idea what we're supposed to use instead.
#nowarn "44"

let publisher : Lazy<Task<PublisherServiceApiClient>> =
  lazy
    (task {
      let! client =
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

let dequeue () : Task<Notification> =
  task {
    let! subscriber = subscriber.Force()
    let! response = subscriber.PullAsync(subscriptionName, maxMessages = 1)
    let mutable notification : Option<Notification> = None
    while notification = None do
      // Messages is allowed return no messages. It will wait a while by default
      let messages = response.ReceivedMessages
      let count = messages.Count
      if count > 0 then
        let message = messages[0].Message
        let data =
          message.Data.ToByteArray()
          |> UTF8.ofBytesUnsafe
          |> Json.Vanilla.deserialize<NotificationData>
        notification <- Some { data = data; pubSubID = message.MessageId }
        print $"dequeue {notification}"
        Telemetry.addTags [ "canvas_id", data.canvasID
                            "queue.event.id", data.id
                            "queue.event.pubsub_id", message.MessageId
                            "queue.event.time_in_queue",
                            ((message.PublishTime.ToDateTime()) - System.DateTime.Now)
                              .TotalMilliseconds ]
      else
        do! Task.Delay(LD.queueDelayBetweenPullsInMillis ())

    return Exception.unwrapOptionInternal "expect a notification" [] notification
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
    print $"enqueue {name}"
    let id = System.Guid.NewGuid()
    do! createEvent canvasID id module' name modifier value
    let data = { id = id; canvasID = canvasID }
    let! publisher = publisher.Force()
    let contents =
      data |> Json.Vanilla.serialize |> Google.Protobuf.ByteString.CopyFromUtf8
    Telemetry.addTag "event.data.content_length" contents.Length
    let message = PubsubMessage(Data = contents)
    let! response = publisher.PublishAsync(topicName, [ message ])
    let ids = seq { response.MessageIds } |> Seq.toList
    print $"enqueued to {id} as {ids}"
    Telemetry.addTag "event.pubsub_id" response.MessageIds[0]
    return ()
  }

let enqueueInAQueue
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (accountID : UserID)
  (module' : string)
  (name : string)
  (modifier : string)
  (value : RT.Dval)
  : Task<unit> =
  if LD.useEventsV2 canvasName then
    enqueue canvasID module' name modifier value
  else
    EventQueue.enqueue canvasName canvasID accountID module' name modifier value

/// Tell PubSub that it can try to deliver this again, waiting [delay] seconds to do so
let requeueEvent (n : Notification) (delay : int) : Task<unit> =
  task {
    print $"requeueevent {n}"
    let! subscriber = subscriber.Force()
    // set the deadline to zero so it'll run again
    let delay = min 600 delay
    let delay = max 0 delay
    let! _ =
      subscriber.ModifyAckDeadlineAsync(subscriptionName, [ n.pubSubID ], delay)
    return ()
  }

/// Tell PubSub not to try again for 5 minutes
let extendDeadline (n : Notification) : Task<unit> =
  task {
    print $"extendDeadline {n}"
    let! subscriber = subscriber.Force()
    do!
      subscriber.ModifyAckDeadlineAsync(
        subscriptionName,
        [ n.pubSubID ],
        LD.queueAllowedExecutionTimeInSeconds ()
      )
    return ()
  }

/// Tell PubSub that we have handled this event. This drops the event.
let acknowledgeEvent (n : Notification) : Task<unit> =
  task {
    let! subscriber = subscriber.Force()
    print $"anknowledge {n}"
    do! subscriber.AcknowledgeAsync(subscriptionName, [ n.pubSubID ])
    return ()
  }


let getRule
  (canvasID : CanvasID)
  (event : T)
  : Task<Option<EventQueue.SchedulingRule.T>> =
  task {
    // Rules seem to ignore modifiers which is fine as they shouldn't have meaning here
    let! rules = EventQueue.getSchedulingRules canvasID
    let rule =
      rules
      |> List.filter (fun r ->
        (r.eventSpace, r.handlerName) = (event.module', event.name))
      |> List.head
    return rule
  }

let init () : Task<unit> =
  task {
    let! (_ : PublisherServiceApiClient) = publisher.Force()
    let! (_ : SubscriberServiceApiClient) = subscriber.Force()
    return ()
  }

let flush () = () // FSTODO
