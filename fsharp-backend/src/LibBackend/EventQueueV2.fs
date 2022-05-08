module LibBackend.EventQueueV2

// All about workers

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

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

/// Notifications are sent by PubSub to say that now would be a good time to try this
/// event. We only load events in response to notifications.
type Notification = { id : id; canvasID : CanvasID }

/// Events are stored in the DB and are the source of truth for when and how an event
/// should be executed. When they are complete, they are deleted.
type T =
  { id : id
    canvasID : CanvasID
    module' : string
    name : string
    modifier : string
    retries : int
    value : RT.Dval
    delayUntil : Instant
    lockedAt : Option<Instant>
    enqueuedAt : Instant }

let dequeue () : Task<Notification> =
  task { return { id = 0UL; canvasID = System.Guid.NewGuid() } }

let enqueue (delayUntil : Instant) (n : Notification) : Task<unit> =
  task { return () }

let loadEvent (n : Notification) : Task<Option<T>> =
  Sql.query
    "SELECT module, name, modifier,
            delay_until, enqueued_at, retries, locked_at,
            value
     FROM events_v2
     WHERE id = @eventID
       AND canvasID = @canvasID"
  |> Sql.parameters [ "id", Sql.id n.id; "canvasID", Sql.uuid n.canvasID ]
  |> Sql.executeRowOptionAsync (fun read ->
    let e =
      { id = n.id
        canvasID = n.canvasID
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
  Sql.query "DELETE FROM events_v2 WHERE id = @eventID AND canvasID = @canvasID"
  |> Sql.parameters [ "eventID", Sql.id event.id
                      "canvasID", Sql.uuid event.canvasID ]
  |> Sql.executeStatementAsync

let requeueEvent (n : Notification) : Task<unit> = task { return () }
let acknowledgeEvent (n : Notification) : Task<unit> = task { return () }

let claimLock (event : T) (n : Notification) : Task<Result<unit, string>> =
  task {
    let currentLockedAt =
      match event.lockedAt with
      | None -> SqlValue.Null
      | Some instant -> Sql.instantWithTimeZone instant

    let! rowCount =
      Sql.query
        "UPDATE events_v2
        SET lockedAt = CURRENT_TIMESTAMP
        WHERE id = @eventID
          AND canvasID = @canvasID
          AND lockedAT = @currentLockedAt"
      |> Sql.parameters [ "eventID", Sql.id event.id
                          "canvasID", Sql.uuid event.canvasID
                          "currentLockedAt", currentLockedAt ]
      |> Sql.executeNonQueryAsync
    if rowCount = 1 then return Ok()
    else if rowCount = 0 then return Error "LockNotClaimed"
    else return Error $"LockError: Invalid count: {rowCount}"
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

// The algorithm here is described in docs/production/eventsV2.md. The algorithm
// below is annotated with names from chart.
type ShouldRetry =
  | Retry
  | NoRetry
