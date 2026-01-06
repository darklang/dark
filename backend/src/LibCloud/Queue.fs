/// Queue/Event system for Workers (including Crons).
/// Note: QueueWorker and CronChecker have been removed, so events stored here
/// are not processed. This module is kept for the emit builtin to work without
/// errors, though events go nowhere.
module LibCloud.Queue

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Db

type Instant = NodaTime.Instant

open Prelude

module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

/// -----------------
/// Types
/// -----------------

type EventID = System.Guid

/// Notification data structure (kept for serialization compatibility)
type NotificationData = { id : EventID; canvasID : CanvasID }

/// Events are stored in the DB. Without QueueWorker, they are never processed.
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
/// -----------------

let createEventAtTime
  (canvasID : CanvasID)
  (id : EventID)
  (module' : string)
  (name : string)
  (modifier : string)
  (dt : Instant)
  (value : RT.Dval)
  : Task<unit> =
  Sql.query
    "INSERT INTO queue_events_v0
      (id, canvas_id, module, name, modifier, value, enqueued_at, locked_at)
     VALUES
      (@id, @canvasID, @module, @name, @modifier, @value, @enqueuedAt, NULL)"
  |> Sql.parameters
    [ "id", Sql.uuid id
      "canvasID", Sql.uuid canvasID
      "module", Sql.string module'
      "name", Sql.string name
      "modifier", Sql.string modifier
      "enqueuedAt", Sql.instant dt
      "value", Sql.string (DvalReprInternalRoundtrippable.toJsonV0 value) ]
  |> Sql.executeStatementAsync

let createEvent
  (canvasID : CanvasID)
  (id : EventID)
  (module' : string)
  (name : string)
  (modifier : string)
  (value : RT.Dval)
  : Task<unit> =
  createEventAtTime canvasID id module' name modifier (Instant.now ()) value

let loadEvent (canvasID : CanvasID) (id : EventID) : Task<Option<T>> =
  Sql.query
    "SELECT module, name, modifier, enqueued_at, locked_at, value
     FROM queue_events_v0
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
  ((module', name, modifier) : PT.Handler.HandlerDesc)
  : Task<List<EventID>> =
  Sql.query
    "SELECT id
    FROM queue_events_v0
    WHERE module = @module
      AND name = @name
      AND modifier = @modifier
      AND canvas_id = @canvasID
    LIMIT 1000"
  |> Sql.parameters
    [ "canvasID", Sql.uuid canvasID
      "module", Sql.string module'
      "name", Sql.string name
      "modifier", Sql.string modifier ]
  |> Sql.executeAsync (fun read -> read.uuid "id")

module Test =
  let loadEvents
    (canvasID : CanvasID)
    ((module', name, modifier) : PT.Handler.HandlerDesc)
    : Task<List<RT.Dval>> =
    Sql.query
      "SELECT value
      FROM queue_events_v0
      WHERE module = @module
        AND name = @name
        AND modifier = @modifier
        AND canvas_id = @canvasID
      LIMIT 1000"
    |> Sql.parameters
      [ "canvasID", Sql.uuid canvasID
        "module", Sql.string module'
        "name", Sql.string name
        "modifier", Sql.string modifier ]
    |> Sql.executeAsync (fun read ->
      read.string "value" |> DvalReprInternalRoundtrippable.parseJsonV0)


let deleteEvent (event : T) : Task<unit> =
  Sql.query
    "DELETE FROM queue_events_v0
    WHERE id = @eventID
      AND canvas_id = @canvasID"
  |> Sql.parameters
    [ "eventID", Sql.uuid event.id; "canvasID", Sql.uuid event.canvasID ]
  |> Sql.executeStatementAsync

/// -----------------
/// Enqueue (stores events but they won't be processed)
/// -----------------

let enqueueAtTime
  (canvasID : CanvasID)
  (module' : string)
  (name : string)
  (modifier : string)
  (dt : Instant)
  (value : RT.Dval)
  : Task<unit> =
  task {
    let id = System.Guid.NewGuid()
    do! createEventAtTime canvasID id module' name modifier dt value
    // Note: Events are stored but never processed (QueueWorker removed)
  }

let enqueueNow
  (canvasID : CanvasID)
  (module' : string)
  (name : string)
  (modifier : string)
  (value : RT.Dval)
  : Task<unit> =
  enqueueAtTime
    canvasID
    module'
    name
    modifier
    (Instant.FromDateTimeUtc System.DateTime.UtcNow)
    value

let init () : Task<unit> =
  task {
    // Queue/PubSub disabled - QueueWorker and CronChecker have been removed
    printTime "Queue init skipped (PubSub disabled)"
    return ()
  }
