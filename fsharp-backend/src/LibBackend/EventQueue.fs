module LibBackend.EventQueue

// All about workers

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Telemetry = LibService.Telemetry

module RT = LibExecution.RuntimeTypes



type Status =
  | OK
  | Err
  | Incomplete
  | Missing

  override this.ToString() =
    match this with
    | OK -> "Ok"
    | Err -> "Err"
    | Incomplete -> "Incomplete"
    | Missing -> "Missing"


type T =
  { id : id
    value : RT.Dval
    retries : int
    canvasID : CanvasID
    ownerID : UserID
    canvasName : CanvasName.T
    space : string
    name : string
    modifier : string
    // Delay in ms since it entered the queue
    delay : float }

let toEventDesc t = (t.space, t.name, t.modifier)

// -------------------------
// Public API
// -------------------------

type queue_action =
  | Enqueue
  | Dequeue
  | NoAction

  override this.ToString() =
    match this with
    | Enqueue -> "enqueue"
    | Dequeue -> "dequeue"
    | NoAction -> "none"

/// <summary>
/// Logs the current status of what's enqueued for the the given queue,
/// both at the "worker level" (ready to be picked up by workers),
/// and at the "canvas level" (ready to be enqueued _for_ a worker).
/// </summary>
///
/// <remarks>
/// `host` is optional because we have it when we dequeue, but not enqueue.
/// </remarks>
let logQueueSize
  (queue_action : queue_action)
  (canvasName : Option<string>)
  (canvasID : CanvasID)
  (space : string)
  (name : string)
  (modifier : string)
  : Task<unit> =
  task {
    use span = Telemetry.child "queue size" []
    let! host =
      match canvasName with
      | Some host -> Task.FromResult(host)
      | None ->
        Sql.query "SELECT name FROM canvases WHERE id = @canvasID"
        |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
        |> Sql.executeRowAsync (fun read -> read.string "name")

    // I suppose we could do a subquery that'd batch these three conditionals, but
    // I'm not sure it's worth the added complexity - we could look at the
    // postgres logs in honeycomb if we wanted to get a sense of how long this
    // takes
    let! canvasQueueSize =
      Sql.query
        "SELECT count(*) FROM events
         WHERE status IN ('new','locked','error') AND canvas_id = @canvasID"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
      |> Sql.executeRowAsync (fun read -> read.int "count")

    let! workerQueueSize =
      Sql.query
        "SELECT count(*) FROM events
         WHERE status IN ('new','locked','error') AND canvas_id = @canvasID
         AND space = @space AND name = @name AND modifier = @modifier"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "space", Sql.string space
                          "name", Sql.string name
                          "modifier", Sql.string modifier ]
      |> Sql.executeRowAsync (fun read -> read.int "count")

    Telemetry.addTags [ ("canvas_queue_size", canvasQueueSize)
                        ("worker_queue_size", workerQueueSize)
                        // Prefixing all of these with queue so we don't overwrite - e.g., 'enqueue'
                        // from a handler that has a space, etc
                        ("queue_canvas_name", host)
                        ("queue_action", string queue_action)
                        ("queue_canvas_id", canvasID)
                        ("queue_event_space", space)
                        ("queue_event_name", name)
                        ("queue_event_modifier", modifier) ]
  }


let enqueue
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (accountID : UserID)
  (space : string)
  (name : string)
  (modifier : string)
  (data : RT.Dval)
  : Task<unit> =
  task {
    use span =
      Telemetry.child
        "enqueue"
        [ "canvas_name", canvasName
          "canvas_id", canvasID
          "space", space
          "handler_name", name
          "modifier", modifier ]
    do! logQueueSize Enqueue None canvasID space name modifier
    return!
      Sql.query
        "INSERT INTO events
          (status, dequeued_by, canvas_id, account_id,
            space, name, modifier, value, delay_until, enqueued_at)
          VALUES ('new', NULL, @canvasID, @accountID, @space, @name, @modifier,
             @data, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "accountID", Sql.uuid accountID
                          "space", Sql.string space
                          "name", Sql.string name
                          "modifier", Sql.string modifier
                          "data",
                          Sql.string (
                            LibExecution.DvalReprInternalDeprecated.toInternalRoundtrippableV0
                              data
                          ) ]
      |> Sql.executeStatementAsync
  }


let dequeue () : Task<Option<T>> =
  task {
    Telemetry.addEvent "dequeue" []
    let! result =
      Sql.query
        "SELECT e.id, e.value, e.retries, e.canvas_id, e.account_id, c.name as canvas_name, e.space, e.name as event_name, e.modifier,
      (extract(epoch from (CURRENT_TIMESTAMP - enqueued_at)) * 1000) as queue_delay_ms
      FROM events AS e
      JOIN canvases AS c ON e.canvas_id = c.id
      WHERE status = 'scheduled'
      ORDER BY e.id ASC
      FOR UPDATE OF e SKIP LOCKED
      LIMIT 1"
      |> Sql.executeRowOptionAsync (fun read ->
        (read.id "id",
         read.string "value",
         read.int "retries",
         read.uuid "canvas_id",
         read.uuid "account_id",
         read.string "canvas_name",
         read.string "space",
         read.string "event_name",
         read.string "modifier",
         read.doubleOrNone "queue_delay_ms" |> Option.defaultValue 0.0))
    match result with
    | None -> return None
    | Some (id,
            value,
            retries,
            canvasID,
            ownerID,
            canvasName,
            space,
            name,
            modifier,
            delay) ->
      do! logQueueSize Dequeue (Some canvasName) canvasID space name modifier
      // TODO better names
      Telemetry.addTags [ ("queue_delay", delay)
                          ("host", canvasName)
                          ("canvas_id", canvasID)
                          ("space", space)
                          ("handler_name", name)
                          ("modifier", modifier) ]

      return
        Some
          { id = id
            value =
              LibExecution.DvalReprInternalDeprecated.ofInternalRoundtrippableV0
                value
            retries = retries
            canvasID = canvasID
            ownerID = ownerID
            canvasName = CanvasName.create canvasName
            space = space
            name = name
            modifier = modifier
            delay = delay }
  }


let fetchAllQueueItems (canvasName : CanvasName.T) : Task<List<string>> =
  Sql.query
    "SELECT e.value
       FROM events AS e, canvases as c
      WHERE c.name = @canvasName
        AND c.id = e.canvas_id
        AND (status = 'new' OR status = 'scheduled')"
  |> Sql.parameters [ "canvasName", canvasName |> string |> Sql.string ]
  |> Sql.executeAsync (fun read -> read.string "value")


/// Fetches (only) the values stored in a queue,
/// only to be used for writing tests
let testingGetQueue canvasID eventName =
  task {
    let! result =
      Sql.query
        "SELECT e.value
      FROM events AS e
      JOIN canvases AS c ON e.canvas_id = c.id
      WHERE c.id = @canvasID
        AND e.name = @eventName
      ORDER BY e.id ASC"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "eventName", Sql.string eventName ]
      |> Sql.executeAsync (fun read -> read.string "value")

    return
      result
      |> List.map (
        LibExecution.DvalReprInternalDeprecated.ofInternalRoundtrippableV0
      )
  }

/// <summary>
/// Puts an event 'back' to the queue, with a given status
/// </summary>
/// <remarks>
/// Used:
/// - in success cases (with "Ok" status) when item on queue processed and "moves on"
/// - in failure cases where we've had an issue processing an item
///   (due to the handler failing in execution, or missing for the event, etc.)
/// </remarks>
let putBack (item : T) (status : Status) : Task<unit> =
  task {
    use _span =
      Telemetry.child
        "event_queue: put_back_transaction"
        [ ("status", string status); ("retries", item.retries) ]

    return!
      match status with
      | OK ->
        Sql.query
          "UPDATE \"events\"
          SET status = 'done', last_processed_at = CURRENT_TIMESTAMP
          WHERE id = @id"
        |> Sql.parameters [ "id", Sql.id item.id ]
        |> Sql.executeStatementAsync
      | Missing ->
        Sql.query
          "UPDATE events
              SET status = 'missing', last_processed_at = CURRENT_TIMESTAMP
            WHERE id = @id"
        |> Sql.parameters [ "id", Sql.id item.id ]
        |> Sql.executeStatementAsync
      | Err ->
        if item.retries < 2 then
          Sql.query
            "UPDATE events
            SET status = 'new'
              , retries = @retries
              , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
              , last_processed_at = CURRENT_TIMESTAMP
            WHERE id = @id"
          |> Sql.parameters [ "id", Sql.id item.id
                              "retries", Sql.int (item.retries + 1) ]
          |> Sql.executeStatementAsync
        else
          Sql.query
            "UPDATE events
            SET status = 'error'
              , last_processed_at = CURRENT_TIMESTAMP
            WHERE id = @id"
          |> Sql.parameters [ "id", Sql.id item.id ]
          |> Sql.executeStatementAsync
      | Incomplete ->
        Sql.query
          "UPDATE events
          SET status = 'new'
            , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
            , last_processed_at = CURRENT_TIMESTAMP
          WHERE id = @id"
        |> Sql.parameters [ "id", Sql.id item.id ]
        |> Sql.executeStatementAsync
  }

let finish (item : T) : Task<unit> = putBack item OK
