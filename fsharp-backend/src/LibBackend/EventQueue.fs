module LibBackend.EventQueue

// All about workers

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp.Tasks
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth
open LibService.Telemetry

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
// Public API *)
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

module SchedulingRule =
  module RuleType =
    type T =
      | Block
      | Pause

    let parse (r : string) : Option<T> =
      match r with
      | "block" -> Some Block
      | "pause" -> Some Pause
      | _ -> None

    let toString r : string =
      match r with
      | Block -> "block"
      | Pause -> "pause"

  type T =
    { id : int
      ruleType : RuleType.T
      canvasID : CanvasID
      handlerName : string
      eventSpace : string
      createdAt : System.DateTime }

  let toDval (r : T) : RT.Dval =
    RT.Dval.obj [ ("id", RT.Dval.int r.id)
                  ("rule_type", r.ruleType |> RuleType.toString |> RT.Dval.DStr)
                  ("canvas_id", RT.Dval.DUuid r.canvasID)
                  ("handler_name", RT.Dval.DStr r.handlerName)
                  ("event_space", RT.Dval.DStr r.eventSpace)
                  ("created_at", RT.Dval.DDate r.createdAt) ]

module WorkerStates =

  // This is used in a number of APIs - be careful of updating it
  type State =
    | Running
    | Blocked
    | Paused

  let toString (s : State) : string =
    match s with
    | Running -> "run"
    | Blocked -> "block"
    | Paused -> "pause"

  let parse (str : string) : State =
    match str with
    | "run" -> Running
    | "block" -> Blocked
    | "pause" -> Paused
    | _ -> failwith "invalid WorkerState: {str}"

  // This is used in a number of APIs - be careful of updating it
  type T = Map<string, State>

  let empty = Map.empty

  module JsonConverter =
    open Newtonsoft.Json
    open Newtonsoft.Json.Converters

    type WorkerStateConverter() =
      inherit JsonConverter<State>()

      override this.ReadJson(reader : JsonReader, _typ, _, _, serializer) : State =
        reader.Value :?> string |> parse

      override this.WriteJson
        (
          writer : JsonWriter,
          value : State,
          _ : JsonSerializer
        ) =
        writer.WriteValue(toString value)

  let find (k : string) (m : T) = Map.get k m


// (* None in case we want to log on a timer or something, not
//                      just on enqueue/dequeue *)
//
// let log_queue_size
//     (queue_action : queue_action)
//     ?(host : string option)
//     (canvas_id : string)
//     (space : string)
//     (name : string)
//     (modifier : string) =
//   (* Prefixing all of these with queue so we don't overwrite - e.g., 'enqueue'
//    * from a handler that has a space, etc *)
//   let params =
//     [ ("queue_action", show_queue_action queue_action)
//     ; ("queue_canvas_id", canvas_id)
//     ; ("queue_event_space", space)
//     ; ("queue_event_name", name)
//     ; ("queue_event_modifier", modifier) ]
//   in
//   (* host is optional b/c we have it when we dequeue, but not enqueue. We could
//    * also do a db lookup here if we decide querying by canvas name in honeycomb
//    * is important *)
//   let host =
//     (* Our Option module seems not to have an or_else type function ("if none,
//      * then run this function" *)
//     match host with
//     | Some host ->
//         Some host
//     | None ->
//         Db.fetch_one
//           ~name:"fetch_canvas_name"
//           "SELECT name FROM canvases WHERE id = $1"
//           ~params:[Uuid (Uuidm.of_string canvas_id |> Option.value_exn)]
//         |> List.hd
//   in
//   (* I suppose we could do a subquery that'd batch these three conditionals, but
//    * I'm not sure it's worth the added complexity - we could look at the
//    * postgres logs in honeycomb if we wanted to get a sense of how long this
//    * takes *)
//   (* TODO: what statuses? *)
//   let canvas_queue_size =
//     Db.fetch_one
//       ~name:"canvas queue size"
//       ~subject:canvas_id
//       "SELECT count(*) FROM events
//       WHERE status IN ('new','locked','error') AND canvas_id = $1"
//       ~params:[Uuid (Uuidm.of_string canvas_id |> Option.value_exn)]
//     |> List.hd_exn
//     |> int_of_string
//   in
//   let worker_queue_size =
//     Db.fetch_one
//       ~name:"canvas queue size"
//       ~subject:canvas_id
//       "SELECT count(*) FROM events
//       WHERE status IN ('new','locked','error') AND canvas_id = $1
//       AND space = $2 AND name = $3 AND modifier = $4"
//       ~params:
//         [ Uuid (Uuidm.of_string canvas_id |> Option.value_exn)
//         ; String space
//         ; String name
//         ; String modifier ]
//     |> List.hd_exn
//     |> int_of_string
//   in
//   let jsonparams =
//     [ ("canvas_queue_size", `Int canvas_queue_size)
//     ; ("worker_queue_size", `Int worker_queue_size) ]
//   in
//   let params =
//     match host with
//     | None ->
//         params
//     | Some host ->
//         ("queue_canvas_name", host) :: params
//   in
//   Log.infO "queue size" ~params ~jsonparams



let enqueue
  (canvasID : CanvasID)
  (accountID : UserID)
  (space : string)
  (name : string)
  (modifier : string)
  (data : RT.Dval)
  : Task<unit> =
  // FSTODO
  // log_queue_size Enqueue (canvas_id |> Uuidm.to_string) space name modifier ;
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
                        LibExecution.DvalRepr.toInternalRoundtrippableV0 data
                      ) ]
  |> Sql.executeStatementAsync


let dequeue (parent : Span.T) : Task<Option<T>> =
  Span.addEvent "dequeue" parent

  Sql.query
    "SELECT e.id, e.value, e.retries, e.canvas_id, e.account_id, c.name as canvas_name, e.space, e.name as event_name, e.modifier,
     (extract(epoch from (CURRENT_TIMESTAMP - enqueued_at)) * 1000) as queue_delay_ms
     FROM events AS e
     JOIN canvases AS c ON e.canvas_id = c.id
     WHERE status = 'scheduled'
     ORDER BY e.id ASC
     FOR UPDATE OF e SKIP LOCKED
     LIMIT 1"
  |> Sql.executeRowOptionAsync
       (fun read ->
         let id = read.id "id"
         let value = read.string "value"
         let retries = read.int "retries"
         let canvasID = read.uuid "canvas_id"
         let ownerID = read.uuid "account_id"
         let canvasName = read.string "canvas_name"
         let space = read.string "space"
         let name = read.string "event_name"
         let modifier = read.string "modifier"
         let delay = read.doubleOrNone "queue_delay_ms" |> Option.defaultValue 0.0

         (id,
          value,
          retries,
          canvasID,
          ownerID,
          canvasName,
          space,
          name,
          modifier,
          delay))
  |> Task.map (
    Option.map
      (fun (id, value, retries, canvasID, ownerID, canvasName, space, name, modifier, delay) ->
        // FSTODO
        // log_queue_size Dequeue ~host canvas_id space name modifier ;
        // TODO better names
        let (_ : Span.T) =
          parent
            .AddTag("queue_delay", delay)
            .AddTag("host", canvasName)
            .AddTag("canvas_id", canvasID)
            .AddTag("space", space)
            .AddTag("handler_name", name)
            .AddTag("modifier", modifier)

        { id = id
          value = LibExecution.DvalRepr.ofInternalRoundtrippableV0 value
          retries = retries
          canvasID = canvasID
          ownerID = ownerID
          canvasName = CanvasName.create canvasName
          space = space
          name = name
          modifier = modifier
          delay = delay })
  )

// schedule_all bypasses actual scheduling logic and is meant only for allowing
// testing without running the queue-scheduler process
let testingScheduleAll () : Task<unit> =
  Sql.query
    "UPDATE events SET status = 'scheduled'
      WHERE status = 'new' AND delay_until <= CURRENT_TIMESTAMP"
  |> Sql.executeStatementAsync

let rowToSchedulingRule (read : RowReader) : SchedulingRule.T =
  { id = read.int "id"
    ruleType =
      read.string "rule_type" |> SchedulingRule.RuleType.parse |> Option.unwrapUnsafe
    canvasID = read.uuid "canvas_id"
    handlerName = read.string "handler_name"
    eventSpace = read.string "event_space"
    createdAt = read.dateTime "created_at" }


// (* DARK INTERNAL FN *)
// (* Gets all event scheduling rules, as used by the queue-scheduler. *)
// let get_all_scheduling_rules unit : Scheduling_rule.t list =
//   Db.fetch
//     ~name:"get_all_scheduling_rules"
//     "SELECT id, rule_type, canvas_id, handler_name, event_space, created_at
//     FROM scheduling_rules"
//     ~params:[]
//   |> List.map ~f:row_to_scheduling_rule
//
//
// Gets event scheduling rules for the specified canvas
let getSchedulingRules (canvasID : CanvasID) : Task<List<SchedulingRule.T>> =
  Sql.query
    "SELECT id, rule_type, canvas_id, handler_name, event_space, created_at
     FROM scheduling_rules
     WHERE canvas_id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> rowToSchedulingRule read)

// Returns an pair of every event handler (worker) in the given canvas
// and its "schedule", which is usually "run", but can be either "block" or
// "pause" if there is a scheduling rule for that handler. A handler can have
// both a block and pause rule, in which case it is "block" (because block is
// an admin action).
let getWorkerSchedules (canvasID : CanvasID) : Task<WorkerStates.T> =
  task {
    // build a pairs (name * Running) for all worker names in canvas
    let! states =
      Sql.query
        "SELECT name
         FROM toplevel_oplists T
         WHERE canvas_id = @canvasID
           AND tipe = 'handler'
           AND module = 'WORKER'"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
      |> Sql.executeAsync (fun read -> (read.string "name", WorkerStates.Running))

    let states = Map.fromList states

    // get scheduling overrides for canvas, partitioned
    let! schedulingRules = getSchedulingRules canvasID

    let blocks, pauses =
      schedulingRules
      |> List.partition (fun r -> r.ruleType = SchedulingRule.RuleType.Block)

    // take the map of workers (where everything is set to Running) and overwrite
    // any worker that has (in order) a pause or a block *)
    return
      pauses @ blocks
      |> List.fold
           states
           (fun states r ->
             let v =
               match r.ruleType with
               | SchedulingRule.RuleType.Block -> WorkerStates.Blocked
               | SchedulingRule.RuleType.Pause -> WorkerStates.Paused in

             Map.add r.handlerName v states)
  }


// Keeps the given worker from executing by inserting a scheduling rule of the passed type.
// Then, un-schedules any currently schedules events for this handler to keep
// them from being processed.
// 'pause' rules are user-controlled whereas 'block' rules are accessible
// only as DarkInternal functions and cannot be removed by users
let addSchedulingRule
  (ruleType : string)
  (canvasID : CanvasID)
  (workerName : string)
  : Task<unit> =
  task {
    do!
      Sql.query
        "INSERT INTO scheduling_rules (rule_type, canvas_id, handler_name, event_space)
         VALUES ( @ruleType::scheduling_rule_type, @canvasID, @workerName, 'WORKER')
         ON CONFLICT DO NOTHING"
      |> Sql.parameters [ "ruleType", Sql.string ruleType
                          "canvasID", Sql.uuid canvasID
                          "workerName", Sql.string workerName ]
      |> Sql.executeStatementAsync

    do!
      Sql.query
        "UPDATE events
            SET status = 'new'
            WHERE space = 'WORKER'
              AND status = 'scheduled'
              AND canvas_id = @canvasID
              AND name = @workerName"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "workerName", Sql.string workerName ]
      |> Sql.executeStatementAsync
  }


// Removes a scheduling rule of the passed type if one exists.
// See also: addSchedulingRule.
let removeSchedulingRule
  (ruleType : string)
  (canvasID : CanvasID)
  (workerName : string)
  : Task<unit> =
  Sql.query
    "DELETE FROM scheduling_rules
     WHERE canvas_id = @canvasID
       AND handler_name = @workerName
       AND event_space = 'WORKER'
       AND rule_type = @ruleType::scheduling_rule_type"
  |> Sql.parameters [ "ruleType", Sql.string ruleType
                      "canvasID", Sql.uuid canvasID
                      "workerName", Sql.string workerName ]
  |> Sql.executeStatementAsync



// DARK INTERNAL FN
let blockWorker = addSchedulingRule "block"

// DARK INTERNAL FN
let unblockWorker = removeSchedulingRule "block"

let pauseWorker : CanvasID -> string -> Task<unit> = addSchedulingRule "pause"

let unpauseWorker : CanvasID -> string -> Task<unit> = removeSchedulingRule "pause"

// Open a database [transaction] and run [f],in it - [f] takes both a [Span.t]
// (for tracing) and a [transaction] id
let withTransaction (f : unit -> Task<'a>) : Task<'a> =
  task {
    let connection = Db.connect () |> Sql.createConnection
    connection.Open()

    let! transaction = connection.BeginTransactionAsync()
    let! result = f ()
    do! transaction.CommitAsync()
    return result
  }

let putBack (parent : Span.T) (item : T) (status : Status) : Task<unit> =
  let span =
    (Span.child "event_queue: put_back_transaction" parent)
      .AddTag("status", toString status)
      .AddTag("retries", item.retries)
    |> ignore

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


let finish (span : Span.T) (item : T) : Task<unit> = putBack span item OK
