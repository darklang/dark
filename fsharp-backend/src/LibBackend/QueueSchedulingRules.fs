/// Blocking and pausing queue events
module LibBackend.QueueSchedulingRules


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

module SchedulingRule =
  module RuleType =
    type T =
      | Block
      | Pause

      override this.ToString() : string =
        match this with
        | Block -> "block"
        | Pause -> "pause"


    let parse (r : string) : Option<T> =
      match r with
      | "block" -> Some Block
      | "pause" -> Some Pause
      | _ -> None

  type T =
    { id : int
      ruleType : RuleType.T
      canvasID : CanvasID
      handlerName : string
      eventSpace : string
      createdAt : NodaTime.Instant }

  let toDval (r : T) : RT.Dval =
    RT.Dval.obj [ ("id", RT.Dval.int r.id)
                  ("rule_type", r.ruleType |> string |> RT.Dval.DStr)
                  ("canvas_id", RT.Dval.DUuid r.canvasID)
                  ("handler_name", RT.Dval.DStr r.handlerName)
                  ("event_space", RT.Dval.DStr r.eventSpace)
                  ("created_at", RT.Dval.DDate(RT.DDateTime.fromInstant r.createdAt)) ]


module WorkerStates =

  // This is used in a number of APIs - be careful of updating it
  type State =
    | Running
    | Blocked
    | Paused

    override this.ToString() : string =
      match this with
      | Running -> "run"
      | Blocked -> "block"
      | Paused -> "pause"

  let parse (str : string) : State =
    match str with
    | "run" -> Running
    | "block" -> Blocked
    | "pause" -> Paused
    | _ -> Exception.raiseInternal "invalid WorkerState" [ "workerState", str ]

  // This is used in a number of APIs - be careful of updating it
  type T = Map<string, State>

  module STJJsonConverter =
    // CLEANUP switch to this
    open System.Text.Json
    open System.Text.Json.Serialization

    type WorkerStateConverter() =
      inherit JsonConverter<State>()

      override _.Read(reader : byref<Utf8JsonReader>, _type, _options) : State =
        reader.GetString() |> parse

      override _.Write(writer : Utf8JsonWriter, value : State, _options) =
        writer.WriteStringValue(string value)

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
        writer.WriteValue(string value)


/// Sets all 'new' events to 'scheduled', bypassing actual scheduling logic
///
/// Meant only for testing, so the queue-scheduler process doesn't have to be involved
let testingScheduleAll () : Task<unit> =
  Sql.query
    "UPDATE events SET status = 'scheduled'
      WHERE status = 'new' AND delay_until <= CURRENT_TIMESTAMP"
  |> Sql.executeStatementAsync

let rowToSchedulingRule (read : RowReader) : SchedulingRule.T =
  let ruleType = read.string "rule_type"
  { id = read.int "id"
    ruleType =
      ruleType
      |> SchedulingRule.RuleType.parse
      |> Exception.unwrapOptionInternal
           "Could not parse ruleType"
           [ "ruleType", ruleType ]
    canvasID = read.uuid "canvas_id"
    handlerName = read.string "handler_name"
    eventSpace = read.string "event_space"
    createdAt = read.instantWithoutTimeZone "created_at" }


// DARK INTERNAL FN
/// Gets all event scheduling rules, as used by the queue-scheduler.
let getAllSchedulingRules () : Task<List<SchedulingRule.T>> =
  Sql.query
    "SELECT id, rule_type::TEXT, canvas_id, handler_name, event_space, created_at
     FROM scheduling_rules"
  |> Sql.executeAsync (fun read -> rowToSchedulingRule read)

/// Gets event scheduling rules for the specified canvas
let getSchedulingRules (canvasID : CanvasID) : Task<List<SchedulingRule.T>> =
  Sql.query
    "SELECT id, rule_type::TEXT, canvas_id, handler_name, event_space, created_at
     FROM scheduling_rules
     WHERE canvas_id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> rowToSchedulingRule read)

/// <summary>
/// Returns a pair of every event handler (worker) and its "schedule"
/// in the given canvas.
/// </summary>
/// </remarks>
/// The schedule is usually "run", but can be either "block" or "pause"
/// if there is a scheduling rule for that handler. A handler can have
/// both a block and pause rule, in which case it is "block"
/// (because block is an admin action).
/// </remarks>
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
      |> Sql.executeAsync (fun read ->
        (read.stringOrNone "name" |> Option.unwrap "", WorkerStates.Running))

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
      |> List.fold states (fun states r ->
        let v =
          match r.ruleType with
          | SchedulingRule.RuleType.Block -> WorkerStates.Blocked
          | SchedulingRule.RuleType.Pause -> WorkerStates.Paused

        Map.add r.handlerName v states)
  }


/// Keeps the given worker from executing by inserting a scheduling rule of the passed type.
/// Then, un-schedules any currently schedules events for this handler to keep
/// them from being processed.
/// 'pause' rules are user-controlled whereas 'block' rules are accessible
/// only as DarkInternal functions and cannot be removed by users
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


/// <summary>Removes a scheduling rule of the passed type if one exists.</summary>
/// <remarks>See also: addSchedulingRule.</remarks>
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
