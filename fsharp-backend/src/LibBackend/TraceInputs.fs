module LibBackend.TraceInputs

// "Stored Events" in OCaml

// These are the "input values" for handler traces, containing the `request` or
// `event` for a trace.

// We keep traces around for a week, and also keep the last 10 regardless of
// age.

// Traces are also used for 404s - which are just traces for which a handler
// doesn't exist.  Note that traces are stored for routes (technically for an
// `event_desc`), not for handlers.  There is a GC process to clean these up.

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

module AT = LibExecution.AnalysisTypes
module RT = LibExecution.RuntimeTypes


// space, path, modifier
type EventDesc = string * string * string

type EventRecord = string * string * string * System.DateTime * AT.TraceID

type F404 = EventRecord

type Limit =
  | All
  | After of System.DateTime
  | Before of System.DateTime

// let event_subject module_ path modifier = module_ ^ "_" ^ path ^ "_" ^ modifier

// Note that this returns munged version of the name, that are designed for
// pattern matching using postgres' LIKE syntax. *)
// CLEANUP: nulls are allowed for name, modules, and modifiers. Why?
let getHandlersForCanvas (canvasID : CanvasID) : Task<List<tlid * EventDesc>> =
  Sql.query
    "SELECT tlid, module, name, modifier FROM toplevel_oplists
      WHERE canvas_id = @canvasID
        AND module IS NOT NULL
        AND name IS NOT NULL
        AND modifier IS NOT NULL
        AND tipe = 'handler'::toplevel_type"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read ->
    (read.tlid "tlid",
     (read.string "module", read.string "name", read.string "modifier")))

// -------------------------
// Event data *)
// -------------------------

let throttled : CanvasID = System.Guid.Parse "730b77ce-f505-49a8-80c5-8cabb481d60d"

// We store a set of events for each host. The events may or may not
// belong to a toplevel. We provide a list in advance so that they can
// be partitioned effectively. Returns the DB-assigned event timestamp.
let storeEvent
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  ((module_, path, modifier) : EventDesc)
  (event : RT.Dval)
  : Task<System.DateTime> =
  if canvasID = throttled then
    Task.FromResult System.DateTime.Now
  else
    Sql.query
      "INSERT INTO stored_events_v2
      (canvas_id, trace_id, module, path, modifier, timestamp, value)
      VALUES (@canvasID, @traceID, @module, @path, @modifier, CURRENT_TIMESTAMP, @value)
      RETURNING timestamp"
    |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                        "traceID", Sql.uuid traceID
                        "module", Sql.string module_
                        "path", Sql.string path
                        "modifier", Sql.string modifier
                        ("value",
                         event
                         |> LibExecution.DvalRepr.toInternalRoundtrippableV0
                         |> Sql.string) ]
    |> Sql.executeRowAsync (fun reader -> reader.dateTime "timestamp")

let listEvents (limit : Limit) (canvasID : CanvasID) : Task<List<EventRecord>> =
  let timestampSql, timestamp =
    match limit with
    | All -> "", System.DateTime.Now
    | After ts -> "AND timestamp > @timestamp", ts
    | Before ts -> "AND timestamp < @timestamp", ts

  let sql =
    // Note we just grab the first one in the group because the ergonomics
    // of SELECT DISTINCT ON is much easier than the complex GROUP BY
    // with row_partition/row_num counting to express "give me the
    // first N in the group" which is probably more what we want
    // from a product POV.
    //
    // Also we _could_ order by timestamp desc here to get the more
    // recent events if we desire in the future
    $"SELECT DISTINCT ON (module, path, modifier)
      module, path, modifier, timestamp, trace_id
      FROM stored_events_v2
      WHERE canvas_id = @canvasID
      {timestampSql}"

  Sql.query sql
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "timestamp", Sql.timestamp timestamp ]
  |> Sql.executeAsync (fun read ->
    (read.string "module",
     read.string "path",
     read.string "modifier",
     read.dateTime "timestamp",
     read.uuid "trace_id"))


// let list_event_descs
//     ~(limit : [`All | `After of RTT.time | `Before of RTT.time])
//     ~(canvas_id : Uuidm.t)
//     () : event_desc list =
//   let timestamp_constraint =
//     match limit with
//     | `All ->
//         ""
//     | `After ts ->
//         "AND timestamp > " ^ Db.escape (Time ts)
//     | `Before ts ->
//         "AND timestamp < " ^ Db.escape (Time ts)
//   in
//   let sql =
//     (* Note we just grab the first one in the group because the ergonomics
//      * of SELECT DISTINCT ON is much easier than the complex GROUP BY
//      * with row_partition/row_num counting to express "give me the
//      * first N in the group" which is probably more what we want
//      * from a product POV.
//      *
//      * Also we _could_ order by timestamp desc here to get the more
//      * recent events if we desire in the future *)
//     "SELECT DISTINCT ON (module, path, modifier)
//      module, path, modifier
//      FROM stored_events_v2
//      WHERE canvas_id = $1"
//     ^ timestamp_constraint
//   in
//   Db.fetch sql ~name:"list_events" ~params:[Db.Uuid canvas_id]
//   |> List.map ~f:(function
//          | [module_; path; modifier] ->
//              (module_, path, modifier)
//          | out ->
//              Exception.internal "Bad DB format for stored_events")


let loadEvents
  (canvasID : CanvasID)
  ((module_, route, modifier) : EventDesc)
  : Task<List<string * AT.TraceID * System.DateTime * RT.Dval>> =
  let route = Routing.routeToPostgresPattern route

  Sql.query
    "SELECT path, value, timestamp, trace_id FROM stored_events_v2
      WHERE canvas_id = @canvasID
        AND module = @module
        AND path LIKE @route
        AND modifier = @modifier
     ORDER BY timestamp DESC
     LIMIT 10"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "module", Sql.string module_
                      "route", Sql.string route
                      "modifier", Sql.string modifier ]
  |> Sql.executeAsync (fun read ->
    (read.string "path",
     read.uuid "trace_id",
     read.dateTime "timestamp",
     read.string "value" |> LibExecution.DvalRepr.ofInternalRoundtrippableV0))


let loadEventForTrace
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  : Task<Option<string * System.DateTime * RT.Dval>> =
  Sql.query
    "SELECT path, value, timestamp FROM stored_events_v2
       WHERE canvas_id = @canvasID
         AND trace_id = @traceID
       LIMIT 1"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "traceID", Sql.uuid traceID ]
  |> Sql.executeRowOptionAsync (fun read ->
    (read.string "path",
     read.dateTime "timestamp",
     read.string "value" |> LibExecution.DvalRepr.ofInternalRoundtrippableV0))


let mungePathForPostgres (module_ : string) (path : string) =
  // Only munge the route for HTTP events, as they have wildcards, whereas
  // background events are completely concrete.
  //
  // `split_uri_path` inside `Routing.routeToPostgresPattern` doesn't like that background
  // events don't have leading slashes
  if String.toLowercase module_ = "http" then
    Routing.routeToPostgresPattern path
  else
    (* https://www.postgresql.org/docs/9.6/functions-matching.html *)
    path.Replace("%", "\\%").Replace("_", "\\_")

let loadEventIDs
  (canvasID : CanvasID)
  ((module_, route, modifier) : EventDesc)
  : Task<List<AT.TraceID * string>> =
  let route = mungePathForPostgres module_ route

  Sql.query
    "SELECT trace_id, path FROM stored_events_v2
     WHERE canvas_id = @canvasID
       AND module = @module
       AND path LIKE @path
       AND modifier = @modifier
     ORDER BY timestamp DESC
     LIMIT 10"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "module", Sql.string module_
                      "path", Sql.string route
                      "modifier", Sql.string modifier ]
  |> Sql.executeAsync (fun read -> (read.uuid "trace_id", read.string "path"))


let get404s (limit : Limit) (canvasID : CanvasID) : Task<List<F404>> =
  task {
    let! events = listEvents limit canvasID
    let! handlers = getHandlersForCanvas canvasID

    let matchEvent h event : bool =
      let space, requestPath, modifier, _ts, _ = event in
      let hSpace, hName, hModifier = h in

      Routing.requestPathMatchesRoute hName requestPath
      && hModifier = modifier
      && hSpace = space

    return
      events
      |> List.filter (fun e ->
        not (List.exists (fun (_tlid, h) -> matchEvent h e) handlers))
  }

let getRecent404s (canvasID : CanvasID) : Task<F404 list> =
  get404s (After(System.DateTime.Now.AddDays(-7.0))) canvasID


let delete404s
  (canvasID : CanvasID)
  (space : string)
  (path : string)
  (modifier : string)
  : Task<unit> =
  Sql.query
    "DELETE FROM stored_events_v2
     WHERE canvas_id = @canvasID
     AND module = @module
     AND path = @path
     AND modifier = @modifier"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "module", Sql.string space
                      "path", Sql.string path
                      "modifier", Sql.string modifier ]
  |> Sql.executeStatementAsync




let clearAllEvents (canvasID : CanvasID) () : Task<unit> =
  Sql.query
    "DELETE FROM stored_events_v2
     WHERE canvas_id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeStatementAsync
