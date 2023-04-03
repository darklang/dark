module LibBackend.TraceInputs

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

module Repr = LibExecution.DvalReprInternalRoundtrippable


type EventRecord = HandlerDesc * NodaTime.Instant * AT.TraceID.T

type F404 = (string * string * string * NodaTime.Instant * AT.TraceID.T)

type Limit =
  | All
  | After of NodaTime.Instant
  | Before of NodaTime.Instant

// let event_subject module_ path modifier = module_ ^ "_" ^ path ^ "_" ^ modifier

// Note that this returns munged version of the name, that are designed for
// pattern matching using postgres' LIKE syntax.
// CLEANUP: nulls are allowed for name, modules, and modifiers. Why?
let getHandlersForCanvas (canvasID : CanvasID) : Task<List<tlid * HandlerDesc>> =
  // CLEANUP: for module, name, modifier do we need to check for nulls since we're using NOT NULL in the schema?
  Sql.query
    "SELECT tlid, module, name, modifier
      FROM toplevel_oplists_v0
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
// Event data
// -------------------------

// We store a set of events for each canvas. The events may or may not
// belong to a toplevel. We provide a list in advance so that they can
// be partitioned effectively. Returns the DB-assigned event timestamp.
let storeEvent
  (canvasID : CanvasID)
  (traceID : AT.TraceID.T)
  ((module_, path, modifier) : HandlerDesc)
  (event : RT.Dval)
  : Task<NodaTime.Instant> =
  Sql.query
    "INSERT INTO trace_old_events_v0
    (canvas_id, trace_id, module, path, modifier, timestamp, value)
    VALUES (@canvasID, @traceID, @module, @path, @modifier, CURRENT_TIMESTAMP, @value)
    RETURNING timestamp"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "traceID", Sql.traceID traceID
                      "module", Sql.string module_
                      "path", Sql.string path
                      "modifier", Sql.string modifier
                      ("value", event |> Repr.toJsonV0 |> Sql.string) ]
  |> Sql.executeRowAsync (fun reader -> reader.instant "timestamp")

let listEvents (limit : Limit) (canvasID : CanvasID) : Task<List<EventRecord>> =
  let timestampSql, timestamp =
    match limit with
    | All -> "", NodaTime.Instant.now ()
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
      FROM trace_old_events_v0
      WHERE canvas_id = @canvasID
      {timestampSql}"

  Sql.query sql
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "timestamp", Sql.instantWithoutTimeZone timestamp ]
  |> Sql.executeAsync (fun read ->
    ((read.string "module", read.string "path", read.string "modifier"),
     read.instant "timestamp",
     read.traceID "trace_id"))

let mungePathForPostgres (module_ : string) (path : string) =
  // Only munge the route for HTTP events, as they have wildcards, whereas
  // background events are completely concrete.
  //
  // `split_uri_path` inside `Routing.routeToPostgresPattern` doesn't like that background
  // events don't have leading slashes
  if String.toLowercase module_ = "http" then
    Routing.routeToPostgresPattern path
  else
    // https://www.postgresql.org/docs/9.6/functions-matching.html
    path.Replace("%", "\\%").Replace("_", "\\_")

let loadEventIDs
  (canvasID : CanvasID)
  ((module_, route, modifier) : HandlerDesc)
  : Task<List<AT.TraceID.T * string>> =
  let route = mungePathForPostgres module_ route

  Sql.query
    "SELECT trace_id, path FROM trace_old_events_v0
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
  |> Sql.executeAsync (fun read -> (read.traceID "trace_id", read.string "path"))


let get404s (limit : Limit) (canvasID : CanvasID) : Task<List<F404>> =
  task {
    let! events = listEvents limit canvasID
    let! handlers = getHandlersForCanvas canvasID

    let matchEvent h event : bool =
      let (space, requestPath, modifier), _ts, _ = event
      let hSpace, hName, hModifier = h

      Routing.requestPathMatchesRoute hName requestPath
      && hModifier = modifier
      && hSpace = space

    return
      events
      |> List.filter (fun e ->
        not (List.exists (fun (_tlid, h) -> matchEvent h e) handlers))
      |> List.map (fun ((space, name, modifier), timestamp, value) ->
        (space, name, modifier, timestamp, value))
  }

let getRecent404s (canvasID : CanvasID) : Task<List<F404>> =
  get404s (After(NodaTime.Instant.now () - NodaTime.Duration.FromDays 7)) canvasID


let delete404s
  (canvasID : CanvasID)
  (space : string)
  (path : string)
  (modifier : string)
  : Task<unit> =
  Sql.query
    "DELETE FROM trace_old_events_v0
     WHERE canvas_id = @canvasID
     AND module = @module
     AND path = @path
     AND modifier = @modifier"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "module", Sql.string space
                      "path", Sql.string path
                      "modifier", Sql.string modifier ]
  |> Sql.executeStatementAsync
