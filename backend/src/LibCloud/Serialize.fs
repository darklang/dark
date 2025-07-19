/// <summary>Serializing to the DB.</summary>
/// <remarks>
/// Serialization formats and binary conversions are stored elsewhere
/// </remarks>
module LibCloud.Serialize

open System.Threading.Tasks
//open FSharp.Control.Tasks
open Microsoft.Data.Sqlite
open Fumble

open LibDB.Db

open Prelude

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization
module PTParser = LibExecution.ProgramTypesParser


// --------------------------------------------------------
// Load serialized data from the DB
// --------------------------------------------------------

type LoadAmount =
  | LiveToplevels
  | IncludeDeletedToplevels

type Deleted =
  | Deleted
  | NotDeleted

let loadToplevels
  (canvasID : CanvasID)
  (tlids : List<tlid>)
  : Task<List<Deleted * PT.Toplevel.T>> =
  task {
    let! data =
      if List.isEmpty tlids then
        // If no specific TLIDs requested, return empty list
        Task.FromResult []
      else
        // Create IN clause with parameters for each TLID
        let tlidParams =
          tlids |> List.mapi (fun i tlid -> ($"tlid{i}", Sql.tlid tlid))
        let tlidPlaceholders =
          tlids |> List.mapi (fun i _ -> $"@tlid{i}") |> String.concat ", "

        let query =
          $"SELECT tlid, data, deleted
           FROM toplevels_v0
           WHERE canvas_id = @canvasID
             AND tlid IN ({tlidPlaceholders})
             AND (
               tipe = 'db'
               OR tipe = 'handler'
             )"

        Sql.query query
        |> Sql.parameters (("canvasID", Sql.uuid canvasID) :: tlidParams)
        |> Sql.executeAsync (fun read ->
          (read.tlid "tlid", read.bytes "data", read.bool "deleted"))

    return
      data
      |> List.map (fun (tlid, tl, deleted) ->
        let isDeleted = if deleted then Deleted else NotDeleted
        (isDeleted, BinarySerialization.Toplevel.deserialize tlid tl))
  }


let fetchReleventTLIDsForHTTP
  (canvasID : CanvasID)
  (path : string)
  (method : string)
  : Task<List<tlid>> =
  // The pattern `$2 like name` is deliberate, to leverage the DB's
  // pattern matching to solve our routing.
  Sql.query
    "SELECT tlid
    FROM toplevels_v0
    WHERE canvas_id = @canvasID
      AND (
        (module = 'HTTP' AND @path like name AND modifier = @method)
        OR tipe <> 'handler'
      )
      AND deleted = 0"
  |> Sql.parameters
    [ "path", Sql.string path
      "method", Sql.string method
      "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchRelevantTLIDsForExecution (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
      WHERE canvas_id = @canvasID
      AND tipe <> 'handler'
      AND deleted = 0"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchRelevantTLIDsForEvent
  (canvasID : CanvasID)
  (module' : string)
  (name : string)
  (modifier : string)
  : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
    WHERE canvas_id = @canvasID
      AND (
        (module = @space AND name = @name AND modifier = @modifier)
        OR tipe <> 'handler'
      )
      AND deleted = 0"
  |> Sql.parameters
    [ "canvasID", Sql.uuid canvasID
      "space", Sql.string module'
      "name", Sql.string name
      "modifier", Sql.string modifier ]
  |> Sql.executeAsync (fun read -> read.id "tlid")


let fetchTLIDsForAllDBs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
    WHERE canvas_id = @canvasID
      AND tipe = 'db'
      AND deleted = 0"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchTLIDsForAllWorkers (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
    WHERE canvas_id = @canvasID
      AND tipe = 'handler'
      AND module <> 'CRON'
      AND module <> 'REPL'
      AND module <> 'HTTP'
      AND deleted = 0"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")


let fetchAllIncludingDeletedTLIDs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
    WHERE canvas_id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchAllLiveTLIDs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevels_v0
    WHERE canvas_id = @canvasID
      AND deleted = 0"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

type CronScheduleData =
  { canvasID : CanvasID
    tlid : id
    cronName : string
    interval : PT.Handler.CronInterval }

/// Fetch cron handlers from the DB. Active here means:
/// - a non-null interval field in the spec
/// - not deleted (When a CRON handler is deleted, we set (module, modifier,
///   deleted) to (NULL, NULL, True);  so our query `WHERE module = 'CRON'`
///   ignores deleted CRONs.)
/// TODO: this is untested, but also essential for running the crons
let fetchActiveCrons () : Task<List<CronScheduleData>> =
  Sql.query
    "SELECT
      canvas_id,
      tlid,
      modifier,
      toplevels_v0.name as handler_name
    FROM toplevels_v0
    JOIN canvases_v0 ON toplevels_v0.canvas_id = canvases_v0.id
    WHERE module = 'CRON'
      AND modifier IS NOT NULL
      AND modifier <> ''
      AND toplevels_v0.name IS NOT NULL
      AND deleted = 0"
  |> Sql.executeAsync (fun read ->
    let interval = read.string "modifier"
    let canvasID = read.uuid "canvas_id"

    { canvasID = canvasID
      tlid = read.id "tlid"
      cronName = read.string "handler_name"
      interval =
        interval
        |> PTParser.Handler.CronInterval.parse
        |> Exception.unwrapOptionInternal
          "Could not parse cron modifier"
          [ "interval", interval; "canvasID", canvasID ] })
