/// <summary>Serializing to the DB.</summary>
/// <remarks>
/// Serialization formats and binary conversions are stored elsewhere
/// </remarks>
module LibBackend.Serialize


open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth
open Prelude.Tablecloth

module PT = LibExecution.ProgramTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization
module PTParser = LibExecution.ProgramTypesParser
module Telemetry = LibService.Telemetry


// let isLatestOpRequest
//   (clientOpCtrID : Option<string>)
//   (opCtr : int)
//   (canvasID : CanvasID)
//   : Task<bool> =
//   // opctr is used to prevent earlier ops from overwriting later ones
//   task {
//     let clientOpCtrID =
//       match clientOpCtrID with
//       | Some ""
//       | None -> System.Guid.NewGuid()
//       | Some s -> System.Guid.Parse s

//     do!
//       (Sql.query
//         // This is "UPDATE ... WHERE browser_id = $1 AND ctr < $2" except
//         // that it also handles the initial case where there is no
//         // browser_id record yet
//         // CLEANUP: this table doesn't exist anymore
//         "INSERT INTO op_ctrs(browser_id, ctr, canvas_id)
//            VALUES(@clientOpCtrID, @opCtr, @canvasID)
//            ON CONFLICT (browser_id)
//            DO UPDATE SET ctr = EXCLUDED.ctr, timestamp = NOW()
//            WHERE op_ctrs.ctr < EXCLUDED.ctr"
//        |> Sql.parameters [ "clientOpCtrID", Sql.uuid clientOpCtrID
//                            "opCtr", Sql.int opCtr
//                            "canvasID", Sql.uuid canvasID ]
//        |> Sql.executeStatementAsync)

//     return!
//       // CLEANUP: this table doesn't exist anymore
//       Sql.query
//         "SELECT TRUE FROM op_ctrs
//            WHERE browser_id = @clientOpCtrID
//              AND ctr = @opCtr"
//       |> Sql.parameters [ "clientOpCtrID", Sql.uuid clientOpCtrID
//                           "opCtr", Sql.int opCtr ]
//       |> Sql.executeExistsAsync
//   }


// --------------------------------------------------------
// Load serialized data from the DB
// --------------------------------------------------------

type LoadAmount =
  | LiveToplevels
  | IncludeDeletedToplevels

// Load oplists for anything that wasn't cached.
// TLs might not be returned from the materialized view/fast loader/cache if:
//  a) they have no materialized view (probably not possible anymore!)
//  b) they are deleted, because the cache query filters out deleted items
//  c) the deserializers for the cache version are broken (due to a binary version
//  change!)
let loadOplists
  (loadAmount : LoadAmount)
  (canvasID : CanvasID)
  (tlids : List<tlid>)
  : Task<List<tlid * PT.Oplist>> =
  let query =
    // Deleted can be null is which case it is DeletedForever
    match loadAmount with
    | LiveToplevels ->
      "SELECT tlid, oplist FROM toplevel_oplists_v0
          WHERE canvas_id = @canvasID
            AND tlid = ANY(@tlids)
            AND deleted IS FALSE"
    | IncludeDeletedToplevels ->
      // IS NOT NULL just skipped DeletedForever
      "SELECT tlid, oplist FROM toplevel_oplists_v0
          WHERE canvas_id = @canvasID
            AND tlid = ANY(@tlids)
            AND deleted IS NOT NULL"

  Sql.query query
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
  |> Sql.executeAsync (fun read -> (read.tlid "tlid", read.bytea "oplist"))
  |> Task.bind (fun list ->
    list
    |> Task.mapWithConcurrency 2 (fun (tlid, oplist) ->
      task { return (tlid, BinarySerialization.deserializeOplist tlid oplist) }))


// This is a special `load_*` function that specifically loads toplevels via the
// `oplist_cache` column on `toplevel_oplists`. This column stores a
// binary-serialized representation of the toplevel after the oplist is applied. This
// should be much faster because we don't have to ship the full oplist across the
// network from Postgres, and similarly they don't have to apply the full history of
// the canvas in memory before they can execute the code.
/// Loads all cached top-levels given
let loadOnlyCachedTLIDs
  (canvasID : CanvasID)
  (tlids : List<tlid>)
  : Task<List<PT.Toplevel.T>> =
  task {
    // We specifically only load where `deleted` IS FALSE (even though the column is
    // nullable). This means we will not load undeleted handlers from the cache if
    // we've never written their `deleted` state. This is less efficient, but still
    // correct, as they'll still be loaded via their oplist.
    // CLEANUP: the situation described below is no longer possible and the data has
    // been cleaned.
    // It avoids loading deleted handlers that have had their cached version written
    // but never their deleted state, which could be true for some handlers that were
    // touched between the addition of the `oplist_cache` column and the addition of
    // the `deleted` column.
    let! data =
      Sql.query
        "SELECT tlid, oplist_cache FROM toplevel_oplists_v0
          WHERE canvas_id = @canvasID
          AND tlid = ANY (@tlids)
          AND deleted IS FALSE
          AND (
              ((tipe = 'handler'::toplevel_type OR tipe = 'db'::toplevel_type))
              OR tipe = 'user_function'::toplevel_type
              OR tipe = 'user_tipe'::toplevel_type)"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
      |> Sql.executeAsync (fun read -> (read.tlid "tlid", read.bytea "oplist_cache"))

    return
      data
      |> List.map (fun (tlid, tl) -> BinarySerialization.deserializeToplevel tlid tl)
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
     FROM toplevel_oplists_v0
     WHERE canvas_id = @canvasID
       AND (((module = 'HTTP' OR module = 'HTTP_BASIC')
             AND @path like name
             AND modifier = @method)
         OR tipe <> 'handler'::toplevel_type)
       AND deleted IS FALSE"
  |> Sql.parameters [ "path", Sql.string path
                      "method", Sql.string method
                      "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchRelevantTLIDsForExecution (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists_v0
      WHERE canvas_id = @canvasID
      AND tipe <> 'handler'::toplevel_type
      AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchRelevantTLIDsForEvent
  (canvasID : CanvasID)
  (module' : string)
  (name : string)
  (modifier : string)
  : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists_v0
      WHERE canvas_id = @canvasID
        AND ((module = @space
              AND name = @name
              AND modifier = @modifier)
              OR tipe <> 'handler'::toplevel_type)
        AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "space", Sql.string module'
                      "name", Sql.string name
                      "modifier", Sql.string modifier ]
  |> Sql.executeAsync (fun read -> read.id "tlid")


let fetchTLIDsForAllDBs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists_v0
     WHERE canvas_id = @canvasID
       AND tipe = 'db'::toplevel_type
       AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchTLIDsForAllWorkers (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists_v0
     WHERE canvas_id = @canvasID
       AND tipe = 'handler'::toplevel_type
       AND module <> 'CRON'
       AND module <> 'REPL'
       AND module <> 'HTTP'
       AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")


let fetchAllTLIDs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists_v0
     WHERE canvas_id = @canvasID
       AND deleted is NOT NULL"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchAllLiveTLIDs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists_v0
     WHERE canvas_id = @canvasID
       AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

type CronScheduleData =
  { canvasID : CanvasID
    ownerID : UserID
    canvasName : CanvasName.T
    tlid : id
    cronName : string
    interval : PT.Handler.CronInterval }

/// Fetch cron handlers from the DB. Active here means:
/// - a non-null interval field in the spec
/// - not deleted (When a CRON handler is deleted, we set (module, modifier,
///   deleted) to (NULL, NULL, True);  so our query `WHERE module = 'CRON'`
///   ignores deleted CRONs.)
let fetchActiveCrons () : Task<List<CronScheduleData>> =
  Sql.query
    "SELECT canvas_id,
                  tlid,
                  modifier,
                  toplevel_oplists_v0.name as handler_name,
                  canvases_v0.name as canvas_name
       FROM toplevel_oplists_v0
       JOIN canvases_v0 ON toplevel_oplists_v0.canvas_id = canvases_v0.id
      WHERE module = 'CRON'
        AND modifier IS NOT NULL
        AND modifier <> ''
        AND toplevel_oplists_v0.name IS NOT NULL
        AND deleted IS FALSE"
  |> Sql.executeAsync (fun read ->
    let interval = read.string "modifier"
    let canvasID = read.uuid "canvas_id"
    let ownerID = read.uuid "account_id"
    { canvasID = canvasID
      ownerID = ownerID
      canvasName = read.string "canvas_name" |> CanvasName.createExn
      tlid = read.id "tlid"
      cronName = read.string "handler_name"
      interval =
        interval
        |> PTParser.Handler.CronInterval.parse
        |> Exception.unwrapOptionInternal
             "Could not parse cron modifier"
             [ "interval", interval; "canvasID", canvasID; "accountID", ownerID ] })


// -------------------------
// hosts
// -------------------------
let currentHosts () : Task<string list> =
  task {
    let! hosts =
      Sql.query "SELECT DISTINCT name FROM canvases_v0"
      |> Sql.executeAsync (fun read -> read.string "name")
    return
      hosts |> List.filter (fun h -> not (String.startsWith "test-" h)) |> List.sort
  }

let getAllCanvases () : Task<List<CanvasName.T>> =
  currentHosts () |> Task.map (List.map CanvasName.createExn)
