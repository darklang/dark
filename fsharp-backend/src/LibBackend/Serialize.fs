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


let isLatestOpRequest
  (clientOpCtrID : Option<string>)
  (opCtr : int)
  (canvasID : CanvasID)
  : Task<bool> =
  // opctr is used to prevent earlier ops from overwriting later ones
  task {
    let clientOpCtrID =
      match clientOpCtrID with
      | Some ""
      | None -> System.Guid.NewGuid()
      | Some s -> System.Guid.Parse s

    do!
      (Sql.query
        // This is "UPDATE ... WHERE browser_id = $1 AND ctr < $2" except
        // that it also handles the initial case where there is no
        // browser_id record yet
        "INSERT INTO op_ctrs(browser_id, ctr, canvas_id)
           VALUES(@clientOpCtrID, @opCtr, @canvasID)
           ON CONFLICT (browser_id)
           DO UPDATE SET ctr = EXCLUDED.ctr, timestamp = NOW()
           WHERE op_ctrs.ctr < EXCLUDED.ctr"
       |> Sql.parameters [ "clientOpCtrID", Sql.uuid clientOpCtrID
                           "opCtr", Sql.int opCtr
                           "canvasID", Sql.uuid canvasID ]
       |> Sql.executeStatementAsync)

    return!
      Sql.query
        "SELECT TRUE FROM op_ctrs
           WHERE browser_id = @clientOpCtrID
             AND ctr = @opCtr"
      |> Sql.parameters [ "clientOpCtrID", Sql.uuid clientOpCtrID
                          "opCtr", Sql.int opCtr ]
      |> Sql.executeExistsAsync
  }


// --------------------------------------------------------
// Load serialized data from the DB
// --------------------------------------------------------
module OT = LibExecution.OCamlTypes

let deserializeOCamlSerializedToplevel
  (tlid : tlid)
  (typ : string)
  (ocamlSerialized : byte [])
  (pos : Option<string>)
  : Task<PT.Toplevel.T> =
  let deserializePos (pos : Option<string>) : pos =
    try
      pos
      |> Option.map Json.OCamlCompatible.deserialize<pos>
      |> Option.unwrap { x = 0; y = 0 }
    with
    | e -> { x = 0; y = 0 }
  match typ with
  | "db" ->
    task {
      let! json = OCamlInterop.bytesToStringReq "bs/db_bin2json" ocamlSerialized
      return
        json
        |> Json.OCamlCompatible.deserialize<OT.RuntimeT.DbT.db<OT.RuntimeT.fluidExpr>>
        |> OT.Convert.ocamlDB2PT (deserializePos pos)
        |> PT.Toplevel.TLDB
    }
  | "handler" ->
    task {
      let! json = OCamlInterop.bytesToStringReq "bs/handler_bin2json" ocamlSerialized
      return
        json
        |> Json.OCamlCompatible.deserialize<OT.RuntimeT.HandlerT.handler<OT.RuntimeT.fluidExpr>>
        |> OT.Convert.ocamlHandler2PT (deserializePos pos)
        |> PT.Toplevel.TLHandler
    }
  | "user_tipe" ->
    task {
      let! json =
        OCamlInterop.bytesToStringReq "bs/user_tipe_bin2json" ocamlSerialized
      return
        json
        |> Json.OCamlCompatible.deserialize<OT.RuntimeT.user_tipe>
        |> OT.Convert.ocamlUserType2PT
        |> PT.Toplevel.TLType
    }
  | "user_function" ->
    task {
      let! json = OCamlInterop.bytesToStringReq "bs/user_fn_bin2json" ocamlSerialized
      return
        json
        |> Json.OCamlCompatible.deserialize<OT.RuntimeT.user_fn<OT.RuntimeT.fluidExpr>>
        |> OT.Convert.ocamlUserFunction2PT
        |> PT.Toplevel.TLFunction
    }
  | _ ->
    Exception.raiseInternal "Invalid tipe for toplevel" [ "type", typ; "tlid", tlid ]

/// Save the oplist using the F# serialization formats, after reading the oplist from
/// OCaml. Takes the existing OCaml value to ensure things are in sync - if they
/// aren't, logs but doesn't save or error (because things can continue just fine).
let saveOplistToFSharpCache
  (canvasID : CanvasID)
  (tlid : tlid)
  (ocamlSerializedBytes : byte [])
  (oplist : PT.Oplist)
  : Task<unit> =
  task {
    let serialized = BinarySerialization.serializeOplist tlid oplist
    let! rowUpdateCount =
      Sql.query
        "UPDATE toplevel_oplists
         SET oplist = @oplist
         WHERE canvas_id = @canvasID
           AND tlid = @tlid
           AND data = @ocamlOplist"
      |> Sql.parameters [ "oplist", Sql.bytea serialized
                          "canvasID", Sql.uuid canvasID
                          "tlid", Sql.id tlid
                          // There might have been writes since then, so don't update
                          // unless it exactly matches what we expect it to have,
                          // otherwise we might overwrite other writes.
                          "ocamlOplist", Sql.bytea ocamlSerializedBytes ]
      |> Sql.executeNonQueryAsync
    match rowUpdateCount with
    | 1 -> ()
    | 0 ->
      Telemetry.addEvent
        "Oplist was not updated"
        [ "tlid", tlid; "canvasID", canvasID ]
    | _ ->
      Telemetry.addEvent
        "More than 1 row of oplists was updated, that's bad"
        [ "tlid", tlid; "canvasID", canvasID ]
    return ()
  }

/// Save the F# toplevel suing the F# serialization format, after reading it from
/// OCaml. Takes the existing OCaml value to ensure things are in sync - if they
/// aren't, logs but doesn't save or error (because things can continue just fine).
let saveToplevelToFSharpCache
  (canvasID : CanvasID)
  (ocamlSerializedBytes : byte [])
  (tl : PT.Toplevel.T)
  : Task<unit> =
  task {
    let tlid = PT.Toplevel.toTLID tl
    let serialized = BinarySerialization.serializeToplevel tl
    let! rowUpdateCount =
      Sql.query
        "UPDATE toplevel_oplists
         SET oplist_cache = @oplistCache
         WHERE canvas_id = @canvasID
           AND tlid = @tlid
           AND rendered_oplist_cache = @renderedOplistCache"
      |> Sql.parameters [ "oplistCache", Sql.bytea serialized
                          "canvasID", Sql.uuid canvasID
                          "tlid", Sql.id tlid
                          // There might have been writes since then, so don't update
                          // unless it exactly matches what we expect it to have,
                          // otherwise we might overwrite other writes.
                          "renderedOplistCache", Sql.bytea ocamlSerializedBytes ]
      |> Sql.executeNonQueryAsync
    match rowUpdateCount with
    | 1 -> ()
    | 0 ->
      Telemetry.addEvent "Row was not updated" [ "tlid", tlid; "canvasID", canvasID ]
    | _ ->
      Telemetry.addEvent
        "More than 1 row was updated, that's bad"
        [ "tlid", tlid; "canvasID", canvasID ]
    return ()
  }

/// Save the F# oplist and cached oplist, after reading them from OCaml. Takes the
/// existing OCaml value to ensure things are in sync - if they aren't, logs but
/// doesn't save or error (because things can continue just fine).
let cacheOplists
  (canvasID : CanvasID)
  (ocamlSerializedBytes : byte [])
  (tl : PT.Toplevel.T)
  : Task<unit> =
  task {
    let tlid = PT.Toplevel.toTLID tl
    let serialized = BinarySerialization.serializeToplevel tl
    let! rowUpdateCount =
      Sql.query
        "UPDATE toplevel_oplists
         SET oplist_cache = @oplist_cache
         WHERE
          canvas_id = @canvasID,
          tlid = @tlid,
          rendered_oplist_cache = @renderedOplistCache"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                          "tlid", Sql.id tlid
                          // There might have been writes since then, so don't update
                          // unless it exactly matches what we expect it to have,
                          // otherwise we might overwrite other writes.
                          "renderedOplistCache", Sql.bytea ocamlSerializedBytes
                          "oplistCache", Sql.bytea serialized ]
      |> Sql.executeNonQueryAsync
    match rowUpdateCount with
    | 1 -> ()
    | 0 ->
      Telemetry.addEvent "Row was not updated" [ "tlid", tlid; "canvasID", canvasID ]
    | _ ->
      Telemetry.addEvent
        "More than 1 row was updated, that's bad"
        [ "tlid", tlid; "canvasID", canvasID ]
    return ()
  }


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
    // CLEANUP stop loading ocaml data
    // Deleted can be null is which case it is DeletedForever
    match loadAmount with
    | LiveToplevels ->
      "SELECT tlid, data, oplist FROM toplevel_oplists
          WHERE canvas_id = @canvasID
            AND tlid = ANY(@tlids)
            AND deleted IS FALSE"
    | IncludeDeletedToplevels ->
      // IS NOT NULL just skipped DeletedForever
      "SELECT tlid, data, oplist FROM toplevel_oplists
          WHERE canvas_id = @canvasID
            AND tlid = ANY(@tlids)
            AND deleted IS NOT NULL"

  Sql.query query
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
  |> Sql.executeAsync (fun read ->
    (read.tlid "tlid", read.bytea "data", read.byteaOrNone "oplist"))
  |> Task.bind (fun list ->
    list
    |> Task.mapWithConcurrency 2 (fun (tlid, ocamlSerialized, fsharpSerialized) ->
      task {
        match fsharpSerialized with
        | Some oplist ->
          return (tlid, BinarySerialization.deserializeOplist tlid oplist)
        | None ->
          let! oplist = OCamlInterop.oplistOfBinary ocamlSerialized
          do! saveOplistToFSharpCache canvasID tlid ocamlSerialized oplist
          return (tlid, oplist)
      }))


// This is a special `load_*` function that specifically loads toplevels
// via the `rendered_oplist_cache` column on `toplevel_oplists`. This column
// stores a binary-serialized representation of the toplevel after the oplist
// is applied. This should be much faster because we don't have to ship
// the full oplist across the network from Postgres to the OCaml boxes,
// and similarly they don't have to apply the full history of the canvas
// in memory before they can execute the code.
/// Loads all cached top-levels given
let loadOnlyRenderedTLIDs
  (canvasID : CanvasID)
  (tlids : List<tlid>)
  : Task<List<PT.Toplevel.T>> =
  task {
    // We specifically only load where `deleted` IS FALSE (even though the column
    // is nullable). This means we will not load undeleted handlers from the
    // cache if we've never written their `deleted` state. This is less
    // efficient, but still correct, as they'll still be loaded via their oplist.
    // It avoids loading deleted handlers that have had their cached version
    // written but never their deleted state, which could be true for some
    // handlers that were touched between the addition of the
    // `rendered_oplist_cache` column and the addition of the `deleted` column.
    let! data =
      Sql.query
        "SELECT tlid, tipe, rendered_oplist_cache, oplist_cache, pos FROM toplevel_oplists
          WHERE canvas_id = @canvasID
          AND tlid = ANY (@tlids)
          AND deleted IS FALSE
          AND (
              ((tipe = 'handler'::toplevel_type OR tipe = 'db'::toplevel_type)
                AND pos IS NOT NULL)
              OR tipe = 'user_function'::toplevel_type
              OR tipe = 'user_tipe'::toplevel_type)"
      |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
      |> Sql.executeAsync (fun read ->
        (read.int64 "tlid" |> uint64,
         read.string "tipe",
         // CLEANUP stop reading ocaml data
         read.bytea "rendered_oplist_cache",
         read.byteaOrNone "oplist_cache",
         read.stringOrNone "pos"))

    // At this point, we need to deserialize the binary data. Some binary data will
    // be serialized using the ocaml serializers and some using the F# serializers.
    //
    // For data that is serialized with the ocaml serializers, we need to send it to
    // the legacy server. However, when sending all of the requests in parallel, we
    // accidentally timeout some http requests while parsing the json that they
    // return. This happened a lot for bigger canvases.
    //
    // Ideally we would make one http request to do that, but we would need to send
    // over some text (type, pos, etc) along with binary data, and then put in some
    // structure so we can parse it at the other end, which seems very hard.
    //
    // Instead we just limit it to 2 threads at a time.
    let semaphore = new System.Threading.SemaphoreSlim(2)
    return!
      data
      |> Task.mapInParallel
        (fun (tlid, typ, ocamlSerialized, fsharpSerialized, pos) ->
          match fsharpSerialized with
          | Some tl ->
            Task.FromResult(BinarySerialization.deserializeToplevel tlid tl)
          | None ->
            Task.execWithSemaphore
              semaphore
              (fun () ->
                task {
                  let! deserialized =
                    deserializeOCamlSerializedToplevel tlid typ ocamlSerialized pos
                  do! saveToplevelToFSharpCache canvasID ocamlSerialized deserialized
                  return deserialized
                })
              ())
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
     FROM toplevel_oplists
     WHERE canvas_id = @canvasID
       AND ((module = 'HTTP'
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
    "SELECT tlid FROM toplevel_oplists
      WHERE canvas_id = @canvasID
      AND tipe <> 'handler'::toplevel_type
      AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchRelevantTLIDsForEvent
  (canvasID : CanvasID)
  (event : EventQueue.T)
  : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists
      WHERE canvas_id = @canvasID
        AND ((module = @space
              AND name = @name
              AND modifier = @modifier)
              OR tipe <> 'handler'::toplevel_type)
        AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID
                      "space", Sql.string event.space
                      "name", Sql.string event.name
                      "modifier", Sql.string event.modifier ]
  |> Sql.executeAsync (fun read -> read.id "tlid")


let fetchTLIDsForAllDBs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists
     WHERE canvas_id = @canvasID
       AND tipe = 'db'::toplevel_type
       AND deleted IS FALSE"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchTLIDsForAllWorkers (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists
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
    "SELECT tlid FROM toplevel_oplists
     WHERE canvas_id = @canvasID
       AND deleted is NOT NULL"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read -> read.tlid "tlid")

let fetchAllLiveTLIDs (canvasID : CanvasID) : Task<List<tlid>> =
  Sql.query
    "SELECT tlid FROM toplevel_oplists
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
                  toplevel_oplists.name as handler_name,
                  toplevel_oplists.account_id,
                  canvases.name as canvas_name
       FROM toplevel_oplists
       JOIN canvases ON toplevel_oplists.canvas_id = canvases.id
      WHERE module = 'CRON'
        AND modifier IS NOT NULL
        AND modifier <> ''
        AND toplevel_oplists.name IS NOT NULL
        AND deleted IS FALSE"
  |> Sql.executeAsync (fun read ->
    let interval = read.string "modifier"
    let canvasID = read.uuid "canvas_id"
    let ownerID = read.uuid "account_id"
    { canvasID = canvasID
      ownerID = ownerID
      canvasName = read.string "canvas_name" |> CanvasName.create
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
      Sql.query "SELECT DISTINCT name FROM canvases"
      |> Sql.executeAsync (fun read -> read.string "name")
    return
      hosts |> List.filter (fun h -> not (String.startsWith "test-" h)) |> List.sort
  }

let getAllCanvases () : Task<List<CanvasName.T>> =
  currentHosts () |> Task.map (List.map CanvasName.create)

let tierOneHosts () : List<CanvasName.T> =
  [ "ian-httpbin"
    "paul-slackermuse"
    "listo"
    "ellen-battery2"
    "julius-tokimeki-unfollow" ]
  |> List.map CanvasName.create
