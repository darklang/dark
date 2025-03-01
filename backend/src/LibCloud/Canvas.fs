module LibCloud.Canvas

// Functions related to Canvases

open System.Threading.Tasks
open FSharp.Control.Tasks
open LibCloud.Db

open Prelude

module BinarySerialization = LibBinarySerialization.BinarySerialization
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Telemetry = LibService.Telemetry
module LD = LibService.LaunchDarkly


let createWithExactID
  (id : CanvasID)
  (owner : UserID)
  (domain : string)
  : Task<unit> =
  task {
    do!
      Sql.query
        "INSERT INTO canvases_v0
          (id, account_id)
         VALUES
          (@id, @owner);

         INSERT INTO domains_v0
           (canvas_id, domain)
         VALUES
           (@id, @domain)"
      |> Sql.parameters
        [ "id", Sql.uuid id; "owner", Sql.uuid owner; "domain", Sql.string domain ]
      |> Sql.executeStatementAsync
  }

let create (owner : UserID) (domain : string) : Task<CanvasID> =
  task {
    let id = System.Guid.NewGuid()
    do! createWithExactID id owner domain
    return id
  }

let canvasIDForDomain (domain : string) : Task<Option<CanvasID>> =
  Sql.query
    "SELECT canvas_id
    FROM domains_v0
    WHERE domain = @domain"
  |> Sql.parameters [ "domain", Sql.string domain ]
  |> Sql.executeRowOptionAsync (fun read -> read.uuid "canvas_id")

let domainsForCanvasID (id : CanvasID) : Task<List<string>> =
  Sql.query
    "SELECT domain
    FROM domains_v0
    WHERE canvas_id = @id"
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeAsync (fun read -> read.string "domain")

let addDomain (canvasID : CanvasID) (domain : string) : Task<unit> =
  Sql.query
    "INSERT INTO domains_v0
       (canvas_id, domain)
     VALUES
       (@canvasID, @domain)"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "domain", Sql.string domain ]
  |> Sql.executeStatementAsync

let allCanvasIDs () : Task<List<CanvasID>> =
  Sql.query "SELECT id FROM canvases_v0"
  |> Sql.executeAsync (fun read -> read.uuid "id")


let getOwner (id : CanvasID) : Task<Option<UserID>> =
  Sql.query
    "SELECT account_id
    FROM canvases_v0
    WHERE id = @id"
  |> Sql.parameters [ "id", Sql.uuid id ]
  |> Sql.executeRowOptionAsync (fun read -> read.uuid "account_id")

/// <summary>
/// Canvas data - contains metadata along with basic handlers, DBs, etc.
/// </summary>
/// <remarks>
/// This includes just a subset of the key program data. It is rare that all of
/// the data for a canvas will be loaded. In addition, there is other canvas
/// data which is meaningful, such as creation date. These can be fetched separately.
/// </remarks>
type T =
  { id : CanvasID

    secrets : Map<string, PT.Secret.T>
    handlers : Map<tlid, PT.Handler.T>
    dbs : Map<tlid, PT.DB.T>
    deletedHandlers : Map<tlid, PT.Handler.T>
    deletedDBs : Map<tlid, PT.DB.T> }

let addToplevel (deleted : Serialize.Deleted) (tl : PT.Toplevel.T) (c : T) : T =
  let tlid = PT.Toplevel.toTLID tl

  match deleted, tl with
  | Serialize.NotDeleted, PT.Toplevel.TLHandler h ->
    { c with handlers = Map.add tlid h c.handlers }
  | Serialize.NotDeleted, PT.Toplevel.TLDB db ->
    { c with dbs = Map.add tlid db c.dbs }
  | Serialize.Deleted, PT.Toplevel.TLHandler h ->
    { c with deletedHandlers = Map.add tlid h c.deletedHandlers }
  | Serialize.Deleted, PT.Toplevel.TLDB db ->
    { c with deletedDBs = Map.add tlid db c.deletedDBs }


let addToplevels (tls : List<Serialize.Deleted * PT.Toplevel.T>) (canvas : T) : T =
  List.fold (fun c (deleted, tl) -> addToplevel deleted tl c) canvas tls

let toplevels (c : T) : Map<tlid, PT.Toplevel.T> =
  let map f l = Map.map f l |> Map.toSeq

  [ map PT.Toplevel.TLHandler c.handlers; map PT.Toplevel.TLDB c.dbs ]
  |> Seq.concat
  |> Map

let deletedToplevels (c : T) : Map<tlid, PT.Toplevel.T> =
  let map f l = Map.map f l |> Map.toSeq

  [ map PT.Toplevel.TLHandler c.deletedHandlers; map PT.Toplevel.TLDB c.deletedDBs ]
  |> Seq.concat
  |> Map




// -------------------------
// Toplevel
// -------------------------

let setDB (db : PT.DB.T) (c : T) : T =
  // if the db had been deleted, remove it from the deleted set. This handles
  // a data race where a Set comes in after a Delete.
  { c with
      dbs = Map.add db.tlid db c.dbs
      deletedDBs = Map.remove db.tlid c.deletedDBs }

let deleteDB (tlid : tlid) c =
  match Map.get tlid c.dbs with
  | None -> c
  | Some db ->
    { c with
        dbs = Map.remove db.tlid c.dbs
        deletedDBs = Map.add db.tlid db c.deletedDBs }

let setHandler (h : PT.Handler.T) c =
  // if the handler had been deleted, remove it from the deleted set. This handles
  // a data race where a Set comes in after a Delete.
  { c with
      handlers = Map.add h.tlid h c.handlers
      deletedHandlers = Map.remove h.tlid c.deletedHandlers }

let deleteHandler (tlid : tlid) c =
  match Map.get tlid c.handlers with
  | None -> c
  | Some h ->
    { c with
        handlers = Map.remove h.tlid c.handlers
        deletedHandlers = Map.add h.tlid h c.deletedHandlers }


// CLEANUP Historically, on the backend, toplevel meant handler or DB
// we want to de-conflate the concepts
let deleteToplevel (tlid : tlid) (c : T) : T =
  c |> deleteHandler tlid |> deleteDB tlid

let applyToMap (tlid : tlid) (f : 'a -> 'a) (m : Map<tlid, 'a>) : Map<tlid, 'a> =
  Map.update tlid (Option.map f) m



let applyToDB (f : PT.DB.T -> PT.DB.T) (tlid : tlid) (c : T) : T =
  { c with dbs = applyToMap tlid f c.dbs }



// NOTE: If you add a new verification here, please ensure all places that
// load canvases/apply ops correctly load the requisite data.
let verify (c : T) : T =
  let dupedNames =
    c.dbs
    |> Map.values
    |> List.groupBy _.name
    |> Map.filter (fun db -> List.length db > 1)
    |> Map.keys

  match dupedNames with
  | [] -> c
  | dupes ->
    let dupes = String.concat "," dupes
    Exception.raiseInternal $"Duplicate DB names: {dupes}" []


// -------------------------
//  Loading/saving
//  -------------------------

let empty (id : CanvasID) =
  { id = id
    handlers = Map.empty
    dbs = Map.empty
    deletedHandlers = Map.empty
    deletedDBs = Map.empty
    secrets = Map.empty }

let loadFrom (id : CanvasID) (tlids : List<tlid>) : Task<T> =
  task {
    try
      Telemetry.addTags [ "tlids", tlids ]

      // load
      let! tls = Serialize.loadToplevels id tlids

      let c = empty id

      let! secrets = Secret.getCanvasSecrets id
      let secrets = secrets |> List.map (fun s -> s.name, s) |> Map

      return { c with secrets = secrets } |> addToplevels tls |> verify
    with e when not (LD.knownBroken id) ->
      let tags = [ "tlids", tlids :> obj ]
      return Exception.reraiseAsPageable "canvas load failed" tags e
  }

let loadAll (id : CanvasID) : Task<T> =
  task {
    let! tlids = Serialize.fetchAllIncludingDeletedTLIDs id
    return! loadFrom id tlids
  }

let loadHttpHandlers (id : CanvasID) (path : string) (method : string) : Task<T> =
  task {
    let! tlids = Serialize.fetchReleventTLIDsForHTTP id path method
    return! loadFrom id tlids
  }

let loadTLIDs (id : CanvasID) (tlids : tlid list) : Task<T> = loadFrom id tlids


let loadTLIDsWithContext (id : CanvasID) (tlids : List<tlid>) : Task<T> =
  task {
    let! context = Serialize.fetchRelevantTLIDsForExecution id
    let tlids = tlids @ context
    return! loadFrom id tlids
  }

let loadForEvent
  (id : CanvasID)
  (module' : string)
  (name : string)
  (modifier : string)
  : Task<T> =
  task {
    let! tlids = Serialize.fetchRelevantTLIDsForEvent id module' name modifier
    return! loadFrom id tlids
  }

let loadAllDBs (id : CanvasID) : Task<T> =
  task {
    let! tlids = Serialize.fetchTLIDsForAllDBs id
    return! loadFrom id tlids
  }

/// Returns a best guess at all workers (excludes what it knows not to be a worker)
let loadAllWorkers (id : CanvasID) : Task<T> =
  task {
    let! tlids = Serialize.fetchTLIDsForAllWorkers id
    return! loadFrom id tlids
  }

let loadTLIDsWithDBs (id : CanvasID) (tlids : List<tlid>) : Task<T> =
  task {
    let! dbTLIDs = Serialize.fetchTLIDsForAllDBs id
    return! loadFrom id (tlids @ dbTLIDs)
  }

let getToplevel (tlid : tlid) (c : T) : Option<Serialize.Deleted * PT.Toplevel.T> =
  let handler () =
    Map.find tlid c.handlers
    |> Option.map (fun h -> (Serialize.NotDeleted, PT.Toplevel.TLHandler h))

  let deletedHandler () =
    Map.find tlid c.deletedHandlers
    |> Option.map (fun h -> (Serialize.Deleted, PT.Toplevel.TLHandler h))

  let db () =
    Map.find tlid c.dbs
    |> Option.map (fun h -> (Serialize.NotDeleted, PT.Toplevel.TLDB h))

  let deletedDB () =
    Map.find tlid c.deletedDBs
    |> Option.map (fun h -> (Serialize.Deleted, PT.Toplevel.TLDB h))


  handler ()
  |> Option.orElseWith deletedHandler
  |> Option.orElseWith db
  |> Option.orElseWith deletedDB


let deleteToplevelForever (canvasID : CanvasID) (tlid : tlid) : Task<unit> =
  // CLEANUP: set deleted column in toplevels_v0 to be not nullable
  Sql.query
    "DELETE from toplevels_v0
      WHERE canvas_id = @canvasID
        AND tlid = @tlid"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlid", Sql.id tlid ]
  |> Sql.executeStatementAsync

let toplevelToDBTypeString (tl : PT.Toplevel.T) : string =
  match tl with
  | PT.Toplevel.TLDB _ -> "db"
  | PT.Toplevel.TLHandler _ -> "handler"

/// Save just the TLIDs listed (a canvas may load more tlids to support
/// calling/testing these TLs, even though those TLs do not need to be updated)
let saveTLIDs
  (id : CanvasID)
  (toplevels : List<PT.Toplevel.T * Serialize.Deleted>)
  : Task<unit> =
  try
    // Use ops rather than just set of toplevels, because toplevels may
    // have been deleted or undone, and therefore not appear, but it's
    // important to record them.
    toplevels
    |> Task.iterInParallel (fun (tl, deleted) ->
      task {
        let string2option (s : string) : Option<string> =
          if s = "" then None else Some s

        let tlid = PT.Toplevel.toTLID tl

        let routingNames =
          match tl with
          | PT.Toplevel.TLHandler({ spec = spec }) ->
            match spec with
            | PT.Handler.HTTP _ ->
              Some(
                PTParser.Handler.Spec.toModule spec,
                Routing.routeToPostgresPattern (PTParser.Handler.Spec.toName spec),
                PTParser.Handler.Spec.toModifier spec
              )
            | PT.Handler.Worker _
            | PT.Handler.Cron _
            | PT.Handler.REPL _ ->
              Some(
                PTParser.Handler.Spec.toModule spec,
                PTParser.Handler.Spec.toName spec,
                PTParser.Handler.Spec.toModifier spec
              )
          | PT.Toplevel.TLDB _ -> None

        let (module_, name, modifier) =
          // Only save info used to find handlers when the handler has not been deleted
          if deleted = Serialize.NotDeleted then
            match routingNames with
            | Some(module_, name, modifier) ->
              (string2option module_, string2option name, string2option modifier)
            | None -> None, None, None
          else
            None, None, None

        let serializedToplevel = BinarySerialization.Toplevel.serialize tl

        let deleted =
          match deleted with
          | Serialize.Deleted -> true
          | Serialize.NotDeleted -> false

        return!
          Sql.query
            "INSERT INTO toplevels_v0
              (canvas_id, tlid, digest, tipe, name,
               module, modifier, deleted, data, updated_at)
            VALUES
              (@canvasID, @tlid, @digest, @typ::toplevel_type, @name,
               @module, @modifier, @deleted, @data, NOW())
            ON CONFLICT
              (canvas_id, tlid)
              DO UPDATE
              SET digest = @digest,
                  tipe = @typ::toplevel_type,
                  name = @name,
                  module = @module,
                  modifier = @modifier,
                  deleted = @deleted,
                  data = @data"
          |> Sql.parameters
            [ "canvasID", Sql.uuid id
              "tlid", Sql.id tlid
              "digest", Sql.string "fsharp"
              "typ", Sql.string (toplevelToDBTypeString tl)
              "name", Sql.stringOrNone name
              "module", Sql.stringOrNone module_
              "modifier", Sql.stringOrNone modifier
              "deleted", Sql.bool deleted
              "data", Sql.bytea serializedToplevel ]
          |> Sql.executeStatementAsync
      })
  with e ->
    Exception.reraiseAsPageable "canvas save failed" [ "canvasID", id ] e


type HealthCheckResult =
  Microsoft.Extensions.Diagnostics.HealthChecks.HealthCheckResult

/// Check a set of known domains to ensure that the serializer works before starting up
let loadDomainsHealthCheck
  (_ : System.Threading.CancellationToken)
  : Task<HealthCheckResult> =
  task {
    let healthy = HealthCheckResult.Healthy()
    try
      let! results =
        LibService.LaunchDarkly.healthCheckDomains ()
        |> String.split ","
        |> Task.mapInParallel (fun hostname ->
          task {
            try
              let! id = canvasIDForDomain hostname
              let id =
                Exception.unwrapOptionInternal
                  "canvas host healthcheck probe"
                  [ "domain", hostname ]
                  id
              let _canvas = Serialize.loadToplevels id
              return healthy
            with _ ->
              return
                HealthCheckResult.Unhealthy(
                  $"error loading canvas host healthcheck probe on {hostname}"
                )
          })
      return
        results
        |> List.fold
          (fun prev current ->
            if prev = healthy && current = healthy then healthy else current)
          healthy

    with _ ->
      return
        HealthCheckResult.Unhealthy("error running Canvas host healthcheck probe")
  }

let healthCheck : LibService.Kubernetes.HealthCheck =
  { name = "canvas"
    checkFn = loadDomainsHealthCheck
    probeTypes = [ LibService.Kubernetes.Startup ] }


let toProgram (c : T) : Ply<RT.Program> =
  uply {
    let dbs =
      c.dbs
      |> Map.values
      |> List.map (fun db -> (db.name, PT2RT.DB.toRT db))
      |> Map.ofList

    let secrets = c.secrets |> Map.values |> List.map PT2RT.Secret.toRT

    return
      { canvasID = c.id
        internalFnsAllowed = List.contains c.id Config.allowedDarkInternalCanvasIDs
        dbs = dbs
        secrets = secrets }
  }
