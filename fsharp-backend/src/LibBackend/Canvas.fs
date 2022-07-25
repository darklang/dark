module LibBackend.Canvas

// Functions related to Canvases

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth
open LibService.Exception

module BinarySerialization = LibBinarySerialization.BinarySerialization
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module OT = LibExecution.OCamlTypes
module Telemetry = LibService.Telemetry


type Meta = { name : CanvasName.T; id : CanvasID; owner : UserID }

let canvasIDForCanvasName
  (owner : UserID)
  (canvasName : CanvasName.T)
  : Task<CanvasID> =
  // https://stackoverflow.com/questions/15939902/is-select-or-insert-in-a-function-prone-to-race-conditions/15950324#15950324
  // TODO: we create the canvas if it doesn't exist here, seems like a poor choice
  Sql.query "SELECT canvas_id(@newUUID, @owner, @canvasName)"
  |> Sql.parameters [ "newUUID", Sql.uuid (System.Guid.NewGuid())
                      "owner", Sql.uuid owner
                      "canvasName", Sql.string (string canvasName) ]
  |> Sql.executeRowAsync (fun read -> read.uuid "canvas_id")

/// Fetch high-level metadata for a canvas
let getMetaAndCreate (canvasName : CanvasName.T) : Task<Meta> =
  task {
    let ownerName = (Account.ownerNameFromCanvasName canvasName).toUserName ()
    let! ownerID = Account.userIDForUserName ownerName
    let! canvasID = canvasIDForCanvasName ownerID canvasName
    return { id = canvasID; owner = ownerID; name = canvasName }
  }

/// Get the metadata for a canvas _without_ creating the canvas if it doesn't exist.
/// We want to use this in nearly all cases, with the only exception being when a
/// user enters a URL to open an editor for a canvas.
let getMeta (canvasName : CanvasName.T) : Task<Option<Meta>> =
  Sql.query "SELECT id, account_id from canvases where name = @canvasName"
  |> Sql.parameters [ "canvasName", Sql.string (string canvasName) ]
  |> Sql.executeRowOptionAsync (fun read ->
    { id = read.uuid "id"; owner = read.uuid "account_id"; name = canvasName })

/// Get the metadata for a canvas _without_ creating the canvas if it doesn't exist.
/// We want to use this in nearly all cases, with the only exception being when a
/// user enters a URL to open an editor for a canvas. Throws if the canvas does not exist.
let getMetaExn (canvasName : CanvasName.T) : Task<Meta> =
  task {
    let! option = getMeta canvasName
    return
      Exception.unwrapOptionInternal
        "getMetaExn expected to find a canvas"
        [ "canvasName", canvasName ]
        option
  }


let getMetaForCustomDomain (customDomain : string) : Task<Option<Meta>> =
  Sql.query
    "SELECT c.id, c.account_id, c.name
           FROM canvases c, custom_domains d
          WHERE d.canvas = c.name
            AND d.host = @customDomain"
  |> Sql.parameters [ "customDomain", Sql.string customDomain ]
  |> Sql.executeRowOptionAsync (fun read ->
    { id = read.uuid "id"
      owner = read.uuid "account_id"
      name = CanvasName.createExn (read.string "name") })


let getMetaFromID (id : CanvasID) : Task<Meta> =
  Sql.query "SELECT name, account_id FROM canvases WHERE id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid id ]
  |> Sql.executeRowAsync (fun read ->
    { id = id
      owner = read.uuid "account_id"
      name = read.string "name" |> CanvasName.createExn })

/// <summary>
/// Canvas data - contains metadata along with basic handlers, DBs, etc.
/// </summary>
/// <remarks>
/// This includes just a subset of the key program data. It is rare that all of
/// the data for a canvas will be loaded. In addition, there is other canvas
/// data which is meaningful, such as CORS info, oplists, creation date. These
/// can be fetched separately. (Oplists in particular are omitted as it can be
/// very tricky to pass this data around safely (esp in regards to loading and
/// saving).)
/// </remarks>
type T =
  { meta : Meta
    handlers : Map<tlid, PT.Handler.T>
    dbs : Map<tlid, PT.DB.T>
    userFunctions : Map<tlid, PT.UserFunction.T>
    userTypes : Map<tlid, PT.UserType.T>
    // TODO: no separate fields for deleted, combine them
    deletedHandlers : Map<tlid, PT.Handler.T>
    deletedDBs : Map<tlid, PT.DB.T>
    deletedUserFunctions : Map<tlid, PT.UserFunction.T>
    deletedUserTypes : Map<tlid, PT.UserType.T>
    secrets : Map<string, PT.Secret.T> }

let addToplevel (tl : PT.Toplevel.T) (c : T) : T =
  let tlid = PT.Toplevel.toTLID tl

  match tl with
  | PT.Toplevel.TLHandler h -> { c with handlers = Map.add tlid h c.handlers }
  | PT.Toplevel.TLDB db -> { c with dbs = Map.add tlid db c.dbs }
  | PT.Toplevel.TLType t -> { c with userTypes = Map.add tlid t c.userTypes }
  | PT.Toplevel.TLFunction f ->
    { c with userFunctions = Map.add tlid f c.userFunctions }

let addToplevels (tls : PT.Toplevel.T list) (canvas : T) : T =
  List.fold canvas (fun c tl -> addToplevel tl c) tls

let toplevels (c : T) : Map<tlid, PT.Toplevel.T> =
  let map f l = Map.map f l |> Map.toSeq

  [ map PT.Toplevel.TLHandler c.handlers
    map PT.Toplevel.TLDB c.dbs
    map PT.Toplevel.TLType c.userTypes
    map PT.Toplevel.TLFunction c.userFunctions ]
  |> Seq.concat
  |> Map

let deletedToplevels (c : T) : Map<tlid, PT.Toplevel.T> =
  let map f l = Map.map f l |> Map.toSeq

  [ map PT.Toplevel.TLHandler c.deletedHandlers
    map PT.Toplevel.TLDB c.deletedDBs
    map PT.Toplevel.TLType c.deletedUserTypes
    map PT.Toplevel.TLFunction c.deletedUserFunctions ]
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

let setFunction (f : PT.UserFunction.T) (c : T) : T =
  // if the fn had been deleted, remove it from the deleted set. This handles
  // a data race where a Set comes in after a Delete.
  { c with
      userFunctions = Map.add f.tlid f c.userFunctions
      deletedUserFunctions = Map.remove f.tlid c.deletedUserFunctions }

let setType (t : PT.UserType.T) (c : T) : T =
  // if the tipe had been deleted, remove it from the deleted set. This handles
  // a data race where a Set comes in after a Delete.
  { c with
      userTypes = Map.add t.tlid t c.userTypes
      deletedUserTypes = Map.remove t.tlid c.deletedUserTypes }

let deleteFunction (tlid : tlid) (c : T) : T =
  match Map.get tlid c.userFunctions with
  | None -> c
  | Some f ->
    { c with
        userFunctions = Map.remove tlid c.userFunctions
        deletedUserFunctions = Map.add tlid f c.deletedUserFunctions }

let deleteType (tlid : tlid) (c : T) : T =
  match Map.get tlid c.userTypes with
  | None -> c
  | Some t ->
    { c with
        userTypes = Map.remove tlid c.userTypes
        deletedUserTypes = Map.add tlid t c.deletedUserTypes }

// CLEANUP Historically, on the backend, toplevel meant handler or DB
// we want to de-conflate the concepts
let deleteToplevel (tlid : tlid) (c : T) : T =
  c |> deleteHandler tlid |> deleteDB tlid

let applyToMap (tlid : tlid) (f : 'a -> 'a) (m : Map<tlid, 'a>) : Map<tlid, 'a> =
  Map.update tlid (Option.map f) m



let applyToDB (f : PT.DB.T -> PT.DB.T) (tlid : tlid) (c : T) : T =
  { c with dbs = applyToMap tlid f c.dbs }


let moveToplevel (tlid : tlid) (pos : PT.Position) (c : T) : T =
  { c with
      handlers = applyToMap tlid (fun h -> { h with pos = pos }) c.handlers
      dbs = applyToMap tlid (fun db -> { db with pos = pos }) c.dbs }

// -------------------------
// Build
// -------------------------
let applyOp (isNew : bool) (op : PT.Op) (c : T) : T =
  try
    match op with
    | PT.SetHandler (_, _, h) -> setHandler h c
    | PT.CreateDB (tlid, pos, name) ->
      if isNew && name = "" then Exception.raiseEditor "DB must have a name"
      let db = UserDB.create tlid name pos
      setDB db c
    | PT.AddDBCol (tlid, colid, typeid) ->
      applyToDB (UserDB.addCol colid typeid) tlid c
    | PT.SetDBColName (tlid, id, name) ->
      applyToDB (UserDB.setColName id name) tlid c
    | PT.ChangeDBColName (tlid, id, name) ->
      applyToDB (UserDB.setColName id name) tlid c
    | PT.SetDBColType (tlid, id, tipe) ->
      let typ =
        PTParser.DType.parse tipe
        |> Exception.unwrapOptionInternal "Cannot parse col type" [ "type", tipe ]
      applyToDB (UserDB.setColType id typ) tlid c
    | PT.ChangeDBColType (tlid, id, tipe) ->
      let typ =
        PTParser.DType.parse tipe
        |> Exception.unwrapOptionInternal "Cannot parse col type" [ "type", tipe ]
      applyToDB (UserDB.setColType id typ) tlid c
    | PT.DeleteDBCol (tlid, id) -> applyToDB (UserDB.deleteCol id) tlid c
    | PT.SetExpr (tlid, id, e) ->
      // Only implemented for DBs for now, and we don't support rollbacks/rollforwards yet
      // applyToAllToplevels (TL.set_expr id e) tlid c
      c
    | PT.DeleteTL tlid -> deleteToplevel tlid c
    | PT.MoveTL (tlid, pos) -> moveToplevel tlid pos c
    | PT.SetFunction user_fn -> setFunction user_fn c
    | PT.DeleteFunction tlid -> deleteFunction tlid c
    | PT.TLSavepoint _ -> c
    | PT.UndoTL _
    | PT.RedoTL _ ->
      Exception.raiseInternal
        "Undo/Redo op should have been preprocessed out!"
        [ "op", op ]
    | PT.RenameDBname (tlid, name) -> applyToDB (UserDB.renameDB name) tlid c
    | PT.CreateDBWithBlankOr (tlid, pos, id, name) ->
      setDB (UserDB.create2 tlid name pos id) c
    | PT.SetType t -> setType t c
    | PT.DeleteType tlid -> deleteType tlid c
  with
  | e ->
    // Log here so we have context, but then re-raise
    let tags = [ ("canvas_name", c.meta.name :> obj); ("op", string op) ]
    Telemetry.addException tags (InternalException("apply_op", e))
    e.Reraise()


// NOTE: If you add a new verification here, please ensure all places that
// load canvases/apply ops correctly load the requisite data.
//
// See `Op.RequiredContext` for how we determine which ops need what other
// context to be loaded to appropriately verify.
let verify (c : T) : T =
  let dupedNames =
    c.dbs
    |> Map.values
    |> List.groupBy (fun db -> db.name)
    |> Map.filter (fun db -> List.length db > 1)
    |> Map.keys

  match dupedNames with
  | [] -> c
  | dupes ->
    let dupes = String.concat "," dupes
    Exception.raiseInternal $"Duplicate DB names: {dupes}" []


let addOps (oldops : PT.Oplist) (newops : PT.Oplist) (c : T) : T =
  let oldops = List.map (fun op -> (false, op)) oldops
  let newops = List.map (fun op -> (true, op)) newops
  let reducedOps = Undo.preprocess (oldops @ newops)
  List.fold c (fun c (isNew, op) -> applyOp isNew op c) reducedOps


let canvasCreationDate (canvasID : CanvasID) : Task<NodaTime.Instant> =
  Sql.query "SELECT created_at from canvases WHERE id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowAsync (fun read -> read.instantWithoutTimeZone "created_at")

let urlFor (canvasName : CanvasName.T) : string =
  $"https://{canvasName}.{Config.publicDomain}"


// -------------------------
//  Loading/saving
//  -------------------------

let empty (meta : Meta) : T =
  { meta = meta
    handlers = Map.empty
    dbs = Map.empty
    userFunctions = Map.empty
    userTypes = Map.empty
    deletedHandlers = Map.empty
    deletedDBs = Map.empty
    deletedUserFunctions = Map.empty
    deletedUserTypes = Map.empty
    secrets = Map.empty }

// DOES NOT LOAD OPS FROM DB
let fromOplist (meta : Meta) (oldOps : PT.Oplist) (newOps : PT.Oplist) : T =
  empty meta |> addOps oldOps newOps |> verify

let knownBrokenCanvases : Set<CanvasName.T> =
  [ "danbowles"
    "danwetherald"
    "ellen-dbproblem18"
    "ellen-dbtests"
    "ellen-preview"
    "ellen-stltrialrun"
    "ellen-trinity"
    "jaeren_sl"
    "jaeren_sl-crud"
    "sydney" ]
  |> List.map CanvasName.createExn
  |> Set

let loadFrom
  (loadAmount : Serialize.LoadAmount)
  (meta : Meta)
  (tlids : List<tlid>)
  : Task<T> =
  task {
    try
      Telemetry.addTags [ "tlids", tlids; "loadAmount", loadAmount ]

      // load
      let! fastLoadedTLs = Serialize.loadOnlyCachedTLIDs meta.id tlids

      let fastLoadedTLIDs = List.map PT.Toplevel.toTLID fastLoadedTLs

      let notLoadedTLIDs =
        List.filter (fun x -> not (List.includes x fastLoadedTLIDs)) tlids

      // canvas initialized via the normal loading path with the non-fast loaded tlids
      // loaded traditionally via the oplist
      let! uncachedOplists = Serialize.loadOplists loadAmount meta.id notLoadedTLIDs
      let uncachedOplists = uncachedOplists |> List.map Tuple2.second |> List.concat
      let c = empty meta

      let! secrets = Secret.getCanvasSecrets meta.id
      let secrets = secrets |> List.map (fun s -> s.name, s) |> Map

      return
        { c with secrets = secrets }
        |> addToplevels fastLoadedTLs
        |> addOps uncachedOplists []
        |> verify
    with
    | e when not (Set.contains meta.name knownBrokenCanvases) ->
      let tags =
        [ "canvasName", meta.name :> obj; "tlids", tlids; "loadAmount", loadAmount ]
      return Exception.reraiseAsPageable "canvas load failed" tags e
  }

let loadAll (meta : Meta) : Task<T> =
  task {
    let! tlids = Serialize.fetchAllTLIDs meta.id
    return! loadFrom Serialize.IncludeDeletedToplevels meta tlids
  }

let loadHttpHandlers (meta : Meta) (path : string) (method : string) : Task<T> =
  task {
    let! tlids = Serialize.fetchReleventTLIDsForHTTP meta.id path method
    return! loadFrom Serialize.LiveToplevels meta tlids
  }

let loadTLIDs (meta : Meta) (tlids : tlid list) : Task<T> =
  loadFrom Serialize.LiveToplevels meta tlids


let loadTLIDsWithContext (meta : Meta) (tlids : List<tlid>) : Task<T> =
  task {
    let! context = Serialize.fetchRelevantTLIDsForExecution meta.id
    let tlids = tlids @ context
    return! loadFrom Serialize.LiveToplevels meta tlids
  }

let loadForEventV2
  (meta : Meta)
  (module' : string)
  (name : string)
  (modifier : string)
  : Task<T> =
  task {
    let! tlids = Serialize.fetchRelevantTLIDsForEvent meta.id module' name modifier
    return! loadFrom Serialize.LiveToplevels meta tlids
  }

let loadAllDBs (meta : Meta) : Task<T> =
  task {
    let! tlids = Serialize.fetchTLIDsForAllDBs meta.id
    return! loadFrom Serialize.LiveToplevels meta tlids
  }

/// Returns a best guess at all workers (excludes what it knows not to be a worker)
let loadAllWorkers (meta : Meta) : Task<T> =
  task {
    let! tlids = Serialize.fetchTLIDsForAllWorkers meta.id
    return! loadFrom Serialize.LiveToplevels meta tlids
  }

let loadTLIDsWithDBs (meta : Meta) (tlids : List<tlid>) : Task<T> =
  task {
    let! dbTLIDs = Serialize.fetchTLIDsForAllDBs meta.id
    return! loadFrom Serialize.LiveToplevels meta (tlids @ dbTLIDs)
  }

type Deleted =
  | Deleted
  | NotDeleted

let getToplevel (tlid : tlid) (c : T) : Option<Deleted * PT.Toplevel.T> =
  let handler () =
    Map.tryFind tlid c.handlers
    |> Option.map (fun h -> (NotDeleted, PT.Toplevel.TLHandler h))

  let deletedHandler () =
    Map.tryFind tlid c.deletedHandlers
    |> Option.map (fun h -> (Deleted, PT.Toplevel.TLHandler h))

  let db () =
    Map.tryFind tlid c.dbs |> Option.map (fun h -> (NotDeleted, PT.Toplevel.TLDB h))

  let deletedDB () =
    Map.tryFind tlid c.deletedDBs
    |> Option.map (fun h -> (Deleted, PT.Toplevel.TLDB h))

  let userFunction () =
    Map.tryFind tlid c.userFunctions
    |> Option.map (fun h -> (NotDeleted, PT.Toplevel.TLFunction h))

  let deletedUserFunction () =
    Map.tryFind tlid c.deletedUserFunctions
    |> Option.map (fun h -> (Deleted, PT.Toplevel.TLFunction h))

  let userType () =
    Map.tryFind tlid c.userTypes
    |> Option.map (fun h -> (NotDeleted, PT.Toplevel.TLType h))

  let deletedUserType () =
    Map.tryFind tlid c.deletedUserTypes
    |> Option.map (fun h -> (Deleted, PT.Toplevel.TLType h))

  handler ()
  |> Option.orElseWith deletedHandler
  |> Option.orElseWith db
  |> Option.orElseWith deletedDB
  |> Option.orElseWith userFunction
  |> Option.orElseWith deletedUserFunction
  |> Option.orElseWith userType
  |> Option.orElseWith deletedUserType

let deleteToplevelForever (meta : Meta) (tlid : tlid) : Task<unit> =
  // CLEANUP: set deleted column in toplevel_oplists to be not nullable
  Sql.query
    "DELETE from toplevel_oplists
      WHERE canvas_id = @canvasID
        AND account_id = @accountID
        AND tlid = @tlid"
  |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                      "accountID", Sql.uuid meta.owner
                      "tlid", Sql.id tlid ]
  |> Sql.executeStatementAsync


/// Save just the TLIDs listed (a canvas may load more tlids to support
/// calling/testing these TLs, even though those TLs do not need to be updated)
let saveTLIDs
  (meta : Meta)
  (oplists : List<tlid * PT.Oplist * PT.Toplevel.T * Deleted>)
  : Task<unit> =
  try
    // Use ops rather than just set of toplevels, because toplevels may
    // have been deleted or undone, and therefore not appear, but it's
    // important to record them.
    oplists
    |> Task.iterInParallel (fun (tlid, oplist, tl, deleted) ->
      task {
        let string2option (s : string) : Option<string> =
          if s = "" then None else Some s

        let routingNames =
          match tl with
          | PT.Toplevel.TLHandler ({ spec = spec }) ->
            match spec with
            | PT.Handler.HTTP _
            | PT.Handler.HTTPBytes _ ->
              Some(
                PTParser.Handler.Spec.toModule spec,
                Routing.routeToPostgresPattern (PTParser.Handler.Spec.toName spec),
                PTParser.Handler.Spec.toModifier spec
              )
            | PT.Handler.Worker _
            | PT.Handler.OldWorker _
            | PT.Handler.Cron _
            | PT.Handler.UnknownHandler _
            | PT.Handler.REPL _ ->
              Some(
                PTParser.Handler.Spec.toModule spec,
                PTParser.Handler.Spec.toName spec,
                PTParser.Handler.Spec.toModifier spec
              )
          | PT.Toplevel.TLDB _
          | PT.Toplevel.TLType _
          | PT.Toplevel.TLFunction _ -> None

        let (module_, name, modifier) =
          // Only save info used to find handlers when the handler has not been deleted
          if deleted = NotDeleted then
            match routingNames with
            | Some (module_, name, modifier) ->
              (string2option module_, string2option name, string2option modifier)
            | None -> None, None, None
          else
            None, None, None

        let pos =
          match tl with
          | PT.Toplevel.TLHandler ({ pos = pos })
          | PT.Toplevel.TLDB { pos = pos } -> Some(Json.Vanilla.serialize pos)
          | PT.Toplevel.TLType _ -> None
          | PT.Toplevel.TLFunction _ -> None

        let serializedOplist = BinarySerialization.serializeOplist tlid oplist
        let serializedOplistCache = BinarySerialization.serializeToplevel tl

        let deleted =
          match deleted with
          | Deleted -> true
          | NotDeleted -> false

        return!
          Sql.query
            "INSERT INTO toplevel_oplists
                    (canvas_id, account_id, tlid, digest, tipe, name, module, modifier,
                     deleted, pos, oplist, oplist_cache, data, rendered_oplist_cache)
                    VALUES (@canvasID, @accountID, @tlid, @digest, @typ::toplevel_type, @name,
                            @module, @modifier, @deleted, @pos,
                            @oplist, @oplistCache,
                            @ocamlData, @ocamlOplistCache)
                    ON CONFLICT (canvas_id, tlid) DO UPDATE
                    SET account_id = @accountID,
                        digest = @digest,
                        tipe = @typ::toplevel_type,
                        name = @name,
                        module = @module,
                        modifier = @modifier,
                        deleted = @deleted,
                        pos = @pos,
                        oplist = @oplist,
                        oplist_cache = @oplistCache,
                        data = @ocamlData,
                        rendered_oplist_cache = @ocamlOplistCache"
          |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                              "accountID", Sql.uuid meta.owner
                              "tlid", Sql.id tlid
                              "digest", Sql.string "fsharp"
                              "typ", Sql.string (PTParser.Toplevel.toDBTypeString tl)
                              "name", Sql.stringOrNone name
                              "module", Sql.stringOrNone module_
                              "modifier", Sql.stringOrNone modifier
                              "deleted", Sql.bool deleted
                              "pos", Sql.jsonbOrNone pos
                              "oplist", Sql.bytea serializedOplist
                              "oplistCache", Sql.bytea serializedOplistCache
                              "ocamlData", Sql.bytea [||]
                              "ocamlOplistCache", Sql.bytea [||] ]
          |> Sql.executeStatementAsync
      })
  with
  | e ->
    Exception.reraiseAsPageable "canvas save failed" [ "canvasName", meta.name ] e


// -------------------------
// Testing/validation
// -------------------------

let jsonFilename (name : string) = $"{name}.json"

/// Load a json-serialized canvas from disk
///
/// If file not found, returns empty oplist
let tryLoadJsonFromDisk
  (root : Config.Root)
  (c : Meta)
  : List<tlid * PT.Oplist> option =
  string c.name
  |> jsonFilename
  |> File.tryReadFile root
  |> Option.map (fun json ->
    json
    |> Json.Vanilla.deserialize<OT.oplist>
    |> OT.Convert.ocamlOplist2PT
    |> Op.oplist2TLIDOplists)



// let save_json_to_disk ~root (filename : string) (ops : Types.tlid_oplists) :
//     unit =
//   Log.infO
//     "serialization"
//     ~params:[("save_to", "disk"); ("format", "json"); ("filename", filename)] ;
//   let module SF = Serialization_format in
//   ops
//   |> Op.tlid_oplists2oplist
//   |> Serialization_converters.oplist_of_fluid
//   |> SF.oplist_to_yojson SF.RuntimeT.expr_to_yojson
//   |> Yojson.Safe.pretty_to_string
//   |> (fun s -> s ^ "\n")
//   |> File.writefile ~root filename


let loadAndResaveFromTestFile (meta : Meta) : Task<unit> =
  task {
    let oplists =
      let tls = meta |> tryLoadJsonFromDisk Config.Testdata

      match tls with
      | Some tls ->
        tls
        |> List.map (fun (tlid, oplist) ->
          let tl =
            let oplist = fromOplist meta [] oplist
            let tls = toplevels oplist
            let dtls = deletedToplevels oplist
            (Map.mergeFavoringLeft tls dtls)
            |> Map.get tlid
            |> Exception.unwrapOptionInternal "could not find tlid" [ "tlid", tlid ]
          (tlid, oplist, tl, NotDeleted))
      | None -> []

    do! saveTLIDs meta oplists
    return ()
  }

// let minimize (c T) T =
//   (* TODO *)
//   (* let ops = *)
//   (*   c.ops *)
//   (*   |> Undo.preprocess *)
//   (*   |> List.filter ~f:Op.has_effect *)
//   (* in { c with ops = ops } *)
//   c
//
//
// let save_test (c T) : string =
//   let c = minimize c in
//   let host = "test-" ^ c.host in
//   let file = json_filename host in
//   let host =
//     if File.file_exists ~root:Testdata file
//     then host ^ "_" ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
//     else host
//   in
//   let file = json_filename host in
//   save_json_to_disk ~root:Testdata file c.ops ;
//   file


let toProgram (c : T) : RT.ProgramContext =

  let dbs =
    c.dbs
    |> Map.values
    |> List.map (fun db -> (db.name, PT2RT.DB.toRT db))
    |> Map.ofList

  let userFns =
    c.userFunctions
    |> Map.values
    |> List.map (fun f -> (f.name, PT2RT.UserFunction.toRT f))
    |> Map.ofList

  let userTypes =
    c.userTypes
    |> Map.values
    |> List.map (fun t -> ((t.name, t.version), PT2RT.UserType.toRT t))
    |> Map.ofList

  let secrets = c.secrets |> Map.values |> List.map PT2RT.Secret.toRT

  { accountID = c.meta.owner
    canvasName = c.meta.name
    canvasID = c.meta.id
    userFns = userFns
    userTypes = userTypes
    dbs = dbs
    secrets = secrets }

let init () =
  Json.Vanilla.allow<OT.oplist> "loadJsonFromDisk"
  Json.Vanilla.allow<PT.Position> "saveTLIDs"
