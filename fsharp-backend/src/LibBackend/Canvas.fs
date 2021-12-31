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

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OT = LibExecution.OCamlTypes
module Telemetry = LibService.Telemetry

type CorsSetting =
  | AllOrigins
  | Origins of List<string>

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


let getMeta (canvasName : CanvasName.T) : Task<Meta> =
  task {
    let ownerName = (Account.ownerNameFromCanvasName canvasName).toUserName ()
    // CLEANUP put into single query
    let! ownerID = Account.userIDForUserName ownerName
    let! canvasID = canvasIDForCanvasName ownerID canvasName
    return { id = canvasID; owner = ownerID; name = canvasName }
  }



// This includes just a subset of the key program data. It is rare that all of
// the data for a canvas will be loaded. In addition, there is other canvas
// data which is meaningful, such as Cors info, oplists, creation date. These
// can be fetched separately. (Oplists in particular are omitted as it can be
// very tricky to pass this data around safely (esp in regards to loading and
// saving).)
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

let addToplevel (tl : PT.Toplevel) (c : T) : T =
  let tlid = tl.toTLID ()

  match tl with
  | PT.TLHandler h -> { c with handlers = Map.add tlid h c.handlers }
  | PT.TLDB db -> { c with dbs = Map.add tlid db c.dbs }
  | PT.TLType t -> { c with userTypes = Map.add tlid t c.userTypes }
  | PT.TLFunction f -> { c with userFunctions = Map.add tlid f c.userFunctions }

let addToplevels (tls : PT.Toplevel list) (canvas : T) : T =
  List.fold canvas (fun c tl -> addToplevel tl c) tls

let toplevels (c : T) : Map<tlid, PT.Toplevel> =
  let map f l = Map.map f l |> Map.toSeq

  [ map PT.TLHandler c.handlers
    map PT.TLDB c.dbs
    map PT.TLType c.userTypes
    map PT.TLFunction c.userFunctions ]
  |> Seq.concat
  |> Map

let deletedToplevels (c : T) : Map<tlid, PT.Toplevel> =
  let map f l = Map.map f l |> Map.toSeq

  [ map PT.TLHandler c.deletedHandlers
    map PT.TLDB c.deletedDBs
    map PT.TLType c.deletedUserTypes
    map PT.TLFunction c.deletedUserFunctions ]
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

let deleteFunctionForever (tlid : tlid) (c : T) : T =
  { c with
      userFunctions = Map.remove tlid c.userFunctions
      deletedUserFunctions = Map.remove tlid c.deletedUserFunctions }

let deleteType (tlid : tlid) (c : T) : T =
  match Map.get tlid c.userTypes with
  | None -> c
  | Some t ->
    { c with
        userTypes = Map.remove tlid c.userTypes
        deletedUserTypes = Map.add tlid t c.deletedUserTypes }

let deleteTypeForever (tlid : tlid) (c : T) : T =
  { c with
      userTypes = Map.remove tlid c.userTypes
      deletedUserTypes = Map.remove tlid c.deletedUserTypes }

let deleteTLForever (tlid : tlid) (c : T) : T =
  { c with
      dbs = Map.remove tlid c.dbs
      handlers = Map.remove tlid c.handlers
      deletedDBs = Map.remove tlid c.deletedDBs
      deletedHandlers = Map.remove tlid c.deletedHandlers }

// CLEANUP Historically, on the backend, toplevel meant handler or DB
let deleteToplevel (tlid : tlid) (c : T) : T =
  c |> deleteHandler tlid |> deleteDB tlid

let applyToMap (tlid : tlid) (f : 'a -> 'a) (m : Map<tlid, 'a>) : Map<tlid, 'a> =
  Map.update tlid (Option.map f) m



let applyToDB (f : PT.DB.T -> PT.DB.T) (tlid : tlid) (c : T) : T =
  { c with dbs = applyToMap tlid f c.dbs }


let moveToplevel (tlid : tlid) (pos : pos) (c : T) : T =
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
        PT.DType.parse tipe
        |> Exception.unwrapOptionInternal "Cannot parse col type" [ "type", tipe ]
      applyToDB (UserDB.setColType id typ) tlid c
    | PT.ChangeDBColType (tlid, id, tipe) ->
      let typ =
        PT.DType.parse tipe
        |> Exception.unwrapOptionInternal "Cannot parse col type" [ "type", tipe ]
      applyToDB (UserDB.setColType id typ) tlid c
    | PT.DeleteDBCol (tlid, id) -> applyToDB (UserDB.deleteCol id) tlid c
    | PT.DeprecatedInitDBm (tlid, id, rbid, rfid, kind) -> c
    | PT.CreateDBMigration (tlid, rbid, rfid, cols) -> c
    | PT.AddDBColToDBMigration (tlid, colid, typeid) -> c
    | PT.SetDBColNameInDBMigration (tlid, id, name) -> c
    | PT.SetDBColTypeInDBMigration (tlid, id, tipe) -> c
    | PT.AbandonDBMigration tlid -> c
    | PT.DeleteColInDBMigration (tlid, id) -> c
    | PT.SetExpr (tlid, id, e) ->
      // FSTODO
      Exception.raiseInternal "setexpr" []
    // applyToAllToplevels (TL.set_expr id e) tlid c
    | PT.DeleteTL tlid -> deleteToplevel tlid c
    | PT.MoveTL (tlid, pos) -> moveToplevel tlid pos c
    | PT.SetFunction user_fn -> setFunction user_fn c
    | PT.DeleteFunction tlid -> deleteFunction tlid c
    | PT.TLSavepoint _ -> c
    | PT.UndoTL _
    | PT.RedoTL _ ->
      Exception.raiseInternal
        $"Undo/Redo op should have been preprocessed out!"
        [ "op", op ]
    | PT.RenameDBname (tlid, name) -> applyToDB (UserDB.renameDB name) tlid c
    | PT.CreateDBWithBlankOr (tlid, pos, id, name) ->
      setDB (UserDB.create2 tlid name pos id) c
    | PT.DeleteTLForever tlid -> deleteTLForever tlid c
    | PT.DeleteFunctionForever tlid -> deleteFunctionForever tlid c
    | PT.SetType t -> setType t c
    | PT.DeleteType tlid -> deleteType tlid c
    | PT.DeleteTypeForever tlid -> deleteTypeForever tlid c
  with
  | e ->
    // Log here so we have context, but then re-raise
    Telemetry.addException
      "apply_op failure"
      e
      [ ("host", c.meta.name); ("op", string op) ]
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
  let reducedOps = Undo.preprocess (oldops @ newops) in
  List.fold c (fun c (isNew, op) -> applyOp isNew op c) reducedOps

let fetchCORSSetting (canvasID : CanvasID) : Task<Option<CorsSetting>> =
  Sql.query "SELECT cors_setting FROM canvases WHERE id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowAsync (fun read ->
    match read.stringOrNone "cors_setting" with
    | None -> None
    | Some str ->
      let json = System.Text.Json.JsonDocument.Parse str

      match json.RootElement.ValueKind with
      | System.Text.Json.JsonValueKind.String when json.RootElement.GetString() = "*" ->
        Some AllOrigins
      | System.Text.Json.JsonValueKind.Array ->
        json.RootElement.EnumerateArray()
        |> Seq.map string
        |> Seq.toList
        |> Origins
        |> Some
      | _ -> Exception.raiseInternal "invalid json in CorsSettings" [ "json", json ])

let canvasCreationDate (canvasID : CanvasID) : Task<System.DateTime> =
  Sql.query "SELECT created_at from canvases WHERE id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowAsync (fun read -> read.dateTime "created_at")

let updateCorsSetting
  (canvasID : CanvasID)
  (setting : CorsSetting option)
  : Task<unit> =
  let corsSettingToDB (setting : CorsSetting option) : SqlValue =
    match setting with
    | None -> Sql.dbnull
    | Some AllOrigins -> Sql.jsonb "\"*\""
    | Some (Origins ss) -> ss |> Json.Vanilla.serialize |> Sql.jsonb
  Sql.query
    "UPDATE canvases
     SET cors_setting = @setting
     WHERE id = @canvasID"
  |> Sql.parameters [ "setting", corsSettingToDB setting
                      "canvasID", Sql.uuid canvasID ]
  |> Sql.executeStatementAsync


// let url_for (id : Uuidm.t) : string =
//   let canvas_name = name_for_id id in
//   "http://" ^ canvas_name ^ "." ^ Config.public_domain


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


// let load_without_tls host : (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   load_from ~f:(fun ~host ~canvas_id () -> []) host owner []
//
//
// let load_only_tlids ~tlids host (newops : Types.oplist) :
//     (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   load_from ~f:(Serialize.load_only_tlids ~tlids) host owner newops
//
//
// (* Same as `load_only_tlids` but filters out deleted tlids via
//  * the denormalized `deleted` attributed on toplevel_oplists *)
// let load_only_undeleted_tlids ~tlids host (newops : Types.oplist) :
//     (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   load_from ~f:(Serialize.load_only_undeleted_tlids ~tlids) host owner newops



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
    match loadAmount with
    | LiveToplevels ->
      "SELECT tlid, data FROM toplevel_oplists
          WHERE canvas_id = @canvasID
            AND tlid = ANY(@tlids)
            AND deleted IS NOT TRUE"
    | IncludeDeletedToplevels ->
      "SELECT tlid, data FROM toplevel_oplists
          WHERE canvas_id = @canvasID
            AND tlid = ANY(@tlids)"

  Sql.query query
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
  |> Sql.executeAsync (fun read -> (read.tlid "tlid", read.bytea "data"))
  |> Task.bind (fun list ->
    list
    |> List.map (fun (tlid, data) ->
      task {
        let! oplist = OCamlInterop.oplistOfBinary data
        return (tlid, oplist)
      })
    |> Task.flatten)


let loadFrom (loadAmount : LoadAmount) (meta : Meta) (tlids : List<tlid>) : Task<T> =
  task {
    try
      // CLEANUP: rename "rendered" and "cached" to be consistent

      // load
      let! fastLoadedTLs = Serialize.loadOnlyRenderedTLIDs meta.id tlids ()

      let fastLoadedTLIDs =
        List.map (fun (tl : PT.Toplevel) -> tl.toTLID ()) fastLoadedTLs

      let notLoadedTLIDs =
        List.filter (fun x -> not (List.includes x fastLoadedTLIDs)) tlids

      // canvas initialized via the normal loading path with the non-fast loaded tlids
      // loaded traditionally via the oplist
      let! uncachedOplists = loadOplists loadAmount meta.id notLoadedTLIDs
      let uncachedOplists = uncachedOplists |> List.map Tuple2.second |> List.concat
      let c = empty meta
      // FSTODO: where are secrets loaded

      return c |> addToplevels fastLoadedTLs |> addOps uncachedOplists [] |> verify
    with
    | e -> return Exception.reraiseAsPageable e
  }

let loadAll (meta : Meta) : Task<T> =
  task {
    let! tlids = Serialize.fetchAllTLIDs meta.id
    return! loadFrom IncludeDeletedToplevels meta tlids
  }

let loadHttpHandlers (meta : Meta) (path : string) (method : string) : Task<T> =
  task {
    let! tlids = Serialize.fetchReleventTLIDsForHTTP meta.id path method
    return! loadFrom LiveToplevels meta tlids
  }

let loadTLIDs (meta : Meta) (tlids : tlid list) : Task<T> =
  loadFrom LiveToplevels meta tlids


let loadTLIDsWithContext (meta : Meta) (tlids : List<tlid>) : Task<T> =
  task {
    let! context = Serialize.fetchRelevantTLIDsForExecution meta.id
    let tlids = tlids @ context
    return! loadFrom LiveToplevels meta tlids
  }

let loadForEvent (e : EventQueue.T) : Task<T> =
  task {
    let meta = { id = e.canvasID; name = e.canvasName; owner = e.ownerID }
    let! tlids = Serialize.fetchRelevantTLIDsForEvent meta.id e
    return! loadFrom LiveToplevels meta tlids
  }

let loadAllDBs (meta : Meta) : Task<T> =
  task {
    let! tlids = Serialize.fetchTLIDsForAllDBs meta.id
    return! loadFrom LiveToplevels meta tlids
  }

let loadTLIDsWithDBs (meta : Meta) (tlids : List<tlid>) : Task<T> =
  task {
    let! dbTLIDs = Serialize.fetchTLIDsForAllDBs meta.id
    return! loadFrom LiveToplevels meta (tlids @ dbTLIDs)
  }




// let load_for_cron_checker_from_cache host : (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   let canvas_id = Serialize.fetch_canvas_id owner host in
//   load_from_cache
//     ~tlids:(Serialize.fetch_relevant_tlids_for_cron_checker ~canvas_id ())
//     host
//     owner

type Deleted =
  | Deleted
  | NotDeleted

let getToplevel (tlid : tlid) (c : T) : Option<Deleted * PT.Toplevel> =
  let handler () =
    Map.tryFind tlid c.handlers
    |> Option.map (fun h -> (NotDeleted, PT.TLHandler h))

  let deletedHandler () =
    Map.tryFind tlid c.deletedHandlers
    |> Option.map (fun h -> (Deleted, PT.TLHandler h))

  let db () = Map.tryFind tlid c.dbs |> Option.map (fun h -> (NotDeleted, PT.TLDB h))

  let deletedDB () =
    Map.tryFind tlid c.deletedDBs |> Option.map (fun h -> (Deleted, PT.TLDB h))

  let userFunction () =
    Map.tryFind tlid c.userFunctions
    |> Option.map (fun h -> (NotDeleted, PT.TLFunction h))

  let deletedUserFunction () =
    Map.tryFind tlid c.deletedUserFunctions
    |> Option.map (fun h -> (Deleted, PT.TLFunction h))

  let userType () =
    Map.tryFind tlid c.userTypes |> Option.map (fun h -> (NotDeleted, PT.TLType h))

  let deletedUserType () =
    Map.tryFind tlid c.deletedUserTypes
    |> Option.map (fun h -> (Deleted, PT.TLType h))

  handler ()
  |> Option.orElseWith deletedHandler
  |> Option.orElseWith db
  |> Option.orElseWith deletedDB
  |> Option.orElseWith userFunction
  |> Option.orElseWith deletedUserFunction
  |> Option.orElseWith userType
  |> Option.orElseWith deletedUserType


// Save just the TLIDs listed (a canvas may load more tlids to support
// calling/testing these TLs, even though those TLs do not need to be updated)
let saveTLIDs
  (meta : Meta)
  (oplists : List<tlid * PT.Oplist * PT.Toplevel * Deleted>)
  : Task =
  try
    // Use ops rather than just set of toplevels, because toplevels may
    // have been deleted or undone, and therefore not appear, but it's
    // important to record them.
    oplists
    |> List.map (fun (tlid, oplist, tl, deleted) ->
      task {
        let string2option (s : string) : Option<string> =
          if s = "" then None else Some s

        let deleted =
          match deleted with
          | Deleted -> true
          | NotDeleted -> false

        let routingNames =
          match tl with
          | PT.TLHandler ({ spec = spec }) ->
            match spec with
            | PT.Handler.HTTP _ ->
              Some(
                spec.module' (),
                Routing.routeToPostgresPattern (spec.name ()),
                spec.modifier ()
              )
            | PT.Handler.Worker _
            | PT.Handler.OldWorker _
            | PT.Handler.Cron _
            | PT.Handler.REPL _ ->
              Some(spec.module' (), spec.name (), spec.modifier ())
          | PT.TLDB _
          | PT.TLType _
          | PT.TLFunction _ -> None

        let (module_, name, modifier) =
          match routingNames with
          | Some (module_, name, modifier) ->
            (string2option module_, string2option name, string2option modifier)
          | None -> None, None, None

        let pos =
          match tl with
          | PT.TLHandler ({ pos = pos })
          | PT.TLDB { pos = pos } -> Some(Json.Vanilla.serialize pos)
          | PT.TLType _ -> None
          | PT.TLFunction _ -> None

        let! oplist = OCamlInterop.oplistToBinary oplist
        let! oplistCache = OCamlInterop.toplevelToCachedBinary tl

        return!
          Sql.query
            "INSERT INTO toplevel_oplists
                  (canvas_id, account_id, tlid, digest, tipe, name, module, modifier, data,
                   rendered_oplist_cache, deleted, pos)
                  VALUES (@canvasID, @accountID, @tlid, @digest, @typ::toplevel_type, @name,
                          @module, @modifier, @data, @renderedOplistCache, @deleted, @pos)
                  ON CONFLICT (canvas_id, tlid) DO UPDATE
                  SET account_id = @accountID,
                      digest = @digest,
                      tipe = @typ::toplevel_type,
                      name = @name,
                      module = @module,
                      modifier = @modifier,
                      data = @data,
                      rendered_oplist_cache = @renderedOplistCache,
                      deleted = @deleted,
                      pos = @pos"
          |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                              "accountID", Sql.uuid meta.owner
                              "tlid", Sql.id tlid
                              "digest", Sql.string (OCamlInterop.digest ())
                              "typ", Sql.string (tl.toDBTypeString ())
                              "name", Sql.stringOrNone name
                              "module", Sql.stringOrNone module_
                              "modifier", Sql.stringOrNone modifier
                              "data", Sql.bytea oplist
                              "renderedOplistCache", Sql.bytea oplistCache
                              "deleted", Sql.bool deleted
                              "pos", Sql.jsonbOrNone pos ]
          |> Sql.executeStatementAsync
      })
    |> List.map (fun t -> t : Task)
    |> Task.WhenAll
  with
  | e -> Exception.reraiseAsPageable e


// -------------------------
// Testing/validation
// -------------------------

let jsonFilename (name : string) = $"{name}.json"

let loadJsonFromDisk (root : Config.Root) (c : Meta) : List<tlid * PT.Oplist> =
  string c.name
  |> jsonFilename
  |> File.readfile root
  |> Json.Vanilla.deserialize<OT.oplist<OT.RuntimeT.fluidExpr>>
  |> OT.Convert.ocamlOplist2PT
  |> Op.oplist2TLIDOplists



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
      meta
      |> loadJsonFromDisk Config.Testdata
      |> List.map (fun (tlid, oplist) ->
        let tl =
          fromOplist meta [] oplist
          |> toplevels
          |> Map.get tlid
          |> Option.unwrapUnsafe
        (tlid, oplist, tl, NotDeleted))

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
//
//
// (* --------------- *)
// (* Validate canvases *)
// (* --------------- *)
//
// (* This is a little broad, since we could limit it to tlids, but good enough
//  * for now. At time of writing, these all have the duplicate DB problem. *)
// let known_invalid_hosts =
//   Tc.StrSet.from_list
//     [ "danbowles"
//     ; "danwetherald"
//     ; "ellen-dbproblem18"
//     ; "ellen-preview"
//     ; "ellen-stltrialrun"
//     ; "ellen-trinity" ]
//
//
// let all_hosts () : string list =
//   List.filter (Serialize.current_hosts ()) ~f:(fun host ->
//       not (Tc.StrSet.member known_invalid_hosts ~value:host))
//
//
// let is_valid_op op : bool = not (Op.is_deprecated op)
//
// let validate_host host : (unit, string) Result.t =
//   try
//     match load_all host [] with
//     | Ok c ->
//         let all_ops = !c.ops |> Op.tlid_oplists2oplist in
//         let ops_valid =
//           if Tc.List.all all_ops ~f:is_valid_op
//           then Ok ()
//           else Error "Ops are not valid"
//         in
//         let cache_valid =
//           try
//             let tlids =
//               all_ops
//               |> List.map ~f:Op.tlidOf
//               |> List.dedup_and_sort ~compare:compare_tlid
//             in
//             load_tlids_from_cache ~tlids host
//             |> Tc.Result.map_error String.concat
//             |> Tc.Result.map (fun _ -> ())
//           with e -> Error "couldn't load cache"
//         in
//         Tc.Result.combine [ops_valid; cache_valid]
//         |> Tc.Result.map (fun _ -> ())
//     | Error errs ->
//         Error ("can't load " ^ host ^ ":\n" ^ Tc.String.join ~sep:", " errs)
//   with e -> Error ("Invalid canvas " ^ host ^ ":\n" ^ Exception.to_string e)
//
//
// let validate_all_hosts () : unit =
//   all_hosts ()
//   |> List.map ~f:validate_host
//   |> Tc.Result.combine
//   |> Tc.Result.map (fun _ -> ())
//   |> Result.ok_or_Exception.raiseInternal


// just load, don't save -- also don't validate the ops don't have deprecate ops (via
// validate_op or validate_host). This function is used on startup so it will prevent
// a deploy from succeeding. We don't want to prevent deploys because someone forgot
// a deprecatedop in a tier 1 canvas somewhere
let checkTierOneHosts () : Task<unit> =
  if Config.checkTierOneHosts then
    Serialize.tierOneHosts ()
    |> Task.iterInParallel (fun host ->
      task {
        let! meta = getMeta host
        let! (_ : T) = loadAll meta
        return ()
      })
  else
    Task.FromResult()


//
// let migrate_host (_host : string) : (string, unit) Tc.Result.t =
//   try
//     Ok ()
//     (*   let canvas_id = id_for_name host in *)
//     (*   Serialize.fetch_all_tlids ~canvas_id () *)
//     (*   |> List.map ~f:(fun tlid -> *)
//     (*          Serialize.transactionally_migrate_oplist *)
//     (*            ~canvas_id *)
//     (*            ~tlid *)
//     (*            ~host *)
//     (*            ~handler_f:migrate_handler *)
//     (*            ~db_f:migrate_db *)
//     (*            ~user_fn_f:migrate_user_function *)
//     (*            ~user_tipe_f:migrate_user_tipe *)
//     (*            ~oplist_f:(List.map ~f:migrate_op) *)
//     (*            ()) *)
//     (*   |> Tc.Result.combine *)
//     (*   |> Tc.Result.map (fun _ -> ()) *)
//   with e -> Error (Exception.to_string e)
//
//
// let migrate_all_hosts () =
//   List.iter (all_hosts ()) ~f:(fun host ->
//       migrate_host host |> Result.ok_or_Exception.raiseInternal)
//
//
// let write_shape_data () =
//   if Config.should_write_shape_data
//   then
//     File.writefile
//       ~root:Serialization
//       Binary_serialization.digest
//       Binary_serialization.shape_string
//   else ()
//
//
// let time (fn : unit -> 'a) : float * 'a =
//   let start = Unix.gettimeofday () in
//   let a = fn () in
//   let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
//   (elapsed, a)


let toProgram (c : T) : RT.ProgramContext =

  let dbs =
    c.dbs
    |> Map.values
    |> List.map (fun db -> (db.name, PT.DB.toRuntimeType db))
    |> Map.ofList

  let userFns =
    c.userFunctions
    |> Map.values
    |> List.map (fun f -> (f.name, PT.UserFunction.toRuntimeType f))
    |> Map.ofList

  let userTypes =
    c.userTypes
    |> Map.values
    |> List.map (fun t -> ((t.name, t.version), PT.UserType.toRuntimeType t))
    |> Map.ofList

  let secrets = (c.secrets |> Map.map (fun pt -> pt.toRuntimeType ()) |> Map.values)

  { accountID = c.meta.owner
    canvasName = c.meta.name
    canvasID = c.meta.id
    userFns = userFns
    userTypes = userTypes
    dbs = dbs
    secrets = secrets }
