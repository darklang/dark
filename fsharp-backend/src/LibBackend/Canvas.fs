module LibBackend.Canvas

// Functions related to Canvases

open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp.Tasks
open Npgsql
open LibBackend.Db
open System.Text.RegularExpressions

open Prelude
open Tablecloth

module PT = ProgramSerialization.ProgramTypes

type CorsSetting =
  | AllOrigins
  | Origins of List<string>

type T =
  { name : CanvasName.T
    owner : UserID
    id : CanvasID
    creationDate : System.DateTime
    ops : PT.TLIDOplists
    corsSetting : Option<CorsSetting>
    handlers : Map<tlid, PT.Handler.T>
    dbs : Map<tlid, PT.DB.T>
    userFunctions : Map<tlid, PT.UserFunction.T>
    userTypes : Map<tlid, PT.UserType.T>
    packageFns : List<PT.PackageManager.Fn>
    // TODO CLEANUP: no separate fields for deleted, combine them
    deletedHandlers : Map<tlid, PT.Handler.T>
    deletedDBs : Map<tlid, PT.DB.T>
    deletedUserFunctions : Map<tlid, PT.UserFunction.T>
    deletedUserTypes : Map<tlid, PT.UserType.T> }

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


(* ------------------------- *)
(* Toplevel *)
(* ------------------------- *)

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

// TODO: CLEANUP Historically, on the backend, toplevel meant handler or DB
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
        if isNew && name = "" then failwith "DB must have a name"
        let db = UserDB.create tlid name pos
        setDB db c
    | PT.AddDBCol (tlid, colid, typeid) ->
        applyToDB (UserDB.addCol colid typeid) tlid c
    | PT.SetDBColName (tlid, id, name) ->
        applyToDB (UserDB.setColName id name) tlid c
    | PT.ChangeDBColName (tlid, id, name) ->
        applyToDB (UserDB.setColName id name) tlid c
    | PT.SetDBColType (tlid, id, tipe) ->
        applyToDB (UserDB.setColType id (PT.DType.parse tipe)) tlid c
    | PT.ChangeDBColType (tlid, id, tipe) ->
        applyToDB (UserDB.setColType id (PT.DType.parse tipe)) tlid c
    | PT.DeleteDBCol (tlid, id) -> applyToDB (UserDB.deleteCol id) tlid c
    | PT.DeprecatedInitDBm (tlid, id, rbid, rfid, kind) -> c
    | PT.CreateDBMigration (tlid, rbid, rfid, cols) -> c
    | PT.AddDBColToDBMigration (tlid, colid, typeid) -> c
    | PT.SetDBColNameInDBMigration (tlid, id, name) -> c
    | PT.SetDBColTypeInDBMigration (tlid, id, tipe) -> c
    | PT.AbandonDBMigration tlid -> c
    | PT.DeleteColInDBMigration (tlid, id) -> c
    | PT.SetExpr (tlid, id, e) -> fstodo "setexpr"
    // applyToAllToplevels (TL.set_expr id e) tlid c
    | PT.DeleteTL tlid -> deleteToplevel tlid c
    | PT.MoveTL (tlid, pos) -> moveToplevel tlid pos c
    | PT.SetFunction user_fn -> setFunction user_fn c
    | PT.DeleteFunction tlid -> deleteFunction tlid c
    | PT.TLSavepoint _ -> c
    | PT.UndoTL _
    | PT.RedoTL _ -> failwith $"This should have been preprocessed out! {op}"
    | PT.RenameDBname (tlid, name) -> applyToDB (UserDB.renameDB name) tlid c
    | PT.CreateDBWithBlankOr (tlid, pos, id, name) ->
        setDB (UserDB.create2 tlid name pos id) c
    | PT.DeleteTLForever tlid -> deleteTLForever tlid c
    | PT.DeleteFunctionForever tlid -> deleteFunctionForever tlid c
    | PT.SetType t -> setType t c
    | PT.DeleteType tlid -> deleteType tlid c
    | PT.DeleteTypeForever tlid -> deleteTypeForever tlid c
  with e ->
    // FSTODO
    (* Log here so we have context, but then re-raise *)
    // Log.erroR
    //   "apply_op failure"
    //   ~params:
    //     [ ("host", !c.host)
    //     ; ("op", Types.show_op op)
    //     ; ("exn", Exception.to_string e) ] ;
    fstodo (e.ToString())


// NOTE: If you add a new verification here, please ensure all places that
// load canvases/apply ops correctly load the requisite data.
//
// See `Op.RequiredContext` for how we determine which ops need what other
// context to be loaded to appropriately verify.
// FSTODO: tests
let verify (c : T) : Result<T, string list> =
  let dupedNames =
    c.dbs
    |> Map.values
    |> List.groupBy (fun db -> db.name)
    |> Map.filter (fun db -> List.length db > 1)
    |> Map.values
    |> List.map
         (fun names ->
           let names = List.map (fun db -> db.name)
           $"Duplicate DB names: {names}")

  match dupedNames with
  | [] -> Ok c
  | dupes -> Error dupes


let addOps (oldops : PT.Oplist) (newops : PT.Oplist) (c : T) : T =
  let oldops = List.map (fun op -> (false, op)) oldops
  let newops = List.map (fun op -> (true, op)) newops
  let reducedOps = Undo.preprocess (oldops @ newops) in
  let c = List.fold c (fun c (isNew, op) -> applyOp isNew op c) reducedOps
  let allops = oldops @ newops |> List.map Tuple2.second
  { c with ops = Op.oplist2TLIDOplists allops }

let fetchCORSSetting (canvasID : CanvasID) : Task<Option<CorsSetting>> =
  Sql.query "SELECT cors_setting FROM canvases WHERE id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowAsync
       (fun read ->
         match read.stringOrNone "cors_setting" with
         | None -> None
         | Some str ->
             let json = System.Text.Json.JsonDocument.Parse str

             match json.RootElement.ValueKind with
             | System.Text.Json.JsonValueKind.String when
               json.RootElement.GetString() = "*" -> Some AllOrigins
             | System.Text.Json.JsonValueKind.Array ->
                 json.RootElement.EnumerateArray()
                 |> Seq.map (fun e -> e.ToString())
                 |> Seq.toList
                 |> Origins
                 |> Some
             | _ -> failwith "invalid json in CorsSettings")

let canvasCreationDate canvasID : Task<System.DateTime> =
  Sql.query "SELECT created_at from canvases WHERE id = @canvasID"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeRowAsync (fun read -> read.dateTime "created_at")

// let init (host : string) (ops : Types.oplist) :
//     (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   let canvas_id = Serialize.fetch_canvas_id owner host in
//   let creation_date = canvas_creation_date canvas_id in
//   let cors = fetch_cors_setting canvas_id in
//   let c =
//     ref
//       { host
//       ; owner
//       ; id = canvas_id
//       ; creation_date
//       ; ops = []
//       ; cors_setting = cors
//       ; handlers = IDMap.empty
//       ; dbs = IDMap.empty
//       ; user_functions = IDMap.empty
//       ; userTypes = IDMap.empty
//       ; package_fns = []
//       ; deleted_handlers = IDMap.empty
//       ; deleted_dbs = IDMap.empty
//       ; deleted_user_functions = IDMap.empty
//       ; deletedUserTypes = IDMap.empty }
//   in
//   add_ops c [] ops ;
//   c |> verify |> Result.map ~f:(fun _ -> c)
//
//
// let name_for_id (id : Uuidm.t) : string =
//   Db.fetch_one
//     ~name:"fetch_canvas_name"
//     "SELECT name FROM canvases WHERE id = $1"
//     ~params:[Uuid id]
//   |> List.hd_exn
//
//
// let id_for_name_option (name : string) : Uuidm.t option =
//   Db.fetch_one_option
//     ~name:"fetch_canvas_id"
//     "SELECT id FROM canvases WHERE name = $1"
//     ~params:[Db.String name]
//   (* If List.hd_exn exn's, it means that `SELECT id` returned a record with more
//    * than one field..  Can't happen. *)
//   |> Option.map ~f:List.hd_exn
//   |> Option.bind ~f:Uuidm.of_string
//
//
// let id_for_name (name : string) : Uuidm.t =
//   name |> id_for_name_option |> Option.value_exn
//
//
// let id_and_account_id_for_name_exn (name : string) : Uuidm.t * Uuidm.t =
//   (* If we're using this in /api/.../ we're already guaranteed that the canvas
//    * exists *)
//   Db.fetch_one
//     ~name:"fetch_canvas_id_and_account_id"
//     "SELECT id, account_id FROM canvases WHERE name = $1"
//     ~params:[Db.String name]
//   |> function
//   | [canvas_id; account_id] ->
//       (* These are guaranteed by the db schema to be uuids *)
//       ( canvas_id |> Uuidm.of_string |> Option.value_exn
//       , account_id |> Uuidm.of_string |> Option.value_exn )
//   | _ ->
//       Exception.internal "Wrong db shape in Canvas.id_and_account_id"
//
//
// let update_cors_setting (c T ref) (setting : cors_setting option) : unit
//     =
//   let cors_setting_to_db (setting : cors_setting option) : Db.param =
//     match setting with
//     | None ->
//         Db.Null
//     | Some AllOrigins ->
//         `String "*" |> Yojson.Safe.to_string |> Db.String
//     | Some (Origins ss) ->
//         ss
//         |> List.map ~f:(fun s -> `String s)
//         |> (fun l -> `List l)
//         |> Yojson.Safe.to_string
//         |> Db.String
//   in
//   Db.run
//     ~name:"update_cors_setting"
//     "UPDATE canvases
//      SET cors_setting = $1
//      WHERE id = $2"
//     ~params:[cors_setting_to_db setting; Uuid !c.id] ;
//   c := {!c with cors_setting = setting} ;
//   ()
//
//
// let url_for (id : Uuidm.t) : string =
//   let canvas_name = name_for_id id in
//   "http://" ^ canvas_name ^ "." ^ Config.public_domain


// -------------------------
//  Loading/saving *)
//  -------------------------

// Loads all ops
type OpsLoader = CanvasID -> Task<List<tlid * PT.Oplist>>

// Loads from the cache app
type TLLoader =
  CanvasID -> List<tlid> -> Task<Result<tlid * PT.Toplevel, string list>>

let loadEmpty
  (canvasID : CanvasID)
  (canvasName : CanvasName.T)
  (owner : UserID)
  : Task<T> =
  task {
    // try
    // TODO: combine into one query
    let! cors = fetchCORSSetting canvasID
    let! creationDate = canvasCreationDate canvasID
    // TODO optimization: can we get only the functions we need (based on
    // fnnames found in the canvas) and/or cache this like we do the oplist?
    let! packageFns = PackageManager.allFunctions ()

    return
      { name = canvasName
        owner = owner
        id = canvasID
        creationDate = creationDate
        ops = []
        corsSetting = cors
        handlers = Map.empty
        dbs = Map.empty
        userFunctions = Map.empty
        userTypes = Map.empty
        packageFns = packageFns
        deletedHandlers = Map.empty
        deletedDBs = Map.empty
        deletedUserFunctions = Map.empty
        deletedUserTypes = Map.empty }

  }


// let loadFrom
//   (canvasID : CanvasID)
//   (canvasName : CanvasName.T)
//   (owner : UserID)
//   (newops : PT.Oplist)
//   (f : OpsLoader)
//   : Task<Result<T, string list>> =
//   task {
//     let! oldops = f canvasID
//     let! c = loadEmpty canvasID canvasName owner
//     return Ok c
//   // |> addOps oldops newops
//   // |> verify
//
//   // FSTODO
//   // addOps c (Op.tlid_oplists2oplist oldops) newops
//   // FSTODO
//   // c |> verify |> Result.map (fun _ -> c)
//   // FSTODO
//   // with e -> Libexecution.Exception.reraise_as_pageable e
//   }
//
// let load_without_tls host : (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   load_from ~f:(fun ~host ~canvas_id () -> []) host owner []
//
//
// let load_all host (newops : Types.oplist) : (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   load_from ~f:Serialize.load_all_from_db host owner newops
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
//
//
// let load_with_dbs ~tlids host (newops : Types.oplist) :
//     (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   load_from ~f:(Serialize.load_with_dbs ~tlids) host owner newops


// This is a special `load_*` function that specifically loads toplevels via
// the `rendered_oplist_cache` column on `toplevel_oplists`. This column stores
// a binary-serialized representation of the toplevel after the oplist is
// applied. This should be much faster because we don't have to ship the full
// oplist across the network from Postgres to the OCaml boxes, and similarly
// they don't have to apply the full history of the canvas in memory before
// they can execute the code.
let loadOnlyRenderedTLIDs (canvasID : CanvasID) (tlids : List<tlid>) () =
  // Binary_serialization.rendered_oplist_cache_query_result =
  // We specifically only load where `deleted` IS FALSE (even though the column
  // is nullable). This means we will not load undeleted handlers from the
  // cache if we've never written their `deleted` state. This is less
  // efficient, but still correct, as they'll still be loaded via their oplist.
  // It avoids loading deleted handlers that have had their cached version
  // written but never their deleted state, which could be true for some
  // handlers that were touched between the addition of the
  // `rendered_oplist_cache` column and the addition of the `deleted` column.
  Sql.query
    "SELECT rendered_oplist_cache, pos FROM toplevel_oplists
      WHERE canvas_id = $1
        AND tlid = ANY (string_to_array($2, $3)::bigint[])
        AND deleted IS FALSE
        AND (((tipe = 'handler'::toplevel_type OR tipe = 'db'::toplevel_type) AND pos IS NOT NULL)
             OR tipe = 'user_function'::toplevel_type OR tipe = 'user_tipe'::toplevel_type)"
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID; "tlids", Sql.idArray tlids ]
  |> Sql.executeAsync
       (fun read ->
         (read.bytea "rendered_oplist_cache", read.stringOrNone "pos")

         |> ProgramSerialization.OCamlInterop.toplevelOfCachedBinary)

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
  |> Sql.executeAsync
       (fun read ->
         (read.tlid "tlid",
          read.bytea "data" |> ProgramSerialization.OCamlInterop.oplistOfBinary))


let loadFrom
  (loadAmount : LoadAmount)
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (owner : UserID)
  (tlids : List<tlid>)
  : Task<Result<T, string list>> =
  task {
    // CLEANUP: rename "rendered" and "cached" to be consistent

    // load
    let! fastLoadedTLs = Serialize.loadOnlyRenderedTLIDs canvasName canvasID tlids ()

    let fastLoadedTLIDs =
      List.map (fun (tl : PT.Toplevel) -> tl.toTLID ()) fastLoadedTLs

    let notLoadedTLIDs =
      List.filter (fun x -> not (List.includes x fastLoadedTLIDs)) tlids

    // canvas initialized via the normal loading path with the non-fast loaded tlids
    // loaded traditionally via the oplist
    let! uncachedOplists = loadOplists loadAmount canvasID notLoadedTLIDs
    let uncachedOplists = uncachedOplists |> List.map Tuple2.second |> List.concat
    let! c = loadEmpty canvasID canvasName owner
    return
      c
      |> addToplevels fastLoadedTLs
      |> addOps uncachedOplists []
      |> verify
      // Empty out the oplist, this prevents anyone accidentally saving
      // a canvas partially loaded from the cache.
      // FSTODO: we don't always want to remove the oplist
      |> Result.map (fun c -> { c with ops = [] })
  }

let loadAll
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (ownerID : UserID)
  : Task<Result<T, List<string>>> =
  task {
    let! tlids = Serialize.fetchAllTLIDs canvasID
    return! loadFrom LiveToplevels canvasName canvasID ownerID tlids
  }


let loadHttpHandlersFromCache
  (canvasName : CanvasName.T)
  (canvasID : CanvasID)
  (ownerID : UserID)
  (path : string)
  (method : string)
  : Task<Result<T, List<string>>> =
  task {
    let! tlids = Serialize.fetchReleventTLIDsForHTTP canvasName canvasID path method
    return! loadFrom LiveToplevels canvasName canvasID ownerID tlids
  }

let loadTLIDsFromCache (tlids : tlid list) (canvasName : CanvasName.T) (canvasID : CanvasID) (ownerID : UserID) : Task<Result<T, List<string>>> =
  loadFrom LiveToplevels canvasName canvasID ownerID tlids


// let load_tlids_with_context_from_cache ~tlids host :
//     (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   let canvas_id = Serialize.fetch_canvas_id owner host in
//   let tlids =
//     let context =
//       Serialize.fetch_relevant_tlids_for_execution ~host ~canvas_id ()
//     in
//     tlids @ context
//   in
//   load_from_cache ~tlids host owner
//
//
// let load_for_event_from_cache (event : Event_queue.t) :
//     (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn event.host in
//   let canvas_id = Serialize.fetch_canvas_id owner event.host in
//   load_from_cache
//     ~tlids:(Serialize.fetch_relevant_tlids_for_event ~event ~canvas_id ())
//     event.host
//     owner
//
//
// let load_all_dbs_from_cache host : (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   let canvas_id = Serialize.fetch_canvas_id owner host in
//   load_from_cache
//     ~tlids:(Serialize.fetch_tlids_for_all_dbs ~canvas_id ())
//     host
//     owner
//
//
// let load_for_cron_checker_from_cache host : (canvas ref, string list) Result.t =
//   let owner = Account.for_host_exn host in
//   let canvas_id = Serialize.fetch_canvas_id owner host in
//   load_from_cache
//     ~tlids:(Serialize.fetch_relevant_tlids_for_cron_checker ~canvas_id ())
//     host
//     owner
//
//
// let serialize_only (tlids : tlid list) (c T) : unit =
//   try
//     let munge_name module_ n =
//       if Ast.blank_to_option module_ = Some "HTTP"
//       then Http.route_to_postgres_pattern n
//       else n
//     in
//     let handler_metadata (h : RTT.HandlerT.handler) =
//       ( Ast.blank_to_option h.spec.name
//         |> Option.map ~f:(munge_name h.spec.module_)
//       , Ast.blank_to_option h.spec.module_
//       , Ast.blank_to_option h.spec.modifier )
//     in
//     (* Use ops rather than just set of toplevels, because toplevels may
//    * have been deleted or undone, and therefore not appear, but it's
//    * important to record them. *)
//     List.iter c.ops ~f:(fun (tlid, oplist) ->
//         (* Only save oplists that have been used. *)
//         if List.mem ~equal:( = ) tlids tlid
//         then
//           let name, module_, modifier =
//             IDMap.find c.handlers tlid
//             |> Option.bind ~f:TL.as_handler
//             |> Option.map ~f:handler_metadata
//             |> Option.value ~default:(None, None, None)
//           in
//           let handler () =
//             IDMap.find c.handlers tlid
//             |> Option.bind ~f:(fun tl ->
//                    tl |> TL.as_handler |> Option.map ~f:(fun h -> (tl.pos, h)))
//             |> Option.map ~f:(fun (pos, h) ->
//                    ( Binary_serialization.handler_to_binary_string h
//                    , false
//                    , Some pos
//                    , TL.TLHandler ))
//           in
//           let deleted_handler () =
//             IDMap.find c.deleted_handlers tlid
//             |> Option.bind ~f:(fun tl ->
//                    tl |> TL.as_handler |> Option.map ~f:(fun h -> (tl.pos, h)))
//             |> Option.map ~f:(fun (pos, h) ->
//                    ( Binary_serialization.handler_to_binary_string h
//                    , true
//                    , Some pos
//                    , TL.TLHandler ))
//           in
//           let db () =
//             IDMap.find c.dbs tlid
//             |> Option.bind ~f:(fun tl ->
//                    tl |> TL.as_db |> Option.map ~f:(fun db -> (tl.pos, db)))
//             |> Option.map ~f:(fun (pos, db) ->
//                    ( Binary_serialization.db_to_binary_string db
//                    , false
//                    , Some pos
//                    , TL.TLDB ))
//           in
//           let deleted_db () =
//             IDMap.find c.deleted_dbs tlid
//             |> Option.bind ~f:(fun tl ->
//                    tl |> TL.as_db |> Option.map ~f:(fun db -> (tl.pos, db)))
//             |> Option.map ~f:(fun (pos, db) ->
//                    ( Binary_serialization.db_to_binary_string db
//                    , true
//                    , Some pos
//                    , TL.TLDB ))
//           in
//           let user_function () =
//             IDMap.find c.user_functions tlid
//             |> Option.map ~f:(fun fn ->
//                    ( Binary_serialization.user_fn_to_binary_string fn
//                    , false
//                    , None
//                    , TL.TLUserFunction ))
//           in
//           let deleted_user_function () =
//             IDMap.find c.deleted_user_functions tlid
//             |> Option.map ~f:(fun fn ->
//                    ( Binary_serialization.user_fn_to_binary_string fn
//                    , true
//                    , None
//                    , TL.TLUserFunction ))
//           in
//           let user_tipe () =
//             IDMap.find c.userTypes tlid
//             |> Option.map ~f:(fun t ->
//                    ( Binary_serialization.user_tipe_to_binary_string t
//                    , false
//                    , None
//                    , TL.TLUserTipe ))
//           in
//           let deleted_user_tipe () =
//             IDMap.find c.deletedUserTypes tlid
//             |> Option.map ~f:(fun t ->
//                    ( Binary_serialization.user_tipe_to_binary_string t
//                    , true
//                    , None
//                    , TL.TLUserTipe ))
//           in
//           let binary_repr, deleted, pos, tipe =
//             handler ()
//             |> Tc.Option.or_else_lazy deleted_handler
//             |> Tc.Option.or_else_lazy db
//             |> Tc.Option.or_else_lazy deleted_handler
//             |> Tc.Option.or_else_lazy deleted_db
//             |> Tc.Option.or_else_lazy user_function
//             |> Tc.Option.or_else_lazy deleted_user_function
//             |> Tc.Option.or_else_lazy user_tipe
//             |> Tc.Option.or_else_lazy deleted_user_tipe
//             |> Option.map ~f:(fun (str, d, pos, t) ->
//                    (Some str, Some d, pos, t))
//             (* If the user calls Undo enough, we might not know
//              * the tipe here. In that case, set to handler cause
//              * it won't be used anyway *)
//             |> Option.value ~default:(None, None, None, TL.TLHandler)
//           in
//           Serialize.save_toplevel_oplist
//             oplist
//             ~binary_repr
//             ~tlid
//             ~canvas_id:c.id
//             ~account_id:c.owner
//             ~name
//             ~module_
//             ~modifier
//             ~deleted
//             ~pos
//             ~tipe
//         else ())
//   with e -> Libexecution.Exception.reraise_as_pageable e
//
//
// let save_tlids (c T) (tlids : tlid list) : unit = serialize_only tlids c
//
// let save_all (c T) : unit =
//   let tlids = List.map ~f:Tuple.T2.get1 c.ops in
//   save_tlids c tlids
//
//
// (* ------------------------- *)
// (* Testing/validation *)
// (* ------------------------- *)
//
// let json_filename name = name ^ "." ^ "json"
//
// let load_json_from_disk
//     ~root ?(preprocess = ident) ~(host : string) ~(canvas_id : Uuidm.t) () :
//     Types.tlid_oplists =
//   Log.infO
//     "serialization"
//     ~params:[("load", "disk"); ("format", "json"); ("host", host)] ;
//   let module SF = Serialization_format in
//   let filename = json_filename host in
//   File.maybereadjsonfile
//     ~root
//     filename
//     ~conv:(SF.oplist_of_yojson SF.RuntimeT.expr_of_yojson)
//     ~stringconv:preprocess
//   |> Option.map ~f:Serialization_converters.oplist_to_fluid
//   |> Option.map ~f:Op.oplist2tlid_oplists
//   |> Option.value ~default:[]
//
//
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
//
//
// let load_and_resave (h : host) : (unit, string list) Result.t =
//   ignore (Db.run ~name:"start_transaction" ~params:[] "BEGIN") ;
//   let result = load_all h [] |> Result.map ~f:(fun c -> save_all !c) in
//   ignore (Db.run ~name:"end_transaction" ~params:[] "COMMIT") ;
//   result
//
//
// let load_and_resave_from_test_file (host : string) : unit =
//   let owner = Account.for_host_exn host in
//   let c =
//     load_from
//       host
//       owner
//       []
//       ~f:(load_json_from_disk ~root:Testdata ~preprocess:ident)
//     |> Result.map_error ~f:(String.concat ~sep:", ")
//     |> Prelude.Result.ok_or_internal_exception "Canvas load error"
//   in
//   save_all !c
//
//
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
//   |> Result.ok_or_failwith
//
//
// (* just load, don't save -- also don't validate the ops don't
//  * have deprecate ops (via validate_op or validate_host). this
//  * function is used by the readiness check to gate deploys, so
//  * we don't want to prevent deploys because someone forgot a deprecatedop
//  * in a tier 1 canvas somewhere *)
// let check_tier_one_hosts () : unit =
//   let hosts = Serialize.tier_one_hosts () in
//   List.iter hosts ~f:(fun host ->
//       match load_all host [] with
//       | Ok _ ->
//           ()
//       | Error errs ->
//           Exception.internal
//             ~info:[("errors", String.concat ~sep:", " errs); ("host", host)]
//             "Bad canvas state")
//
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
//       migrate_host host |> Result.ok_or_failwith)
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


let canvasIDForCanvasName
  (owner : UserID)
  (canvasName : CanvasName.T)
  : Task<CanvasID> =
  let canvasName = canvasName.ToString()
  // TODO: we create the canvas if it doesn't exist here, seems like a poor choice
  Sql.query "SELECT canvas_id(@newUUID, @owner, @canvasName)"
  |> Sql.parameters [ "newUUID", Sql.uuid (System.Guid.NewGuid())
                      "owner", Sql.uuid owner
                      "canvasName", Sql.string canvasName ]
  |> Sql.executeRowAsync (fun read -> read.uuid "canvas_id")

let canvasNameFromCustomDomain (customDomain : string) : Task<Option<CanvasName.T>> =
  Sql.query
    "SELECT canvas
     FROM custom_domains
     WHERE host = @host"
  |> Sql.parameters [ "host", Sql.string customDomain ]
  |> Sql.executeRowOptionAsync
       (fun read -> read.string "canvas" |> CanvasName.create)
