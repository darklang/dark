open Core_kernel
open Libexecution
open Libcommon
open Util
open Types
module RTT = Types.RuntimeT
module TL = Toplevel

type cors_setting =
  | AllOrigins
  | Origins of string list
[@@deriving eq, show]

type 'expr_type canvas =
  { host : string
  ; owner : Uuidm.t
  ; id : Uuidm.t
  ; creation_date : Time.t [@opaque]
  ; ops : (tlid * 'expr_type Op.oplist) list
  ; cors_setting : cors_setting option
  ; handlers : 'expr_type TL.toplevels
  ; dbs : 'expr_type TL.toplevels
  ; user_functions : 'expr_type RTT.user_fn IDMap.t
  ; user_tipes : RTT.user_tipe IDMap.t
  ; package_fns : 'expr_type RTT.fn list [@opaque]
  ; deleted_handlers : 'expr_type TL.toplevels
  ; deleted_dbs : 'expr_type TL.toplevels
  ; deleted_user_functions : 'expr_type RTT.user_fn IDMap.t
  ; deleted_user_tipes : RTT.user_tipe IDMap.t }
[@@deriving eq, show]

(* --------------- *)
(* Convert to fluid *)
(* --------------- *)
let to_fluid (c : RTT.expr canvas) : Types.fluid_expr canvas =
  { host = c.host
  ; owner = c.owner
  ; id = c.id
  ; creation_date = c.creation_date
  ; cors_setting = c.cors_setting
  ; package_fns = List.map ~f:Toplevel.fn_to_fluid c.package_fns
  ; ops =
      List.map c.ops ~f:(fun (tlid, oplist) ->
          (tlid, Op.oplist_to_fluid oplist))
  ; handlers = IDMap.map ~f:Toplevel.to_fluid c.handlers
  ; dbs = IDMap.map ~f:Toplevel.to_fluid c.dbs
  ; user_functions = IDMap.map ~f:Toplevel.user_fn_to_fluid c.user_functions
  ; user_tipes = c.user_tipes
  ; deleted_handlers = IDMap.map ~f:Toplevel.to_fluid c.deleted_handlers
  ; deleted_dbs = IDMap.map ~f:Toplevel.to_fluid c.deleted_dbs
  ; deleted_user_functions =
      IDMap.map ~f:Toplevel.user_fn_to_fluid c.deleted_user_functions
  ; deleted_user_tipes = c.deleted_user_tipes }


let to_fluid_ref (c : RTT.expr canvas ref) : Types.fluid_expr canvas ref =
  ref (to_fluid !c)


let of_fluid (c : Types.fluid_expr canvas) : RTT.expr canvas =
  { host = c.host
  ; owner = c.owner
  ; id = c.id
  ; creation_date = c.creation_date
  ; cors_setting = c.cors_setting
  ; package_fns = List.map ~f:Toplevel.fn_of_fluid c.package_fns
  ; ops =
      List.map c.ops ~f:(fun (tlid, oplist) ->
          (tlid, Op.oplist_of_fluid oplist))
  ; handlers = IDMap.map ~f:Toplevel.of_fluid c.handlers
  ; dbs = IDMap.map ~f:Toplevel.of_fluid c.dbs
  ; user_functions = IDMap.map ~f:Toplevel.user_fn_of_fluid c.user_functions
  ; user_tipes = c.user_tipes
  ; deleted_handlers = IDMap.map ~f:Toplevel.of_fluid c.deleted_handlers
  ; deleted_dbs = IDMap.map ~f:Toplevel.of_fluid c.deleted_dbs
  ; deleted_user_functions =
      IDMap.map ~f:Toplevel.user_fn_of_fluid c.deleted_user_functions
  ; deleted_user_tipes = c.deleted_user_tipes }


(* ------------------------- *)
(* Toplevel *)
(* ------------------------- *)

let set_db tlid pos data c =
  (* if the db had been deleted, remove it from the deleted set. This handles
   * a data race where a Set comes in after a Delete. *)
  { c with
    dbs = IDMap.set c.dbs tlid {tlid; pos; data}
  ; deleted_dbs = IDMap.remove c.deleted_dbs tlid }


let set_handler tlid pos data c =
  (* if the handler had been deleted, remove it from the deleted set. This handles
   * a data race where a Set comes in after a Delete. *)
  { c with
    handlers = IDMap.set c.handlers tlid {tlid; pos; data}
  ; deleted_handlers = IDMap.remove c.deleted_handlers tlid }


let set_function (user_fn : 'expr_type RuntimeT.user_fn) (c : 'expr_type canvas)
    : 'expr_type canvas =
  (* if the fn had been deleted, remove it from the deleted set. This handles
   * a data race where a Set comes in after a Delete. *)
  { c with
    user_functions = IDMap.set c.user_functions user_fn.tlid user_fn
  ; deleted_user_functions = IDMap.remove c.deleted_user_functions user_fn.tlid
  }


let set_tipe (user_tipe : RuntimeT.user_tipe) (c : 'expr_type canvas) :
    'expr_type canvas =
  (* if the tipe had been deleted, remove it from the deleted set. This handles
   * a data race where a Set comes in after a Delete. *)
  { c with
    user_tipes = IDMap.set c.user_tipes user_tipe.tlid user_tipe
  ; deleted_user_tipes = IDMap.remove c.deleted_user_tipes user_tipe.tlid }


let delete_function (tlid : tlid) (c : 'expr_type canvas) : 'expr_type canvas =
  match IDMap.find c.user_functions tlid with
  | None ->
      c
  | Some user_fn ->
      { c with
        user_functions = IDMap.remove c.user_functions tlid
      ; deleted_user_functions = IDMap.set c.deleted_user_functions tlid user_fn
      }


let delete_function_forever (tlid : tlid) (c : 'expr_type canvas) :
    'expr_type canvas =
  { c with
    user_functions = IDMap.remove c.user_functions tlid
  ; deleted_user_functions = IDMap.remove c.deleted_user_functions tlid }


let delete_tipe (tlid : tlid) (c : 'expr_type canvas) : 'expr_type canvas =
  match IDMap.find c.user_tipes tlid with
  | None ->
      c
  | Some user_tipe ->
      { c with
        user_tipes = IDMap.remove c.user_tipes tlid
      ; deleted_user_tipes = IDMap.set c.deleted_user_tipes tlid user_tipe }


let delete_tipe_forever (tlid : tlid) (c : 'expr_type canvas) :
    'expr_type canvas =
  { c with
    user_tipes = IDMap.remove c.user_tipes tlid
  ; deleted_user_tipes = IDMap.remove c.deleted_user_tipes tlid }


let delete_tl_forever (tlid : tlid) (c : 'expr_type canvas) : 'expr_type canvas
    =
  { c with
    dbs = IDMap.remove c.dbs tlid
  ; handlers = IDMap.remove c.handlers tlid
  ; deleted_dbs = IDMap.remove c.deleted_dbs tlid
  ; deleted_handlers = IDMap.remove c.deleted_handlers tlid }


let delete_toplevel (tlid : tlid) (c : 'expr_type canvas) : 'expr_type canvas =
  let db = IDMap.find c.dbs tlid in
  let handler = IDMap.find c.handlers tlid in
  { c with
    dbs = IDMap.remove c.dbs tlid
  ; handlers = IDMap.remove c.handlers tlid
  ; deleted_dbs = IDMap.change c.deleted_dbs tlid ~f:(fun _ -> db)
  ; deleted_handlers =
      IDMap.change c.deleted_handlers tlid ~f:(fun _ -> handler) }


let apply_to_toplevel
    ~(f : 'expr_type TL.toplevel -> 'expr_type TL.toplevel)
    (tlid : tlid)
    (tls : 'expr_type TL.toplevels) =
  IDMap.change tls tlid ~f:(Option.map ~f)


let apply_to_all_toplevels
    ~(f : 'expr_type TL.toplevel -> 'expr_type TL.toplevel)
    (tlid : tlid)
    (c : 'expr_type canvas) : 'expr_type canvas =
  { c with
    handlers = apply_to_toplevel ~f tlid c.handlers
  ; dbs = apply_to_toplevel ~f tlid c.dbs }


let apply_to_db
    ~(f : 'expr_type RTT.DbT.db -> 'expr_type RTT.DbT.db)
    (tlid : tlid)
    (c : 'expr_type canvas) : 'expr_type canvas =
  let tlf (tl : 'expr_type TL.toplevel) =
    let data =
      match tl.data with
      | TL.DB db ->
          TL.DB (f db)
      | _ ->
          Exception.internal "Provided ID is not for a DB"
    in
    {tl with data}
  in
  {c with dbs = apply_to_toplevel tlid ~f:tlf c.dbs}


let move_toplevel (tlid : tlid) (pos : pos) (c : 'expr_type canvas) :
    'expr_type canvas =
  apply_to_all_toplevels ~f:(fun tl -> {tl with pos}) tlid c


(* ------------------------- *)
(* Build *)
(* ------------------------- *)

let apply_op (is_new : bool) (op : 'expr_type Op.op) (c : 'expr_type canvas ref)
    : unit =
  try
    c :=
      !c
      |>
      match op with
      | SetHandler (tlid, pos, handler) ->
          set_handler tlid pos (TL.Handler handler)
      | CreateDB (tlid, pos, name) ->
          if is_new && name = "" then Exception.client "DB must have a name" ;
          let db = User_db.create name tlid in
          set_db tlid pos (TL.DB db)
      | AddDBCol (tlid, colid, typeid) ->
          apply_to_db ~f:(User_db.add_col colid typeid) tlid
      | SetDBColName (tlid, id, name) ->
          apply_to_db ~f:(User_db.set_col_name id name) tlid
      | ChangeDBColName (tlid, id, name) ->
          apply_to_db ~f:(User_db.change_col_name id name) tlid
      | SetDBColType (tlid, id, tipe) ->
          apply_to_db
            ~f:(User_db.set_col_type id (Dval.tipe_of_string tipe))
            tlid
      | ChangeDBColType (tlid, id, tipe) ->
          apply_to_db
            ~f:(User_db.change_col_type id (Dval.tipe_of_string tipe))
            tlid
      | DeleteDBCol (tlid, id) ->
          apply_to_db ~f:(User_db.delete_col id) tlid
      | DeprecatedInitDbm (tlid, id, rbid, rfid, kind) ->
          ident
      | CreateDBMigration (tlid, rbid, rfid, cols) ->
          let typed_cols =
            List.map cols ~f:(fun (n, t) ->
                match t with
                | Filled (id, ts) ->
                    (n, Filled (id, Dval.tipe_of_string ts))
                | Partial _ as b ->
                    (n, b)
                | Blank _ as b ->
                    (n, b))
          in
          apply_to_db ~f:(User_db.create_migration rbid rfid typed_cols) tlid
      | AddDBColToDBMigration (tlid, colid, typeid) ->
          apply_to_db ~f:(User_db.add_col_to_migration colid typeid) tlid
      | SetDBColNameInDBMigration (tlid, id, name) ->
          apply_to_db ~f:(User_db.set_col_name_in_migration id name) tlid
      | SetDBColTypeInDBMigration (tlid, id, tipe) ->
          apply_to_db
            ~f:(User_db.set_col_type_in_migration id (Dval.tipe_of_string tipe))
            tlid
      | AbandonDBMigration tlid ->
          apply_to_db ~f:User_db.abandon_migration tlid
      | DeleteColInDBMigration (tlid, id) ->
          apply_to_db ~f:(User_db.delete_col_in_migration id) tlid
      | SetExpr (tlid, id, e) ->
          apply_to_all_toplevels ~f:(TL.set_expr id e) tlid
      | DeleteTL tlid ->
          delete_toplevel tlid
      | MoveTL (tlid, pos) ->
          move_toplevel tlid pos
      | SetFunction user_fn ->
          set_function user_fn
      | DeleteFunction tlid ->
          delete_function tlid
      | TLSavepoint _ ->
          ident
      | UndoTL _ | RedoTL _ ->
          Exception.internal
            ( "This should have been preprocessed out! "
            ^ Op.show_op (fun _ _ -> ()) op )
      | RenameDBname (tlid, name) ->
          apply_to_db ~f:(User_db.rename_db name) tlid
      | CreateDBWithBlankOr (tlid, pos, id, name) ->
          let db = User_db.create2 name tlid id in
          set_db tlid pos (TL.DB db)
      | DeleteTLForever tlid ->
          delete_tl_forever tlid
      | DeleteFunctionForever tlid ->
          delete_function_forever tlid
      | SetType user_tipe ->
          set_tipe user_tipe
      | DeleteType tlid ->
          delete_tipe tlid
      | DeleteTypeForever tlid ->
          delete_tipe_forever tlid
  with e ->
    (* Log here so we have context, but then re-raise *)
    Log.erroR
      "apply_op failure"
      ~params:
        [ ("host", !c.host)
        ; ("op", Op.show_op (fun _ _ -> ()) op)
        ; ("exn", Exception.to_string e) ] ;
    Exception.reraise e


(* NOTE: If you add a new verification here, please ensure all places that
 * load canvases/apply ops correctly load the requisite data.
 *
 *
 * See `Op.required_context` for how we determine which ops need what other
 * context to be loaded to appropriately verify.
 *
 * *)
let verify (c : 'expr_type canvas ref) : (unit, string list) Result.t =
  let duped_db_names =
    !c.dbs
    |> TL.dbs
    |> List.filter_map ~f:(fun db ->
           Option.map
             ~f:(fun name -> (db.tlid, name))
             (Ast.blank_to_option db.name))
    |> List.group ~break:(fun (_, name1) (_, name2) -> name1 <> name2)
    |> List.filter ~f:(fun g -> List.length g > 1)
    |> List.map ~f:(fun gs ->
           let string_of_pair (tlid, name) =
             Printf.sprintf "(%s, %s)" (string_of_id tlid) name
           in
           let string_of_pairs ps =
             String.concat ~sep:", " (List.map ~f:string_of_pair ps)
           in
           Printf.sprintf "Duplicate DB names: %s" (string_of_pairs gs))
  in
  match duped_db_names with [] -> Ok () | dupes -> Error dupes


let add_ops
    (c : 'expr_type canvas ref)
    (oldops : 'expr_type Op.op list)
    (newops : 'expr_type Op.op list) : unit =
  let oldops = List.map ~f:(fun op -> (false, op)) oldops in
  let newops = List.map ~f:(fun op -> (true, op)) newops in
  let reduced_ops = Undo.preprocess (oldops @ newops) in
  List.iter ~f:(fun (is_new, op) -> apply_op is_new op c) reduced_ops ;
  let allops = oldops @ newops |> List.map ~f:Tuple.T2.get2 in
  c := {!c with ops = Op.oplist2tlid_oplists allops}


let fetch_cors_setting (id : Uuidm.t) : cors_setting option =
  let cors_setting_of_db_string (string_from_db : string) : cors_setting option
      =
    (* none if null from datastore *)
    (if string_from_db = "" then None else Some string_from_db)
    (* parse json, handle if it's not valid... *)
    |> Option.map ~f:Yojson.Safe.from_string
    (* json -> string list *)
    |> Option.bind ~f:(fun j ->
           match j with
           | `String "*" ->
               Some AllOrigins
           | `List js ->
               js
               |> List.map ~f:(fun s ->
                      match s with
                      | `String s ->
                          s
                      | _ ->
                          Exception.internal
                            "CORS setting from DB is a list containing a non-string")
               |> Origins
               |> Some
           | _ ->
               Exception.internal
                 "CORS setting from DB is neither a string or a list.")
  in
  Db.fetch_one
    ~name:"fetch_cors_setting"
    "SELECT cors_setting FROM canvases WHERE id = $1"
    ~params:[Uuid id]
  |> List.hd_exn
  |> cors_setting_of_db_string


let canvas_creation_date canvas_id : Core_kernel.Time.t =
  Db.fetch_one
    ~name:"canvas_creation_date"
    "SELECT created_at from canvases
      WHERE canvases.id = $1"
    ~params:[Uuid canvas_id]
  |> List.hd_exn
  |> Db.date_of_sqlstring


let init (host : string) (ops : 'expr_type Op.op list) :
    ('expr_type canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let creation_date = canvas_creation_date canvas_id in
  let cors = fetch_cors_setting canvas_id in
  let c =
    ref
      { host
      ; owner
      ; id = canvas_id
      ; creation_date
      ; ops = []
      ; cors_setting = cors
      ; handlers = IDMap.empty
      ; dbs = IDMap.empty
      ; user_functions = IDMap.empty
      ; user_tipes = IDMap.empty
      ; package_fns = []
      ; deleted_handlers = IDMap.empty
      ; deleted_dbs = IDMap.empty
      ; deleted_user_functions = IDMap.empty
      ; deleted_user_tipes = IDMap.empty }
  in
  add_ops c [] ops ;
  c |> verify |> Result.map ~f:(fun _ -> c)


let name_for_id (id : Uuidm.t) : string =
  Db.fetch_one
    ~name:"fetch_canvas_name"
    "SELECT name FROM canvases WHERE id = $1"
    ~params:[Uuid id]
  |> List.hd_exn


let id_for_name_option (name : string) : Uuidm.t option =
  Db.fetch_one_option
    ~name:"fetch_canvas_id"
    "SELECT id FROM canvases WHERE name = $1"
    ~params:[Db.String name]
  (* If List.hd_exn exn's, it means that `SELECT id` returned a record with more
   * than one field..  Can't happen. *)
  |> Option.map ~f:List.hd_exn
  |> Option.bind ~f:Uuidm.of_string


let id_for_name (name : string) : Uuidm.t =
  name |> id_for_name_option |> Option.value_exn


let update_cors_setting
    (c : 'expr_type canvas ref) (setting : cors_setting option) : unit =
  let cors_setting_to_db (setting : cors_setting option) : 'expr_type Db.param =
    match setting with
    | None ->
        Db.Null
    | Some AllOrigins ->
        `String "*" |> Yojson.Safe.to_string |> Db.String
    | Some (Origins ss) ->
        ss
        |> List.map ~f:(fun s -> `String s)
        |> (fun l -> `List l)
        |> Yojson.Safe.to_string
        |> Db.String
  in
  Db.run
    ~name:"update_cors_setting"
    "UPDATE canvases
     SET cors_setting = $1
     WHERE id = $2"
    ~params:[cors_setting_to_db setting; Uuid !c.id] ;
  c := {!c with cors_setting = setting} ;
  ()


let url_for (id : Uuidm.t) : string =
  let canvas_name = name_for_id id in
  "http://" ^ canvas_name ^ "." ^ Config.public_domain


(* ------------------------- *)
(* Loading/saving *)
(* ------------------------- *)

let load_from
    (host : string)
    (owner : Uuidm.t)
    (newops : 'expr_type Op.op list)
    ~(f :
       host:string -> canvas_id:Uuidm.t -> unit -> 'expr_type Op.tlid_oplists) :
    ('expr_type canvas ref, string list) Result.t =
  try
    let canvas_id = Serialize.fetch_canvas_id owner host in
    let cors = fetch_cors_setting canvas_id in
    let creation_date = canvas_creation_date canvas_id in
    let oldops = f ~host ~canvas_id () in
    (* TODO optimization: can we get only the functions we need (based on
     * fnnames found in the canvas) and/or cache this like we do the oplist? *)
    let package_fns =
      Package_manager.all_functions ()
      |> List.map ~f:Package_manager.runtime_fn_of_package_fn
    in
    let c =
      ref
        { host
        ; owner
        ; id = canvas_id
        ; creation_date
        ; ops = []
        ; cors_setting = cors
        ; handlers = IDMap.empty
        ; dbs = IDMap.empty
        ; user_functions = IDMap.empty
        ; user_tipes = IDMap.empty
        ; package_fns
        ; deleted_handlers = IDMap.empty
        ; deleted_dbs = IDMap.empty
        ; deleted_user_functions = IDMap.empty
        ; deleted_user_tipes = IDMap.empty }
    in
    add_ops c (Op.tlid_oplists2oplist oldops) newops ;
    c |> verify |> Result.map ~f:(fun _ -> c)
  with e -> Libexecution.Exception.reraise_as_pageable e


let load_without_tls host : ('expr_type canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  load_from ~f:(fun ~host ~canvas_id () -> []) host owner []


let load_all host (newops : RTT.expr Op.op list) :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  load_from ~f:Serialize.load_all_from_db host owner newops


let load_only_tlids ~tlids host (newops : RTT.expr Op.op list) :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  load_from ~f:(Serialize.load_only_tlids ~tlids) host owner newops


(* Same as `load_only_tlids` but filters out deleted tlids via
 * the denormalized `deleted` attributed on toplevel_oplists *)
let load_only_undeleted_tlids ~tlids host (newops : RTT.expr Op.op list) :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  load_from ~f:(Serialize.load_only_undeleted_tlids ~tlids) host owner newops


let load_with_dbs ~tlids host (newops : RTT.expr Op.op list) :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  load_from ~f:(Serialize.load_with_dbs ~tlids) host owner newops


(* `uncached_loader` is the function used to initialize the canvas, and load
 * any necessary tlids that were not returned from the fast_loader/materialized view.
 *
 * tlids might not be returned from the materialized view/fast loader/cache if:
 *  a) they have no materialized view (probably not possible anymore!)
 *  b) they are deleted, because the cache query filters out deleted items
 *  c) the deserializers for the cache version are broken (due to a binary version
 *  change!)
 *
 *  The default is `load_only_undeleted_tlids` which also filters out deleted
 *  toplevels, but is parameterized in case a caller wants to load deleted toplevels.
 *)
let load_from_cache
    ?(uncached_loader = load_only_undeleted_tlids) ~tlids host owner :
    (RTT.expr canvas ref, string list) Result.t =
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let ( fast_loaded_handlers
      , fast_loaded_dbs
      , fast_loaded_user_fns
      , fast_loaded_user_tipes ) =
    Serialize.load_only_rendered_tlids ~host ~canvas_id ~tlids ()
  in
  let fast_loaded_tlids =
    IDMap.keys fast_loaded_handlers
    @ IDMap.keys fast_loaded_dbs
    @ IDMap.keys fast_loaded_user_fns
    @ IDMap.keys fast_loaded_user_tipes
  in
  let not_loaded_tlids =
    List.filter
      ~f:(fun x -> not (List.mem ~equal:( = ) fast_loaded_tlids x))
      tlids
  in
  (* canvas initialized via the normal loading path with the non-fast loaded tlids
   * loaded traditionally via the oplist *)
  let canvas = uncached_loader ~tlids:not_loaded_tlids host [] in
  canvas
  |> Result.map ~f:(fun canvas ->
         List.iter
           (IDMap.to_alist fast_loaded_handlers)
           ~f:(fun (tlid, (h, pos)) ->
             let c = !canvas in
             let c =
               { c with
                 handlers =
                   IDMap.set c.handlers tlid {tlid; pos; data = Handler h} }
             in
             canvas := c) ;
         List.iter (IDMap.to_alist fast_loaded_dbs) ~f:(fun (tlid, (db, pos)) ->
             let c = !canvas in
             let c =
               {c with dbs = IDMap.set c.dbs tlid {tlid; pos; data = DB db}}
             in
             canvas := c) ;
         List.iter (IDMap.to_alist fast_loaded_user_fns) ~f:(fun (tlid, ufn) ->
             let c = !canvas in
             let c =
               {c with user_functions = IDMap.set c.user_functions tlid ufn}
             in
             canvas := c) ;
         List.iter (IDMap.to_alist fast_loaded_user_tipes) ~f:(fun (tlid, ut) ->
             let c = !canvas in
             let c = {c with user_tipes = IDMap.set c.user_tipes tlid ut} in
             canvas := c) ;
         canvas)
  |> Result.map ~f:(fun canvas ->
         (* Empty out the oplist, this prevents anyone accidentally saving a canvas
          * partially loaded from the cache. *)
         canvas := {!canvas with ops = []} ;
         canvas)


let load_all_from_cache host : (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  load_from_cache
  (* As we're loading _all_, we want to pass in an uncached_loader that
   * will ensure we also load deleted toplevels *)
    ~uncached_loader:load_only_tlids
    ~tlids:(Serialize.fetch_all_tlids ~canvas_id ())
    host
    owner


let load_http_from_cache ~verb ~path host :
    (RTT.expr canvas ref, string list) Result.t =
  (* Attempt to load all required toplvels via their
   * cached repr, and then go and fetch whatever we were missing*)
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  load_from_cache
    ~tlids:
      (Serialize.fetch_relevant_tlids_for_http ~host ~canvas_id ~path ~verb ())
    host
    owner


let load_tlids_from_cache ~tlids host :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  load_from_cache ~tlids host owner


let load_tlids_with_context_from_cache ~tlids host :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let tlids =
    let context =
      Serialize.fetch_relevant_tlids_for_execution ~host ~canvas_id ()
    in
    tlids @ context
  in
  load_from_cache ~tlids host owner


let load_for_event_from_cache (event : RTT.expr Event_queue.t) :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn event.host in
  let canvas_id = Serialize.fetch_canvas_id owner event.host in
  load_from_cache
    ~tlids:(Serialize.fetch_relevant_tlids_for_event ~event ~canvas_id ())
    event.host
    owner


let load_all_dbs_from_cache host : (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  load_from_cache
    ~tlids:(Serialize.fetch_tlids_for_all_dbs ~canvas_id ())
    host
    owner


let load_for_cron_checker_from_cache host :
    (RTT.expr canvas ref, string list) Result.t =
  let owner = Account.for_host_exn host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  load_from_cache
    ~tlids:(Serialize.fetch_relevant_tlids_for_cron_checker ~canvas_id ())
    host
    owner


let serialize_only (tlids : tlid list) (c : RTT.expr canvas) : unit =
  try
    let munge_name module_ n =
      if Ast.blank_to_option module_ = Some "HTTP"
      then Http.route_to_postgres_pattern n
      else n
    in
    let handler_metadata (h : RTT.expr RTT.HandlerT.handler) =
      ( Ast.blank_to_option h.spec.name
        |> Option.map ~f:(munge_name h.spec.module_)
      , Ast.blank_to_option h.spec.module_
      , Ast.blank_to_option h.spec.modifier )
    in
    (* Use ops rather than just set of toplevels, because toplevels may
   * have been deleted or undone, and therefore not appear, but it's
   * important to record them. *)
    List.iter c.ops ~f:(fun (tlid, oplist) ->
        (* Only save oplists that have been used. *)
        if List.mem ~equal:( = ) tlids tlid
        then
          let name, module_, modifier =
            IDMap.find c.handlers tlid
            |> Option.bind ~f:TL.as_handler
            |> Option.map ~f:handler_metadata
            |> Option.value ~default:(None, None, None)
          in
          let handler () =
            IDMap.find c.handlers tlid
            |> Option.bind ~f:(fun tl ->
                   tl |> TL.as_handler |> Option.map ~f:(fun h -> (tl.pos, h)))
            |> Option.map ~f:(fun (pos, h) ->
                   ( Serialize.handler_to_binary_string h
                   , false
                   , Some pos
                   , TL.TLHandler ))
          in
          let deleted_handler () =
            IDMap.find c.deleted_handlers tlid
            |> Option.bind ~f:(fun tl ->
                   tl |> TL.as_handler |> Option.map ~f:(fun h -> (tl.pos, h)))
            |> Option.map ~f:(fun (pos, h) ->
                   ( Serialize.handler_to_binary_string h
                   , true
                   , Some pos
                   , TL.TLHandler ))
          in
          let db () =
            IDMap.find c.dbs tlid
            |> Option.bind ~f:(fun tl ->
                   tl |> TL.as_db |> Option.map ~f:(fun db -> (tl.pos, db)))
            |> Option.map ~f:(fun (pos, db) ->
                   (Serialize.db_to_binary_string db, false, Some pos, TL.TLDB))
          in
          let deleted_db () =
            IDMap.find c.deleted_dbs tlid
            |> Option.bind ~f:(fun tl ->
                   tl |> TL.as_db |> Option.map ~f:(fun db -> (tl.pos, db)))
            |> Option.map ~f:(fun (pos, db) ->
                   (Serialize.db_to_binary_string db, true, Some pos, TL.TLDB))
          in
          let user_function () =
            IDMap.find c.user_functions tlid
            |> Option.map ~f:(fun fn ->
                   ( Serialize.user_fn_to_binary_string fn
                   , false
                   , None
                   , TL.TLUserFunction ))
          in
          let deleted_user_function () =
            IDMap.find c.deleted_user_functions tlid
            |> Option.map ~f:(fun fn ->
                   ( Serialize.user_fn_to_binary_string fn
                   , true
                   , None
                   , TL.TLUserFunction ))
          in
          let user_tipe () =
            IDMap.find c.user_tipes tlid
            |> Option.map ~f:(fun t ->
                   ( Serialize.user_tipe_to_binary_string t
                   , false
                   , None
                   , TL.TLUserTipe ))
          in
          let deleted_user_tipe () =
            IDMap.find c.deleted_user_tipes tlid
            |> Option.map ~f:(fun t ->
                   ( Serialize.user_tipe_to_binary_string t
                   , true
                   , None
                   , TL.TLUserTipe ))
          in
          let binary_repr, deleted, pos, tipe =
            handler ()
            |> Tc.Option.or_else_lazy deleted_handler
            |> Tc.Option.or_else_lazy db
            |> Tc.Option.or_else_lazy deleted_handler
            |> Tc.Option.or_else_lazy deleted_db
            |> Tc.Option.or_else_lazy user_function
            |> Tc.Option.or_else_lazy deleted_user_function
            |> Tc.Option.or_else_lazy user_tipe
            |> Tc.Option.or_else_lazy deleted_user_tipe
            |> Option.map ~f:(fun (str, d, pos, t) ->
                   (Some str, Some d, pos, t))
            (* If the user calls Undo enough, we might not know
             * the tipe here. In that case, set to handler cause
             * it won't be used anyway *)
            |> Option.value ~default:(None, None, None, TL.TLHandler)
          in
          Serialize.save_toplevel_oplist
            oplist
            ~binary_repr
            ~tlid
            ~canvas_id:c.id
            ~account_id:c.owner
            ~name
            ~module_
            ~modifier
            ~deleted
            ~pos
            ~tipe
        else ())
  with e -> Libexecution.Exception.reraise_as_pageable e


let save_tlids (c : RTT.expr canvas) (tlids : tlid list) : unit =
  serialize_only tlids c


let save_all (c : RTT.expr canvas) : unit =
  let tlids = List.map ~f:Tuple.T2.get1 c.ops in
  save_tlids c tlids


(* ------------------------- *)
(* Testing/validation *)
(* ------------------------- *)

let load_and_resave (h : host) : (unit, string list) Result.t =
  ignore (Db.run ~name:"start_transaction" ~params:[] "BEGIN") ;
  let result = load_all h [] |> Result.map ~f:(fun c -> save_all !c) in
  ignore (Db.run ~name:"end_transaction" ~params:[] "COMMIT") ;
  result


let load_and_resave_from_test_file (host : string) : unit =
  let owner = Account.for_host_exn host in
  let c =
    load_from
      host
      owner
      []
      ~f:(Serialize.load_json_from_disk ~root:Testdata ~preprocess:ident)
    |> Result.map_error ~f:(String.concat ~sep:", ")
    |> Prelude.Result.ok_or_internal_exception "Canvas load error"
  in
  save_all !c


let minimize (c : 'expr_type canvas) : 'expr_type canvas =
  (* TODO *)
  (* let ops = *)
  (*   c.ops *)
  (*   |> Undo.preprocess *)
  (*   |> List.filter ~f:Op.has_effect *)
  (* in { c with ops = ops } *)
  c


let save_test (c : 'expr_type canvas) : string =
  let c = minimize c in
  let host = "test-" ^ c.host in
  let file = Serialize.json_filename host in
  let host =
    if File.file_exists ~root:Testdata file
    then host ^ "_" ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
    else host
  in
  let file = Serialize.json_filename host in
  Serialize.save_json_to_disk ~root:Testdata file c.ops ;
  file


(* --------------- *)
(* Validate fluid *)
(* --------------- *)
(* Types for fluid validation which have an `equal` function that papers over
 * superficial differences that occur as part of the conversion process (eg ids
 * don't need to be equal). It's done this way so that we can inject the
 * non-derived equality function, but use all the other derived equality
 * functions. *)
type id_insensitive_expr = RTT.expr [@@deriving show]

let equal_id_insensitive_expr
    (e1 : id_insensitive_expr) (e2 : id_insensitive_expr) =
  Fluid.testExprEqualIgnoringIds e1 e2


type id_insensitive_fluid_expr = fluid_expr [@@deriving show]

let equal_id_insensitive_fluid_expr
    (e1 : id_insensitive_fluid_expr) (e2 : id_insensitive_fluid_expr) =
  Libshared.FluidExpression.testEqualIgnoringIds e1 e2


type id_insensitive_expr_canvas = id_insensitive_expr canvas
[@@deriving show, eq]

type id_insensitive_fluid_expr_canvas = id_insensitive_fluid_expr canvas
[@@deriving show, eq]

type id_insensitive_expr_oplist = id_insensitive_expr Op.oplist
[@@deriving show, eq]

type id_insensitive_fluid_expr_oplist = id_insensitive_fluid_expr Op.oplist
[@@deriving show, eq]

let validate_roundtrip_to_fluid (c : RTT.expr canvas) : (unit, string) Result.t
    =
  (* fluid to expr creates new ids, but so does expr to fluid. Check
   * structure of both identical, ignoring ids *)
  let all_ops = c.ops |> Op.tlid_oplists2oplist in
  let all_roundtripped_ops =
    all_ops |> Op.oplist_to_fluid |> Op.oplist_of_fluid
  in
  let roundtrip_valid =
    equal_id_insensitive_expr_oplist all_ops all_roundtripped_ops
  in
  if roundtrip_valid
  then Ok ()
  else
    (* Log.inspecT "all_ops" ~f:show_expr_oplist all_ops ; *)
    (* Log.inspecT *)
    (*   "all_roundtripped_ops" *)
    (*   ~f:show_expr_oplist *)
    (*   all_roundtripped_ops ; *)
    Error ("roundtrip not valid: " ^ c.host)


let is_valid_roundtrip_of_fluid (c : Types.fluid_expr canvas) :
    (unit, string) Result.t =
  (* fluid to expr creates new ids, but so does expr to fluid. Check
   * structure of both identical, ignoring ids *)
  let all_ops = c.ops |> Op.tlid_oplists2oplist in
  let all_roundtripped_ops =
    all_ops |> Op.oplist_of_fluid |> Op.oplist_to_fluid
  in
  let roundtrip_valid =
    equal_id_insensitive_fluid_expr_oplist all_ops all_roundtripped_ops
  in
  if roundtrip_valid
  then Ok ()
  else
    (* Log.inspecT "all_ops" ~f:show_expr_oplist all_ops ; *)
    (* Log.inspecT *)
    (*   "all_roundtripped_ops" *)
    (*   ~f:show_expr_oplist *)
    (*   all_roundtripped_ops ; *)
    Error ("fluid roundtrip not valid: " ^ c.host)


(* --------------- *)
(* Validate canvases *)
(* --------------- *)

(* This is a little broad, since we could limit it to tlids, but good enough
 * for now. At time of writing, these all have the duplicate DB problem. *)
let known_invalid_hosts =
  Tc.StrSet.from_list
    [ "danbowles"
    ; "danwetherald"
    ; "ellen-dbproblem18"
    ; "ellen-preview"
    ; "ellen-stltrialrun"
    ; "ellen-trinity" ]


let all_hosts () : string list =
  List.filter (Serialize.current_hosts ()) ~f:(fun host ->
      not (Tc.StrSet.member known_invalid_hosts ~value:host))


let is_valid_op op : bool = not (Op.is_deprecated op)

let validate_host host : (unit, string) Result.t =
  try
    match load_all host [] with
    | Ok c ->
        let all_ops = !c.ops |> Op.tlid_oplists2oplist in
        let ops_valid =
          if Tc.List.all all_ops ~f:is_valid_op
          then Ok ()
          else Error "Ops are not valid"
        in
        let cache_valid =
          try
            let tlids =
              all_ops
              |> List.map ~f:Op.tlidOf
              |> List.dedup_and_sort ~compare:compare_tlid
            in
            load_tlids_from_cache ~tlids host
            |> Tc.Result.map_error String.concat
            |> Tc.Result.andThen ~f:(fun c ->
                   let roundtripped = !c |> to_fluid |> of_fluid in
                   if equal_id_insensitive_expr_canvas !c roundtripped
                   then Ok ()
                   else Error ("cache roundtrip not valid: " ^ host))
          with e -> Error "couldn't load cache"
        in
        Tc.Result.combine [ops_valid; cache_valid]
        |> Tc.Result.map (fun _ -> ())
    | Error errs ->
        Error ("can't load " ^ host ^ ":\n" ^ Tc.String.join ~sep:", " errs)
  with e -> Error ("Invalid canvas " ^ host ^ ":\n" ^ Exception.to_string e)


let validate_all_hosts () : unit =
  all_hosts ()
  |> List.map ~f:validate_host
  |> Tc.Result.combine
  |> Tc.Result.map (fun _ -> ())
  |> Result.ok_or_failwith


(* just load, don't save -- also don't validate the ops don't
 * have deprecate ops (via validate_op or validate_host). this
 * function is used by the readiness check to gate deploys, so
 * we don't want to prevent deploys because someone forgot a deprecatedop
 * in a tier 1 canvas somewhere *)
let check_tier_one_hosts () : unit =
  let hosts = Serialize.tier_one_hosts () in
  List.iter hosts ~f:(fun host ->
      match load_all host [] with
      | Ok _ ->
          ()
      | Error errs ->
          Exception.internal
            ~info:[("errors", String.concat ~sep:", " errs); ("host", host)]
            "Bad canvas state")


(* --------------- *)
(* Migrate canvases *)
(* --------------- *)

let migrate_or_blank (ob : 'a or_blank) : 'a or_blank =
  (* Remove Partial: this implementation of partials didn't solve the problem
   * and so didn't get use. *)
  match ob with Blank _ -> ob | Partial (id, _) -> Blank id | Filled _ -> ob


let rec migrate_expr (expr : RuntimeT.expr) =
  let f e = migrate_expr e in
  match expr with
  | Partial (id, _) ->
      Blank id
  | Blank _ ->
      expr
  | Filled (id, nexpr) ->
      Filled
        ( id
        , match nexpr with
          | Value _ | Variable _ ->
              nexpr
          | Let (lhs, rhs, body) ->
              Let (migrate_or_blank lhs, f rhs, f body)
          | If (cond, ifbody, elsebody) ->
              If (f cond, f ifbody, f elsebody)
          | FnCall (name, exprs) ->
              FnCall (name, List.map ~f exprs)
          | FnCallSendToRail (name, exprs) ->
              FnCallSendToRail (name, List.map ~f exprs)
          | Lambda (vars, lexpr) ->
              Lambda (List.map ~f:migrate_or_blank vars, f lexpr)
          | Thread exprs ->
              Thread (List.map ~f exprs)
          | FieldAccess (obj, field) ->
              FieldAccess (f obj, migrate_or_blank field)
          | ListLiteral exprs ->
              ListLiteral (List.map ~f exprs)
          | ObjectLiteral pairs ->
              ObjectLiteral
                (List.map ~f:(fun (k, v) -> (migrate_or_blank k, f v)) pairs)
          | FeatureFlag (msg, cond, a, b) ->
              FeatureFlag (migrate_or_blank msg, f cond, f a, f b)
          | Match (matchExpr, cases) ->
              Match
                ( f matchExpr
                , List.map ~f:(fun (k, v) -> (migrate_or_blank k, f v)) cases )
          | Constructor (name, args) ->
              Constructor (name, List.map ~f args)
          | FluidPartial (name, old_val) ->
              FluidPartial (name, f old_val)
          | FluidRightPartial (name, old_val) ->
              FluidRightPartial (name, f old_val)
          | FluidLeftPartial (name, old_val) ->
              FluidLeftPartial (name, f old_val) )


let migrate_handler (h : 'expr_type RuntimeT.HandlerT.handler) :
    'expr_type RuntimeT.HandlerT.handler =
  {h with ast = migrate_expr h.ast}


let migrate_user_function (fn : 'expr_type RuntimeT.user_fn) =
  let migrate_bo_ufn_param (p : RuntimeT.ufn_param) : RuntimeT.ufn_param =
    {p with name = migrate_or_blank p.name; tipe = migrate_or_blank p.tipe}
  in
  let migrate_bo_metadata (m : RuntimeT.ufn_metadata) : RuntimeT.ufn_metadata =
    { m with
      name = migrate_or_blank m.name
    ; parameters = List.map m.parameters ~f:migrate_bo_ufn_param
    ; return_type = migrate_or_blank m.return_type }
  in
  {fn with ast = migrate_expr fn.ast; metadata = migrate_bo_metadata fn.metadata}


let migrate_user_tipe (tipe : RuntimeT.user_tipe) : RuntimeT.user_tipe =
  let open RuntimeT in
  let migrate_bo_definition (UTRecord fields) =
    UTRecord
      (List.map fields ~f:(fun {name; tipe} ->
           {name = migrate_or_blank name; tipe = migrate_or_blank tipe}))
  in
  { tipe with
    name = migrate_or_blank tipe.name
  ; definition = migrate_bo_definition tipe.definition }


let migrate_col (k, v) = (migrate_or_blank k, migrate_or_blank v)

let migrate_db (db : 'expr_type RuntimeT.DbT.db) : 'expr_type RuntimeT.DbT.db =
  { db with
    cols = List.map ~f:migrate_col db.cols
  ; name = migrate_or_blank db.name }


let migrate_op (op : 'expr_type Op.op) : 'expr_type Op.op =
  match op with
  | SetHandler (tlid, pos, handler) ->
      SetHandler (tlid, pos, migrate_handler handler)
  | SetFunction fn ->
      SetFunction (migrate_user_function fn)
  | SetExpr (tlid, id, expr) ->
      SetExpr (tlid, id, migrate_expr expr)
  | CreateDBMigration (tlid, id1, id2, list) ->
      CreateDBMigration (tlid, id1, id2, List.map list ~f:migrate_col)
  | SetType tipe ->
      SetType (migrate_user_tipe tipe)
  | _ ->
      op


let migrate_host (host : string) : (string, unit) Tc.Result.t =
  try
    let canvas_id = id_for_name host in
    Serialize.fetch_all_tlids ~canvas_id ()
    |> List.map ~f:(fun tlid ->
           Serialize.transactionally_migrate_oplist
             ~canvas_id
             ~tlid
             ~host
             ~handler_f:migrate_handler
             ~db_f:migrate_db
             ~user_fn_f:migrate_user_function
             ~user_tipe_f:migrate_user_tipe
             ~oplist_f:(List.map ~f:migrate_op)
             ())
    |> Tc.Result.combine
    |> Tc.Result.map (fun _ -> ())
  with e -> Error (Exception.to_string e)


let migrate_all_hosts () =
  List.iter (all_hosts ()) ~f:(fun host ->
      migrate_host host |> Result.ok_or_failwith)


let time (fn : unit -> 'a) : float * 'a =
  let start = Unix.gettimeofday () in
  let a = fn () in
  let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
  (elapsed, a)


(* --------------- *)
(* Cleanup canvases *)
(* --------------- *)
let cleanup_old_traces () : float =
  let logdata = ref [] in
  let total_time, _ =
    time (fun _ ->
        let t_events, n_events = time Stored_event.trim_events in
        let t_res, n_res = time Stored_function_result.trim_results in
        let t_args, n_args = time Stored_function_arguments.trim_arguments in
        logdata :=
          [ ("trimmed_results", `Int n_res)
          ; ("trimmed_events", `Int n_events)
          ; ("trimmed_arguments", `Int n_args)
          ; ("stored_events_time", `Float t_events)
          ; ("function_results_time", `Float t_res)
          ; ("function_arguments_time", `Float t_args) ] ;
        ())
  in
  Log.infO
    "cleanup_old_traces"
    ~jsonparams:(("total_time", `Float total_time) :: !logdata) ;
  total_time


(** cleanup_old_traces_for_canvas is like cleanup_old_traces for a specific canvas *)
let cleanup_old_traces_for_canvas (cid : Uuidm.t) : float =
  let logdata = ref [] in
  let total_time, _ =
    time (fun _ ->
        let t_events, n_events =
          time (fun _ -> Stored_event.trim_events_for_canvas cid)
        in
        let t_res, n_res =
          time (fun _ -> Stored_function_result.trim_results_for_canvas cid)
        in
        let t_args, n_args =
          time (fun _ ->
              Stored_function_arguments.trim_arguments_for_canvas cid)
        in
        logdata :=
          [ ("trimmed_results", `Int n_res)
          ; ("trimmed_events", `Int n_events)
          ; ("trimmed_arguments", `Int n_args)
          ; ("stored_events_time", `Float t_events)
          ; ("function_results_time", `Float t_res)
          ; ("function_arguments_time", `Float t_args) ] ;
        ())
  in
  Log.infO
    "cleanup_old_traces_for_canvas"
    ~jsonparams:
      ( [ ("canvas_id", `String (Uuidm.to_string cid))
        ; ("total_time", `Float total_time) ]
      @ !logdata ) ;
  total_time


let to_string (host : string) : string =
  (* TODO: user_tipes *)
  let c =
    load_all host []
    |> Result.map_error ~f:(String.concat ~sep:", ")
    |> Prelude.Result.ok_or_internal_exception "Canvas load error"
  in
  let handlers = !c.handlers |> IDMap.data |> List.map ~f:TL.to_string in
  let dbs = !c.dbs |> IDMap.data |> List.map ~f:TL.to_string in
  let user_fns =
    !c.user_functions |> IDMap.data |> List.map ~f:TL.user_fn_to_string
  in
  let deleted_handlers =
    !c.handlers |> IDMap.data |> List.map ~f:TL.to_string
  in
  let deleted_dbs = !c.dbs |> IDMap.data |> List.map ~f:TL.to_string in
  let deleted_user_functions =
    !c.deleted_user_functions |> IDMap.data |> List.map ~f:TL.user_fn_to_string
  in
  String.concat
    ~sep:"\n\n\n"
    ( [" ------------- Handlers ------------- "]
    @ handlers
    @ [" ------------- DBs ------------- "]
    @ dbs
    @ [" ------------- Functions ------------- "]
    @ user_fns
    @ [" ------------- Deleted Handlers ------------- "]
    @ deleted_handlers
    @ [" ------------- Deleted DBs ------------- "]
    @ deleted_dbs
    @ [" ------------- Deleted Functions ------------- "]
    @ deleted_user_functions )
