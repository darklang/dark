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

type canvas =
  { host : string
  ; owner : Uuidm.t
  ; id : Uuidm.t
  ; ops : (tlid * Op.oplist) list
  ; handlers : TL.toplevel_list
  ; dbs : TL.toplevel_list
  ; deleted_toplevels : TL.toplevel_list
  ; user_functions : RTT.user_fn list
  ; deleted_user_functions : RTT.user_fn list
  ; cors_setting : cors_setting option }
[@@deriving eq, show]

(* ------------------------- *)
(* Toplevel *)
(* ------------------------- *)
let upsert_tl
    (tlid : tlid) (pos : pos) (data : TL.tldata) (tls : TL.toplevel_list) :
    TL.toplevel_list =
  let tl : TL.toplevel = {tlid; pos; data} in
  tls |> List.filter ~f:(fun x -> x.tlid <> tl.tlid) |> ( @ ) [tl]


let upsert_db tlid pos data c = {c with dbs = upsert_tl tlid pos data c.dbs}

let upsert_handler tlid pos data c =
  {c with handlers = upsert_tl tlid pos data c.handlers}


let upsert_function (user_fn : RuntimeT.user_fn) (c : canvas) : canvas =
  let fns =
    List.filter ~f:(fun x -> x.tlid <> user_fn.tlid) c.user_functions
  in
  {c with user_functions = fns @ [user_fn]}


let remove_function (tlid : tlid) (c : canvas) : canvas =
  let deletedFn =
    c.user_functions |> List.find ~f:(fun x -> x.tlid = tlid) |> Option.to_list
  in
  let fns = List.filter ~f:(fun x -> x.tlid <> tlid) c.user_functions in
  { c with
    user_functions = fns
  ; deleted_user_functions = c.deleted_user_functions @ deletedFn }


let remove_function_forever (tlid : tlid) (c : canvas) : canvas =
  let f (uf : RTT.user_fn) = uf.tlid <> tlid in
  { c with
    user_functions = List.filter ~f c.user_functions
  ; deleted_user_functions = List.filter ~f c.deleted_user_functions }


let remove_tl_forever (tlid : tlid) (c : canvas) : canvas =
  let f (tl : Toplevel.toplevel) = tl.tlid <> tlid in
  { c with
    dbs = List.filter ~f c.dbs
  ; handlers = List.filter ~f c.handlers
  ; deleted_toplevels = List.filter ~f c.deleted_toplevels }


let remove_toplevel (tlid : tlid) (c : canvas) : canvas =
  let oldh, handlers =
    List.partition_tf ~f:(fun x -> x.tlid = tlid) c.handlers
  in
  let olddb, dbs = List.partition_tf ~f:(fun x -> x.tlid = tlid) c.dbs in
  let olddel, deleted =
    List.partition_tf ~f:(fun x -> x.tlid = tlid) c.deleted_toplevels
  in
  (* It's possible to delete something twice. Or more I guess. In that
   * case, only keep the latest deleted toplevel. *)
  let removed =
    oldh @ olddb @ olddel
    |> List.hd
    |> Option.value_map ~f:(fun x -> [x]) ~default:[]
  in
  {c with handlers; dbs; deleted_toplevels = deleted @ removed}


let apply_to_toplevel
    ~(f : TL.toplevel -> TL.toplevel) (tlid : tlid) (tls : TL.toplevel_list) =
  match List.find ~f:(fun t -> t.tlid = tlid) tls with
  | Some tl ->
      let newtl = f tl in
      upsert_tl newtl.tlid newtl.pos newtl.data tls
  | None ->
      tls


let apply_to_all_toplevels
    ~(f : TL.toplevel -> TL.toplevel) (tlid : tlid) (c : canvas) : canvas =
  { c with
    handlers = apply_to_toplevel ~f tlid c.handlers
  ; dbs = apply_to_toplevel ~f tlid c.dbs }


let apply_to_db ~(f : RTT.DbT.db -> RTT.DbT.db) (tlid : tlid) (c : canvas) :
    canvas =
  let tlf (tl : TL.toplevel) =
    let data =
      match tl.data with
      | TL.DB db ->
          TL.DB (f db)
      | _ ->
          Exception.client "Provided ID is not for a DB"
    in
    {tl with data}
  in
  {c with dbs = apply_to_toplevel tlid ~f:tlf c.dbs}


let move_toplevel (tlid : tlid) (pos : pos) (c : canvas) : canvas =
  apply_to_all_toplevels ~f:(fun tl -> {tl with pos}) tlid c


(* ------------------------- *)
(* Build *)
(* ------------------------- *)

let apply_op (is_new : bool) (op : Op.op) (c : canvas ref) : unit =
  c :=
    !c
    |>
    match op with
    | SetHandler (tlid, pos, handler) ->
        upsert_handler tlid pos (TL.Handler handler)
    | CreateDB (tlid, pos, name) ->
        if is_new
        then (
          if name = "" then Exception.client "DB must have a name" ;
          List.iter (TL.dbs !c.dbs) ~f:(fun db ->
              if Ast.blank_to_string db.name = name
              then Exception.client "Duplicate DB name" ) ) ;
        let db = User_db.create name tlid in
        upsert_db tlid pos (TL.DB db)
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
              | Blank id as b ->
                  (n, b) )
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
        remove_toplevel tlid
    | MoveTL (tlid, pos) ->
        move_toplevel tlid pos
    | SetFunction user_fn ->
        upsert_function user_fn
    | DeleteFunction tlid ->
        remove_function tlid
    | TLSavepoint _ ->
        ident
    | UndoTL _ | RedoTL _ ->
        Exception.internal
          ("This should have been preprocessed out! " ^ Op.show_op op)
    | RenameDBname (tlid, name) ->
        apply_to_db ~f:(User_db.rename_db name) tlid
    | CreateDBWithBlankOr (tlid, pos, id, name) ->
        List.iter (TL.dbs !c.dbs) ~f:(fun db ->
            if Ast.blank_to_string db.name = name
            then Exception.client "Duplicate DB name" ) ;
        let db = User_db.create2 name tlid id in
        upsert_db tlid pos (TL.DB db)
    | DeleteTLForever tlid ->
        remove_tl_forever tlid
    | DeleteFunctionForever tlid ->
        remove_function_forever tlid


let add_ops (c : canvas ref) (oldops : Op.op list) (newops : Op.op list) : unit
    =
  let oldops = List.map ~f:(fun op -> (false, op)) oldops in
  let newops = List.map ~f:(fun op -> (true, op)) newops in
  let reduced_ops = Undo.preprocess (oldops @ newops) in
  List.iter ~f:(fun (is_new, op) -> apply_op is_new op c) reduced_ops ;
  let allops = oldops @ newops |> List.map ~f:Tuple.T2.get2 in
  c := {!c with ops = Op.oplist2tlid_oplists allops}


let fetch_cors_setting (id : Uuidm.t) : cors_setting option =
  let cors_setting_of_db_string (string_from_db : string) : cors_setting option
      =
    (* none if null from database *)
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
                            "CORS setting from DB is a list containing a non-string"
                  )
               |> Origins
               |> Some
           | _ ->
               Exception.internal
                 "CORS setting from DB is neither a string or a list." )
  in
  Db.fetch_one
    ~name:"fetch_cors_setting"
    "SELECT cors_setting FROM canvases WHERE id = $1"
    ~params:[Uuid id]
  |> List.hd_exn
  |> cors_setting_of_db_string


let init (host : string) (ops : Op.op list) : canvas ref =
  let owner = Account.for_host host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let cors = fetch_cors_setting canvas_id in
  let c =
    ref
      { host
      ; owner
      ; id = canvas_id
      ; ops = []
      ; handlers = []
      ; dbs = []
      ; deleted_toplevels = []
      ; user_functions = []
      ; deleted_user_functions = []
      ; cors_setting = cors }
  in
  add_ops c [] ops ;
  c


let name_for_id (id : Uuidm.t) : string =
  Db.fetch_one
    ~name:"fetch_canvas_name"
    "SELECT name FROM canvases WHERE id = $1"
    ~params:[Uuid id]
  |> List.hd_exn


let id_for_name (name : string) : Uuidm.t =
  Db.fetch_one
    ~name:"fetch_canvas_id"
    "SELECT id FROM canvases WHERE name = $1"
    ~params:[String name]
  |> List.hd_exn
  |> Uuidm.of_string
  |> Option.value_exn


let update_cors_setting (c : canvas ref) (setting : cors_setting option) : unit
    =
  let cors_setting_to_db (setting : cors_setting option) : Db.param =
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
    (newops : Op.op list)
    ~(f : host:string -> canvas_id:Uuidm.t -> unit -> Op.tlid_oplists) :
    canvas ref =
  let owner = Account.for_host host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let cors = fetch_cors_setting canvas_id in
  let oldops = f ~host ~canvas_id () in
  let c =
    ref
      { host
      ; owner
      ; id = canvas_id
      ; ops = []
      ; handlers = []
      ; dbs = []
      ; user_functions = []
      ; deleted_toplevels = []
      ; deleted_user_functions = []
      ; cors_setting = cors }
  in
  add_ops c (Op.tlid_oplists2oplist oldops) newops ;
  c


let load_all host (newops : Op.op list) : canvas ref =
  load_from ~f:Serialize.load_all_from_db host newops


let load_only ~tlids host (newops : Op.op list) : canvas ref =
  load_from ~f:(Serialize.load_only_for_tlids ~tlids) host newops


let load_http ~verb ~path host : canvas ref =
  load_from ~f:(Serialize.load_for_http ~path ~verb) host []


let load_cron host : canvas ref = load_from ~f:Serialize.load_for_cron host []

let load_for_event (event : Event_queue.t) =
  (* TODO: slim down by event description once we can do that *)
  load_all event.host []


let serialize_only (tlids : tlid list) (c : canvas) : unit =
  let munge_name module_ n =
    if Ast.blank_to_option module_ = Some "HTTP"
    then Http.route_to_postgres_pattern n
    else n
  in
  let handler_metadata (h : RTT.HandlerT.handler) =
    ( h.tlid
    , ( Ast.blank_to_option h.spec.name
        |> Option.map ~f:(munge_name h.spec.module_)
      , Ast.blank_to_option h.spec.module_
      , Ast.blank_to_option h.spec.modifier ) )
  in
  let hmeta =
    c.handlers |> Toplevel.handlers |> List.map ~f:handler_metadata
  in
  let routes = IDMap.of_alist_exn hmeta in
  let tipes_list =
    List.map c.handlers ~f:(fun h -> (h.tlid, `Handler))
    @ List.map c.user_functions ~f:(fun f -> (f.tlid, `User_function))
    @ List.map c.dbs ~f:(fun d -> (d.tlid, `DB))
    @ List.map c.deleted_toplevels ~f:(fun t ->
          match t.data with
          | Handler _ ->
              (t.tlid, `Handler)
          | DB _ ->
              (t.tlid, `DB) )
  in
  let tipes = IDMap.of_alist_exn tipes_list in
  (* Use ops rather than just set of toplevels, because toplevels may
   * have been deleted or undone, and therefore not appear, but it's
   * important to record them. *)
  List.iter c.ops ~f:(fun (tlid, oplist) ->
      (* Only save oplists that have been used. *)
      if List.mem ~equal:( = ) tlids tlid
      then
        let name, module_, modifier =
          IDMap.find routes tlid |> Option.value ~default:(None, None, None)
        in
        let tipe =
          IDMap.find tipes tlid
          (* If the user calls Undo enough, we might not know
                    * the tipe here. In that case, set to handler cause
                    * it won't be used anyway *)
          |> Option.value ~default:`Handler
        in
        Serialize.save_toplevel_oplist
          oplist
          ~tlid
          ~canvas_id:c.id
          ~account_id:c.owner
          ~name
          ~module_
          ~modifier
          ~tipe
      else () )


let save_tlids (c : canvas) (tlids : tlid list) : unit = serialize_only tlids c

let save_all (c : canvas) : unit =
  let tlids = List.map ~f:Tuple.T2.get1 c.ops in
  save_tlids c tlids


(* ------------------------- *)
(* Testing/validation *)
(* ------------------------- *)

let load_and_resave_from_test_file (host : string) : unit =
  let c =
    load_from
      host
      []
      ~f:(Serialize.load_json_from_disk ~root:Testdata ~preprocess:ident)
  in
  save_all !c


let minimize (c : canvas) : canvas =
  (* TODO *)
  (* let ops = *)
  (*   c.ops *)
  (*   |> Undo.preprocess *)
  (*   |> List.filter ~f:Op.has_effect *)
  (* in { c with ops = ops } *)
  c


let save_test (c : canvas) : string =
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


let validate_op host op =
  if Op.is_deprecated op
  then
    Exception.internal "bad op" ~info:[("host", host)] ~actual:(Op.show_op op)


let validate_host host =
  let c = load_all host [] in
  (* check ops *)
  List.iter (Op.tlid_oplists2oplist !c.ops) ~f:(validate_op host)


(* just load, don't save -- also don't validate the ops don't
 * have deprecate ops (via validate_op or validate_host). this
 * function is used by the readiness check to gate deploys, so
 * we don't want to prevent deploys because someone forgot a deprecatedop
 * in a tier 1 canvas somewhere *)
let check_tier_one_hosts () : unit =
  let hosts = Serialize.tier_one_hosts () in
  List.iter ~f:(fun host -> ignore (load_all host [])) hosts


let migrate_all_hosts () : unit =
  (* let hosts = Serialize.current_hosts () in *)
  (*  *)
  (* List.iter hosts *)
  (*   ~f:(fun host -> *)
  (*     let c = load_all host [] in *)
  (*  *)
  (*     (* check ops *) *)
  (*     List.iter (Op.tlid_oplists2oplist !c.ops) *)
  (*       ~f:(validate_op host); *)
  (*  *)
  (*     let new_ops = *)
  (*     in *)
  (*     c := { !c with ops = new_ops}; *)
  (*     save_all !c); *)
  (*  *)
  ()


let cleanup_old_traces (host : string) : unit =
  let owner = Account.for_host host in
  let canvas_id = Serialize.fetch_canvas_id owner host in
  let keep = Stored_event.get_all_recent_canvas_traceids canvas_id in
  Log.infO
    "cleanup_trace"
    ~params:[("host", host); ("count", List.length keep |> string_of_int)] ;
  Stored_event.trim_events ~canvas_id ~keep () ;
  Stored_function_result.trim_results ~canvas_id ~keep () ;
  ()


let to_string (host : string) : string =
  let c = load_all host [] in
  let handlers = List.map ~f:TL.to_string !c.handlers in
  let user_fns = List.map ~f:TL.user_fn_to_string !c.user_functions in
  let dbs = List.map ~f:TL.to_string !c.dbs in
  let deleted = List.map ~f:TL.to_string !c.deleted_toplevels in
  String.concat
    ~sep:"\n\n\n"
    ( [" ------------- Handlers ------------- "]
    @ handlers
    @ [" ------------- User functions ------------- "]
    @ user_fns
    @ [" ------------- DBs ------------- "]
    @ dbs
    @ [" ------------- Deleted ------------- "]
    @ deleted )
