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
  ; cors_setting : cors_setting option
  ; handlers : TL.toplevels
  ; dbs : TL.toplevels
  ; user_functions : RTT.user_fn IDMap.t
  ; user_tipes : RTT.user_tipe IDMap.t
  ; deleted_handlers : TL.toplevels
  ; deleted_dbs : TL.toplevels
  ; deleted_user_functions : RTT.user_fn IDMap.t
  ; deleted_user_tipes : RTT.user_tipe IDMap.t }
[@@deriving eq, show]

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


let set_function (user_fn : RuntimeT.user_fn) (c : canvas) : canvas =
  (* if the fn had been deleted, remove it from the deleted set. This handles
   * a data race where a Set comes in after a Delete. *)
  { c with
    user_functions = IDMap.set c.user_functions user_fn.tlid user_fn
  ; deleted_user_functions = IDMap.remove c.deleted_user_functions user_fn.tlid
  }


let set_tipe (user_tipe : RuntimeT.user_tipe) (c : canvas) : canvas =
  (* if the tipe had been deleted, remove it from the deleted set. This handles
   * a data race where a Set comes in after a Delete. *)
  { c with
    user_tipes = IDMap.set c.user_tipes user_tipe.tlid user_tipe
  ; deleted_user_tipes = IDMap.remove c.deleted_user_tipes user_tipe.tlid }


let delete_function (tlid : tlid) (c : canvas) : canvas =
  match IDMap.find c.user_functions tlid with
  | None ->
      c
  | Some user_fn ->
      { c with
        user_functions = IDMap.remove c.user_functions tlid
      ; deleted_user_functions =
          IDMap.set c.deleted_user_functions tlid user_fn }


let delete_function_forever (tlid : tlid) (c : canvas) : canvas =
  { c with
    user_functions = IDMap.remove c.user_functions tlid
  ; deleted_user_functions = IDMap.remove c.deleted_user_functions tlid }


let delete_tipe (tlid : tlid) (c : canvas) : canvas =
  match IDMap.find c.user_tipes tlid with
  | None ->
      c
  | Some user_tipe ->
      { c with
        user_tipes = IDMap.remove c.user_tipes tlid
      ; deleted_user_tipes = IDMap.set c.deleted_user_tipes tlid user_tipe }


let delete_tipe_forever (tlid : tlid) (c : canvas) : canvas =
  { c with
    user_tipes = IDMap.remove c.user_tipes tlid
  ; deleted_user_tipes = IDMap.remove c.deleted_user_tipes tlid }


let delete_tl_forever (tlid : tlid) (c : canvas) : canvas =
  { c with
    dbs = IDMap.remove c.dbs tlid
  ; handlers = IDMap.remove c.handlers tlid
  ; deleted_dbs = IDMap.remove c.deleted_dbs tlid
  ; deleted_handlers = IDMap.remove c.deleted_handlers tlid }


let delete_toplevel (tlid : tlid) (c : canvas) : canvas =
  let db = IDMap.find c.dbs tlid in
  let handler = IDMap.find c.handlers tlid in
  { c with
    dbs = IDMap.remove c.dbs tlid
  ; handlers = IDMap.remove c.handlers tlid
  ; deleted_dbs = IDMap.change c.deleted_dbs tlid ~f:(fun _ -> db)
  ; deleted_handlers =
      IDMap.change c.deleted_handlers tlid ~f:(fun _ -> handler) }


let apply_to_toplevel
    ~(f : TL.toplevel -> TL.toplevel) (tlid : tlid) (tls : TL.toplevels) =
  IDMap.change tls tlid ~f:(Option.map ~f)


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
  try
    c :=
      !c
      |>
      match op with
      | SetHandler (tlid, pos, handler) ->
          set_handler tlid pos (TL.Handler handler)
      | CreateDB (tlid, pos, name) ->
          if is_new
          then (
            if name = "" then Exception.client "DB must have a name" ;
            List.iter (TL.dbs !c.dbs) ~f:(fun db ->
                if Ast.blank_to_string db.name = name
                then Exception.client "Duplicate DB name" ) ) ;
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
            ~f:
              (User_db.set_col_type_in_migration id (Dval.tipe_of_string tipe))
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
            ("This should have been preprocessed out! " ^ Op.show_op op)
      | RenameDBname (tlid, name) ->
          apply_to_db ~f:(User_db.rename_db name) tlid
      | CreateDBWithBlankOr (tlid, pos, id, name) ->
          List.iter (TL.dbs !c.dbs) ~f:(fun db ->
              if Ast.blank_to_string db.name = name
              then Exception.client "Duplicate DB name" ) ;
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
    Log.erroR
      "apply_op failure"
      ~params:
        [ ("host", !c.host)
        ; ("op", Op.show_op op)
        ; ("exn", Exception.to_string e) ]


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
      ; cors_setting = cors
      ; handlers = IDMap.empty
      ; dbs = IDMap.empty
      ; user_functions = IDMap.empty
      ; user_tipes = IDMap.empty
      ; deleted_handlers = IDMap.empty
      ; deleted_dbs = IDMap.empty
      ; deleted_user_functions = IDMap.empty
      ; deleted_user_tipes = IDMap.empty }
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
  try
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
        ; cors_setting = cors
        ; handlers = IDMap.empty
        ; dbs = IDMap.empty
        ; user_functions = IDMap.empty
        ; user_tipes = IDMap.empty
        ; deleted_handlers = IDMap.empty
        ; deleted_dbs = IDMap.empty
        ; deleted_user_functions = IDMap.empty
        ; deleted_user_tipes = IDMap.empty }
    in
    add_ops c (Op.tlid_oplists2oplist oldops) newops ;
    c
  with e -> Libexecution.Exception.reraise_as_pageable e


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
  try
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
      ( c.handlers
      |> IDMap.keys
      |> List.map ~f:(fun tlid -> (tlid, TL.TLHandler)) )
      @ (c.dbs |> IDMap.keys |> List.map ~f:(fun tlid -> (tlid, TL.TLDB)))
      @ ( c.user_functions
        |> IDMap.keys
        |> List.map ~f:(fun tlid -> (tlid, TL.TLUserFunction)) )
      @ ( c.user_tipes
        |> IDMap.keys
        |> List.map ~f:(fun tlid -> (tlid, TL.TLUserTipe)) )
      @ ( c.deleted_handlers
        |> IDMap.keys
        |> List.map ~f:(fun tlid -> (tlid, TL.TLHandler)) )
      @ ( c.deleted_dbs
        |> IDMap.keys
        |> List.map ~f:(fun tlid -> (tlid, TL.TLDB)) )
      @ ( c.deleted_user_functions
        |> IDMap.keys
        |> List.map ~f:(fun tlid -> (tlid, TL.TLUserFunction)) )
      @ ( c.deleted_user_tipes
        |> IDMap.keys
        |> List.map ~f:(fun tlid -> (tlid, TL.TLUserTipe)) )
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
            |> Option.value ~default:TL.TLHandler
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
  with e -> Libexecution.Exception.reraise_as_pageable e


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


let cleanup_old_traces () : float =
  let runtime_since (start : float) : float =
    (Unix.gettimeofday () -. start) *. 1000.0
  in
  let start = Unix.gettimeofday () in
  let stored_events_start = Unix.gettimeofday () in
  let trimmed_events = Stored_event.trim_events () in
  let stored_events_time = runtime_since stored_events_start in
  let function_results_start = Unix.gettimeofday () in
  let trimmed_results = Stored_function_result.trim_results () in
  let function_results_time = runtime_since function_results_start in
  let function_arguments_start = Unix.gettimeofday () in
  let trimmed_arguments = Stored_function_arguments.trim_arguments () in
  let function_arguments_time = runtime_since function_arguments_start in
  let total_time = runtime_since start in
  Log.infO
    "cleanup_old_traces"
    ~jsonparams:
      [ ("trimmed_results", `Int trimmed_results)
      ; ("trimmed_events", `Int trimmed_events)
      ; ("trimmed_arguments", `Int trimmed_arguments)
      ; ("stored_events_time", `Float stored_events_time)
      ; ("function_results_time", `Float function_results_time)
      ; ("function_arguments_time", `Float function_arguments_time)
      ; ("total_time", `Float total_time) ] ;
  total_time


let to_string (host : string) : string =
  (* TODO: user_tipes *)
  let c = load_all host [] in
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
