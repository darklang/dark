open Core_kernel
open Libexecution

open Util
open Types

module RTT = Types.RuntimeT
module RT = Runtime
module TL = Toplevel
module PReq = Parsed_request
module FF = Feature_flag
module SE = Stored_event
module Dbp = Dbprim

type toplevellist = TL.toplevel list [@@deriving eq, show, yojson]
type canvas = { host : string
              ; owner : Uuidm.t
              ; id : Uuidm.t
              ; ops : Op.oplist
              ; toplevels: toplevellist
              ; user_functions: RTT.user_fn list
              } [@@deriving eq, show]

(* ------------------------- *)
(* Toplevel *)
(* ------------------------- *)
let upsert_toplevel (tlid: tlid) (pos: pos) (data: TL.tldata) (c: canvas) : canvas =
  let toplevel : TL.toplevel = { tlid = tlid
                               ; pos = pos
                               ; data = data} in
  let tls = List.filter ~f:(fun x -> x.tlid <> toplevel.tlid) c.toplevels
  in
  { c with toplevels = tls @ [toplevel] }

let upsert_function (user_fn: RuntimeT.user_fn) (c: canvas) : canvas =
  let fns = List.filter ~f:(fun x -> x.tlid <> user_fn.tlid) c.user_functions
  in
  { c with user_functions = fns @ [user_fn] }

let remove_toplevel_by_id (tlid: tlid) (c: canvas) : canvas =
  let tls =
    List.filter
      ~f:(fun x -> x.tlid <> tlid)
      c.toplevels
  in
  { c with toplevels = tls }

let apply_to_toplevel ~(f:(TL.toplevel -> TL.toplevel)) (tlid: tlid) (c:canvas) =
  match List.find ~f:(fun t -> t.tlid = tlid) c.toplevels with
  | Some tl ->
    let newtl = f tl in
    upsert_toplevel newtl.tlid newtl.pos newtl.data c
  | None ->
    Exception.client "No toplevel for this ID"

let move_toplevel (tlid: tlid) (pos: pos) (c: canvas) : canvas =
  apply_to_toplevel ~f:(fun tl -> { tl with pos = pos }) tlid c

let apply_to_db ~(f:(RTT.DbT.db -> RTT.DbT.db)) (tlid: tlid) (c:canvas) : canvas =
  let tlf (tl: TL.toplevel) =
    let data = match tl.data with
               | TL.DB db -> TL.DB (f db)
               | _ -> Exception.client "Provided ID is not for a DB"
    in { tl with data = data }
  in apply_to_toplevel tlid ~f:tlf c

(* ------------------------- *)
(* Build *)
(* ------------------------- *)

let apply_op (op : Op.op) (c : canvas ref) : unit =
  c :=
    !c |>
    match op with
    | SetHandler (tlid, pos, handler) ->
      upsert_toplevel tlid pos (TL.Handler handler)
    | CreateDB (tlid, pos, name) ->
      if name = ""
      then Exception.client ("DB must have a name")
      else
        let db = User_db.create !c.host name tlid in
        upsert_toplevel tlid pos (TL.DB db)
    | AddDBCol (tlid, colid, typeid) ->
      apply_to_db ~f:(User_db.add_col colid typeid) tlid
    | SetDBColName (tlid, id, name) ->
      apply_to_db ~f:(User_db.set_col_name id name) tlid
    | ChangeDBColName (tlid, id, name) ->
      apply_to_db ~f:(User_db.change_col_name id name) tlid
    | SetDBColType (tlid, id, tipe) ->
      apply_to_db ~f:(User_db.set_col_type id (Dval.tipe_of_string tipe)) tlid
    | ChangeDBColType (tlid, id, tipe) ->
      apply_to_db ~f:(User_db.change_col_type id (Dval.tipe_of_string tipe)) tlid
    | InitDBMigration (tlid, id, rbid, rfid, kind) ->
      apply_to_db ~f:(User_db.initialize_migration id rbid rfid kind) tlid
    | SetExpr (tlid, id, e) ->
      apply_to_toplevel ~f:(TL.set_expr id e) tlid
    | DeleteTL tlid -> remove_toplevel_by_id tlid
    | MoveTL (tlid, pos) -> move_toplevel tlid pos
    | Savepoint _ -> ident
    | DeprecatedSavepoint -> ident
    | SetFunction user_fn ->
      upsert_function user_fn
    | DeprecatedDeleteAll
    | DeprecatedUndo | DeprecatedRedo
    | UndoTL _ | RedoTL _ ->
      Exception.internal ("This should have been preprocessed out! " ^ (Op.show_op op))

(* https://stackoverflow.com/questions/15939902/is-select-or-insert-in-a-function-prone-to-race-conditions/15950324#15950324 *)
let fetch_canvas_id (owner:Uuidm.t) (host:string) : Uuidm.t =
  Printf.sprintf
    "CREATE OR REPLACE FUNCTION canvas_id(_new_id uuid, _account_id uuid, _name VARCHAR(40), OUT _id uuid) AS
     $func$
     BEGIN
     LOOP
       SELECT id
       FROM   canvases
       WHERE  name = _name
       INTO   _id;

       EXIT WHEN FOUND;

       INSERT INTO canvases AS c
       (id, account_id, name)
       VALUES (_new_id, _account_id, _name)
       ON     CONFLICT (name) DO NOTHING
       RETURNING c.id
       INTO   _id;

       EXIT WHEN FOUND;
     END LOOP;
     END
     $func$ LANGUAGE plpgsql;
     SELECT canvas_id(%s, %s, %s); "
    (Dbp.uuid (Util.create_uuid ()))
    (Dbp.uuid owner)
    (Dbp.host host)
  |> Db.fetch_via_sql ~quiet:false
  |> List.concat
  |> List.hd_exn
  |> Uuidm.of_string
  |> Option.value_exn

let add_ops (c: canvas ref) (oldops: Op.op list) (newops: Op.op list) : unit =
  let reduced_ops = Undo.preprocess (oldops @ newops) in
  List.iter ~f:(fun op -> apply_op op c) reduced_ops;
  c := { !c with ops = oldops @ newops }

let minimize (c : canvas) : canvas =
  let ops =
    c.ops
    |> Undo.preprocess
    |> List.filter ~f:Op.has_effect
  in { c with ops = ops }


(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let owner (host:string) : Uuidm.t =
  host
  |> Account.auth_domain_for
  |> Account.owner
  |> fun o ->
       match o with
       | Some owner -> owner
       | None -> Exception.client ("No Canvas found for host " ^ host)


let create ?(load=true) (host: string) (newops: Op.op list) : canvas ref =
  let oldops =
    if load
    then Serialize.search_and_load host
    else []
  in

  let owner = owner host in

  let id = fetch_canvas_id owner host in
  let c =
    ref { host = host
        ; owner = owner
        ; id = id
        ; ops = []
        ; toplevels = []
        ; user_functions = []
        }
  in
  add_ops c oldops newops;
  c

let load = create ~load:true
let init = create ~load:false

let save (c : canvas) : unit =
  Serialize.save_binary_to_db c.host c.ops;
  let json = Serialize.json_unversioned_filename c.host in
  let root = Serialize.root_of c.host in
  ignore (File.convert_bin_to_json ~root c.host json)


let save_test (c: canvas) : string =
  let c = minimize c in
  let host = "test-" ^ c.host in
  let file = Serialize.json_unversioned_filename host in
  let host = if File.file_exists ~root:Testdata file
             then
               host
               ^ "_"
               ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
             else host in
  let file = Serialize.json_unversioned_filename host in
  Serialize.save_json_to_disk ~root:Testdata file c.ops;
  file

(* ------------------------- *)
(* Routing *)
(* ------------------------- *)

let matching_routes ~(uri: Uri.t) ~(verb: string) (c: canvas) : (bool * Handler.handler) list =
  let path = Uri.path uri in
  c.toplevels
  |> TL.http_handlers
  |> List.filter
    ~f:(fun h -> Handler.event_name_for h <> None)
  |> List.filter
    ~f:(fun h -> Http.path_matches_route ~path:path (Handler.event_name_for_exn h))
  |> List.filter
    ~f:(fun h ->
      (match Handler.modifier_for h with
        | Some m -> String.Caseless.equal m verb
        (* we specifically want to allow handlers without method specifiers for now *)
        | None -> true))
  |> List.map
    ~f:(fun h -> (Http.has_route_variables (Handler.event_name_for_exn h), h))

let pages_matching_route ~(uri: Uri.t) ~(verb: string) (c: canvas) : (bool * Handler.handler) list =
  matching_routes ~uri ~verb c

(* ------------------------- *)
(* Events *)
(* ------------------------- *)

let all_tlids (c: canvas) : tlid list =
  List.map ~f:(fun tl -> tl.tlid) c.toplevels

let initial_dval_map (c: canvas) : RTT.dval_map =
  c.toplevels
  |> TL.dbs
  |> User_db.dbs_as_env

let sample_request =
  PReq.to_dval PReq.sample

let sample_event =
  RTT.DIncomplete

let default_env (c: canvas) : RTT.dval_map =
  initial_dval_map c
  |> RTT.DvalMap.set ~key:"request" ~data:sample_request
  |> RTT.DvalMap.set ~key:"event" ~data:sample_event

let global_vars (c: canvas) : string list =
  RTT.DvalMap.keys (default_env c)

let initial_envs (c: canvas) (h: Handler.handler)
  : RTT.dval_map list =
  let initial_env = initial_dval_map c in
  let default =
     match Handler.module_type h with
     | `Http ->
       RTT.DvalMap.set initial_env "request" sample_request
     | `Event ->
       RTT.DvalMap.set initial_env "event" sample_event
     | `Cron -> initial_env
     | `Unknown -> default_env c
  in
  (match Handler.event_desc_for h with
  | None -> [default]
  | Some (space, path, modifier as d) ->
    let events = SE.load_events c.id d in
    if events = []
    then [default]
    else
      List.map events
        ~f:(fun e ->
            match Handler.module_type h with
            | `Http ->
              let with_r = RTT.DvalMap.set initial_env "request" e in
              let name = Handler.event_name_for_exn h in
              let bound = Http.bind_route_params_exn path name in
              Util.merge_left with_r bound
            | `Event ->
              RTT.DvalMap.set initial_env "event" e
            | `Cron  -> initial_env
            | `Unknown -> initial_env (* can't happen *)
        ))


let get_404s (c: canvas) : SE.four_oh_four list =
  let match_desc h d : bool =
    let (space, path, modifier) = d in
    match Handler.event_desc_for h with
    | Some (h_space, h_path, h_modifier) ->
      Http.path_matches_route ~path h_path
      && h_modifier = modifier
      && h_space = space
    | None -> false
  in

  let unused_descs =
    SE.list_events c.id
    |> List.filter
      ~f:(fun d ->
          not (List.exists (TL.handlers c.toplevels)
                 ~f:(fun h -> match_desc h d)))
    |> List.map ~f:(fun d -> (d, SE.load_events c.id d))
  in

  unused_descs


(* ------------------------- *)
(* Execution *)
(* ------------------------- *)
type analysis_result = tlid * Analysis.analysis list
type executable_fn_id = (tlid * id * int)

let analysis_result_to_yojson (id, results) =
  `Assoc [ ("id", `Int id)
         ; ("results", Analysis.analysis_list_to_yojson results)
         ]

let unlocked (c: canvas) : tlid list =
  c.toplevels
  |> TL.dbs
  |> User_db.unlocked c.id c.owner
  |> List.map ~f:(fun x -> x.tlid)

let function_value
    ~(exe_fn_ids: executable_fn_id list)
    ~(execution_id: id)
    (c: canvas)
    (f: RTT.user_fn)
  : analysis_result =
  let fn_ids =
    exe_fn_ids
    |> List.filter_map
      ~f:(fun (tlid, id, cursor) ->
          if tlid = f.tlid && cursor = 0
          then Some id
          else None)
  in
  let env = Analysis.environment_for_user_fn f in
  let state : RTT.exec_state =
    { ff = FF.analysis
    ; tlid = f.tlid
    ; host = c.host
    ; account_id = c.owner
    ; canvas_id = c.id
    ; user_fns = c.user_functions
    ; exe_fn_ids = fn_ids
    ; env = env
    ; dbs = TL.dbs c.toplevels
    ; id = execution_id
    }
  in
  (f.tlid, [Analysis.execute_function_for_analysis state f])

let handler_value
    ~(exe_fn_ids : executable_fn_id list)
    ~(execution_id: id)
    (c: canvas)
    (h : Handler.handler)
  : analysis_result
   =
  let fn_ids i =
    List.filter_map exe_fn_ids
      ~f:(fun (tlid, id, cursor) ->
          if tlid = h.tlid && i = cursor
          then Some id
          else None)
  in
  let state i env : RTT.exec_state =
    { ff = FF.analysis
    ; tlid = h.tlid
    ; host = c.host
    ; account_id = c.owner
    ; canvas_id = c.id
    ; user_fns = c.user_functions
    ; exe_fn_ids = fn_ids i
    ; env = env
    ; dbs = TL.dbs c.toplevels
    ; id = execution_id
    }
  in
  let envs = initial_envs c h in
  let values =
    List.mapi
      ~f:(fun i env ->
          Analysis.execute_handler_for_analysis (state i env) h)
      envs
  in
  (h.tlid, values)

(* The full response with everything *)
type get_analysis_response =
  { analyses: analysis_result list
  ; global_varnames : string list
  ; unlocked_dbs : tlid list
  ; fofs : SE.four_oh_four list [@key "404s"]
  } [@@deriving to_yojson]

(* A subset of responses to be merged in *)
type rpc_response =
  { new_analyses: analysis_result list (* merge: overwrite existing analyses *)
  ; global_varnames : string list (* replace *)
  ; toplevels : TL.toplevel_list (* replace *)
  ; user_functions : RTT.user_fn list (* replace *)
  ; unlocked_dbs : tlid list (* replace *)
  } [@@deriving to_yojson]


let to_get_analysis_frontend (vals : analysis_result list)
      (unlocked : tlid list)
      (f404s: SE.four_oh_four list)
      (c : canvas) : string =
  { analyses = vals
  ; global_varnames = global_vars c
  ; unlocked_dbs = unlocked
  ; fofs = f404s
  }
  |> get_analysis_response_to_yojson
  |> Yojson.Safe.to_string ~std:true


let to_rpc_response_frontend (c : canvas) (vals : analysis_result list)
    (unlocked : tlid list)
  : string =
  { new_analyses = vals
  ; global_varnames = global_vars c
  ; toplevels = c.toplevels
  ; user_functions = c.user_functions
  ; unlocked_dbs = unlocked
  }
  |> rpc_response_to_yojson
  |> Yojson.Safe.to_string ~std:true


