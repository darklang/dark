open Core
open Util
open Types

module RTT = Types.RuntimeT
module RT = Runtime
module TL = Toplevel
module PReq = Parsed_request
module FF = Feature_flag
module SE = Stored_event

type toplevellist = TL.toplevel list [@@deriving eq, show, yojson]
type canvas = { host : string
              ; ops : Op.oplist
              ; toplevels: toplevellist
              ; user_functions: RTT.user_fn list
              } [@@deriving eq, show]

let create (host : string) : canvas ref =
  ref { host = host
      ; ops = []
      ; toplevels = []
      ; user_functions = []
      }

(* forgive me simon peyton-jones *)
let cur_canvas : (canvas ref) option ref =
  ref None

(* ------------------------- *)
(* Undo *)
(* ------------------------- *)

(* Undo's worked on the whole canvas before we realized that sucked. We
 * keep the old ones because our "list of ops" DB format relies on us
 * processing old ops correctly. *)
let preprocess_deprecated (ops: (Op.op * bool) list) : (Op.op * bool) list =
  (* - The client can add undopoints when it chooses. *)
  (* - When we get an undo, we go back to the previous undopoint. *)
  (* - When we get a redo, we ignore the undo immediately preceding it. If there *)
  (*   are multiple redos, they'll gradually eliminate the previous undos. *)
  (* undo algorithm: *)
  (*   - Step 1: go through the list and remove all undo-redo pairs. After *)
  (*   removing one pair, reprocess the list to remove others. *)
  (*   - Step 2: A redo without an undo just before it is pointless. Error if this *)
  (*   happens. *)
  (*   - Step 3: there should now only be undos. Going from the front, each time *)
  (*   there is an undo, drop the undo and all the ops going back to the previous *)
  (*   savepoint, including the savepoint. Use the undos to go the the *)
  (*   previous save point, dropping the ops between the undo and the *)
  ops
  (* Step 1: remove undo-redo pairs. We do by processing from the back, adding each *)
  (* element onto the front *)
  |> List.fold_right ~init:[] ~f:(fun op ops ->
    match (op :: ops) with
    | [] -> []
    | [op] -> [op]
    | (Op.DeprecatedUndo, _) :: (Op.DeprecatedRedo, _) :: rest -> rest
    | (Op.DeprecatedRedo, a) :: (Op.DeprecatedRedo, b) :: rest ->
      (Op.DeprecatedRedo, a) :: (Op.DeprecatedRedo, b) :: rest
    | _ :: (Op.DeprecatedRedo, _) :: rest -> (* Step 2: error on solo redos *)
        Exception.client "Already at latest redo"
    | ops -> ops)
  (* Step 3: remove undos and all the ops up to the savepoint. *)
  (* Go from the front and build the list up. If we hit an undo, drop back until *)
  (* the last favepoint. *)
  |> List.fold_left ~init:[] ~f:(fun ops op ->
       if Tuple.T2.get1 op = Op.DeprecatedUndo
       then
         ops
         |> List.drop_while ~f:(fun (o, _) -> o <> Op.DeprecatedSavepoint)
         |> (fun ops -> List.drop ops 1)   (* also drop the savepoint *)
       else
         op :: ops)
  |> List.rev (* previous step leaves the list reversed *)


let preprocess (ops: (Op.op * bool) list) : (Op.op * bool) list =

  (* The client can add undopoints when it chooses. When we get an undo,
   * we go back to the previous undopoint for that TL. *)

  (* When we get a redo, we ignore the undo immediately preceding it.
   * If there are multiple redos, they'll gradually eliminate the
   * previous undos. *)

  (* undo algorithm: *)

  (*   - Step 1: go through the list and remove all undo-redo pairs.
   *   After removing one pair, reprocess the list to remove others. *)

  (*   - Step 2: A redo without an undo just before it is pointless, but
   *   the client might allow it. Error. *)

  (*   - Step 3: there should now only be undos. Going from the front,
   *   each time there is an undo, drop the undo and all the ops going
   *   back to the previous savepoint, including the savepoint. Use the
   *   undos to go the the previous save point, dropping the ops between
   *   the undo and the *)

  ops
  |> preprocess_deprecated (* Deal with old Redo/Undo/Savepoint format. *)

  (* Step 1: remove undo-redo pairs. We do by processing from the back,
   * adding each element onto the front *)

  |> List.fold_right ~init:[] ~f:(fun op ops ->
    match (op :: ops) with
    | [] -> []
    | [op] -> [op]
    | (Op.UndoTL uid, _) :: (Op.RedoTL rid, _) :: rest when rid = uid ->
      rest

    (* Step 2: error on solo redos *)
    | (Op.RedoTL id1, _) :: (Op.RedoTL id2, _) :: rest when id1 = id2 ->
      op :: ops
    | _ :: (Op.RedoTL _, _) :: rest ->
        Exception.client "Already at latest redo"
    | ops -> ops)

  (* Step 3: remove undos and all the ops up to the savepoint. *)
  (* Go from the front and build the list up. If we hit an undo, drop *)
  (* back until the last favepoint. *)
  |> List.fold_left ~init:[]
     ~f:(fun ops op ->
         match op with
         | (Op.UndoTL tlid, _) ->
           let not_savepoint (o, _) =
             (match o with
             | Op.Savepoint tlids when List.mem tlids tlid ~equal:(=) ->
               false
             | _ -> true)
           in

           let after = List.drop_while ~f:not_savepoint ops in
           let before = List.take_while ~f:not_savepoint ops in
           (* if the canvas is older than the new Savepoints, then its
            * possible to undo to a point with no Savepoints anymore *)
           let (savepoint, sp_bool) = match after with
             | [] -> Exception.client "Cannot undo any more"
             | a :: _  -> a in

           let new_before = List.filter before
             ~f:(fun (o, _) -> Op.tlidsOf o <> [tlid]) in
           let new_savepoint =
             savepoint
             |> Op.tlidsOf
             |> List.filter ~f:((<>) tlid)
             |> fun tlids -> (Op.Savepoint tlids, sp_bool) in
           (* drop savepoint *)
           let new_after = after
                           |> List.tl
                           |> Option.value ~default:[] in

           new_before @ [new_savepoint] @ new_after
         | _ -> op :: ops
      )

  |> List.rev (* previous step leaves the list reversed *)


let undo_count (c: canvas) (tlid: tlid) : int =
  c.ops
    |> List.rev
    |> List.take_while ~f:((=) (Op.UndoTL tlid))
    |> List.length

let is_undoable (c: canvas) (tlid: tlid) : bool =
  c.ops
  |> List.map ~f:(fun op -> (op, false))
  |> preprocess
  |> List.exists ~f:(function | (Op.Savepoint tlids, false) ->
      List.mem ~equal:(=) tlids tlid
                              | _ -> false)

let is_redoable (c: canvas) (tlid: tlid) : bool =
  c.ops |> List.last |> (=) (Some (Op.UndoTL tlid))

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
  let tls, removed =
    List.partition_map
      ~f:(fun x -> if x.tlid <> tlid then `Fst x else `Snd x)
      c.toplevels
  in
  let drop_table_for_db_tl_exn () =
    let dbs = List.filter_map ~f:TL.as_db removed in
    match dbs with
    | [] -> ()
    | db :: [] ->
      (try
         if Db.db_locked db
         then
           Exception.client "Cannot delete DBs with data"
         else
           Db.drop db
       with
       | e ->
         ()
      )
    | oops -> Exception.internal "Multiple DBs with same tlid"
  in
  drop_table_for_db_tl_exn ();
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

let apply_op (op : Op.op) (do_db_ops: bool) (c : canvas ref) : unit =
  (* uncomment if you've cleared the DB for an existing canvas and want
   * to rebuild it *)
  (* let do_db_ops = true in *)
  c :=
    !c |>
    match op with
    | SetHandler (tlid, pos, handler) ->
      upsert_toplevel tlid pos (TL.Handler handler)
    | CreateDB (tlid, pos, name) ->
      if name = ""
      then Exception.client ("DB must have a name")
      else
        let db = Db.create !c.host name tlid in
        if do_db_ops
        then
          Db.init_storage db;
        upsert_toplevel tlid pos (TL.DB db)
    | AddDBCol (tlid, colid, typeid) ->
      apply_to_db ~f:(Db.add_col colid typeid) tlid
    | SetDBColName (tlid, id, name) ->
      apply_to_db ~f:(Db.set_col_name id name do_db_ops) tlid
    | ChangeDBColName (tlid, id, name) ->
      apply_to_db ~f:(Db.change_col_name id name do_db_ops) tlid
    | SetDBColType (tlid, id, tipe) ->
      apply_to_db ~f:(Db.set_col_type id (Dval.tipe_of_string tipe) do_db_ops) tlid
    | ChangeDBColType (tlid, id, tipe) ->
      apply_to_db ~f:(Db.change_col_type id (Dval.tipe_of_string tipe) do_db_ops) tlid
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

let is_uninitialized_db_error (host: string) (e: Postgresql.error) : bool =
  let str = Postgresql.string_of_error e in
  String.is_prefix str
      ~prefix: "Result status PGRES_FATAL_ERROR unexpected (expected status:PGRES_TUPLES_OK); ERROR:  relation"
  &&
  String.is_substring str
    ~substring:"does not exist\nLINE 1:"

let rerun_all_db_ops (host: string) : unit =
  Log.infO "Reruning all ops for" host;
  let (_, ops) = Serialize.search_and_load host in
  let op_pairs = List.map ~f:(fun op -> (op, true)) ops in
  let reduced_ops = preprocess op_pairs in
  let new_canvas = ref { !(create host) with ops = ops } in
  List.iter ~f:(fun (op, _) -> apply_op op true new_canvas) reduced_ops;
  ()

let initialize_host (host:string) : unit =
  Log.infO "Initializing host" host;
  Db.create_namespace host;
  Event_queue.initialize_queue host;
  Db.initialize_migrations host


let add_ops (c: canvas ref) ?(run_old_db_ops=false) (oldops: Op.op list) (newops: Op.op list) : unit =
  if oldops = [] || run_old_db_ops
  then initialize_host !c.host;

  let oldpairs = List.map ~f:(fun o -> (o, run_old_db_ops)) oldops in
  let newpairs = List.map ~f:(fun o -> (o, true)) newops in
  let reduced_ops = preprocess (oldpairs @ newpairs) in
  List.iter ~f:(fun (op, do_db_ops) -> apply_op op do_db_ops c) reduced_ops;
  c := { !c with ops = oldops @ newops }

let minimize (c : canvas) : canvas =
  let ops =
    c.ops
    |> List.map ~f:(fun op -> (op, false))
    |> preprocess
    |> List.map ~f:Tuple.T2.get1
    |> List.filter ~f:Op.has_effect
  in { c with ops = ops }


(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)

let load (host: string) (newops: Op.op list) : canvas ref =
  let c = create host in
  let (run_old_db_ops, oldops) = Serialize.search_and_load host in
  add_ops ~run_old_db_ops c oldops newops;
  c

let save (c : canvas) : unit =
  Serialize.save_in_db c.host c.ops;
  let json = Serialize.json_unversioned_filename c.host in
  let root = Serialize.root_of c.host in
  ignore (Util.convert_bin_to_json ~root c.host json)


let save_test (c: canvas) : string =
  let c = minimize c in
  let host = "test_" ^ c.host in
  let file = Serialize.json_unversioned_filename host in
  let host = if Util.file_exists ~root:Testdata file
             then
               host
               ^ "_"
               ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
             else host in
  let file = Serialize.json_unversioned_filename host in
  Serialize.save_json ~root:Testdata file c.ops;
  file

(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)

let to_frontend
    (environments: RTT.env_map) (f404s : SE.four_oh_four list)
    (execution_id: int)
    (exe_fn_ids: Api.executable_fns) (c : canvas)
  : Yojson.Safe.json =
  let available_reqs id =
    match RTT.EnvMap.find environments id with
    | Some e -> e
    | None -> RTT.EnvMap.find_exn environments 0
  in
  let unlocked =
    c.toplevels
    |> TL.dbs
    |> Db.unlocked
    |> List.map ~f:(fun x -> x.tlid)
    |> List.map ~f:tlid_to_yojson
    |> fun x -> `List x
  in
  let fvals =
    List.map
      c.user_functions
      ~f:(fun f ->
          let fn_ids =
            exe_fn_ids
            |> List.filter_map
              ~f:(fun (tlid, id, cursor) ->
                  if tlid = f.tlid && cursor = 0
                  then Some id
                  else None)
          in
          let env = Functions.environment_for_user_fn f in
          let state : RTT.exec_state =
            { ff = FF.analysis
            ; tlid = f.tlid
            ; host = c.host
            ; user_fns = c.user_functions
            ; exe_fn_ids = fn_ids
            ; env = env
            ; dbs = TL.dbs c.toplevels
            ; id = execution_id
            }
          in
          let value = Functions.execute_for_analysis state f in
          (f.tlid, [value]))
  in
  let vals =
    c.toplevels
    |> TL.handlers
    |> List.map
      ~f:(fun h ->
          let envs = available_reqs h.tlid in
          let fn_ids i =
            exe_fn_ids
            |> List.filter_map
              ~f:(fun (tlid, id, cursor) ->
                  if tlid = h.tlid && i = cursor
                  then Some id
                  else None)
          in
          let state i env : RTT.exec_state =
            { ff = FF.analysis
            ; tlid = h.tlid
            ; host = c.host
            ; user_fns = c.user_functions
            ; exe_fn_ids = fn_ids i
            ; env = env
            ; dbs = TL.dbs c.toplevels
            ; id = execution_id
            }
          in
          let values =
            List.mapi
              ~f:(fun i env ->
                  Handler.execute_for_analysis (state i env) h)
              envs
          in
          (h.tlid, values))
   in
   let vals_to_results vs =
     List.map vs ~f:(fun (id, results) ->
         let to_result (v, ds, syms, inputs) =
           let inputs_to_json i =
             i
             |> Map.to_alist
             |> List.map
               ~f:(fun (k, v) ->
                   (k, v
                       |> Ast.dval_to_livevalue
                       |> Ast.livevalue_to_yojson))
           in
           `Assoc [ ("ast_value"
                    , v |> Ast.dval_to_livevalue |> Ast.livevalue_to_yojson)
                  ; ("live_values", Ast.dval_store_to_yojson ds)
                  ; ("available_varnames", Ast.sym_store_to_yojson syms)
                  ; ("input_values", `Assoc (inputs_to_json inputs))
                  ]
         in
         `Assoc [ ("id", `Int id)
                ; ("results", `List (List.map ~f:to_result results))
                ])
   in
   let analyses = vals_to_results (vals @ fvals)
   in `Assoc
     [ ("analyses", `List analyses)
     ; ("global_varnames",
        (* TODO(ian) *)
        `List (RTT.EnvMap.find_exn environments 0
               |> List.hd_exn
               |> RTT.DvalMap.keys
               |> List.map ~f:(fun s -> `String s)))
     ; ("toplevels", TL.toplevel_list_to_yojson c.toplevels)
     ; ("unlocked_dbs", unlocked )
     ; ("404s", `List (List.map ~f:SE.four_oh_four_to_yojson f404s))
     ; ("user_functions",
        `List (List.map ~f:RTT.user_fn_to_yojson c.user_functions))
     ]

let to_frontend_string (environments: RTT.env_map)
    (f404s : SE.four_oh_four list)
    (execution_id: int)
    (exe_fn_ids: Api.executable_fns) (c: canvas) : string =
  c
  |> to_frontend environments f404s execution_id exe_fn_ids
  |> Yojson.Safe.to_string ~std:true

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

let create_environments (c: canvas) (host: string) :
  (RTT.env_map * SE.four_oh_four list) =

  let dbs = TL.dbs c.toplevels in
  let initial_env = Db.dbs_as_env dbs in
  let sample_request = PReq.sample |> PReq.to_dval in
  let sample_event = RTT.DIncomplete in
  let default_env =
    initial_env
    |> RTT.DvalMap.set ~key:"request" ~data:sample_request
    |> RTT.DvalMap.set ~key:"event" ~data:sample_event
  in

  let descs = SE.list_events host in
  let match_desc h d : bool =
    let (space, path, modifier) = d in
    match Handler.event_desc_for h with
    | Some (h_space, h_path, h_modifier) ->
      Http.path_matches_route ~path h_path
      && h_modifier = modifier
      && h_space = space
    | None -> false
  in


  let env_map acc (h : Handler.handler) =
    let h_envs : (Stored_event.event_desc option * RTT.dval) list =
      let default =
        if Handler.is_http h
        then [sample_request]
        else [sample_event] in
      try
        (* super sorry about this, this is how we go from
         * "here are all the matching event descs for this handler"
         * to a list of input values to the handler tupled with
         * (potentially, if it exists) the event_desc that describes
         * the event that produced that input value
         *
         *
         * The nested fold does this transformation:
         *
         * (event_desc option, dval list) list
         * -> (event_desc option, dval) list
         *)
        descs
        |> List.filter ~f:(match_desc h)
        |> List.map ~f:(fun d -> (Some d, SE.load_events host d))
        |> List.fold_left
             ~f:(fun acc (desc, events) ->
                List.fold_left
                ~f:(fun nacc e ->
                     (desc, e) :: nacc)
                ~init:acc
                events)
             ~init:[]
        |> fun ds ->
            if List.is_empty ds
            then List.map ~f:(fun d -> (None, d)) default
            else ds
      with _ ->
        List.map ~f:(fun d -> (None, d)) default
    in
    let new_envs =
      List.map
        h_envs
        ~f:(fun (maybe_desc, e) ->
            if Handler.is_http h
            then
              let with_r = RTT.DvalMap.set initial_env "request" e in
              (match maybe_desc with
               | Some (space, path, modifier) ->
                 let bound =
                    Http.bind_route_params_exn
                      path
                      (Handler.event_name_for_exn h)
                 in
                 Util.merge_left with_r bound
               | None -> with_r)
            else RTT.DvalMap.set initial_env "event" e)
    in
    RTT.EnvMap.set acc h.tlid new_envs
  in

  let unused_descs =
    descs
    |> List.filter
      ~f:(fun d ->
          not (List.exists (TL.handlers c.toplevels)
                 ~f:(fun h -> match_desc h d)))
    |> List.map ~f:(fun d -> (d, SE.load_events host d))

  in

  let tls_map =
    List.fold_left ~init:RTT.EnvMap.empty ~f:env_map
      (TL.handlers c.toplevels)
  in


  (* TODO(ian): using 0 as a default, come up with better idea
   * later *)
  let envs = RTT.EnvMap.set tls_map 0 [default_env] in
  (envs, unused_descs)



