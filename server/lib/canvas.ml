open Core
open Util
open Types

module RTT = Types.RuntimeT
module RT = Runtime
module TL = Toplevel
module PReq = Parsed_request

type toplevellist = TL.toplevel list [@@deriving eq, show, yojson]
type canvas = { name : string
              ; ops : Op.oplist
              ; toplevels: toplevellist
              } [@@deriving eq, show]


let create (name : string) : canvas ref =
  ref { name = name
      ; ops = []
      ; toplevels = []
      }

(* forgive me simon peyton-jones *)
let cur_canvas : (canvas ref) option ref =
  ref None

(* ------------------------- *)
(* Undo *)
(* ------------------------- *)

let preprocess (ops: (Op.op * bool) list) : (Op.op * bool) list =
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
    | (Op.Undo, _) :: (Op.Redo, _) :: rest -> rest
    | (Op.Redo, a) :: (Op.Redo, b) :: rest -> (Op.Redo, a) :: (Op.Redo, b) :: rest
    | _ :: (Op.Redo, _) :: rest -> (* Step 2: error on solo redos *)
        Exception.internal "Found a redo with no previous undo"
    | ops -> ops)
  (* Step 3: remove undos and all the ops up to the savepoint. *)
  (* Go from the front and build the list up. If we hit an undo, drop back until *)
  (* the last favepoint. *)
  |> List.fold_left ~init:[] ~f:(fun ops op ->
       if Tuple.T2.get1 op = Op.Undo
       then
         ops
         |> List.drop_while ~f:(fun (o, _) -> o <> Op.Savepoint)
         |> (fun ops -> List.drop ops 1)   (* also drop the savepoint *)
       else
         op :: ops)
  |> List.rev (* previous step leaves the list reversed *)
  (* Bonus: remove noops *)
  |> List.filter ~f:(fun (op, _) -> Op.has_effect op)

let undo_count (c: canvas) : int =
  c.ops
    |> List.rev
    |> List.take_while ~f:((=) Op.Undo)
    |> List.length

let is_undoable (c: canvas) : bool =
  c.ops
  |> List.map ~f:(fun op -> (op, false))
  |> preprocess
  |> List.exists ~f:((=) (Op.Savepoint, false))

let is_redoable (c: canvas) : bool =
  c.ops |> List.last |> (=) (Some Op.Undo)

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

let remove_toplevel_by_id (tlid: tlid) (c: canvas) : canvas =
  let tls = List.filter ~f:(fun x -> x.tlid <> tlid) c.toplevels
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

let apply_to_db ~(f:(DbT.db -> DbT.db)) (tlid: tlid) (c:canvas) : canvas =
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
  c :=
    !c |>
    match op with
    | SetHandler (tlid, pos, handler) ->
      upsert_toplevel tlid pos (TL.Handler handler)
    | CreateDB (tlid, pos, name) ->
      let db : DbT.db = { tlid = tlid
                        ; display_name = name
                                         |> String.lowercase
                                         |> String.capitalize
                        ; actual_name = (!c.name ^ "_" ^ name)
                                        |> String.lowercase
                        ; cols = []} in
      if do_db_ops
      then Db.create_new_db tlid db
      else ();
      upsert_toplevel tlid pos (TL.DB db)
    | AddDBCol (tlid, colid, typeid) ->
      apply_to_db ~f:(Db.add_db_col colid typeid) tlid
    | SetDBColName (tlid, id, name) ->
      apply_to_db ~f:(Db.set_col_name id name do_db_ops) tlid
    | SetDBColType (tlid, id, tipe) ->
      apply_to_db ~f:(Db.set_db_col_type id (Dval.tipe_of_string tipe) do_db_ops) tlid
    | DeleteTL tlid -> remove_toplevel_by_id tlid
    | MoveTL (tlid, pos) -> move_toplevel tlid pos
    | Savepoint -> ident
    | DeleteAll | Undo | Redo ->
      Exception.internal ("This should have been preprocessed out! " ^ (Op.show_op op))


let add_ops (c: canvas ref) (oldops: Op.op list) (newops: Op.op list) : unit =
  let oldpairs = List.map ~f:(fun o -> (o, false)) oldops in
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
    |> List.fold_left ~init:[]
        ~f:(fun ops op -> if op = Op.DeleteAll
                          then []
                          else (ops @ [op]))
    |> List.filter ~f:Op.has_effect
  in { c with ops = ops }


(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let load ?(filename=None) (name: string) (newops: Op.op list) : canvas ref =
  let c = create name in
  let oldops =
    match filename with
    | Some file -> Serialize.load_binary file
    | None -> Serialize.search_and_load name
  in
  add_ops c oldops newops;
  c

let save ?(filename=None) (c : canvas) : unit =
  match filename with
  | None ->
      let filename = Serialize.dark_save_filename c.name in
      let json = Serialize.json_save_filename c.name in
      Serialize.save_binary filename c.ops;
      let _ = Spawn.spawn
                ~prog:(Config.bin_root_dir ^ "darkfile_bin_to_json.exe")
                ~argv:[""; filename; json]
                ()
      in
      ()
  | Some file ->
      Serialize.save_binary file c.ops



let save_test (c: canvas) : string =
  let c = minimize c in
  let name = "test_" ^ c.name in
  let name = if Sys.file_exists (Serialize.json_save_filename name) = `Yes
             then name ^ "_"
                  ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
             else name in
  let c = {c with name = name } in
  save c;
  Serialize.json_save_filename c.name


(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)

let to_frontend (environments: RTT.env_map) (c : canvas) : Yojson.Safe.json =
  let available_reqs id =
    match RTT.EnvMap.find environments id with
    | Some e -> e
    | None -> RTT.EnvMap.find_exn environments 0
  in
  let vals = c.toplevels
             |> TL.handlers
             |> List.map
               ~f:(fun h ->
                   let envs = available_reqs h.tlid in
                   let values =
                     List.map
                       ~f:(Handler.execute_for_analysis h)
                       envs
                   in
                   (h.tlid, values)
                 )
             |> List.map ~f:(fun (id, results) ->
                 let to_result (v, ds, syms) =
                   `Assoc [ ("ast_value", v |> Ast.dval_to_livevalue
                                            |> Ast.livevalue_to_yojson)
                          ; ("live_values", Ast.dval_store_to_yojson ds)
                          ; ("available_varnames", Ast.sym_store_to_yojson syms)
                          ]
                  in
                  `Assoc [ ("id", `Int id)
                         ; ("results", `List (List.map ~f:to_result results))
                         ])
  in `Assoc
        [ ("analyses", `List vals)
        ; ("global_varnames",
           (* TODO(ian) *)
           `List (RTT.DvalMap.keys (RTT.EnvMap.find_exn environments 0 |> List.hd_exn)
                  |> List.map ~f:(fun s -> `String s)))
        ; ("toplevels", TL.toplevel_list_to_yojson c.toplevels)
        ; ("redoable", `Bool (is_redoable c))
        ; ("undo_count", `Int (undo_count c))
        ; ("undoable", `Bool (is_undoable c)) ]

let to_frontend_string (environments: RTT.env_map) (c: canvas) : string =
  c |> to_frontend environments |> Yojson.Safe.pretty_to_string ~std:true

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

let create_environments (c: canvas) (host: string) : RTT.env_map =
  let dbs = TL.dbs c.toplevels in
  let dbs_env = Db.dbs_as_env dbs in
  Db.cur_dbs := dbs;
  let sample_request = PReq.sample |> PReq.to_dval in
  let sample_event = RTT.DObj (RTT.DvalMap.empty) in
  let default_env =
    dbs_env
    |> RTT.DvalMap.set ~key:"request" ~data:sample_request
    |> RTT.DvalMap.set ~key:"event" ~data:sample_event
  in
  let env_map acc (h : Handler.handler) =
    let h_envs =
      try
        Stored_event.load_all host h.tlid
      with
      | _ ->
        if Handler.is_http h
        then [sample_request]
        else [sample_event]
    in
    let new_envs =
      List.map h_envs
        ~f:(fun e ->
            if Handler.is_http h
            then RTT.DvalMap.set dbs_env "request" e
            else RTT.DvalMap.set dbs_env "event" e)
    in
    RTT.EnvMap.set acc h.tlid new_envs
  in
  let tls_map =
    List.fold_left ~init:RTT.EnvMap.empty ~f:env_map (TL.handlers c.toplevels)
  in
  (* TODO(ian): using 0 as a default, come up with better idea
   * later *)
  RTT.EnvMap.set tls_map 0 [default_env]



