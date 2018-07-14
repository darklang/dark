open Core_kernel
open Libexecution
open Libcommon

open Util
open Types

module RTT = Types.RuntimeT
module RT = Runtime
module TL = Toplevel

type toplevellist = TL.toplevel list [@@deriving eq, show, yojson]
type canvas = { host : string
              ; owner : Uuidm.t
              ; id : Uuidm.t
              ; ops : (tlid * Op.oplist) list
              ; handlers : toplevellist
              ; dbs: toplevellist
              ; user_functions: RTT.user_fn list
              } [@@deriving eq, show]

(* ------------------------- *)
(* Toplevel *)
(* ------------------------- *)
let upsert_tl (tlid: tlid) (pos: pos) (data: TL.tldata) (tls : toplevellist)
  : toplevellist =
  let tl : TL.toplevel =
    { tlid = tlid
    ; pos = pos
    ; data = data}
  in
  tls
  |> List.filter ~f:(fun x -> x.tlid <> tl.tlid)
  |> (@) [tl]

let upsert_db tlid pos data c =
  { c with dbs = upsert_tl tlid pos data c.dbs}

let upsert_handler tlid pos data c =
  { c with handlers = upsert_tl tlid pos data c.handlers}

let upsert_function (user_fn: RuntimeT.user_fn) (c: canvas) : canvas =
  let fns = List.filter ~f:(fun x -> x.tlid <> user_fn.tlid) c.user_functions in
  { c with user_functions = fns @ [user_fn] }

let remove_toplevel (tlid: tlid) (c: canvas) : canvas =
  let handlers = List.filter ~f:(fun x -> x.tlid <> tlid) c.handlers in
  let dbs = List.filter ~f:(fun x -> x.tlid <> tlid) c.dbs in
  { c with handlers = handlers
         ; dbs = dbs }

let apply_to_toplevel ~(f:(TL.toplevel -> TL.toplevel)) (tlid: tlid) (tls: toplevellist) =
  match List.find ~f:(fun t -> t.tlid = tlid) tls with
  | Some tl ->
    let newtl = f tl in
    upsert_tl newtl.tlid newtl.pos newtl.data tls
  | None ->
    tls

let apply_to_all_toplevels ~(f:(TL.toplevel -> TL.toplevel)) (tlid:tlid) (c: canvas) : canvas =
  { c with handlers = apply_to_toplevel ~f tlid c.handlers
         ; dbs = apply_to_toplevel ~f tlid c.dbs }

let apply_to_db ~(f:(RTT.DbT.db -> RTT.DbT.db)) (tlid: tlid) (c:canvas) : canvas =
  let tlf (tl: TL.toplevel) =
    let data =
      match tl.data with
      | TL.DB db -> TL.DB (f db)
      | _ -> Exception.client "Provided ID is not for a DB"
    in
    { tl with data = data }
  in { c with dbs = apply_to_toplevel tlid ~f:tlf c.dbs }


let apply_to_handler  ~f tlid c =
  { c with handlers = apply_to_toplevel ~f tlid c.handlers}

let move_toplevel (tlid: tlid) (pos: pos) (c: canvas) : canvas =
  apply_to_all_toplevels ~f:(fun tl -> { tl with pos = pos }) tlid c

(* ------------------------- *)
(* Build *)
(* ------------------------- *)

let apply_op (op : Op.op) (c : canvas ref) : unit =
  c :=
    !c |>
    match op with
    | SetHandler (tlid, pos, handler) ->
      upsert_handler tlid pos (TL.Handler handler)
    | CreateDB (tlid, pos, name) ->
      if name = ""
      then Exception.client ("DB must have a name")
      else
        let db = User_db.create !c.host name tlid in
        upsert_db tlid pos (TL.DB db)
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
      apply_to_all_toplevels ~f:(TL.set_expr id e) tlid
    | DeleteTL tlid ->
      remove_toplevel (Log.inspect "removingtl" tlid)
    | MoveTL (tlid, pos) -> move_toplevel tlid pos
    | SetFunction user_fn ->
      upsert_function user_fn
    | TLSavepoint _ -> ident
    | Deprecated0
    | Deprecated1
    | Deprecated2
    | Deprecated3
    | Deprecated4 _ ->
      Exception.internal ("Deprecated ops shouldn't be here anymore! " ^
                          (Op.show_op op) ^ " in " ^ !c.host)
    | UndoTL _
    | RedoTL _ ->
      Exception.internal ("This should have been preprocessed out! " ^ (Op.show_op op))

let oplist2ops (oplist: Op.oplist) : (int * Op.oplist) list =
  oplist
  |> List.filter_map ~f:Op.tlidOf
  |> List.stable_dedup
  |> List.map ~f:(fun tlid ->
      (tlid, List.filter oplist
         ~f:(fun op -> Op.tlidOf op = Some tlid)))

let ops2oplist (ops: (int * Op.oplist) list) : Op.oplist =
  ops
  |> List.unzip
  |> Tuple.T2.get2
  |> List.concat


let add_ops (c: canvas ref) (oldops: Op.op list) (newops: Op.op list) : unit =
  let reduced_ops = Undo.preprocess (oldops @ newops) in
  List.iter ~f:(fun op -> apply_op op c) reduced_ops;
  c := { !c with ops = oplist2ops (oldops @ newops) }

let minimize (c : canvas) : canvas =
  (* TODO *)
  (* let ops = *)
  (*   c.ops *)
  (*   |> Undo.preprocess *)
  (*   |> List.filter ~f:Op.has_effect *)
  (* in { c with ops = ops } *)
  c


(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)

let create ?(load=true) (host: string) (newops: Op.op list) : canvas ref =
  let owner = Account.for_host host in
  let canvas_id = Serialize.fetch_canvas_id owner host in

  let oldops =
    if load
    then Serialize.search_and_load host canvas_id
    else []
  in

  let c =
    ref { host = host
        ; owner = owner
        ; id = canvas_id
        ; ops = []
        ; handlers = []
        ; dbs = []
        ; user_functions = []
        }
  in
  add_ops c oldops newops;
  c


let load host tlids newops =
  let c = create ~load:true host newops in
  c :=
    { !c with handlers =
                List.filter !c.handlers
                  ~f:(fun tl -> List.mem ~equal:(=) tlids tl.tlid)
    };
  c

let http_handlers ~(uri: Uri.t) ~(verb: string) (handlers : toplevellist) :
  toplevellist =
  let path = Uri.path uri in
  List.filter handlers
    ~f:(fun tl ->
        match tl.data with
        | Handler h ->
          Handler.event_name_for h <> None
          && Http.path_matches_route ~path:path (Handler.event_name_for_exn h)
          && (match Handler.modifier_for h with
              | Some m -> String.Caseless.equal m verb
              (* we specifically want to allow handlers without method specifiers for now *)
              | None -> true)
        | _ -> false)


let load_http host ~verb ~uri =
  let c = create ~load:true host [] in
  c := { !c with handlers = http_handlers ~uri ~verb !c.handlers };
  c


let load_all host newops = create ~load:true host newops
let init = create ~load:false


let load_in_new_form host newops : canvas ref =
  let owner = Account.for_host host in
  let canvas_id = Serialize.fetch_canvas_id owner host in

  let oldops =
    Serialize.load_from_per_tlid_oplists ~host ~canvas_id ()
    |> ops2oplist
  in

  let c =
    ref { host = host
        ; owner = owner
        ; id = canvas_id
        ; ops = []
        ; handlers = []
        ; dbs = []
        ; user_functions = []
        }
  in
  add_ops c oldops newops;
  c

let save_everything_in_new_form c : unit =
  let handler_metadata (h: Handler.handler) =
    ( h.tlid
    , ( Ast.blank_to_option h.spec.name
      , Ast.blank_to_option h.spec.module_
      , Ast.blank_to_option h.spec.modifier))
  in
  let hmeta =
    c.handlers
    |> Toplevel.handlers
    |> List.map ~f:handler_metadata
  in
  let set = Int.Map.of_alist_exn hmeta in
  (* Use ops rather than just set of toplevels, because toplevels may
   * have been deleted, and therefore not appear. *)
  List.iter c.ops ~f:(fun (tlid, oplist) ->
      let (name, module_, modifier) =
        Int.Map.find set tlid
        |> Option.value ~default:(None, None, None)
      in
      Serialize.save_toplevel_oplist oplist
        ~tlid ~canvas_id:c.id ~account_id:c.owner
        ~name:name ~module_:module_ ~modifier:modifier)

let save (c : canvas) : unit =
  save_everything_in_new_form c;
  Serialize.save c.host (ops2oplist c.ops)



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
  Serialize.save_json_to_disk ~root:Testdata file (ops2oplist c.ops);
  file

let check_all_oplists () : unit =
  Serialize.current_hosts ()
  |> List.map ~f:(fun host ->
      let c = load_all host [] in
      save_everything_in_new_form !c;
      let c2 = load_in_new_form host [] in
      let sort l = List.sort l ~compare:(fun (tlid1, _) (tlid2, _) ->
          compare tlid1 tlid2) in
      let ops1 = !c.ops |> sort in
      let ops2 = !c2.ops |> sort in
      if ops1 <> ops2
      then Exception.internal ("Not equal for host " ^ host);
      ())
  |> ignore

(* ------------------------- *)
(* Routing *)
(* ------------------------- *)

let matching_routes ~(uri: Uri.t) ~(verb: string) (c: canvas) : (bool * Handler.handler) list =
  let path = Uri.path uri in
  c.handlers
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



