open Core
open Util
open Types

module RT = Runtime
module TL = Toplevel

type oplist = Op.op list [@@deriving eq, show, yojson]
type toplevellist = TL.toplevel list [@@deriving eq, show, yojson]
type canvas = { name : string
              ; ops : oplist
              ; toplevels: toplevellist
              } [@@deriving eq, show]

let create (name : string) : canvas ref =
  ref { name = name
      ; ops = []
      ; toplevels = []
      }

(* ------------------------- *)
(* Undo *)
(* ------------------------- *)

let preprocess (ops: Op.op list) : Op.op list =
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
    | Op.Undo :: Op.Redo :: rest -> rest
    | Op.Redo :: Op.Redo :: rest -> Op.Redo :: Op.Redo :: rest
    | _ :: Op.Redo :: rest -> (* Step 2: error on solo redos *)
        Exception.internal "Found a redo with no previous undo"
    | ops -> ops)
  (* Step 3: remove undos and all the ops up to the savepoint. *)
  (* Go from the front and build the list up. If we hit an undo, drop back until *)
  (* the last favepoint. *)
  |> List.fold_left ~init:[] ~f:(fun ops (op: Op.op) ->
       if op = Op.Undo
       then
         ops
         |> List.drop_while ~f:(fun o -> o <> Op.Savepoint)
         |> (fun ops -> List.drop ops 1)   (* also drop the savepoint *)
       else
         op :: ops)
  |> List.rev (* previous step leaves the list reversed *)
  (* Bonus: remove noops *)
  |> List.filter ~f:((<>) Op.NoOp)


let undo_count (c: canvas) : int =
  c.ops
    |> List.rev
    |> List.take_while ~f:((=) Op.Undo)
    |> List.length

let is_undoable (c: canvas) : bool =
  c.ops
    |> preprocess
    |> List.exists ~f:((=) Op.Savepoint)

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

let apply_op (op : Op.op) (c : canvas ref) : unit =
  c :=
    !c |>
    match op with
    | NoOp -> ident
    | SetHandler (tlid, pos, handler) ->
      upsert_toplevel tlid pos (TL.Handler handler)
    | CreateDB (tlid, pos, name) ->
      let db : DbT.db = { tlid = tlid
                        ; name = name
                        ; cols = []} in
      Db.create_new_db tlid name;
      upsert_toplevel tlid pos (TL.DB db)
    | AddDBCol (tlid, colid, typeid) ->
      apply_to_db ~f:(Db.add_db_col colid typeid) tlid
    | SetDBColName (tlid, id, name) ->
      apply_to_db ~f:(Db.set_col_name id name) tlid
    | SetDBColType (tlid, id, tipe) ->
      apply_to_db ~f:(Db.set_db_col_type id (Dval.tipe_of_string tipe)) tlid
    | DeleteTL tlid -> remove_toplevel_by_id tlid
    | MoveTL (tlid, pos) -> move_toplevel tlid pos
    | Savepoint -> ident
    | _ ->
      Exception.internal ("applying unimplemented op: " ^ Op.show_op op)


let add_ops (c: canvas ref) (ops: Op.op list) : unit =
  let reduced_ops = preprocess ops in
  List.iter ~f:(fun op -> apply_op op c) reduced_ops;
  c := { !c with ops = ops }


(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let filename_for name = "appdata/" ^ name ^ ".dark"

let load ?(filename=None) (name: string) (ops: Op.op list) : canvas ref =
  let c = create name in
  let filename = Option.value filename ~default:(filename_for name) in
  filename
  |> Util.readfile ~default:"[]"
  |> Yojson.Safe.from_string
  |> oplist_of_yojson
  |> Result.ok_or_failwith
  |> (fun os -> os @ ops)
  |> add_ops c;
  c

let save ?(filename=None) (c : canvas) : unit =
  let filename = Option.value filename ~default:(filename_for c.name) in
  c.ops
  |> oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> Util.writefile filename

let minimize (c : canvas) : canvas =
  let ops =
    c.ops
    |> preprocess
    |> List.fold_left ~init:[]
        ~f:(fun ops op -> if op = Op.DeleteAll
                          then []
                          else (ops @ [op]))
    |> List.filter ~f:((<>) Op.Savepoint)
  in { c with ops = ops }


(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)


let to_frontend (environment: Ast.symtable) (c : canvas) : Yojson.Safe.json =
  let vals = c.toplevels
             |> TL.handlers
             |> List.map
               ~f:(Handler.execute_for_analysis environment)
             |> List.concat
             |> List.map ~f:(fun (id, v, ds, syms) ->
                 `Assoc [ ("id", `Int id)
                        ; ("ast_value", v |> Ast.dval_to_livevalue
                                          |> Ast.livevalue_to_yojson)
                        ; ("live_values", Ast.dval_store_to_yojson ds)
                        ; ("available_varnames", Ast.sym_store_to_yojson syms)
                        ])
  in `Assoc
        [ ("analyses", `List vals)
        ; ("toplevels", TL.toplevel_list_to_yojson c.toplevels)
        ; ("redoable", `Bool (is_redoable c))
        ; ("undo_count", `Int (undo_count c))
        ; ("undoable", `Bool (is_undoable c)) ]

let to_frontend_string (environment: Ast.symtable) (c: canvas) : string =
  c |> to_frontend environment |> Yojson.Safe.pretty_to_string ~std:true

let save_test (c: canvas) : string =
  let c = minimize c in
  let filename = "appdata/test_" ^ c.name ^ ".dark" in
  let name = if Sys.file_exists filename = `Yes
             then c.name ^ "_"
                  ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
             else c.name in
  let c = {c with name = name } in
  let filename = "appdata/test_" ^ name ^ ".dark" in
  save ~filename:(Some filename) c;
  filename

let matching_routes ~(uri: Uri.t) (c: canvas) : Handler.handler list =
  let path = Uri.path uri in
  c.toplevels
  |> TL.handlers
  |> List.filter
    ~f:(fun h -> Handler.url_for h <> None)
  |> List.filter
    ~f:(fun h -> Http.path_matches_route ~path:path (Handler.url_for_exn h))

let pages_matching_route ~(uri: Uri.t) (c: canvas) : Handler.handler list =
  matching_routes ~uri:uri c

