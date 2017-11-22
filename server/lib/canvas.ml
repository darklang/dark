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

let page_GETs (c: canvas) : 'a list =
  []

let page_POSTs (c: canvas) : 'a list =
  []


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
         |> List.drop_while ~f:(fun o -> o <> Op.SavePoint)
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
    |> List.exists ~f:((=) Op.SavePoint)

let is_redoable (c: canvas) : bool =
  c.ops |> List.last |> (=) (Some Op.Undo)

let upsert_toplevel (toplevel: TL.toplevel) (c: canvas) : canvas =
  let tls = List.filter ~f:(fun x -> x.id <> toplevel.id) c.toplevels
  in
  { c with toplevels = tls @ [toplevel] }

let remove_toplevel_by_id (id: int) (c: canvas) : canvas =
  let tls = List.filter ~f:(fun x -> x.id <> id) c.toplevels
  in
  { c with toplevels = tls }

let move_toplevel (id: int) (pos: pos) (c: canvas) : canvas =
  match List.find ~f:(fun t -> t.id = id) c.toplevels with
  | Some (tl) ->
    let ntl = { tl with pos = pos } in
    upsert_toplevel ntl c
  | None -> c

let close_thread (tlid: int) (threadid: int) (c: canvas) : canvas =
  c
  (* match List.find ~f:(fun t -> t.id = tlid) c.toplevels with *)
  (* | Some (tl) -> *)
  (* | None -> c *)


(* ------------------------- *)
(* Build *)
(* ------------------------- *)

let apply_op (op : Op.op) (c : canvas ref) : unit =
  c :=
    !c |>
    match op with
    | NoOp -> ident
    | SavePoint -> ident
    | SetTL toplevel -> upsert_toplevel toplevel
    | DeleteTL id -> remove_toplevel_by_id id
    | CloseThread (tlid, thid) -> close_thread tlid thid
    | MoveTL (id, pos) -> move_toplevel id pos
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
    |> List.filter ~f:((<>) Op.SavePoint)
  in { c with ops = ops }


let execute_ast (c: canvas) (tl: TL.toplevel) : (id * RT.dval * Ast.dval_store * Ast.sym_store) list =
  let traced_symbols = Ast.symbolic_execute tl.ast in
  let (ast_value, traced_values) = Ast.execute tl.ast in
  let _ = Log.pp "Hashtable:" traced_values in
  [(tl.id, ast_value, traced_values, traced_symbols)]



(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)


let to_frontend (c : canvas) : Yojson.Safe.json =
  let vals = c.toplevels
             |> List.map ~f:(execute_ast c)
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
        ; ("toplevels", `List (List.map ~f:TL.toplevel_to_frontend c.toplevels))
        ; ("redoable", `Bool (is_redoable c))
        ; ("undo_count", `Int (undo_count c))
        ; ("undoable", `Bool (is_undoable c)) ]

let to_frontend_string (c: canvas) : string =
  c |> to_frontend |> Yojson.Safe.pretty_to_string ~std:true

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

