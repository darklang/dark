open Core
open Util
open Types

module RT = Runtime

type oplist = Op.op list [@@deriving eq, show, yojson]
type toplevellist = Ast.toplevel list [@@deriving eq, show, yojson]
type graph = { name : string
             ; ops : oplist
             ; toplevels: toplevellist
             } [@@deriving eq, show]

let create (name : string) : graph ref =
  ref { name = name
      ; ops = []
      ; toplevels = []
      }

let page_GETs (g: graph) : 'a list =
  []

let page_POSTs (g: graph) : 'a list =
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


let undo_count (g: graph) : int =
  g.ops
    |> List.rev
    |> List.take_while ~f:((=) Op.Undo)
    |> List.length

let is_undoable (g: graph) : bool =
  g.ops
    |> preprocess
    |> List.exists ~f:((=) Op.SavePoint)

let is_redoable (g: graph) : bool =
  g.ops |> List.last |> (=) (Some Op.Undo)

let add_toplevel (toplevel: Ast.toplevel) (g: graph) : graph =
  { g with toplevels = g.toplevels @ [toplevel] }

(* ------------------------- *)
(* Build *)
(* ------------------------- *)

let apply_op (op : Op.op) (g : graph ref) : unit =
  g :=
    !g |>
    match op with
    | NoOp -> ident
    | SavePoint -> ident
    | SetAST toplevel -> add_toplevel toplevel
    | _ ->
      Exception.internal ("applying unimplemented op: " ^ Op.show_op op)


let add_ops (g: graph ref) (ops: Op.op list) : unit =
  let reduced_ops = preprocess ops in
  List.iter ~f:(fun op -> apply_op op g) reduced_ops;
  g := { !g with ops = ops }


(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let filename_for name = "appdata/" ^ name ^ ".dark"

let load ?(filename=None) (name: string) (ops: Op.op list) : graph ref =
  let g = create name in
  let filename = Option.value filename ~default:(filename_for name) in
  filename
  |> Util.readfile ~default:"[]"
  |> Yojson.Safe.from_string
  |> oplist_of_yojson
  |> Result.ok_or_failwith
  |> (fun os -> os @ ops)
  |> add_ops g;
  g

let save ?(filename=None) (g : graph) : unit =
  let filename = Option.value filename ~default:(filename_for g.name) in
  g.ops
  |> oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> Util.writefile filename

let minimize (g : graph) : graph =
  let ops =
    g.ops
    |> preprocess
    |> List.fold_left ~init:[]
        ~f:(fun ops op -> if op = Op.DeleteAll
                          then []
                          else (ops @ [op]))
    |> List.filter ~f:((<>) Op.SavePoint)
  in { g with ops = ops }


(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)
let to_frontend (g : graph) : Yojson.Safe.json =
  `Assoc [ ("redoable", `Bool (is_redoable g))
         ; ("undo_count", `Int (undo_count g))
         ; ("undoable", `Bool (is_undoable g)) ]

let to_frontend_string (g: graph) : string =
  g |> to_frontend |> Yojson.Safe.pretty_to_string ~std:true

let save_test (g: graph) : string =
  let g = minimize g in
  let filename = "appdata/test_" ^ g.name ^ ".dark" in
  let name = if Sys.file_exists filename = `Yes
             then g.name ^ "_"
                  ^ (Unix.gettimeofday () |> int_of_float |> string_of_int)
             else g.name in
  let g = {g with name = name } in
  let filename = "appdata/test_" ^ name ^ ".dark" in
  save ~filename:(Some filename) g;
  filename

