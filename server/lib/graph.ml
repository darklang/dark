open Core
open Util
open Types

module RT = Runtime

module ParamMap = String.Map
module NodeMap = Node.NodeMap

(* ------------------------- *)
(* Graph *)
(* ------------------------- *)
type oplist = Op.op list [@@deriving eq, yojson, show]
type targetpair = (id * string)
type graph = { name : string
             ; ops : oplist
             ; def : Node.fndef
             ; last_node : id option
             } [@@deriving eq, show]

let create (name : string) : graph ref =
  ref { name = name
      ; ops = []
      ; def = { nodes = Node.NodeMap.empty }
      ; last_node = None
      }

(* ------------------------- *)
(* Traversing *)
(* ------------------------- *)

let get_node (id : id)  (g : graph) : Node.node =
  NodeMap.find_exn g.def.nodes id

(* ------------------------- *)
(* Updating *)
(* ------------------------- *)
let change_node (id: id) (g : graph) (f: (Node.node option -> Node.node option)) : graph =
  { g with def = Node.edit_fn id g.def f }

let update_node (id: id) (g : graph) (f: (Node.node -> Node.node option)) : graph =
  let r = change_node
      id
      g
      (function Some node -> f node
              | None -> Exception.raise "can't update missing node") in
  { r with last_node = Some id }

let update_node_position (id: id) (loc: loc) (g: graph) : graph =
  update_node id g (fun n -> n#update_loc loc; Some n)

let set_arg (a: RT.argument) (t: id) (param: string) (g: graph) : graph =
  update_node t g
    (fun n ->
       if not (n#has_parameter param)
       then Exception.raise ("Node " ^ n#name ^ " has no parameter " ^ param);
       n#set_arg param a;
       Some n)

let set_const (v : string) (t: id) (param: string) (g: graph) : graph =
  set_arg (RT.AConst (RT.parse v)) t param g

let set_edge (s : id) (t : id) (param: string) (g: graph) : graph =
  set_arg (RT.AEdge s) t param g

let clear_args (id: id) (g: graph) : graph =
  update_node id g (fun n -> n#clear_args; Some n)

let delete_arg (t: id) (param:string) (g: graph) : graph =
  update_node t g (fun n -> n#delete_arg param; Some n)

let add_node (node : Node.node) (g : graph) : graph =
  let r = change_node node#id g (fun x -> Some node) in
  { r with last_node = Some node#id }

let delete_node id (g: graph) : graph =
  update_node id g (fun x -> None)

(* ------------------------- *)
(* Executing *)
(* ------------------------- *)
module ValCache = Int.Map
type valcache = RT.dval ValCache.t
let rec execute (id: id) ?(eager: valcache=ValCache.empty) (g: graph) : RT.dval =
  match ValCache.find eager id with
  | Some v -> v
  | None ->
    let n = get_node id g in
    n#arguments
    |> RT.ArgMap.mapi ~f:(fun ~key:(param:string) ~data:(arg:RT.argument) ->
        match arg with
        | RT.AConst dv -> dv
        | RT.AEdge id -> execute id ~eager g)
    |> String.Map.to_alist
    |> RT.DvalMap.of_alist_exn
    |> n#execute

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
let apply_op (op : Op.op) (g : graph ref) : unit =
  g :=
    !g |>
    match op with
    | Add_fn_call (name, id, loc) ->
      add_node (new Node.func name id loc)
    | Add_datastore (table, id, loc) ->
      add_node (new Node.datastore table id loc)
    | Add_value (expr, id, loc) ->
      add_node (new Node.value expr id loc)
    | Add_anon (id, loc) ->
      add_node (new Node.anonfn id loc)
    | Update_node_position (id, loc) -> update_node_position id loc
    | Set_constant (value, target, param) ->
      set_const value target param
    | Set_edge (src, target, param) -> set_edge src target param
    | Delete_arg (t, param) -> delete_arg t param
    | Clear_args (id) -> clear_args id
    | Delete_node (id) -> delete_node id
    | _ -> failwith "applying unimplemented op"

let add_op (op: Op.op) (g: graph ref) : unit =
  apply_op op g;
  g := { !g with ops = !g.ops @ [op]}

(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let filename_for name = "appdata/" ^ name ^ ".dark"

let load name : graph ref =
  let g = create name in
  name
  |> filename_for
  |> Util.readfile ~default:"[]"
  |> Yojson.Safe.from_string
  |> oplist_of_yojson
  |> Result.ok_or_failwith
  |> List.iter ~f:(fun op -> add_op op g);
  g

let save (g : graph) : unit =
  let filename = filename_for g.name in
  g.ops
  |> oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> Util.writefile filename

(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)
let node_value n g : (string * string * string) =
  try
    let dv = execute n#id g in
    ( RT.to_repr dv
    , RT.get_type dv
    , dv |> RT.dval_to_yojson |> Yojson.Safe.to_string)
  with
  | Exception.UserException e -> ("Error: " ^ e, "Error", "Error")

let to_frontend_nodes g : Yojson.Safe.json =
  g.def.nodes
  |> NodeMap.data
  |> List.map ~f:(fun n -> n#to_frontend (node_value n g))
  |> Node.nodejsonlist_to_yojson

let to_frontend (g : graph) : Yojson.Safe.json =
  `Assoc [ ("nodes", to_frontend_nodes g)
         ; ("last_node", match g.last_node with
           | None -> `Null
           | Some id -> `Int id)
         ]

let to_frontend_string (g: graph) : string =
  g |> to_frontend |> Yojson.Safe.pretty_to_string ~std:true
