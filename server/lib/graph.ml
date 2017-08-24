open Core
open Util
open Types

module RT = Runtime

module ParamMap = String.Map
module NodeMap = Int.Map

type nodemap = Node.node NodeMap.t [@@deriving eq]
type json = Yojson.Safe.json

let pp_nodemap nm =
  let to_s ~key ~data = (show_id key) ^ ": " ^ (Node.show_node data) in
  let objs = NodeMap.mapi ~f:to_s nm in
  "{"
  ^ (String.concat ~sep:", " (NodeMap.data objs))
  ^ "}"

(* ------------------------- *)
(* Graph *)
(* ------------------------- *)
type oplist = Op.op list [@@deriving eq, yojson, show]
type targetpair = (id * string)
type graph = { name : string
             ; ops : oplist
             ; nodes : nodemap [@printer fun fmt nm -> fprintf fmt "%s" (pp_nodemap nm)]
             ; just_added : id option
             } [@@deriving eq, show]

let create (name : string) : graph ref =
  ref { name = name
      ; ops = []
      ; nodes = NodeMap.empty
      ; just_added = None
      }

(* ------------------------- *)
(* Traversing *)
(* ------------------------- *)

let get_node (id : id)  (g : graph) : Node.node =
  NodeMap.find_exn g.nodes id

(* ------------------------- *)
(* Updating *)
(* ------------------------- *)
let update_node (id: id) (g: graph) (fn: (Node.node -> Node.node)) : graph =
  { g with
    nodes = NodeMap.update g.nodes id
        ~f:(fun vopt -> match vopt with
            | None -> Exception.raise "Updating a node that doesn't exist"
            | Some n -> fn n)
  ; just_added = None
  }

let update_node_position (id: id) (loc: loc) (g: graph) : graph =
  update_node id g (fun n -> n#update_loc loc; n)

let set_arg (a: RT.argument) (t: id) (param: string) (g: graph) : graph =
  update_node t g
    (fun n ->
       if not (n#has_parameter param)
       then Exception.raise ("Node " ^ n#name ^ " has no parameter " ^ param);
       n#set_arg param a; n)

let set_const (v : string) (t: id) (param: string) (g: graph) : graph =
  set_arg (RT.AConst (RT.parse v)) t param g

let set_edge (s : id) (t : id) (param: string) (g: graph) : graph =
  set_arg (RT.AEdge s) t param g

let add_node (node : Node.node) (g : graph) : graph =
  { g with nodes = NodeMap.add g.nodes ~key:(node#id) ~data:node;
           just_added = Some node#id}

let clear_args (id: id) (g: graph) : graph =
  update_node id g (fun n -> n#clear_args; n)

let delete_arg (s: id) (t: id) (param:string) (g: graph) : graph =
  update_node t g (fun n -> n#delete_arg param; n)

let delete_node id (g: graph) : graph =
  { g with nodes = NodeMap.remove g.nodes id;
           just_added = None}

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

let executor id (g: graph ref) : (RT.dval -> RT.dval) =
  (* We specifically need a graph ref here, not an immutable graph as we
     want to execute this later on the completed graph *)
  (fun v ->
     let eager = ValCache.of_alist_exn [(id, v)] in
     let node = get_node id !g in
     let parent = RT.ArgMap.find node#arguments "return" in
     match parent with
     | Some (RT.AEdge parentid) -> execute parentid ~eager !g
     | _ -> DIncomplete
  )

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
    | Add_constant (value, target, param) ->
      set_const value target param
    | Add_anon (id, inner_id, loc) ->
      (fun _g ->
         let _g = add_node (new Node.anon_inner inner_id loc) _g in
         add_node (new Node.anon id (executor inner_id g) loc) _g
      )
    | Add_edge (src, target, param) -> set_edge src target param
    | Update_node_position (id, loc) -> update_node_position id loc
    | Delete_edge (s, t, param) -> delete_arg s t param
    | Clear_edges (id) -> clear_args id
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

let to_frontend_nodes g : json =
  g.nodes
  |> NodeMap.data
  |> List.map ~f:(fun n -> n#to_frontend (node_value n g))
  |> Node.nodejsonlist_to_yojson

let to_frontend (g : graph) : json =
  `Assoc [ ("nodes", to_frontend_nodes g)
         ; ("just_added", match g.just_added with
           | None -> `Null
           | Some id -> `Int id)
         ]

let to_frontend_string (g: graph) : string =
  g |> to_frontend |> Yojson.Safe.pretty_to_string ~std:true
