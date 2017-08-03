open Core
open Util
open Types

module ParamMap = String.Map
module NodeMap = Int.Map

type dval = Runtime.dval [@@deriving eq, show]
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
type edge = { source : id
            ; target : id
            ; param : param
            } [@@deriving eq, yojson, show, fields]
type edgelist = edge list [@@deriving eq, yojson, show]
type targetpair = (id * param)
type graph = { name : string
             ; ops : oplist
             ; nodes : nodemap [@printer fun fmt nm -> fprintf fmt "%s" (pp_nodemap nm)]
             ; edges : edgelist
             ; just_added : id option
             } [@@deriving eq, show]

let create (name : string) : graph ref =
  ref { name = name
      ; ops = []
      ; nodes = NodeMap.empty
      ; edges = []
      ; just_added = None
      }

(* ------------------------- *)
(* Traversing *)
(* ------------------------- *)

let get_children id g : edge list =
  g.edges
  |> List.filter ~f:(fun e -> e.source=id)

let get_parents id g : edge list =
  g.edges
  |> List.filter ~f:(fun e -> e.target=id)

let get_named_parent id param g : id =
  g.edges
  |> List.filter ~f:(fun e -> e.target=id && e.param=param)
  |> List.hd_exn
  |> source

let get_node (id : id)  (g : graph) : Node.node =
  NodeMap.find_exn g.nodes id

let has_edge (s: id) (t: id) (param: param) (g : graph) : bool =
  List.exists ~f:(fun e -> e.source=s && e.target=t && e.param=param) g.edges

let param_exists (t: id) (param: param) (g : graph) : bool =
  List.exists ~f:(fun e -> e.target=t && e.param=param) g.edges

(* ------------------------- *)
(* Updating *)
(* ------------------------- *)

let add_constant (v : string) (t: id) (param: param) (g: graph) : graph =
  let n = get_node t g in
  if not (n#has_parameter param) then
    Exception.raise ("Node " ^ n#name ^ " has no parameter " ^ param);

  { g with nodes = NodeMap.update g.nodes t
               ~f:(fun vopt -> match vopt with
                   | None -> Exception.raise "Adding a constant to a node that doesn't exist"
                   | Some n -> n#add_constant param v; n)
  }

let add_edge (s : id) (t : id) (param : param) (g: graph) : graph =
  if s = t then
    (* TODO: deal with cycles *)
    Exception.raise "The source and the target can't be the same node";

  (* Can't have two edges to the same target *)
  (* TODO: exception for datasinks like DBs and APIs *)
  if has_edge s t param g then
    Exception.raise "Edge already exists";

  if param_exists t param g then
    Exception.raise "Can't have two arguments to the same parameter";

  (* Check the target has that parameter *)
  let n = get_node t g in
  if not (n#has_parameter param) then
    Exception.raise ("Node " ^ n#name ^ " has no parameter " ^ param);

  { g with edges = {source=s; target=t; param} :: g.edges }

  (* TODO check the types at both ends of the edge are compatible *)
  (* src_type = src.get_return_type() *)
  (* target_type = target.get_parameter_type(param) *)
  (* try: *)
  (*   types.check(src_type, target_type) *)
  (* except types.DTypeError as e: *)
  (*   raise Exception("Can't turn a %s into a %s (%s -> %s)" % (e.p1, e.p2, src.name(), target.name())) *)


let add_node (node : Node.node) (g : graph) : graph =
  { g with nodes = NodeMap.add g.nodes ~key:(node#id) ~data:node;
           just_added = Some node#id}

let update_node_position (id: id) (loc: loc) (g: graph) : graph =
  g |> get_node id |> (fun n -> n#update_loc loc);
  { g with just_added = None}

let clear_edges (id: id) (g: graph) : graph =
  let f e = e.source <> id && e.target <> id in
  { g with edges = List.filter ~f:f g.edges;
           just_added = None }

let delete_edge s t param (g: graph) : graph =
  let f e = (e.source, e.target, e.param) <> (s, t, param) in
  { g with edges = List.filter ~f:f g.edges;
           just_added = None}

let delete_node id (g: graph) : graph =
  let g = clear_edges id g in
  { g with nodes = NodeMap.remove g.nodes id;
           just_added = None}

(* ------------------------- *)
(* Executing *)
(* ------------------------- *)
type dvalmap = dval Int.Map.t
module DValMap = Int.Map
let rec execute (id: id) ?(eager: dvalmap = DValMap.empty) (g: graph) : dval =
  match DValMap.find eager id with
  | Some v -> v
  | None ->
    let n = get_node id g in
    get_parents id g
    |> List.map ~f:(fun e -> (e.param, execute e.source ~eager g))
    |> List.append (ParamMap.to_alist n#constants)
    |> ParamMap.of_alist_exn
    |> n#execute

let executor id (g: graph ref) : (dval -> dval) =
  (* We specifically need a graph ref here, not an immutable graph as we
     want to execute this later on the completed graph *)
  (fun v ->
     let eager = DValMap.of_alist_exn [(id, v)] in
     let parent = get_named_parent id "return" !g in
     execute parent ~eager !g
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
      add_constant value target param
    | Add_anon (id, inner_id, loc) ->
      (fun _g ->
         let _g = add_node (new Node.anon_inner inner_id loc) _g in
         add_node (new Node.anon id (executor inner_id g) loc) _g
      )
    | Add_edge (src, target, param) -> add_edge src target param
    | Update_node_position (id, loc) -> update_node_position id loc
    | Delete_edge (s, t, param) -> delete_edge s t param
    | Clear_edges (id) -> clear_edges id
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
let node_value n g : (string * string) =
  try
    let dv = execute n#id g in ( Runtime.to_repr dv,  Runtime.get_type dv)
  with
  | Exception.UserException e -> ("Error: " ^ e, "Error")

let to_frontend_nodes g : json =
  g.nodes
  |> NodeMap.data
  |> List.map ~f:(fun n -> n#to_frontend (node_value n g))
  |> Node.nodejsonlist_to_yojson

let to_frontend_edges g : json = edgelist_to_yojson g.edges

let to_frontend (g : graph) : json =
  `Assoc [ ("nodes", to_frontend_nodes g)
         ; ("edges", to_frontend_edges g)
         ; ("just_added", match g.just_added with
           | None -> `Null
           | Some id -> `Int id)
         ]

let to_frontend_string (g: graph) : string =
  g |> to_frontend |> Yojson.Safe.pretty_to_string ~std:true
