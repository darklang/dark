open Core
open Util
open Types

module ParamMap = String.Map
module NodeMap = Int.Map

type dval = Runtime.dval [@@deriving eq]
type nodemap = Node.node NodeMap.t [@@deriving eq]
type json = Yojson.Basic.json


(* ------------------------- *)
(* Graph *)
(* ------------------------- *)
type targetpair = (id * param)
type graph = {
  name : string;
  ops : Op.op list;
  nodes : nodemap;
  edges : (id * id * param) list;
  cursor : id option
} [@@deriving eq]

let create (name : string) : graph ref =
  ref { name = name
      ; ops = []
      ; nodes = NodeMap.empty
      ; edges = []
      ; cursor = None
      }

(* ------------------------- *)
(* Traversing *)
(* ------------------------- *)

let get_children id g : (param * id) list =
  g.edges
  |> List.filter ~f:(fun (s,_,_) -> s = id)
  |> List.map ~f:(fun (_,t,p) -> (p,t))

let get_parents id g : (param * id) list =
  g.edges
  |> List.filter ~f:(fun (_,t,_) -> t = id)
  |> List.map ~f:(fun (s,_,p) -> (p,s))

let get_named_parent id param g : id =
  g.edges
  |> List.filter ~f:(fun (_,t,p) -> t = id && p = param)
  |> List.hd_exn
  |> Tuple3.get1

let get_node (id : id)  (g : graph) : Node.node =
  NodeMap.find_exn g.nodes id

let has_edge s t param (g : graph) : bool =
  List.exists ~f:(fun a -> a = (s, t, param)) g.edges


(* ------------------------- *)
(* Updating *)
(* ------------------------- *)
let add_edge (s : id) (t : id) (param : param) (g: graph) : graph =
  (* Can't have two edges to the same target *)
  (* TODO: exception for datasinks like DBs and APIs *)
  if has_edge s t param g then
    Exception.raise "Edge already exists";

  (* Check the target has that parameter *)
  let n = get_node t g in
  if not (List.mem ~equal:String.equal n#parameters param) then
    Exception.raise ("Node " ^ n#name ^ " has no parameter " ^ param);

  { g with edges = (s, t, param) :: g.edges }

  (* TODO check the types at both ends of the edge are compatible *)
  (* src_type = src.get_return_type() *)
  (* target_type = target.get_parameter_type(param) *)
  (* try: *)
  (*   types.check(src_type, target_type) *)
  (* except types.DTypeError as e: *)
  (*   raise Exception("Can't turn a %s into a %s (%s -> %s)" % (e.p1, e.p2, src.name(), target.name())) *)


let add_node (node : Node.node) (edges : id list) (g : graph) : graph =
  let len = List.length edges in
  let params = List.take node#parameters len in
  let sources = List.zip_exn edges params in
  let g = { g with nodes = NodeMap.add g.nodes ~key:(node#id) ~data:node;
                   cursor = Some node#id }
  in
  List.fold_left
    ~f:(fun g (s, p) -> add_edge s node#id p g)
    ~init:g
    sources

let update_node_position (id: id) (loc: loc) (g: graph) : graph =
  g |> get_node id |> (fun n -> n#update_loc loc);
  { g with cursor = Some id }

let select_node (id: id) (g: graph) : graph =
  { g with cursor = Some id }

let clear_edges (id: id) (g: graph) : graph =
  let f (s, t, _) = s <> id && t <> id in
  { g with edges = List.filter ~f:f g.edges }

let delete_edge s t param (g: graph) : graph =
  let f a = a <> (s, t, param) in
  { g with edges = List.filter ~f:f g.edges }

let delete_node id (g: graph) : graph =
  let g = clear_edges id g in
  { g with nodes = NodeMap.remove g.nodes id; cursor = None }

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
    let args = List.map
        ~f:(fun (p,s) -> (p, execute s ~eager g))
        (get_parents id g) in
    let args = ParamMap.of_alist_exn args in
    n#execute args

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
    | Add_fn_call (name, id, loc, edges) ->
      add_node (new Node.func name id loc) edges
    | Add_datastore (table, id, loc) ->
      add_node (new Node.datastore table id loc) []
    | Add_value (expr, id, loc) ->
      add_node (new Node.value expr id loc) []
    | Add_anon (id, inner_id, loc) ->
      (fun _g ->
         let _g = add_node (new Node.anon_inner inner_id loc) [] _g in
         add_node (new Node.anon id (executor inner_id g) loc) [] _g
      )
    | Add_edge (src, target, param) -> add_edge src target param
    | Update_node_position (id, loc) -> update_node_position id loc
    | Delete_edge (s, t, param) -> delete_edge s t param
    | Clear_edges (id) -> clear_edges id
    | Delete_node (id) -> delete_node id
    | Select_node (id) -> select_node id
    | _ -> failwith "applying unimplemented op"

let add_op (op: Op.op) (g: graph ref) : unit =
  apply_op op g;
  g := { !g with ops = !g.ops @ [op]}

(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let load name : graph ref =
  let filename = "appdata/" ^ name ^ ".dark" in
  let str = Util.readfile filename ~default:"[]" in
  let jsonops = Yojson.Basic.from_string str in
  let ops = match jsonops with
  | `List ops -> List.map ~f:Op.serial2op ops
  | _ -> failwith "unexpected deserialization" in
  let g = create name in
  List.iter ops ~f:(fun op -> add_op op g);
  g

let save (g : graph) : unit =
  let ops = List.map ~f:Op.op2serial g.ops in
  let str = `List ops |> Yojson.Basic.to_string in
  let str = str ^ "\n" in
  let filename = "appdata/" ^ g.name ^ ".dark" in
  Util.writefile filename str

(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)
let to_frontend_nodes g : json =
  `Assoc (
    List.map
      ~f:(fun n -> (Int.to_string n#id, n#to_frontend))
      (NodeMap.data g.nodes)
  )

let to_frontend_edges g : json =
  let toobj = fun (s, t, p) -> `Assoc [ ("source", `Int s)
                                      ; ("target", `Int t)
                                      ; ("param", `String p)] in
  `List (List.map ~f:toobj g.edges)


let to_frontend (g : graph) : json =
  `Assoc [ ("nodes", to_frontend_nodes g)
         ; ("edges", to_frontend_edges g)
         (* TODO: remove. Should be done on the frontend *)
         ; ("cursor", match g.cursor with
             | None -> `Null
             | Some id -> `Int id)
         ; ("live", match g.cursor with
             | None -> `Null
             | Some id ->
               try
                 let dv = execute id g in
                 `Assoc [ ("value", `String (Runtime.to_repr dv))
                        ; ("type", `String (Runtime.get_type dv))]
               with
               | Exception.UserException e ->
                 `Assoc [ ("value", `String ("Error: " ^ e))
                        ; ("type", `String "Error")]
           )

         ]

let to_frontend_string (g: graph) : string =
  g |> to_frontend |> Yojson.Basic.pretty_to_string ~std:true
