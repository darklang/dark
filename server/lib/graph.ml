open Core
open Util

type id = Node.id [@@deriving eq]
type loc = Node.loc [@@deriving eq]
type param = Node.param [@@deriving eq]
type dval = Runtime.dval [@@deriving eq]

module IMap = Int.Map
module SMap = String.Map
type json = Yojson.Basic.json
module J = Yojson.Basic.Util


(* ------------------------- *)
(* Types *)
(* ------------------------- *)
type op = Add_fn_call of string * id * loc * id list
        | Add_fn_def of string * id * loc
        | Add_datastore of string * id * loc
        | Add_value of string * id * loc
        (* id, name, type, is_list *)
        | Add_datastore_field of id * string * string * bool
        | Update_node_position of id * loc
        | Delete_node of id
        | Add_edge of id * id * param
        | Delete_edge of id * id * param
        | Clear_edges of id
        | Select_node of id
[@@deriving eq]

type nodemap = (Node.node) IMap.t [@@deriving eq]
type targetpair = (Node.id * param)
type graph = {
  name : string;
  ops : op list;
  nodes : nodemap;
  edges : (id * id * param) list;
  cursor : id option
} [@@deriving eq]

(* ------------------------- *)
(* Graph *)
(* ------------------------- *)

let debug name (g : graph) : unit =
  inspecT "name" name;
  g.ops |> List.length |> Int.to_string |> inspecT "ops";
  g.nodes |> IMap.length |> Int.to_string |> inspecT "nodes";
  g.edges |> List.length |> Int.to_string |> inspecT "nodes";
  ()

let create (name : string) : graph =
  { name = name
  ; ops = []
  ; nodes = IMap.empty
  ; edges = []
  ; cursor = None
  }

(* ------------------------- *)
(* Updating *)
(* ------------------------- *)
let get_node (id : id)  (g : graph) : Node.node =
  IMap.find_exn g.nodes id

let has_edge s t param (g : graph) : bool =
  List.exists ~f:(fun a -> a = (s, t, param)) g.edges

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
  let g = { g with nodes = IMap.add g.nodes ~key:(node#id) ~data:node;
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
  { g with nodes = IMap.remove g.nodes id; cursor = None }


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


(* ------------------------- *)
(* Executing *)
(* ------------------------- *)
let rec execute (id: id) (g: graph) : dval =
  let n = get_node id g in
  (* We dont match up the arguments to the parameter names, so we're just applying this in whatever order we happen to have added things *)
  let args = List.map ~f:(fun (p,s) -> (p, execute s g)) (get_parents id g) in
  let args = SMap.of_alist_exn args in
  n#execute args


(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
let apply_op (op : op) (g : graph) : graph =
  g |>
  match op with
  | Add_fn_call (name, id, loc, edges) ->
    add_node (new Node.func name id loc) edges
  | Add_fn_def (_, id, loc) -> add_node (new Node.anon id loc) []
  | Add_datastore (table, id, loc) -> add_node (new Node.datastore table id loc) []
  | Add_value (expr, id, loc) -> add_node (new Node.value expr id loc) []

  | Add_edge (src, target, param) -> add_edge src target param
  | Update_node_position (id, loc) -> update_node_position id loc
  | Delete_edge (s, t, param) -> delete_edge s t param
  | Clear_edges (id) -> clear_edges id
  | Delete_node (id) -> delete_node id
  | Select_node (id) -> select_node id
  | _ -> failwith "applying unimplemented op"

let add_op (op: op) (g: graph) : graph =
  let g = apply_op op g in
  { g with ops = g.ops @ [op]}

let id_of = function
  | Add_fn_call (_, id, _, _) -> id
  | Add_datastore (_, id, _) -> id
  | Add_value (_, id, _) -> id
  | Update_node_position (id, _) -> id
  | Clear_edges (id) -> id
  | Delete_node (id) -> id
  | Select_node (id) -> id
  | _ -> failwith "getting id of op without id"


(* ------------------------- *)
(* Serialization *)
let json2op (json : json) : op =
  match json with
  | `Assoc [optype, args] -> (
    let str field = J.member field args |> J.to_string in
    let int field = J.member field args |> J.to_int in
    let intlist field = args
                        |> J.member field
                        |> J.to_list
                        |> List.map ~f:J.to_int in
    let id = match J.member "id" args with
      | `Int id -> id
      (* When they come in first, they don't have an id, so add one. *)
      | `Null -> Util.create_id ()
      | j -> "IDs must be ints, not '" ^ (Yojson.Basic.to_string j) ^ "'"
             |> Exception.raise
    in
    let loc : (unit -> Node.loc) =
      (fun _ : Node.loc -> { x = int "x"; y = int "y" }) in
    match optype with
    | "add_datastore" -> Add_datastore (str "name", id, loc ())
    | "add_function_call" ->
      Add_fn_call (str "name", id, loc (), intlist "edges")
    | "add_function_def" -> Add_fn_def (str "name", id, loc ())
    | "add_value" -> Add_value (str "value", id, loc ())
    | "update_node_position" -> Update_node_position (int "id", loc ())
    | "add_edge" -> Add_edge (int "src", int "target", str "param")
    | "delete_edge" -> Delete_edge (int "src", int "target", str "param")
    | "delete_node" -> Delete_node (int "id")
    | "clear_edges" -> Clear_edges (int "id")
    | "select_node" -> Select_node (int "id")
    (* TODO: put this into the frontend *)
    | "add_datastore_field" ->

      let (list, tipe) =
        match String.split_on_chars
                (str "tipe") ~on:['['; ']'] with
        | ["["; s; "]"] -> (true, s)
        | [s] -> (false, s)
        | _ -> failwith "other pattern"
      in
      Add_datastore_field (int "id", str "name", tipe, list)
    | _ -> failwith ("not a valid optype: " ^ optype))
  | _ ->
    failwith ("incorrect op structure" ^ (Yojson.Basic.to_string json))


let op2json op : json =
  let str k v = (k, `String v) in
  let int k v = (k, `Int v) in
  let bool k v = (k, `Bool v) in
  let intlist k vs = (k, `List (List.map ~f:(fun i -> `Int i) vs)) in
  let id id = int "id" id in
  let x (loc : Node.loc) = int "x" loc.x in
  let y (loc : Node.loc) = int "y" loc.y in
  let (name, args) = match op with
    | Add_fn_call (name, _id, loc, edges) ->
      "add_function_call",
      [str "name" name; id _id; x loc; y loc; intlist "edges" edges]
    | Add_fn_def (name, _id, loc) ->
      "add_function_def", [str "name" name; id _id; x loc; y loc]
    | Add_datastore (name, _id, loc) ->
      "add_datastore", [str "name" name; id _id; x loc; y loc]
    | Add_value (expr, _id, loc) ->
      "add_value", [str "value" expr; id _id; x loc; y loc]
    (* TODO: deal with is_list *)
    | Add_datastore_field (_id, name, tipe, _) ->
      "add_datastore_field",
      [id _id; str "name" name; str "tipe" tipe; bool "is_list" false]
    | Update_node_position (_id, loc) ->
      "update_node_position", [id _id; x loc; y loc]
    | Select_node (_id) ->
      "select_node", [id _id]
    | Delete_node _id ->
      "delete_node", [id _id]
    | Add_edge (sid, tid, param) ->
      "add_edge", [int "src" sid; int "target" tid; str "param" param]
    | Delete_edge (sid, tid, param) ->
      "delete_edge", [int "src" sid; int "target" tid; str "param" param]
    | Clear_edges _id -> "clear_edges", [id _id]
  in `Assoc [name, `Assoc args]


let load name : graph =
  let filename = "appdata/" ^ name ^ ".dark" in
  let flags = [Unix.O_RDONLY; Unix.O_CREAT] in
  let file = Unix.openfile filename ~mode:flags ~perm:0o640 in
  let raw = Bytes.create 1000000 in
  let count = Unix.read file ~buf:raw ~pos:0 ~len:1000000 in
  let str = Caml.Bytes.sub_string raw 0 count in
  let str = if String.equal str "" then "[]" else str in
  let jsonops = Yojson.Basic.from_string str in
  let ops = match jsonops with
  | `List ops -> List.map ~f:json2op ops
  | _ -> failwith "unexpected deserialization" in
  List.fold_left ops ~f:(fun g op -> add_op op g) ~init:(create name)



let save (g : graph) : unit =
  let ops = List.map ~f:op2json g.ops in
  let str = `List ops |> Yojson.Basic.to_string in
  let str = str ^ "\n" in
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  let filename = "appdata/" ^ g.name ^ ".dark" in
  let file = Unix.openfile filename ~mode:flags ~perm:0o640 in
  let _ = Unix.write file ~buf:str in
  Unix.close file



(* ------------------------- *)
(* To JSON *)
(* ------------------------- *)
let to_frontend_nodes g : json =
  `Assoc (
    List.map
      ~f:(fun n -> (Int.to_string n#id, n#to_frontend))
      (IMap.data g.nodes)
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
