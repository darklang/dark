type id = Node.id
type loc = Node.loc
type param = Node.param

module Map = Core.Map.Poly
type json = Yojson.Basic.json
module J = Yojson.Basic.Util

(* ------------------------- *)
(* Types *)
(* ------------------------- *)
type op = Add_fn of string * id * loc
        | Add_datastore of string * id * loc
        | Add_value of string * id * loc
        (* id, name, type, is_list *)
        | Add_datastore_field of id * string * string * bool
        | Update_node_position of id * loc
        | Delete_node of id
        | Add_edge of id * id * Node.param
        | Delete_edge of id * id * Node.param
        | Clear_edges of id

type graph = {
  name : string;
  ops : op list;
  nodes : (Node.id, Node.node) Map.t;
  edges : (Node.id, (Node.id * Node.param) list) Map.t;
};;

(* ------------------------- *)
(* Graph*)
(* ------------------------- *)
let create (name : string) : graph =
  { name = name
  ; ops = []
  ; nodes = Map.empty
  ; edges = Map.empty
  }

let add_node (g : graph) (node : Node.node) : graph =
  let nodes = Map.add g.nodes (node#id) node in
  let edges = if Map.mem g.edges (node#id)
    then g.edges
    else Map.add g.edges (node#id) []
  in
  { g with nodes = nodes; edges = edges }


(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
let apply_op (g : graph) (op : op) : graph =
  match op with
  | Add_fn (name, id, loc) -> add_node g (new Node.func name id loc)
  | Add_datastore (table, id, loc) -> add_node g (new Node.datastore table id loc)
  | Add_value (expr, id, loc) -> add_node g (new Node.value expr id loc)
  | _ -> failwith "other"

let add_op (g : graph) (op : op) : graph =
  let g = { g with ops = List.append g.ops [op]} in
  apply_op g op



(* ------------------------- *)
(* Serialization *)
let load name = create name
let save name (g : graph) : unit = ()

let json2op (optype : string) (args : json) =
  let str field = J.member field args |> J.to_string in
  let int field = J.member field args |> J.to_int in
  let id = Util.create_id in
  let loc : (unit -> Node.loc) =
    (fun _ : Node.loc -> { x = int "x"; y = int "y" }) in
  match optype with
  | "add_datastore" -> Add_datastore (str "name", id, loc ())
  | "add_function_call" -> Add_fn (str "name", id, loc ())
  | "add_value" -> Add_value (str "value", id, loc ())
  | "update_node_position" -> Update_node_position (int "id", loc ())
  | "add_edge" -> Add_edge (int "src", int "target", str "param")
  | "delete_node" -> Delete_node (int "id")
  | "clear_edges" -> Clear_edges (int "id")
  (* TODO: put this into the frontend *)
  | "add_datastore_field" ->
    let (list, tipe) =
      match Core.String.split_on_chars
              (str "tipe") ~on:['['; ']'] with
      | ["["; s; "]"] -> (true, s)
      | [s] -> (false, s)
      | _ -> failwith "other pattern"
    in
    Add_datastore_field (int "id", str "name", tipe, list)
  | _ -> failwith "impossible"


let op2json op : json =
  let str k v = (k, `String v) in
  let int k v = (k, `Int v) in
  let bool k v = (k, `Bool v) in
  let id id = int "id" id in
  let x (loc : Node.loc) = int "x" loc.x in
  let y (loc : Node.loc) = int "y" loc.y in
  let (name, args) = match op with
    | Add_fn (name, _id, loc) ->
      "add_function_call", [str "name" name; id _id; x loc; y loc]
    | Add_datastore (name, _id, loc) ->
      "add_datastore", [str "name" name; id _id; x loc; y loc]
    | Add_value (expr, _id, loc) ->
      "add_value", [str "value" expr; id _id; x loc; y loc]
    | Add_datastore_field (_id, name, tipe, is_list) ->
      "add_datastore_field",
      [id _id; str "name" name; str "tipe" tipe; bool "is_list" false]
    | Update_node_position (_id, loc) ->
      "update_node_position", [id _id; x loc; y loc]
    | Delete_node _id ->
      "delete_node", [id _id]
    | Add_edge (sid, tid, param) ->
      "add_param", [int "src" sid; int "target" tid; str "param" param]
    | Delete_edge (sid, tid, param) ->
      "delete_edge", [int "src" sid; int "target" tid; str "param" param]
    | Clear_edges _id -> "clear_edges", [id _id]
  in `Assoc ((str "command" name)::args)



(* ------------------------- *)
(* To JSON *)
(* ------------------------- *)
let to_frontend_nodes g : json =
  `Assoc (
    List.map
      (fun n -> (n#idstr, n#to_frontend))
      (Map.data g.nodes)
  )

let to_frontend_edges g : json =
  let toobj = fun s (t, p) -> `Assoc [ ("source", `Int s)
                                     ; ("target", `Int t)
                                     ; ("param", `String p)] in
  let edges = Map.to_alist g.edges in
  let jsons =
    List.map
      (fun (source, targets) ->
         List.map (toobj source) targets) edges
  in
  `List (List.flatten jsons)

let to_frontend (g : graph) : json =
  `Assoc [ ("nodes", to_frontend_nodes g)
         ; ("edges", to_frontend_edges g)
         (* TODO: remove. Should be done on the frontend *)
         ; ("cursor", `Null)
         ]
