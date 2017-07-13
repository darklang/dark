type id = Node.id
type loc = Node.loc
type param = Node.param

module Map = Core.Map.Poly
type json = Yojson.Basic.json

(* ------------------------- *)
(* Types *)
(* ------------------------- *)
type op = Add_fn of string * id * loc
        | Add_datastore of string * id * loc
        | Add_value of string * id * loc
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
