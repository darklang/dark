type id = Node.id
type loc = Node.loc
type param = Node.param

module Map = Core.Map.Poly
type json = Yojson.Basic.json
module J = Yojson.Basic.Util
let inspect = Util.inspect

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
        | Add_edge of id * id * param
        | Delete_edge of id * id * param
        | Clear_edges of id

type nodemap = (Node.id, Node.node) Map.t
type targetpair = (Node.id * param)
type edgemap = (Node.id, targetpair list) Map.t
type reverse_edgemap = (Node.id, Node.id list) Map.t
type graph = {
  name : string;
  ops : op list;
  nodes : nodemap;
  edges : edgemap;
}

(* ------------------------- *)
(* Graph *)
(* ------------------------- *)

let debug name (g : graph) : unit =
  let _ = inspect "name" name in
  let _ = g.ops |> List.length |> Core.Int.to_string |> inspect "ops" in
  let _ = g.nodes |> Core.Map.Poly.count ~f:(fun _ -> true) |> Core.Int.to_string |> inspect "nodes" in
  let _ = g.edges |> Core.Map.Poly.count ~f:(fun _ -> true) |> Core.Int.to_string |> inspect "nodes" in
  ()

let create (name : string) : graph =
  { name = name
  ; ops = []
  ; nodes = Map.empty
  ; edges = Map.empty
  }

(* ------------------------- *)
(* Traversal *)
(* ------------------------- *)
let flat_edge_list (g : graph) : (id * id * param) list =
  let expand_list (k, v) : (id * id * param) list =
    List.map (fun (t, p) -> (k, t, p)) v
  in
  g.edges |> Map.to_alist |> List.map expand_list |> List.flatten

let reverse_edges (g : graph) : reverse_edgemap =
  let f ~(key : Node.id) ~(data : targetpair list) (rem : reverse_edgemap) : reverse_edgemap =
    let pair_to_map map (t, p) =
      Map.add_multi map t key
    in
    List.fold_left pair_to_map rem data in
  Map.fold g.edges ~init:Map.empty ~f:f

let get_parents (g : graph) (n : id) : (param, id) Map.t =
  g
  |> flat_edge_list
  |> List.filter (fun (s, t, p) -> t == n)
  |> List.fold_left (fun m (s, t, p) -> Map.add m p s) Map.empty


(* ------------------------- *)
(* Updating *)
(* ------------------------- *)
let add_node (g : graph) (node : Node.node) : graph =
  let nodes = Map.add g.nodes (node#id) node in
  (* TODO: replace with add_multi *)
  let edges = if Map.mem g.edges (node#id)
    then g.edges
    else Map.add g.edges (node#id) []
  in
  { g with nodes = nodes; edges = edges }

let add_edge (g: graph) (sid : id) (tid : id) (param : param) : graph =
  (* let src = Map.find_exn g.nodes sid in *)
  (* let target = Map.find_exn g.nodes tid in *)

  (* Can't have two edges to the same target *)
  (* TODO: exception for datasinks like DBs and APIs *)
  let _ = if Map.mem (get_parents g tid) param then
      failwith "Edge already exists" in

  (* TODO check the types at both ends of the edge are compatible *)
  (* src_type = src.get_return_type() *)
  (* target_type = target.get_parameter_type(param) *)
  (* try: *)
  (*   types.check(src_type, target_type) *)
  (* except types.DTypeError as e: *)
  (*   raise Exception("Can't turn a %s into a %s (%s -> %s)" % (e.p1, e.p2, src.name(), target.name())) *)

  { g with edges = Map.add_multi g.edges sid (tid, param) }

    (* self.edges[src.id()].append((target.id(), param)) *)



(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
let apply_op (g : graph) (op : op) : graph =
  match op with
  | Add_fn (name, id, loc) -> add_node g (new Node.func name id loc)
  | Add_datastore (table, id, loc) -> add_node g (new Node.datastore table id loc)
  | Add_value (expr, id, loc) -> add_node g (new Node.value expr id loc)
  | Add_edge (src, target, param) -> add_edge g src target param
  | _ -> failwith "applying unimplemented op"

let add_op (g : graph) (op : op) : graph =
  let g = { g with ops = List.append g.ops [op]} in
  apply_op g op



(* ------------------------- *)
(* Serialization *)
let json2op (json : json) : op =
  match json with
  | `Assoc [optype, args] -> (
    let str field = J.member field args |> J.to_string in
    let int field = J.member field args |> J.to_int in
    (* TODO: ints encoded as strings: fix *)
    let istr field = J.member field args |> J.to_string |> Pervasives.int_of_string in
    let id = Util.create_id () in
    let loc : (unit -> Node.loc) =
      (fun _ : Node.loc -> { x = int "x"; y = int "y" }) in
    match optype with
    | "add_datastore" -> Add_datastore (str "name", id, loc ())
    | "add_function_call" -> Add_fn (str "name", id, loc ())
    | "add_value" -> Add_value (str "value", id, loc ())
    | "update_node_position" -> Update_node_position (istr "id", loc ())
    | "add_edge" -> Add_edge (istr "src", istr "target", str "param")
    | "delete_node" -> Delete_node (istr "id")
    | "clear_edges" -> Clear_edges (istr "id")
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
    | _ -> failwith "not a valid optype")
  | _ ->
    failwith ("incorrect op structure" ^ (Yojson.Basic.to_string json))


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
  in `Assoc [name, `Assoc args]


let load name : graph =
  let filename = "appdata/" ^ name ^ ".dark" in
  let flags = [Unix.O_RDONLY; Unix.O_CREAT] in
  let file = Unix.openfile filename flags 0o640 in
  let raw = Bytes.create 10000 in
  let count = Unix.read file raw 0 10000 in
  let str = Bytes.sub_string raw 0 count in
  let str = if String.equal str "" then "[]" else str in
  let jsonops = Yojson.Basic.from_string str in
  let ops = match jsonops with
  | `List ops -> List.map json2op ops
  | _ -> failwith "unexpected deserialization" in
  List.fold_left add_op (create name) ops



let save name (g : graph) : unit =
  let ops = List.map op2json g.ops in
  let str = `List ops |> Yojson.Basic.to_string in
  let str = str ^ "\n" in
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  let filename = "appdata/" ^ name ^ ".dark" in
  let file = Unix.openfile filename flags 0o640 in
  let _ = Unix.write file str 0 (String.length str) in
  Unix.close file



(* ------------------------- *)
(* To JSON *)
(* ------------------------- *)
let to_frontend_nodes g : json =
  `Assoc (
    List.map
      (fun n -> (Core.Int.to_string n#id, n#to_frontend))
      (Map.data g.nodes)
  )

let to_frontend_edges g : json =
  let toobj = fun s (t, p) -> `Assoc [ ("source", `String (Core.Int.to_string s))
                                     ; ("target", `String (Core.Int.to_string t))
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
