open Core
open Util
open Types

module RT = Runtime

module NodeMap = Int.Map
type nodemap = Node.node NodeMap.t [@@deriving eq]

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
             } [@@deriving eq, show]


let get_node (g: graph) (id: id) : Node.node =
  NodeMap.find_exn g.nodes id

let has_node (id: id) (g: graph) : bool =
  NodeMap.mem g.nodes id

let create (name : string) : graph ref =
  ref { name = name
      ; ops = []
      ; nodes = NodeMap.empty
      }

let outgoing_nodes id g : (id * string) list =
  g.nodes
  |> NodeMap.data
  |> List.map
    ~f:(fun n ->
        Map.filter_mapi n#arguments
          ~f:(fun ~key ~data -> match data with | RT.AConst _ -> None
                                                | RT.AEdge i ->
                                                  if i = id
                                                  then Some (n#id,key)
                                                  else None))
  |> List.map ~f:String.Map.data
  |> List.concat



let get_children (g: graph) (id: id) : Node.node list =
  outgoing_nodes id g |> List.map ~f:Tuple.T2.get1 |> List.map ~f:(get_node g)

let get_parents (g: graph) (id: id) : Node.node list =
  (get_node g id)#arguments
  |> Map.data
  |> List.filter_map ~f:(fun arg -> match arg with | RT.AEdge id -> Some id
                                                   | _ -> None)
  |> List.map ~f:(get_node g)
 
let rec get_deepest ?(depth:int=0) (g: graph) (id: id) : (int * Node.node) list =
  let cs = get_children g id in
  if cs = []
  then [depth+1, get_node g id]
  else cs
       |> List.map ~f:(fun n -> get_deepest ~depth:(depth+1) g n#id)
       |> List.concat

let gfns (g: graph) : Node.gfns =
  { getf = get_node g
  ; get_children = get_children g
  ; get_deepest = get_deepest g
  }


(* ------------------------- *)
(* Updating *)
(* ------------------------- *)
let change_node (id: id) (g: graph) ~(f: (Node.node option -> Node.node option)) : graph =
  { g with nodes = NodeMap.change g.nodes id ~f }

let update_node (id: id) (g : graph) ~(f: (Node.node -> Node.node option)) : graph =
  change_node
    id
    g
    (function Some node -> f node
            | None -> Exception.client "can't update missing node")

let update_node_position (id: id) (pos: pos) (g: graph) : graph =
  update_node id g ~f:(fun n -> n#update_pos pos; Some n)

let update_node_cursor (id: id) (cursor: int) (g: graph) : graph =
  update_node id g ~f:(fun n -> n#update_cursor cursor; Some n)

let set_arg (a: RT.argument) (t: id) (param: string) (g: graph) : graph =
  update_node t g
    ~f:(fun n ->
        if not (n#has_parameter param)
        then Exception.client ("Node " ^ n#name ^ " has no parameter " ^ param);
        n#set_arg param a;
        Some n)

let set_const (t: id) (param: string) (v : string) (g: graph) : graph =
  set_arg (RT.AConst (RT.parse v)) t param g

let set_edge (s : id) (t : id) (param: string) (g: graph) : graph =
  set_arg (RT.AEdge s) t param g

let delete_arg (t: id) (param:string) (g: graph) : graph =
  update_node t g ~f:(fun n -> n#delete_arg param; Some n)

let delete_all (g: graph) : graph =
  { g with nodes = NodeMap.empty }

let add_node (node : Node.node) (g : graph) : graph =
  if has_node node#id g then
    Exception.client "A node with this ID already exists!";
  change_node node#id g ~f:(fun x -> Some node)

let rec delete_node id (g: graph) : graph =
  let node = get_node g id in
  let deps = node#dependent_nodes (gfns g) in
  let nodes = outgoing_nodes id g in
  let g = List.fold_left ~init:g nodes
      ~f:(fun g_ (id2, param) -> delete_arg id2 param g_) in
  let g = update_node id ~f:(fun x -> None) g in
  let g = List.fold_left ~init:g deps
      ~f:(fun g_ d ->
          if has_node d g_
           then delete_node d g_
           else g_) in
  g

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
let apply_op (op : Op.op) (g : graph ref) : unit =
  g :=
    !g |>
    match op with
    | Add_fn_call (id, pos, name) ->
      add_node (new Node.func id pos name)
    | Add_datastore (id, pos, table) ->
      add_node (new Node.datastore id pos table)
    | Add_value (id, pos, expr) ->
      add_node (new Node.value id pos expr)
    | Add_anon (nid, pos, argids, anon_names) ->
      (fun g ->
         argids
         |> List.zip_exn anon_names
         |> List.mapi ~f:(fun i (argname, argid) -> new Node.argnode
                          argid
                          Dependent
                          argname
                          i
                          nid
                          argids)
         |> List.append [ new Node.anonfn nid
                          NoPos
                          argids ]
         |> List.fold_left ~init:g ~f:(fun g n -> add_node n g))
    | Update_node_position (id, pos) -> update_node_position id pos
    | Update_node_cursor (id, cursor) -> update_node_cursor id cursor
    | Set_constant (target, param, value) ->
      set_const target param value
    | Set_edge (src, target, param) -> set_edge src target param
    | Delete_node (id) -> delete_node id
    | Delete_all -> delete_all
    | NoOp -> ident
    | SavePoint -> ident
    | _ ->
      Exception.internal ("applying unimplemented op: " ^ Op.show_op op)

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


(* ------------------------- *)
(* Build *)
(* ------------------------- *)

let add_ops (g: graph ref) (ops: Op.op list) : unit =
  let reduced_ops = preprocess ops in
  List.iter ~f:(fun op -> apply_op op g) reduced_ops;
  g := { !g with ops = ops }


let rec count_subgraph_positions g (traversed:Int_set.t ref) (n:Node.node) : (int * int * int * int * int) = 
  if Int_set.mem !traversed n#id
  then (0,0,0,0,0)
  else
    (traversed := Int_set.add !traversed n#id;
    let sum = List.fold_left ~init:(0,0,0,0,0)
       ~f:(fun (cs1, cs2, cs3, cs4, cs5) (c1, c2, c3, c4, c5) ->
             (c1+cs1, c2+cs2, c3+cs3, c4+cs4, c5+cs5)) in
    let parent_counts = n#id |> get_parents g |> List.map ~f:(count_subgraph_positions g traversed) in
    let child_counts = n#id |> get_children g |> List.map ~f:(count_subgraph_positions g traversed) in 
    let mine = match n#pos with
    | Root _ -> if get_parents g n#id |> List.length |> (=) 0
                then (1,0,0,0,0)
                else (0,1,0,0,0)
    | Free -> (0,0,1,0,0)
    | Dependent -> (0,0,0,1,0)
    | NoPos -> (0,0,0,0,1) in
     sum (mine :: (List.append parent_counts child_counts))
    )

let verify (g: graph) : unit =
  (* if all the nodes in a subgraph are dependent/noPos, then that isn't a valid state. *)
  (* For each node, get all of its parents and children, transitively. Mark them done. Check the subgraph has exactly one root/free. *)
  let traversed = ref (Int_set.empty) in
  NodeMap.iter ~f:(fun n ->
    if not (Int_set.mem !traversed n#id)
    then
      let (real, fake, free, dep, none) = count_subgraph_positions g traversed n in
      if real + free <> 1
      then Exception.user
             ("Nodes have the wrong counts - "
             ^ "real root: " ^ (string_of_int real)
             ^ "fake root: " ^ (string_of_int fake)
             ^ "free: " ^ (string_of_int free)
             ^ "dep: " ^ (string_of_int dep)
             ^ "none: " ^ (string_of_int none))
      else ()
     ) g.nodes 


(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let filename_for name = "appdata/" ^ name ^ ".dark"

let load (name: string) (ops: Op.op list) : graph ref =
  let g = create name in
  name
  |> filename_for
  |> Util.readfile ~default:"[]"
  |> Yojson.Safe.from_string
  |> oplist_of_yojson
  |> Result.ok_or_failwith
  |> (fun os -> os @ ops)
  |> add_ops g;
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
let node_value (n: Node.node) (g: graph) : (string * string * string * Exception.exception_data option) =
  try
    Log.pP ~name:"execution" ~ind:0 ("\n\n*************node_value************") n#debug_name;
    let dv = Node.execute ~ind:0 ~scope:RT.Scope.empty n#id (gfns g) in
    ( RT.to_repr dv
    , RT.tipename dv
    , dv |> RT.dval_to_yojson |> Yojson.Safe.pretty_to_string
    , None)
  with
  | Exception.DarkException e -> ( "use exc field"
                                 , "Error"
                                 , "intentionally invalid json"
                                 , Some e)

let to_frontend_nodes (g: graph) : Yojson.Safe.json =
  g.nodes
  |> NodeMap.data
  |> List.map ~f:(fun n -> n#to_frontend (node_value n g))
  |> Node.nodejsonlist_to_yojson

let to_frontend (g : graph) : Yojson.Safe.json =
  `Assoc [ ("nodes", to_frontend_nodes g)
         ; ("redoable", `Bool (is_redoable g))
         ; ("undo_count", `Int (undo_count g))
         ; ("undoable", `Bool (is_undoable g)) ]

let to_frontend_string (g: graph) : string =
  g |> to_frontend |> Yojson.Safe.pretty_to_string ~std:true
