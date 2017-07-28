open Core
open Types

open Op
module G = Graph




(* Opcodes as sent via the API. We do this to get type checking *)
type pos = Types.loc [@@deriving yojson]


(* ---------------- *)
(* Edges, sent as part of add_node opcodes *)
(* ---------------- *)
type receiving_edge = { source: int
                      } [@@deriving yojson]
type param_edge = { target: int
                  ; param: string
                  } [@@deriving yojson]
type implicit_edge = { receiving_edge : receiving_edge option [@default None]
                     ; param_edge : param_edge option [@default None]
                     } [@@deriving yojson]

(* ---------------- *)
(* opcodes *)
(* ---------------- *)
type add_anon = { pos: pos} [@@deriving yojson]
type delete_node = { id: int } [@@deriving yojson]
type clear_edges = { id: int } [@@deriving yojson]
type update_node_position = { id: int ; pos: pos} [@@deriving yojson]
type add_datastore = { name: string ; pos: pos} [@@deriving yojson]
type add_function_call = { name: string
                         ; pos: pos
                         ; edges : implicit_edge list
                         } [@@deriving yojson]
type add_value = { value: string
                 ; pos: pos
                 ; edges : implicit_edge list
                 } [@@deriving yojson]
type add_edge = { source: int
                ; target: int
                ; param: string
                } [@@deriving yojson]
type delete_edge = { source: int
                   ; target: int
                   ; param: string
                   } [@@deriving yojson]
type add_datastore_field = { tipe: string [@key "type"]
                           ; name: string
                           ; id: int
                           } [@@deriving yojson]
type load_initial_graph = { fake: int option [@default None]
                          } [@@deriving yojson]

(* ---------------- *)
(* Read the command out *)
(* ---------------- *)
type opjson =
  { add_value : add_value option [@default None]
  ; add_datastore: add_datastore option [@default None]
  ; add_function_call: add_function_call option [@default None]
  ; add_anon: add_anon option [@default None]
  ; update_node_position: update_node_position option [@default None]
  ; add_edge: add_edge option [@default None]
  ; delete_edge: delete_edge option [@default None]
  ; delete_node: delete_node option [@default None]
  ; clear_edges: clear_edges option [@default None]
  ; add_datastore_field: add_datastore_field option [@default None]
  ; load_initial_graph: load_initial_graph option [@default None]
  } [@@deriving yojson]
type opjsonlist = opjson list [@@deriving yojson]



let json2op (op : opjson) : op list =
  let id = Util.create_id in
  match op with
  | { load_initial_graph = Some _} -> []
  | { add_value = Some a } -> [Add_value (a.value, id (), a.pos)]
  | { add_datastore = Some a } -> [Add_datastore (a.name, id (), a.pos)]
  | { add_anon = Some a } -> [Add_anon (id (), id (), a.pos)]
  | { add_edge = Some a } -> [Add_edge (a.source, a.target, a.param)]
  | { delete_edge = Some a } -> [Delete_edge (a.source, a.target, a.param)]
  | { delete_node = Some a } -> [Delete_node a.id]
  | { clear_edges = Some a } -> [Clear_edges a.id]

  | { add_function_call = Some a } ->
    let nodeid = id () in
    let fn_node = Add_fn_call (a.name, nodeid, a.pos) in
    let implicit = List.hd_exn (a.edges) in
    let edge = match implicit with

      | { receiving_edge = Some e } ->
        Add_edge ( e.source
                 , nodeid
                 , List.hd_exn (Lib.get_fn_exn a.name).parameters)

      | { param_edge = Some e } ->
        Add_edge (nodeid, e.target, e.param)
    in
    [ fn_node; edge]


  | { update_node_position = Some a } ->
    [Update_node_position (a.id, a.pos)]

  | { add_datastore_field = Some a } ->
    let (list, tipe) =
      match String.split_on_chars a.tipe ~on:['['; ']'] with
      | ["["; s; "]"] -> (true, s)
      | [s] -> (false, s)
      | _ -> failwith "invalid datastore field type"
    in
    [Add_datastore_field (a.id, a.name, tipe, list)]

  | _ -> failwith "Unexpected opcode"

let apply_ops g payload =
  payload
  |> opjsonlist_of_yojson
  |> Result.ok_or_failwith
  |> List.map ~f:(fun op -> json2op op)
  |> List.concat
  |> List.iter ~f:(fun op -> G.add_op op g)
