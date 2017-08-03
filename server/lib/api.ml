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
type constant = { value: string
                ; param: string
                } [@@deriving yojson]
type implicit_edge = { receiving_edge : receiving_edge option [@default None]
                     ; param_edge : param_edge option [@default None]
                     ; constant : constant option [@default None]
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
type add_constant = { value: string
                    ; target: int
                    ; param: string
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
  ; add_constant : add_constant option [@default None]
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

let convert_edge (id: id) (param: string option) (edge: implicit_edge) : op =
  match edge with
    | { receiving_edge = Some e } -> Add_edge (e.source, id, Option.value_exn param)
    | { param_edge = Some e } -> Add_edge (id, e.target, e.param)
    | { constant = Some e } -> Add_constant (e.value, id, e.param)
    | _ -> failwith "Unexpected edge type"

let json2op (op : opjson) : op list =
  let id = Util.create_id in
  match op with
  | { load_initial_graph = Some _} -> []
  | { add_datastore = Some a } -> [Add_datastore (a.name, id (), a.pos)]
  | { add_anon = Some a } -> [Add_anon (id (), id (), a.pos)]
  | { add_edge = Some a } -> [Add_edge (a.source, a.target, a.param)]
  | { delete_edge = Some a } -> [Delete_edge (a.source, a.target, a.param)]
  | { delete_node = Some a } -> [Delete_node a.id]
  | { clear_edges = Some a } -> [Clear_edges a.id]

  | { add_constant = Some a } ->
    [Add_constant (a.value, a.target, a.param)]

  | { add_function_call = Some a } ->
    let nodeid = id () in
    let fn_node = Add_fn_call (a.name, nodeid, a.pos) in
    (* TODO: not the first parameter, but the next unused parameter. Which we don't actually know at this point... *)
    let name = List.hd_exn (Libs.get_fn_exn a.name).parameters in
    [fn_node] @ (List.map ~f:(convert_edge nodeid (Some name)) a.edges)

  | { add_value = Some a } ->
    let nodeid = id () in
    let node = Add_value (a.value, nodeid, a.pos) in
    [node] @ (List.map ~f:(convert_edge nodeid None) a.edges)

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

let apply_ops (g : G.graph ref) (payload: string) : unit =
  payload
  |> Yojson.Safe.from_string
  |> opjsonlist_of_yojson
  |> Result.ok_or_failwith
  |> List.map ~f:json2op
  |> List.concat
  |> List.iter ~f:(fun op -> G.add_op op g)
