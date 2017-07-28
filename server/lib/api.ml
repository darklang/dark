open Core
open Types

open Op
module G = Graph




(* Opcodes as sent via the API. We do this to get type checking *)
type pos = Types.loc [@@deriving yojson]


(* ---------------- *)
(* Edges, sent as part of add_node opcodes *)
(* ---------------- *)
type receiving_edge = { id: int
                      } [@@deriving yojson]
type param_edge = { id: int
                  ; param: string
                  } [@@deriving yojson]
type edge = { receiving_edge : receiving_edge option [@default None]
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
                         ; edges : edge list
                         } [@@deriving yojson]
type add_value = { value: string
                 ; pos: pos
                 ; edges : edge list
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


let json2op (json : json) : op =
  let id = Util.create_id in
  let api_op : opjson =
    json
    |> opjson_of_yojson
    |> Result.ok_or_failwith in
  match api_op with
  | { add_value = Some a } -> Add_value (a.value, id (), a.pos)
  | { add_datastore = Some a } -> Add_datastore (a.name, id (), a.pos)
  | { add_anon = Some a } -> Add_anon (id (), id (), a.pos)
  | { add_edge = Some a } -> Add_edge (a.source, a.target, a.param)
  | { delete_edge = Some a } -> Delete_edge (a.source, a.target, a.param)
  | { delete_node = Some a } -> Delete_node a.id
  | { clear_edges = Some a } -> Clear_edges a.id

  | { add_function_call = Some a } ->
    Add_fn_call (a.name, id (), a.pos, [])

  | { update_node_position = Some a } ->
    Update_node_position (a.id, a.pos)

  | { add_datastore_field = Some a } ->
    let (list, tipe) =
      match String.split_on_chars a.tipe ~on:['['; ']'] with
      | ["["; s; "]"] -> (true, s)
      | [s] -> (false, s)
      | _ -> failwith "invalid datastore field type"
    in
    Add_datastore_field (a.id, a.name, tipe, list)

  | _ -> failwith "Unexpected opcode"




let backfill_id (head : op) (rest : op list) : op list =
  let id = id_of_option head in
  let rest = match id with
    | None -> rest
    | Some id ->
      List.map ~f:(fun op -> match op with
          | (Add_edge (s, -1, p)) -> Add_edge (s, id, p)
          | op -> op)
        rest
  in
  head :: rest

let apply_ops g payload =
  match payload with
  | `List [`Assoc [("load_initial_graph", `Assoc [])]] -> ()
  | `List (head::rest) ->
    let rest = List.map ~f:json2op rest in
    let ops = backfill_id (json2op head) rest in
    List.iter ~f:(fun op -> G.add_op op g) ops
  | _ -> Exception.raise "Unexpected request structure"
