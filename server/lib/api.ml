open Core
open Types

open Op
module G = Graph

(* Opcodes as sent via the API. We do this to get type checking *)
type pos = Types.loc [@@deriving yojson]

(* ---------------- *)
(* opcodes *)
(* ---------------- *)
type delete_node = { id: int } [@@deriving yojson]
type clear_args = { id: int } [@@deriving yojson]
type update_node_position = { id: int ; pos: pos} [@@deriving yojson]
type add_datastore = { name: string ; pos: pos} [@@deriving yojson]
type add_function_call = { name: string
                         ; pos: pos
                         } [@@deriving yojson]
type add_anon = { pos: pos } [@@deriving yojson]
type add_value = { value: string
                 ; pos: pos
                 } [@@deriving yojson]
type set_constant = { value: string
                    ; target: int
                    ; param: string
                    } [@@deriving yojson]
type set_constant_implicit = { value: string
                             ; param: string
                             } [@@deriving yojson]
type set_edge = { source: int
                ; target: int
                ; param: string
                } [@@deriving yojson]
type set_edge_implicit_target = { source: int
                                } [@@deriving yojson]
type set_edge_implicit_source = { target: int
                                ; param: string
                                } [@@deriving yojson]
type delete_arg = { source: int
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
  ; set_constant : set_constant option [@default None]
  ; set_constant_implicit : set_constant option [@default None]
  ; add_datastore: add_datastore option [@default None]
  ; add_function_call: add_function_call option [@default None]
  ; add_anon: add_anon option [@default None]
  ; update_node_position: update_node_position option [@default None]
  ; set_edge: set_edge option [@default None]
  ; set_edge_implicit_source: set_edge_implicit_source option [@default None]
  ; set_edge_implicit_target: set_edge_implicit_target option [@default None]
  ; delete_arg: delete_arg option [@default None]
  ; delete_node: delete_node option [@default None]
  ; clear_args: clear_args option [@default None]
  ; add_datastore_field: add_datastore_field option [@default None]
  ; load_initial_graph: load_initial_graph option [@default None]
  } [@@deriving yojson]
type opjsonlist = opjson list [@@deriving yojson]

let json2op (g: G.graph) (op: opjson) : op list =
  let id = Util.create_id in
  match op with
  | { load_initial_graph = Some _} -> []
  | { add_datastore = Some a } -> [Add_datastore (id (), a.pos, a.name)]
  | { delete_arg = Some a } -> [Delete_arg (a.target, a.param)]
  | { delete_node = Some a } -> [Delete_node a.id]
  | { clear_args = Some a } -> [Clear_args a.id]
  | { add_anon = Some a } -> [Add_anon (id (), a.pos, id (), [id ()])]
  | { add_function_call = Some a } -> [Add_fn_call (id (), a.pos, a.name)]
  | { add_value = Some a } -> [Add_value (id (), a.pos, a.value)]
  | { update_node_position = Some a } -> [Update_node_position (a.id, a.pos)]

  | { set_edge = Some a } -> [Set_edge (a.source, a.target, a.param)]
  | { set_edge_implicit_source = Some a } ->
    [Set_edge (Option.value_exn g.last_node, a.target, a.param)]
  | { set_constant = Some a } ->
    [Set_constant (a.target, a.param, a.value)]
  | { set_constant_implicit = Some a } ->
    [Set_constant (Option.value_exn g.last_node, a.param, a.value)]

  | { set_edge_implicit_target = Some a } ->
    let targetid = Option.value_exn g.last_node in
    let param = (G.get_node g targetid)#parameters |> List.hd_exn in
    [Set_edge (a.source, targetid, param.name)]

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
  |> List.iter ~f:(
    fun op ->
      op
      |> json2op !g
      |> List.iter ~f:(fun op -> G.add_op op g)
    )


(*------------------*)
(* Functions *)
(*------------------*)
type function_ = { name: string
                 ; parameters : Runtime.param list
                 ; description : string
                 ; return_type : string} [@@deriving yojson]
type functionlist = function_ list [@@deriving yojson]

let functions =
  Libs.fns
  |> String.Map.to_alist
  |> List.map ~f:(fun (k,(v:Runtime.fn))
                   -> { name = k
                      ; parameters = v.parameters
                      ; description = v.description
                      ; return_type = v.return_type
                      })
  |> functionlist_to_yojson
  |> Yojson.Safe.to_string
