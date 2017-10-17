open Core
open Types

open Op
module G = Graph
module RT = Runtime

(* Opcodes as sent via the API. We do this to get type checking *)
type pos = Types.pos [@@deriving yojson]

(* ---------------- *)
(* opcodes *)
(* ---------------- *)
type delete_node = { id: int } [@@deriving yojson]
type update_node_position = { id: int ; pos: pos} [@@deriving yojson]
type update_node_cursor = { id: int ; cursor: int} [@@deriving yojson]
type add_datastore = { id: int
                     ; name: string
                     ; pos: pos} [@@deriving yojson]
type add_function_call = { id: int
                         ; name: string
                         ; pos: pos
                         } [@@deriving yojson]
type add_anon = { id: int
                ; pos: pos
                ; args: int list
                ; argnames: string list} [@@deriving yojson]
type add_value = { id: int
                 ; value: string
                 ; pos: pos
                 } [@@deriving yojson]
type set_constant = { value: string
                    ; target: int
                    ; param: string
                    } [@@deriving yojson]
type set_edge = { source: int
                ; target: int
                ; param: string
                } [@@deriving yojson]
type delete_all = { fake : int option [@default None]
                  } [@@deriving yojson]
type add_datastore_field = { tipe: string [@key "type"]
                           ; name: string
                           ; id: int
                           } [@@deriving yojson]
type noop = { fake: int option [@default None]
            } [@@deriving yojson]

type redo = { fake: int option [@default None]
            } [@@deriving yojson]

type undo = { fake: int option [@default None]
            } [@@deriving yojson]

type savepoint = { fake: int option [@default None]
                 } [@@deriving yojson]
(* ---------------- *)
(* Read the command out *)
(* ---------------- *)
type opjson =
  { add_value : add_value option [@default None]
  ; set_constant : set_constant option [@default None]
  ; add_datastore: add_datastore option [@default None]
  ; add_function_call: add_function_call option [@default None]
  ; add_anon: add_anon option [@default None]
  ; update_node_position: update_node_position option [@default None]
  ; update_node_cursor: update_node_cursor option [@default None]
  ; set_edge: set_edge option [@default None]
  ; delete_all: delete_all option [@default None]
  ; delete_node: delete_node option [@default None]
  ; add_datastore_field: add_datastore_field option [@default None]
  ; redo: redo option [@default None]
  ; undo: undo option [@default None]
  ; savepoint: savepoint option [@default None]
  ; noop: noop option [@default None]
  } [@@deriving yojson]
type opjsonlist = opjson list [@@deriving yojson]

let json2op (op: opjson) : op =
  match op with
  | { noop = Some _} -> NoOp
  | { redo = Some _} -> Redo
  | { undo = Some _} -> Undo
  | { savepoint = Some _} -> SavePoint
  | { add_datastore = Some a } -> Add_datastore (a.id, a.pos, a.name)
  | { delete_all = Some a } -> Delete_all
  | { delete_node = Some a } -> Delete_node a.id
  | { add_anon = Some a } -> Add_anon (a.id, a.pos, a.args, a.argnames)
  | { add_function_call = Some a } -> Add_fn_call (a.id, a.pos, a.name)
  | { add_value = Some a } -> Add_value (a.id, a.pos, a.value)
  | { update_node_position = Some a } -> Update_node_position (a.id, a.pos)
  | { update_node_cursor = Some a } -> Update_node_cursor(a.id, a.cursor)
  | { set_edge = Some a } -> Set_edge (a.source, a.target, a.param)
  | { set_constant = Some a } -> Set_constant (a.target, a.param, a.value)

  | { add_datastore_field = Some a } ->
    let (list, tipe) =
      match String.split_on_chars a.tipe ~on:['['; ']'] with
      | ["["; s; "]"] -> (true, s)
      | [s] -> (false, s)
      | _ -> failwith "invalid datastore field type"
    in
    Add_datastore_field (a.id, a.name, tipe, list)

  | _ -> failwith "Unexpected opcode"

let to_ops (payload: string) : op list =
  payload
  |> Yojson.Safe.from_string
  |> opjsonlist_of_yojson
  |> Result.ok_or_failwith
  |> List.map ~f:json2op


(*------------------*)
(* Functions *)
(*------------------*)
type param_ = { name: string
              ; tipe: string
              ; anon_args : string list
              ; optional: bool
              ; description: string
              } [@@deriving yojson]

type function_ = { name: string
                 ; parameters : param_ list
                 ; description : string
                 ; return_type : string} [@@deriving yojson]
type functionlist = function_ list [@@deriving yojson]

let functions =
  Libs.fns
  |> String.Map.to_alist
  |> List.map ~f:(fun (k,(v:RT.fn))
                   -> { name = k
                      ; parameters =
                        List.map ~f:(fun p : param_ ->
                          { name = p.name
                          ; tipe = RT.tipe2str p.tipe
                          ; anon_args = p.anon_args
                          ; optional = p.optional
                          ; description = p.description })
                        v.parameters
                      ; description = v.description
                      ; return_type = RT.tipe2str v.return_type
                      })
  |> functionlist_to_yojson
  |> Yojson.Safe.pretty_to_string
