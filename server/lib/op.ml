open Core
open Types

module J = Yojson.Safe.Util

type json = Yojson.Safe.json

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
type op = Add_fn_call of id * pos * string
        | Add_datastore of id * pos * string
        | Add_value of id * pos * string
        (* id, pos, args, argnames *)
        | Add_anon of id * pos * id list * string list
        (* id, name, type, is_list *)
        | Add_datastore_field of id * string * string * bool
        | Update_node_position of id * pos
        | Update_node_cursor of id * int
        | Delete_node of id
        | Set_constant of id * string * string
        | Set_edge of id * id * string
        | Delete_arg of id * string
        | Clear_args of id
        | Noop
[@@deriving eq, yojson, show]

let id_of_option op : id option =
  match op with
  | Add_fn_call (id, _, _) -> Some id
  | Add_datastore (id, _, _) -> Some id
  | Add_value (id, _, _) -> Some id
  | Set_constant (id, _, _) -> Some id
  | Add_anon (id, _, _, _) -> Some id
  | Update_node_position (id, _) -> Some id
  | Update_node_cursor (id, _) -> Some id
  | Clear_args (id) -> Some id
  | Delete_node (id) -> Some id
  | Add_datastore_field _ -> None
  | Set_edge _ -> None
  | Delete_arg _ -> None
  | Noop -> None

let id_of op : id =
  match id_of_option op with
  | Some id -> id
  | _ -> failwith "getting id of op without id"
