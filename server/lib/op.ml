open Core
open Types

module J = Yojson.Safe.Util

type json = Yojson.Safe.json

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
type op = Add_fn_call of string * id * loc
        | Add_datastore of string * id * loc
        | Add_value of string * id * loc
        (* return, args, id, loc *)
        | Add_anon of id * id list * id * loc
        (* id, name, type, is_list *)
        | Add_datastore_field of id * string * string * bool
        | Update_node_position of id * loc
        | Delete_node of id
        | Set_constant of string * id * string
        | Set_edge of id * id * string
        | Delete_arg of id * string
        | Clear_args of id
[@@deriving eq, yojson, show]

let id_of_option op : id option =
  match op with
  | Add_fn_call (_, id, _) -> Some id
  | Add_datastore (_, id, _) -> Some id
  | Add_value (_, id, _) -> Some id
  | Set_constant (_, id, _) -> Some id
  | Add_anon (_, _, id, _) -> Some id
  | Update_node_position (id, _) -> Some id
  | Clear_args (id) -> Some id
  | Delete_node (id) -> Some id
  | Add_datastore_field _ -> None
  | Set_edge _ -> None
  | Delete_arg _ -> None

let id_of op : id =
  match id_of_option op with
  | Some id -> id
  | _ -> failwith "getting id of op without id"
