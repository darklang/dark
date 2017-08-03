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
        | Add_constant of string * id * param
        (* id in the outer graph, id in the inner graph *)
        | Add_anon of id * id * loc
        (* id, name, type, is_list *)
        | Add_datastore_field of id * string * string * bool
        | Update_node_position of id * loc
        | Delete_node of id
        | Add_edge of id * id * param
        | Delete_edge of id * id * param
        | Clear_edges of id
[@@deriving eq, yojson, show]

let id_of_option op : id option =
  match op with
  | Add_fn_call (_, id, _) -> Some id
  | Add_datastore (_, id, _) -> Some id
  | Add_value (_, id, _) -> Some id
  | Add_constant (_, id, _) -> Some id
  | Add_anon (id, _, _) -> Some id
  | Update_node_position (id, _) -> Some id
  | Clear_edges (id) -> Some id
  | Delete_node (id) -> Some id
  | Add_datastore_field _ -> None
  | Add_edge _ -> None
  | Delete_edge _ -> None

let id_of op : id =
  match id_of_option op with
  | Some id -> id
  | _ -> failwith "getting id of op without id"
