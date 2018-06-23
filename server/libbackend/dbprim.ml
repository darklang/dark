open Core_kernel
open Libexecution

module PG = Postgresql
module RTT = Types.RuntimeT

let conn = Dbconnection.conn

let escape_single (s: string) : string =
  conn#escape_string s

let escape_double (s: string) : string =
  Util.string_replace "\"" "\\\"" s

let escapea (s: string) : string =
  conn#escape_bytea s

let binary_to_string = Postgresql.unescape_bytea

(* ------------------------------- *)
(* High-level DB serializers *)
(* ------------------------------- *)
let single_quote v = "'" ^ v ^ "'"
let double_quote v = "\"" ^ v ^ "\""

let uuid uuid =
  uuid
  |> Uuidm.to_string
  |> escape_single
  |> fun u -> "'" ^ u ^ "'::uuid"

let string s =
  s
  |> escape_single
  |> single_quote

let binary s =
  s
  |> escapea
  |> single_quote

let list ~serializer xs =
  xs
  |> List.map ~f:serializer
  |> String.concat ~sep:", "

let host = string

let sql s =
  (* not sure about this but it was working before *)
  Printf.sprintf "'%s'" (escape_single s)

(* ------------------------- *)
(* Dvals *)
(* ------------------------- *)

let cast_type_for (dv : RTT.dval) : string option =
  if Dval.is_json_primitive dv
  then None
  else Some "jsonb"

let cast_expression_for (dv: RTT.dval) : string option =
  dv
  |> cast_type_for
  |> Option.map ~f:(fun exp -> "::" ^ exp)

let dvaljson dv =
  Dval.dval_to_json_string dv
  |> escape_single
  |> single_quote

let dvals (dvals : RTT.dval list) : string =
  dvals
  |> List.map ~f:dvaljson
  |> String.concat ~sep:", "

let dvalmap_jsonb dv =
  Dval.dvalmap_to_string dv
  |> escape_single
  |> single_quote
  |> fun s -> s ^ "::jsonb"

(* ------------------------- *)
(* DB plumbing *)
(* ------------------------- *)
let table name : string =
  name
  |> double_quote


