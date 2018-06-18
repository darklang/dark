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


(* Hex converter stolen from PGOcaml, with modifications because our
 * string does not start with "\\x" *)
let is_hex_digit = function '0'..'9' | 'a'..'f' | 'A'..'F' -> true
                                     | _ -> false

let hex_val c =
  let offset = match c with
    | '0'..'9' -> 0x30
    | 'a'..'f' -> 0x57
    | 'A'..'F' -> 0x37
    | _	       -> failwith "hex_val"
  in Char.to_int c - offset

(* Deserialiser for the new 'hex' format introduced in PostgreSQL 9.0. *)
let bytea_of_string_hex str =
  let len = String.length str in
  let buf = Buffer.create ((len)/2) in
  let i = ref 1 in
  while !i < len do
    let hi_nibble = str.[!i-1] in
    let lo_nibble = str.[!i] in
    i := !i+2;
    if is_hex_digit hi_nibble && is_hex_digit lo_nibble
    then begin
      let byte = ((hex_val hi_nibble) lsl 4) + (hex_val lo_nibble) in
      Buffer.add_char buf (Char.of_int_exn byte)
    end
  done;
  Buffer.contents buf

let binary_to_string = bytea_of_string_hex

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

let tlid tlid =
  string_of_int tlid

let id id =
  string_of_int id

let int id =
  string_of_int id

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


