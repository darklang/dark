open Core
module PG = Postgresql

let conn = Dbconnection.conn

let escape (s: string) : string =
  conn#escape_string s

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

