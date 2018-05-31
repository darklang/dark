open Core
module PG = Postgresql
module RTT = Types.RuntimeT

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

let binary_to_string = bytea_of_string_hex

(* ------------------------------- *)
(* High-level DB serializers *)
(* ------------------------------- *)
let single_quote v = "'" ^ v ^ "'"
let double_quote v = "\"" ^ v ^ "\""

let uuid uuid =
  uuid
  |> Uuid.to_string
  |> escape
  |> fun u -> "'" ^ u ^ "'::uuid"

let string s =
  s
  |> escape
  |> single_quote

let binary s =
  s
  |> escapea
  |> single_quote

let tlid tlid =
  string_of_int tlid

let id id =
  string_of_int id

let host = string

let sql s =
  (* not sure about this but it was working before *)
  Printf.sprintf "'%s'" (escape s)

(* ------------------------- *)
(* Dvals *)
(* ------------------------- *)

let rec sql_tipe_for (tipe: RTT.tipe) : string =
  match tipe with
  | TAny -> failwith "todo sql type"
  | TInt -> "INT"
  | TFloat -> "REAL"
  | TBool -> "BOOLEAN"
  | TNull -> failwith "todo sql type"
  | TChar -> failwith "todo sql type"
  | TStr -> "TEXT"
  | TList -> failwith "todo sql type"
  | TObj -> failwith "todo sql type"
  | TIncomplete -> failwith "todo sql type"
  | TError -> failwith "todo sql type"
  | TBlock -> failwith "todo sql type"
  | TResp -> failwith "todo sql type"
  | TDB -> failwith "todo sql type"
  | TID | TBelongsTo _ -> "UUID"
  | THasMany _ -> "uuid ARRAY"
  | TDate -> "TIMESTAMP WITH TIME ZONE"
  | TTitle -> "TEXT"
  | TUrl -> "TEXT"
  | TDbList t -> (sql_tipe_for t) ^ " ARRAY"

let tipe = sql_tipe_for

let tipe_default (tipe: RTT.tipe) : string =
  match tipe with
  | TAny -> failwith "todo sql type"
  | TInt -> "0"
  | TFloat -> "0.0"
  | TBool -> "FALSE"
  | TNull -> failwith "todo sql type"
  | TChar -> failwith "todo sql type"
  | TStr -> "''"
  | TList -> failwith "todo sql type"
  | TObj -> failwith "todo sql type"
  | TIncomplete -> failwith "todo sql type"
  | TError -> failwith "todo sql type"
  | TBlock -> failwith "todo sql type"
  | TResp -> failwith "todo sql type"
  | TDB -> failwith "todo sql type"
  | TID | TBelongsTo _ -> "'00000000-0000-0000-0000-000000000000'::uuid"
  | THasMany _ -> "'{}'"
  | TDate -> "CURRENT_TIMESTAMP"
  | TTitle -> "''"
  | TUrl -> "''"
  | TDbList _ -> "'{}'"



let rec cast_expression_for (dv : RTT.dval) : string option =
  match dv with
  | DID _ -> Some "uuid"
  | DList l ->
    l
    |> List.filter_map ~f:cast_expression_for
    |> List.hd
    |> Option.map ~f:(fun cast -> cast ^ "[]")
  | _ -> None

let rec dval_to_sql ~quote ~cast (dv: RTT.dval) : string =
  let literal =
    match dv with
    | DInt i -> string_of_int i
    | DID i ->
      quote ^ Uuid.to_string i ^ quote
    | DBool b -> if b then "TRUE" else "FALSE"
    | DChar c -> Char.to_string c
    | DStr s -> quote ^ (escape s) ^ quote
    | DFloat f -> string_of_float f
    | DNull -> "NULL"
    | DDate d ->
      "TIMESTAMP WITH TIME ZONE "
      ^ quote
      ^ Dval.sqlstring_of_date d
      ^ quote
    | DList l ->
      quote
      ^ "{ "
      ^ (String.concat ~sep:", " (List.map ~f:(dval_to_sql ~quote:"\"" ~cast:false) l))
      ^ " }"
      ^ quote
    | _ -> Exception.client ("We don't know how to convert this into the DB: " ^ (Dval.dval_to_json_string dv))
  in
  match cast_expression_for dv with
  | Some e when cast = true -> literal ^ "::" ^ e
  | _ ->  literal

let dval = dval_to_sql ~cast:true ~quote:"'"

let dvals (dvals : RTT.dval list) : string =
  dvals
  |> List.map ~f:dval
  |> String.concat ~sep:", "

let dvaljson dv =
  Dval.dval_to_json_string dv
  |> escape
  |> single_quote


(* ------------------------- *)
(* DB plumbing *)
(* ------------------------- *)
let col (name: string) : string =
  name
  |> escape
  |> double_quote

let cols (names : string list) : string =
  names
  |> List.map ~f:col
  |> String.concat ~sep:", "

let table name : string =
  name
  |> escape
  |> double_quote





