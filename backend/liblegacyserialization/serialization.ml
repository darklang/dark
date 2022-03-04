open Core_kernel
open Libexecution
module BS = Libserialize.Binary_serialization

(* ----------------------- *)
(* Convert binary to JSON for F# *)
(* ----------------------- *)
let bin2json
    (bin_parser : string -> 'a)
    (to_json_string : 'a -> Yojson.Safe.t)
    (str : string) : string =
  str |> bin_parser |> to_json_string |> Yojson.Safe.to_string


let user_fn_bin2json (str : string) : string =
  bin2json BS.user_fn_of_binary_string Types.RuntimeT.user_fn_to_yojson str


let user_tipe_bin2json (str : string) : string =
  bin2json BS.user_tipe_of_binary_string Types.RuntimeT.user_tipe_to_yojson str


let handler_bin2json (str : string) : string =
  bin2json
    BS.handler_of_binary_string
    Types.RuntimeT.HandlerT.handler_to_yojson
    str


let db_bin2json (str : string) : string =
  bin2json BS.db_of_binary_string Types.RuntimeT.DbT.db_to_yojson str


let oplist_bin2json (str : string) : string =
  bin2json BS.oplist_of_binary_string Types.oplist_to_yojson str


let pos_bin2json (str : string) : string =
  bin2json BS.pos_of_binary_string Types.pos_to_yojson str


let expr_bin2json (str : string) : string =
  bin2json BS.expr_of_binary_string Types.fluid_expr_to_yojson str


let expr_tlid_pair_bin2json (str : string) : string =
  bin2json BS.expr_tlid_pair_of_binary_string BS.expr_tlid_pair_to_yojson str



(* ----------------------- *)
(* Convert JSON to binary for F# *)
(* ----------------------- *)
let json2bin
    (json_parser : Yojson.Safe.t -> ('a, string) Result.t)
    (bin_to_string : 'a -> string)
    (str : string) : string =
  str
  |> Yojson.Safe.from_string
  |> json_parser
  |> Result.ok_or_failwith
  |> bin_to_string


let user_fn_json2bin (str : string) : string =
  json2bin Types.RuntimeT.user_fn_of_yojson BS.user_fn_to_binary_string str


let user_tipe_json2bin (str : string) : string =
  json2bin Types.RuntimeT.user_tipe_of_yojson BS.user_tipe_to_binary_string str


let handler_json2bin (str : string) : string =
  json2bin
    Types.RuntimeT.HandlerT.handler_of_yojson
    BS.handler_to_binary_string
    str


let db_json2bin (str : string) : string =
  json2bin Types.RuntimeT.DbT.db_of_yojson BS.db_to_binary_string str


let oplist_json2bin (str : string) : string =
  json2bin Types.oplist_of_yojson BS.oplist_to_binary_string str


let pos_json2bin (str : string) : string =
  json2bin Types.pos_of_yojson BS.pos_to_binary_string str


let expr_json2bin (str : string) : string =
  json2bin Types.fluid_expr_of_yojson BS.expr_to_binary_string str


let expr_tlid_pair_json2bin (str : string) : string =
  json2bin BS.expr_tlid_pair_of_yojson BS.expr_tlid_pair_to_binary_string str
