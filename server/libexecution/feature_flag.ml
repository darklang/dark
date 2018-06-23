open Core_kernel

open Types
open Types.RuntimeT

let session_name = "dark_session"

let make (str : string) : feature_flag =
  RealKey str

(* note, this should be safe to put in a cookie *)
let generate () : feature_flag =
  Util.random_string 42
  |> B64.encode
  |> make

let to_sql (ff: feature_flag) : string =
  ff
  |> feature_flag_to_yojson
  |> Yojson.Safe.to_string

let from_sql (sql: string) : feature_flag =
  sql
  |> Yojson.Safe.from_string
  |> feature_flag_of_yojson
  |> Result.ok_or_failwith

let to_session_string (ff: feature_flag) : string =
  match ff with
  | Analysis -> ""
  | RealKey str -> str

let analysis : feature_flag =
  Analysis

let select (id: id) (setting : int) (l: 'a or_blank) (r: 'a or_blank) ff : 'a or_blank =
  match ff with
  | Analysis ->
    if setting > 50
    then r
    else l
  | RealKey str ->
    let sum = str
              |> Batteries.String.to_list
              |> List.map ~f:Char.to_int
              |> List.reduce ~f:(+)
              |> Option.value ~default:0
              (* We want each feature flag to be different, but keyed
               * off the user. *)
              |> fun x -> x % id
              |> fun x -> x % 100
    in
    if setting < sum
    then l
    else r


