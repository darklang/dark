open Core_kernel
open Libexecution

let of_internal_queryable_v0 (str : string) : string =
  let dval = Dval.of_internal_queryable_v0 str in
  dval |> Types.RuntimeT.dval_to_yojson |> Yojson.Safe.to_string


let of_internal_queryable_v0 (str : string) : string =
  let dval = Dval.of_internal_queryable_v0 str in
  dval |> Types.RuntimeT.dval_to_yojson |> Yojson.Safe.to_string


let of_internal_queryable_v1 (str : string) : string =
  let dval = Dval.of_internal_queryable_v1 str in
  dval |> Types.RuntimeT.dval_to_yojson |> Yojson.Safe.to_string


let of_internal_roundtrippable_v0 (str : string) : string =
  let dval = Dval.of_internal_roundtrippable_v0 str in
  dval |> Types.RuntimeT.dval_to_yojson |> Yojson.Safe.to_string


let of_unknown_json_v1 (str : string) : string =
  let dval = Dval.of_unknown_json_v1 str in
  dval |> Types.RuntimeT.dval_to_yojson |> Yojson.Safe.to_string


let to_developer_repr_v0 (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.to_developer_repr_v0 dval


let to_enduser_readable_text_v0 (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.to_enduser_readable_text_v0 dval


let to_hashable_repr (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.fuzzing_to_hashable_repr dval


let to_internal_queryable_v0 (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.to_internal_queryable_v0 dval


let to_internal_queryable_v1 (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  match dval with
  | DObj dvm ->
      Dval.to_internal_queryable_v1 dvm
  | _ ->
      failwith "Incorrect should be DObj"


let to_internal_roundtrippable_v0 (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.to_internal_roundtrippable_v0 dval


let to_pretty_machine_json_v1 (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.to_pretty_machine_json_v1 dval


let to_url_string (json : string) : string =
  let dval =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.to_url_string_exn dval


let hash_v0 (json : string) : string =
  let dvallist =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_list_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.hash 0 dvallist


let hash_v1 (json : string) : string =
  let dvallist =
    json
    |> Yojson.Safe.from_string
    |> Types.RuntimeT.dval_list_of_yojson
    |> Result.ok_or_failwith
  in
  Dval.hash 1 dvallist
