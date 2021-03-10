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


(* ---------------------- *)
(* Below this is fuzzing execution *)
(* ---------------------- *)
let exec_state : Types.RuntimeT.exec_state =
  { tlid = Types.id_of_int 7
  ; callstack = Tc.StrSet.empty
  ; account_id = Uuidm.v `V4
  ; canvas_id = Uuidm.v `V4
  ; user_fns = []
  ; user_tipes = []
  ; package_fns = []
  ; secrets = []
  ; fail_fn = None
  ; executing_fnname = ""
  ; dbs = []
  ; execution_id = Types.id_of_int 8
  ; trace = (fun ~on_execution_path _ _ -> ())
  ; trace_tlid = (fun _ -> ())
  ; on_execution_path = true
  ; exec = Ast.exec
  ; context = Real
  ; load_fn_result = Execution.load_no_results
  ; store_fn_result = Execution.store_no_results
  ; load_fn_arguments = Execution.load_no_arguments
  ; store_fn_arguments = Execution.store_no_arguments }


type execute_type =
  Types.fluid_expr
  * (string * Types.RuntimeT.dval) list
  * Types.RuntimeT.DbT.db list
  * Types.RuntimeT.user_fn list
[@@deriving yojson]

let execute (json : string) : string =
  Libexecution.Init.init `Inspect `Json [] ;
  let program, args, dbs, user_fns =
    json
    |> Yojson.Safe.from_string
    |> execute_type_of_yojson
    |> Result.ok_or_failwith
  in
  let exec_state = {exec_state with dbs; user_fns} in
  Ast.execute_ast ~input_vars:args ~state:exec_state program
  |> Types.RuntimeT.dval_to_yojson
  |> Yojson.Safe.to_string
