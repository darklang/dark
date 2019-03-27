open Core_kernel
open Libcommon
open Types
open Types.RuntimeT

type type_error =
  { expected : tipe
  ; actual : tipe }

module TypeEnv = Map.Make (struct
  type t = string * int [@@deriving sexp, compare]
end)

type type_env = user_tipe TypeEnv.t

(* This converts our list of user_tipes to a (name, version) -> user_tipe lookup
 * table. This corresponds to our lookup key in a TUserType of string * int variant
 * of a tipe *)
let user_tipe_list_to_type_env (tipes : user_tipe list) : type_env =
  List.fold_left tipes ~init:TypeEnv.empty ~f:(fun map t ->
      match t.name with
      | Filled (_, name) ->
          TypeEnv.add_exn map ~key:(name, t.version) ~data:t
      | Blank _ ->
          map )


let rec unify ~(type_env : type_env) (expected : tipe) (value : dval) :
    (unit, type_error list) Result.t =
  match (expected, value) with
  | TInt, DInt _ ->
      Ok ()
  | TFloat, DFloat _ ->
      Ok ()
  | TBool, DBool _ ->
      Ok ()
  | TNull, DNull ->
      Ok ()
  | TChar, DChar _ ->
      Ok ()
  | TStr, DStr _ ->
      Ok ()
  | TList, DList _ ->
      Ok ()
  | TDate, DDate _ ->
      Ok ()
  | TObj, DObj _ ->
      Ok ()
  | TBlock, DBlock _ ->
      Ok ()
  | TPassword, DPassword _ ->
      Ok ()
  | TUuid, DUuid _ ->
      Ok ()
  | TOption, DOption _ ->
      Ok ()
  | TResult, DResult _ ->
      Ok ()
  | TCharacter, DCharacter _ ->
      Ok ()
  | TDB, DDB _ ->
      Ok ()
  | TResp, DResp _ ->
      Ok ()
  | TUserType (expected_name, expected_version), DObj dmap ->
    ( match TypeEnv.find type_env (expected_name, expected_version) with
    | None ->
        failwith "could not find user tipe in env"
    | Some ut ->
      ( match ut.definition with UTRecord utd ->
          unify_user_record_with_dval_map ~type_env utd dmap ) )
  | _ ->
      failwith "unexpected (tipe, value) pair"


and unify_user_record_with_dval_map
    ~(type_env : type_env)
    (definition : user_record_field list)
    (value : dval_map) : (unit, type_error list) Result.t =
  let complete_definition =
    definition
    |> List.filter_map ~f:(fun d ->
           match (d.name, d.tipe) with
           | Filled (_, n), Filled (_, t) ->
               Some (n, t)
           | _ ->
               None )
    |> TipeMap.of_alist_exn
  in
  let definition_names =
    complete_definition |> TipeMap.keys |> String.Set.of_list
  in
  let obj_names = value |> DvalMap.keys |> String.Set.of_list in
  let same_names = String.Set.equal definition_names obj_names in
  if same_names
  then
    value
    |> DvalMap.to_alist
    |> List.map ~f:(fun (key, data) ->
           unify ~type_env (TipeMap.find_exn complete_definition key) data )
    |> Result.combine_errors_unit
    |> Result.map_error ~f:List.concat
  else failwith "diff names"


let check_function_call
    ~(user_tipes : user_tipe list) (fn : fn) (args : dval_map) :
    (unit, type_error list) Result.t =
  let type_env = user_tipe_list_to_type_env user_tipes in
  let args = DvalMap.to_alist args in
  let withParams =
    List.map
      ~f:(fun (argname, argval) ->
        (List.find_exn ~f:(fun p -> p.name = argname) fn.parameters, argval) )
      args
  in
  withParams
  |> List.map ~f:(fun (param, value) -> unify ~type_env param.tipe value)
  |> Result.combine_errors_unit
  |> Result.map_error ~f:List.concat
