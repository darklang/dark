open Core_kernel
open Libcommon
open Types
open Types.RuntimeT

module Error = struct
  type t =
    | TypeLookupFailure of string * int
    | TypeUnificationFailure of {expected_tipe : tipe; actual_value : dval}
    | MismatchedRecordFields of
        { expected_fields : String.Set.t
        ; actual_fields : String.Set.t }

  let to_string t =
    match t with
    | TypeLookupFailure (lookup_name, lookup_version) ->
        let lookup_string =
          "(" ^ lookup_name ^ ", v" ^ string_of_int lookup_version ^ ")"
        in
        "Type " ^ lookup_string ^ " could not be found on the canvas"
    | TypeUnificationFailure {expected_tipe; actual_value} ->
        "Expected to see a value of type "
        ^ Dval.tipe_to_string expected_tipe
        ^ " but found a "
        ^ Dval.tipename actual_value
    | MismatchedRecordFields {expected_fields; actual_fields} ->
        (* More or less wholesale from User_db's type checker *)
        let missing_fields = String.Set.diff expected_fields actual_fields in
        let missing_msg =
          "Expected but did not find: ["
          ^ (missing_fields |> String.Set.to_list |> String.concat ~sep:", ")
          ^ "]"
        in
        let extra_fields = String.Set.diff actual_fields expected_fields in
        let extra_msg =
          "Found but did not expect: ["
          ^ (extra_fields |> String.Set.to_list |> String.concat ~sep:", ")
          ^ "]"
        in
        ( match
            ( String.Set.is_empty missing_fields
            , String.Set.is_empty extra_fields )
          with
        | false, false ->
            missing_msg ^ " & " ^ extra_msg
        | false, true ->
            missing_msg
        | true, false ->
            extra_msg
        | true, true ->
            "Type checker error! Deduced expected fields from type and actual fields in value did not match, but could not find any examples!"
        )
end

open Error

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


let error err = Error [err]

let rec unify ~(type_env : type_env) (expected : tipe) (value : dval) :
    (unit, Error.t list) Result.t =
  match (expected, value) with
  (* Any should be removed, but we currently allow it as a param tipe
   * in user functions, so we should allow it here.
   *
   * Potentially needs to be removed before we use this type checker for DBs?
   *   - Could always have a type checking context that allows/disallows any *)
  | TAny, _ ->
      Ok ()
  | TInt, DInt _ ->
      Ok ()
  | TFloat, DFloat _ ->
      Ok ()
  | TBool, DBool _ ->
      Ok ()
  | TNull, DNull ->
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
        error (TypeLookupFailure (expected_name, expected_version))
    | Some ut ->
      ( match ut.definition with UTRecord utd ->
          unify_user_record_with_dval_map ~type_env utd dmap ) )
  | expected_tipe, actual_value ->
      error (TypeUnificationFailure {expected_tipe; actual_value})


and unify_user_record_with_dval_map
    ~(type_env : type_env)
    (definition : user_record_field list)
    (value : dval_map) : (unit, Error.t list) Result.t =
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
  else
    error
      (MismatchedRecordFields
         {expected_fields = definition_names; actual_fields = obj_names})


let check_function_call
    ~(user_tipes : user_tipe list) (fn : fn) (args : dval_map) :
    (unit, Error.t list) Result.t =
  let type_env = user_tipe_list_to_type_env user_tipes in
  let args = DvalMap.to_alist args in
  let with_params =
    List.map
      ~f:(fun (argname, argval) ->
        (List.find_exn ~f:(fun p -> p.name = argname) fn.parameters, argval) )
      args
  in
  with_params
  |> List.map ~f:(fun (param, value) -> unify ~type_env param.tipe value)
  |> Result.combine_errors_unit
  |> Result.map_error ~f:List.concat
