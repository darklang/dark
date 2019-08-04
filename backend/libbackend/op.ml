open Core_kernel
open Libexecution
open Types

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
type op =
  | SetHandler of tlid * pos * RuntimeT.HandlerT.handler
  | CreateDB of tlid * pos * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid
  | MoveTL of tlid * pos
  | SetFunction of RuntimeT.user_fn
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | DeprecatedInitDbm of tlid * id * id * id * RuntimeT.DbT.migration_kind
  | SetExpr of tlid * id * RuntimeT.expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid
  | CreateDBMigration of
      tlid * id * id * (string or_blank * string or_blank) list
  | AddDBColToDBMigration of tlid * id * id
  | SetDBColNameInDBMigration of tlid * id * string
  | SetDBColTypeInDBMigration of tlid * id * string
  | AbandonDBMigration of tlid
  | DeleteColInDBMigration of tlid * id
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * pos * id * string
  | DeleteTLForever of tlid
  | DeleteFunctionForever of tlid
  | SetType of RuntimeT.user_tipe
  | DeleteType of tlid
  | DeleteTypeForever of tlid
[@@deriving eq, yojson, show, bin_io]

(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

type oplist = op list [@@deriving eq, yojson, show, bin_io]

type tlid_oplists = (tlid * oplist) list [@@deriving eq, yojson, show, bin_io]

type expr = RuntimeT.expr

let is_deprecated (op : op) : bool = false

let has_effect (op : op) : bool =
  match op with TLSavepoint _ -> false | _ -> true


let tlidOf (op : op) : tlid option =
  match op with
  | SetHandler (tlid, _, _) ->
      Some tlid
  | CreateDB (tlid, _, _) ->
      Some tlid
  | AddDBCol (tlid, _, _) ->
      Some tlid
  | SetDBColName (tlid, _, _) ->
      Some tlid
  | ChangeDBColName (tlid, _, _) ->
      Some tlid
  | SetDBColType (tlid, _, _) ->
      Some tlid
  | ChangeDBColType (tlid, _, _) ->
      Some tlid
  | DeprecatedInitDbm (tlid, _, _, _, _) ->
      Some tlid
  | SetExpr (tlid, _, _) ->
      Some tlid
  | TLSavepoint tlid ->
      Some tlid
  | UndoTL tlid ->
      Some tlid
  | RedoTL tlid ->
      Some tlid
  | DeleteTL tlid ->
      Some tlid
  | MoveTL (tlid, _) ->
      Some tlid
  | SetFunction f ->
      Some f.tlid
  | DeleteFunction tlid ->
      Some tlid
  | CreateDBMigration (tlid, _, _, _) ->
      Some tlid
  | AddDBColToDBMigration (tlid, _, _) ->
      Some tlid
  | SetDBColNameInDBMigration (tlid, _, _) ->
      Some tlid
  | SetDBColTypeInDBMigration (tlid, _, _) ->
      Some tlid
  | AbandonDBMigration tlid ->
      Some tlid
  | DeleteColInDBMigration (tlid, _) ->
      Some tlid
  | DeleteDBCol (tlid, _) ->
      Some tlid
  | RenameDBname (tlid, _) ->
      Some tlid
  | CreateDBWithBlankOr (tlid, _, _, _) ->
      Some tlid
  | DeleteTLForever tlid ->
      Some tlid
  | DeleteFunctionForever tlid ->
      Some tlid
  | SetType ut ->
      Some ut.tlid
  | DeleteType tlid ->
      Some tlid
  | DeleteTypeForever tlid ->
      Some tlid


let oplist_to_string (ops : op list) : string =
  ops |> Core_extended.Bin_io_utils.to_line bin_oplist |> Bigstring.to_string


let oplist_of_string (str : string) : op list =
  Core_extended.Bin_io_utils.of_line str bin_oplist


let oplist2tlid_oplists (oplist : oplist) : tlid_oplists =
  oplist
  |> List.map ~f:(fun op -> tlidOf op |> Option.value_exn)
  |> List.stable_dedup
  |> List.map ~f:(fun tlid ->
         (tlid, List.filter oplist ~f:(fun op -> tlidOf op = Some tlid)) )


let tlid_oplists2oplist (tos : tlid_oplists) : oplist =
  tos |> List.unzip |> Tuple.T2.get2 |> List.concat


let ast_of (op : op) : Types.RuntimeT.expr option =
  match op with
  | SetFunction {ast} | SetExpr (_, _, ast) | SetHandler (_, _, {ast}) ->
      Some ast
  | _ ->
      None
