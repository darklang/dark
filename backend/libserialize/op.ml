open Core_kernel
open Libexecution
module SF = Serialization_format

let event_name_of_op (op : Types.op) : string =
  match op with
  | SetHandler _ ->
      "SetHandler"
  | CreateDB _ ->
      "CreateDB"
  | AddDBCol _ ->
      "AddDBCol"
  | SetDBColName _ ->
      "SetDBColName"
  | SetDBColType _ ->
      "SetDBColType"
  | DeleteTL _ ->
      "DeleteTL"
  | MoveTL _ ->
      "MoveTL"
  | SetFunction _ ->
      "SetFunction"
  | ChangeDBColName _ ->
      "ChangeDBColName"
  | ChangeDBColType _ ->
      "ChangeDBColType"
  | UndoTL _ ->
      "UndoTL"
  | RedoTL _ ->
      "RedoTL"
  | DeprecatedInitDbm _ ->
      "DeprecatedInitDbm"
  | SetExpr _ ->
      "SetExpr"
  | TLSavepoint _ ->
      "TLSavepoint"
  | DeleteFunction _ ->
      "DeleteFunction"
  | CreateDBMigration _ ->
      "CreateDBMigration"
  | AddDBColToDBMigration _ ->
      "AddDBColToDBMigration"
  | SetDBColNameInDBMigration _ ->
      "SetDBColNameInDBMigration"
  | SetDBColTypeInDBMigration _ ->
      "SetDBColTypeInDBMigration"
  | AbandonDBMigration _ ->
      "AbandonDBMigration"
  | DeleteColInDBMigration _ ->
      "DeleteColInDBMigration"
  | DeleteDBCol _ ->
      "DeleteDBCol"
  | RenameDBname _ ->
      "RenameDBname"
  | CreateDBWithBlankOr _ ->
      "CreateDBWithBlankOr"
  | DeleteTLForever _ ->
      "DeleteTLForever"
  | DeleteFunctionForever _ ->
      "DeleteFunctionForever"
  | SetType _ ->
      "SetType"
  | DeleteType _ ->
      "DeleteType"
  | DeleteTypeForever _ ->
      "DeleteTypeForever"


(* Note the ord *)
type required_context =
  | NoContext
  | AllDatastores
[@@deriving eq, ord]

(* Returns the 'context', ie. the other stuff on the canvas, that
 * you need to also load in order validate that this op could be added
 * to the oplist/canvas correctly *)
let required_context_to_validate (op : Types.op) : required_context =
  match op with
  | SetHandler _ ->
      NoContext
  | CreateDB _ ->
      NoContext
  | AddDBCol _ ->
      NoContext
  | SetDBColName _ ->
      AllDatastores
  | ChangeDBColName _ ->
      AllDatastores
  | SetDBColType _ ->
      NoContext
  | ChangeDBColType _ ->
      NoContext
  | DeprecatedInitDbm _ ->
      NoContext
  | SetExpr _ ->
      NoContext
  | TLSavepoint _ ->
      NoContext
  | UndoTL _ ->
      (* Can undo/redo ops on dbs *)
      AllDatastores
  | RedoTL _ ->
      (* Can undo/redo ops on dbs *)
      AllDatastores
  | DeleteTL _ ->
      NoContext
  | MoveTL _ ->
      NoContext
  | SetFunction _ ->
      NoContext
  | DeleteFunction _ ->
      NoContext
  | CreateDBMigration _ ->
      NoContext
  | AddDBColToDBMigration _ ->
      NoContext
  | SetDBColNameInDBMigration _ ->
      NoContext
  | SetDBColTypeInDBMigration _ ->
      NoContext
  | AbandonDBMigration _ ->
      NoContext
  | DeleteColInDBMigration _ ->
      NoContext
  | DeleteDBCol _ ->
      NoContext
  | RenameDBname _ ->
      AllDatastores
  | CreateDBWithBlankOr _ ->
      AllDatastores
  | DeleteTLForever _ ->
      NoContext
  | DeleteFunctionForever _ ->
      NoContext
  | SetType _ ->
      NoContext
  | DeleteType _ ->
      NoContext
  | DeleteTypeForever _ ->
      NoContext


let required_context_to_validate_oplist (oplist : Types.oplist) :
    required_context =
  oplist
  |> List.map ~f:required_context_to_validate
  |> List.max_elt ~compare:compare_required_context
  |> Option.value ~default:NoContext


let is_deprecated (op : Types.op) : bool = false

let has_effect (op : Types.op) : bool =
  match op with TLSavepoint _ -> false | _ -> true


let tlidOf (op : Types.op) : Types.tlid =
  match op with
  | SetHandler (tlid, _, _) ->
      tlid
  | CreateDB (tlid, _, _) ->
      tlid
  | AddDBCol (tlid, _, _) ->
      tlid
  | SetDBColName (tlid, _, _) ->
      tlid
  | ChangeDBColName (tlid, _, _) ->
      tlid
  | SetDBColType (tlid, _, _) ->
      tlid
  | ChangeDBColType (tlid, _, _) ->
      tlid
  | DeprecatedInitDbm (tlid, _, _, _, _) ->
      tlid
  | SetExpr (tlid, _, _) ->
      tlid
  | TLSavepoint tlid ->
      tlid
  | UndoTL tlid ->
      tlid
  | RedoTL tlid ->
      tlid
  | DeleteTL tlid ->
      tlid
  | MoveTL (tlid, _) ->
      tlid
  | SetFunction f ->
      f.tlid
  | DeleteFunction tlid ->
      tlid
  | CreateDBMigration (tlid, _, _, _) ->
      tlid
  | AddDBColToDBMigration (tlid, _, _) ->
      tlid
  | SetDBColNameInDBMigration (tlid, _, _) ->
      tlid
  | SetDBColTypeInDBMigration (tlid, _, _) ->
      tlid
  | AbandonDBMigration tlid ->
      tlid
  | DeleteColInDBMigration (tlid, _) ->
      tlid
  | DeleteDBCol (tlid, _) ->
      tlid
  | RenameDBname (tlid, _) ->
      tlid
  | CreateDBWithBlankOr (tlid, _, _, _) ->
      tlid
  | DeleteTLForever tlid ->
      tlid
  | DeleteFunctionForever tlid ->
      tlid
  | SetType ut ->
      ut.tlid
  | DeleteType tlid ->
      tlid
  | DeleteTypeForever tlid ->
      tlid


let oplist2tlid_oplists (oplist : Types.oplist) : Types.tlid_oplists =
  oplist
  |> List.map ~f:tlidOf
  |> List.stable_dedup
  |> List.map ~f:(fun tlid ->
         (tlid, List.filter oplist ~f:(fun op -> tlidOf op = tlid)))


let tlid_oplists2oplist (tos : Types.tlid_oplists) : Types.oplist =
  tos |> List.unzip |> Tuple.T2.get2 |> List.concat


let ast_of (op : Types.op) : Types.fluid_expr option =
  match op with
  | SetFunction {ast; _} | SetExpr (_, _, ast) | SetHandler (_, _, {ast; _}) ->
      Some ast
  | CreateDB (_, _, _)
  | AddDBCol (_, _, _)
  | SetDBColName (_, _, _)
  | SetDBColType (_, _, _)
  | DeleteTL _
  | MoveTL (_, _)
  | TLSavepoint _
  | UndoTL _
  | RedoTL _
  | DeleteFunction _
  | ChangeDBColName (_, _, _)
  | ChangeDBColType (_, _, _)
  | DeprecatedInitDbm (_, _, _, _, _)
  | CreateDBMigration (_, _, _, _)
  | AddDBColToDBMigration (_, _, _)
  | SetDBColNameInDBMigration (_, _, _)
  | SetDBColTypeInDBMigration (_, _, _)
  | DeleteColInDBMigration (_, _)
  | AbandonDBMigration _
  | DeleteDBCol (_, _)
  | RenameDBname (_, _)
  | CreateDBWithBlankOr (_, _, _, _)
  | DeleteTLForever _
  | DeleteFunctionForever _
  | SetType _
  | DeleteType _
  | DeleteTypeForever _ ->
      None


let with_ast (new_ast : Types.fluid_expr) (op : Types.op) =
  match op with
  | Types.SetFunction userfn ->
      Types.SetFunction {userfn with ast = new_ast}
  | SetExpr (tlid, id, _) ->
      SetExpr (tlid, id, new_ast)
  | SetHandler (tlid, id, handler) ->
      SetHandler (tlid, id, {handler with ast = new_ast})
  | CreateDB (_, _, _)
  | AddDBCol (_, _, _)
  | SetDBColName (_, _, _)
  | SetDBColType (_, _, _)
  | DeleteTL _
  | MoveTL (_, _)
  | TLSavepoint _
  | UndoTL _
  | RedoTL _
  | DeleteFunction _
  | ChangeDBColName (_, _, _)
  | ChangeDBColType (_, _, _)
  | DeprecatedInitDbm (_, _, _, _, _)
  | CreateDBMigration (_, _, _, _)
  | AddDBColToDBMigration (_, _, _)
  | SetDBColNameInDBMigration (_, _, _)
  | SetDBColTypeInDBMigration (_, _, _)
  | DeleteColInDBMigration (_, _)
  | AbandonDBMigration _
  | DeleteDBCol (_, _)
  | RenameDBname (_, _)
  | CreateDBWithBlankOr (_, _, _, _)
  | DeleteTLForever _
  | DeleteFunctionForever _
  | SetType _
  | DeleteType _
  | DeleteTypeForever _ ->
      op


(* filter down to only those ops which can be applied out of order
 * without overwriting previous ops' state - eg, if we have
 * SetHandler1 setting a handler's value to "aaa", and then
 * SetHandler2's value is "aa", applying them out of order (SH2,
 * SH1) will result in SH2's update being overwritten *)
(* NOTE: DO NOT UPDATE WITHOUT UPDATING THE CLIENT-SIDE LIST *)
let filter_ops_received_out_of_order (ops : Types.oplist) : Types.oplist =
  ops
  |> List.filter ~f:(fun op ->
         match op with
         | SetHandler _
         | SetFunction _
         | SetType _
         | MoveTL _
         | SetDBColName _
         | ChangeDBColName _
         | ChangeDBColType _
         | SetExpr _
         | CreateDBMigration _
         | SetDBColNameInDBMigration _
         | SetDBColTypeInDBMigration _
         | UndoTL _
         | RedoTL _
         | TLSavepoint _
         | RenameDBname _ ->
             false
         | CreateDB _
         | AddDBCol _
         | SetDBColType _
         | DeleteTL _
         | DeprecatedInitDbm _
         | DeleteFunction _
         | AddDBColToDBMigration _
         | AbandonDBMigration _
         | DeleteColInDBMigration _
         | DeleteDBCol _
         | CreateDBWithBlankOr _
         | DeleteTLForever _
         | DeleteFunctionForever _
         | DeleteType _
         | DeleteTypeForever _ ->
             true)
