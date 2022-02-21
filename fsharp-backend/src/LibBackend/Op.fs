/// Helper functions related to Ops
module LibBackend.Op

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT


let eventNameOfOp (op : PT.Op) : string =
  match op with
  | PT.SetHandler _ -> "SetHandler"
  | PT.CreateDB _ -> "CreateDB"
  | PT.AddDBCol _ -> "AddDBCol"
  | PT.SetDBColName _ -> "SetDBColName"
  | PT.SetDBColType _ -> "SetDBColType"
  | PT.DeleteTL _ -> "DeleteTL"
  | PT.MoveTL _ -> "MoveTL"
  | PT.SetFunction _ -> "SetFunction"
  | PT.ChangeDBColName _ -> "ChangeDBColName"
  | PT.ChangeDBColType _ -> "ChangeDBColType"
  | PT.UndoTL _ -> "UndoTL"
  | PT.RedoTL _ -> "RedoTL"
  | PT.DeprecatedInitDBm _ -> "DeprecatedInitDbm"
  | PT.SetExpr _ -> "SetExpr"
  | PT.TLSavepoint _ -> "TLSavepoint"
  | PT.DeleteFunction _ -> "DeleteFunction"
  | PT.CreateDBMigration _ -> "CreateDBMigration"
  | PT.AddDBColToDBMigration _ -> "AddDBColToDBMigration"
  | PT.SetDBColNameInDBMigration _ -> "SetDBColNameInDBMigration"
  | PT.SetDBColTypeInDBMigration _ -> "SetDBColTypeInDBMigration"
  | PT.AbandonDBMigration _ -> "AbandonDBMigration"
  | PT.DeleteColInDBMigration _ -> "DeleteColInDBMigration"
  | PT.DeleteDBCol _ -> "DeleteDBCol"
  | PT.RenameDBname _ -> "RenameDBname"
  | PT.CreateDBWithBlankOr _ -> "CreateDBWithBlankOr"
  | PT.DeleteTLForever _ -> "DeleteTLForever"
  | PT.DeleteFunctionForever _ -> "DeleteFunctionForever"
  | PT.SetType _ -> "SetType"
  | PT.DeleteType _ -> "DeleteType"
  | PT.DeleteTypeForever _ -> "DeleteTypeForever"

type RequiredContext =
  | NoContext
  | AllDatastores

// Returns the 'context', ie. the other stuff on the canvas, that
// you need to also load in order validate that this op could be added
// to the oplist/canvas correctly
let requiredContextToValidate (op : PT.Op) : RequiredContext =
  match op with
  | PT.SetHandler _ -> NoContext
  | PT.CreateDB _ -> NoContext
  | PT.AddDBCol _ -> NoContext
  | PT.SetDBColName _ -> AllDatastores
  | PT.ChangeDBColName _ -> AllDatastores
  | PT.SetDBColType _ -> NoContext
  | PT.ChangeDBColType _ -> NoContext
  | PT.DeprecatedInitDBm _ -> NoContext
  | PT.SetExpr _ -> NoContext
  | PT.TLSavepoint _ -> NoContext
  | PT.UndoTL _ ->
    (* Can undo/redo ops on dbs *)
    AllDatastores
  | PT.RedoTL _ ->
    (* Can undo/redo ops on dbs *)
    AllDatastores
  | PT.DeleteTL _ -> NoContext
  | PT.MoveTL _ -> NoContext
  | PT.SetFunction _ -> NoContext
  | PT.DeleteFunction _ -> NoContext
  | PT.CreateDBMigration _ -> NoContext
  | PT.AddDBColToDBMigration _ -> NoContext
  | PT.SetDBColNameInDBMigration _ -> NoContext
  | PT.SetDBColTypeInDBMigration _ -> NoContext
  | PT.AbandonDBMigration _ -> NoContext
  | PT.DeleteColInDBMigration _ -> NoContext
  | PT.DeleteDBCol _ -> NoContext
  | PT.RenameDBname _ -> AllDatastores
  | PT.CreateDBWithBlankOr _ -> AllDatastores
  | PT.DeleteTLForever _ -> NoContext
  | PT.DeleteFunctionForever _ -> NoContext
  | PT.SetType _ -> NoContext
  | PT.DeleteType _ -> NoContext
  | PT.DeleteTypeForever _ -> NoContext


let requiredContextToValidateOplist (oplist : PT.Oplist) : RequiredContext =
  if List.isEmpty oplist then
    NoContext
  else
    oplist
    |> List.map requiredContextToValidate
    |> List.maxBy (function
      | AllDatastores -> 1
      | NoContext -> 0)


let isDeprecated (op : PT.Op) : bool = false

let hasEffect (op : PT.Op) : bool =
  match op with
  | PT.TLSavepoint _ -> false
  | _ -> true


let tlidOf (op : PT.Op) : tlid =
  match op with
  | PT.SetHandler (tlid, _, _) -> tlid
  | PT.CreateDB (tlid, _, _) -> tlid
  | PT.AddDBCol (tlid, _, _) -> tlid
  | PT.SetDBColName (tlid, _, _) -> tlid
  | PT.ChangeDBColName (tlid, _, _) -> tlid
  | PT.SetDBColType (tlid, _, _) -> tlid
  | PT.ChangeDBColType (tlid, _, _) -> tlid
  | PT.DeprecatedInitDBm (tlid, _, _, _, _) -> tlid
  | PT.SetExpr (tlid, _, _) -> tlid
  | PT.TLSavepoint tlid -> tlid
  | PT.UndoTL tlid -> tlid
  | PT.RedoTL tlid -> tlid
  | PT.DeleteTL tlid -> tlid
  | PT.MoveTL (tlid, _) -> tlid
  | PT.SetFunction f -> f.tlid
  | PT.DeleteFunction tlid -> tlid
  | PT.CreateDBMigration (tlid, _, _, _) -> tlid
  | PT.AddDBColToDBMigration (tlid, _, _) -> tlid
  | PT.SetDBColNameInDBMigration (tlid, _, _) -> tlid
  | PT.SetDBColTypeInDBMigration (tlid, _, _) -> tlid
  | PT.AbandonDBMigration tlid -> tlid
  | PT.DeleteColInDBMigration (tlid, _) -> tlid
  | PT.DeleteDBCol (tlid, _) -> tlid
  | PT.RenameDBname (tlid, _) -> tlid
  | PT.CreateDBWithBlankOr (tlid, _, _, _) -> tlid
  | PT.DeleteTLForever tlid -> tlid
  | PT.DeleteFunctionForever tlid -> tlid
  | PT.SetType ut -> ut.tlid
  | PT.DeleteType tlid -> tlid
  | PT.DeleteTypeForever tlid -> tlid


let oplist2TLIDOplists (oplist : PT.Oplist) : PT.TLIDOplists =
  oplist |> List.groupBy tlidOf |> Map.toList

let tlidOplists2oplist (tos : PT.TLIDOplists) : PT.Oplist =
  tos |> List.unzip |> Tuple2.second |> List.concat


let astOf (op : PT.Op) : PT.Expr option =
  match op with
  | PT.SetFunction f -> Some f.body
  | PT.SetExpr (_, _, ast) -> Some ast
  | PT.SetHandler (_, _, h) -> Some h.ast
  | PT.CreateDB (_, _, _)
  | PT.AddDBCol (_, _, _)
  | PT.SetDBColName (_, _, _)
  | PT.SetDBColType (_, _, _)
  | PT.DeleteTL _
  | PT.MoveTL (_, _)
  | PT.TLSavepoint _
  | PT.UndoTL _
  | PT.RedoTL _
  | PT.DeleteFunction _
  | PT.ChangeDBColName (_, _, _)
  | PT.ChangeDBColType (_, _, _)
  | PT.DeprecatedInitDBm (_, _, _, _, _)
  | PT.CreateDBMigration (_, _, _, _)
  | PT.AddDBColToDBMigration (_, _, _)
  | PT.SetDBColNameInDBMigration (_, _, _)
  | PT.SetDBColTypeInDBMigration (_, _, _)
  | PT.DeleteColInDBMigration (_, _)
  | PT.AbandonDBMigration _
  | PT.DeleteDBCol (_, _)
  | PT.RenameDBname (_, _)
  | PT.CreateDBWithBlankOr (_, _, _, _)
  | PT.DeleteTLForever _
  | PT.DeleteFunctionForever _
  | PT.SetType _
  | PT.DeleteType _
  | PT.DeleteTypeForever _ -> None


let withAST (newAST : PT.Expr) (op : PT.Op) =
  match op with
  | PT.SetFunction userfn -> PT.SetFunction { userfn with body = newAST }
  | PT.SetExpr (tlid, id, _) -> PT.SetExpr(tlid, id, newAST)
  | PT.SetHandler (tlid, id, handler) ->
    PT.SetHandler(tlid, id, { handler with ast = newAST })
  | PT.CreateDB (_, _, _)
  | PT.AddDBCol (_, _, _)
  | PT.SetDBColName (_, _, _)
  | PT.SetDBColType (_, _, _)
  | PT.DeleteTL _
  | PT.MoveTL (_, _)
  | PT.TLSavepoint _
  | PT.UndoTL _
  | PT.RedoTL _
  | PT.DeleteFunction _
  | PT.ChangeDBColName (_, _, _)
  | PT.ChangeDBColType (_, _, _)
  | PT.DeprecatedInitDBm (_, _, _, _, _)
  | PT.CreateDBMigration (_, _, _, _)
  | PT.AddDBColToDBMigration (_, _, _)
  | PT.SetDBColNameInDBMigration (_, _, _)
  | PT.SetDBColTypeInDBMigration (_, _, _)
  | PT.DeleteColInDBMigration (_, _)
  | PT.AbandonDBMigration _
  | PT.DeleteDBCol (_, _)
  | PT.RenameDBname (_, _)
  | PT.CreateDBWithBlankOr (_, _, _, _)
  | PT.DeleteTLForever _
  | PT.DeleteFunctionForever _
  | PT.SetType _
  | PT.DeleteType _
  | PT.DeleteTypeForever _ -> op


// Filter down to only those ops which can be applied out of order
// without overwriting previous ops' state - eg, if we have
// SetHandler1 setting a handler's value to "aaa", and then
// SetHandler2's value is "aa", applying them out of order (SH2,
// SH1) will result in SH2's update being overwritten
// NOTE: DO NOT UPDATE WITHOUT UPDATING THE CLIENT-SIDE LIST
let filterOpsReceivedOutOfOrder (ops : PT.Oplist) : PT.Oplist =
  ops
  |> List.filter (fun op ->
    match op with
    | PT.SetHandler _
    | PT.SetFunction _
    | PT.SetType _
    | PT.MoveTL _
    | PT.SetDBColName _
    | PT.ChangeDBColName _
    | PT.ChangeDBColType _
    | PT.SetExpr _
    | PT.CreateDBMigration _
    | PT.SetDBColNameInDBMigration _
    | PT.SetDBColTypeInDBMigration _
    | PT.UndoTL _
    | PT.RedoTL _
    | PT.TLSavepoint _
    | PT.RenameDBname _ -> false
    | PT.CreateDB _
    | PT.AddDBCol _
    | PT.SetDBColType _
    | PT.DeleteTL _
    | PT.DeprecatedInitDBm _
    | PT.DeleteFunction _
    | PT.AddDBColToDBMigration _
    | PT.AbandonDBMigration _
    | PT.DeleteColInDBMigration _
    | PT.DeleteDBCol _
    | PT.CreateDBWithBlankOr _
    | PT.DeleteTLForever _
    | PT.DeleteFunctionForever _
    | PT.DeleteType _
    | PT.DeleteTypeForever _ -> true)

type AddOpResult =
  { toplevels : ORT.toplevel list (* replace *)
    deleted_toplevels : ORT.toplevel list (* replace, see note above *)
    user_functions : ORT.fluidExpr ORT.user_fn list (* replace *)
    deleted_user_functions : ORT.fluidExpr ORT.user_fn list
    user_tipes : ORT.user_tipe list
    deleted_user_tipes : ORT.user_tipe list (* replace, see deleted_toplevels *)  }

type AddOpParams =
  { ops : ORT.fluidExpr OT.oplist
    opCtr : int
    // option means that we can still deserialize if this field is null, as
    // doc'd at https://github.com/ocaml-ppx/ppx_deriving_yojson
    clientOpCtrId : string option }

type AddOpEvent = { result : AddOpResult; ``params`` : AddOpParams }
