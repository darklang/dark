open Tc
open Types

let tlidStrOfReference (r : tlReference) : string =
  match r with
  | OutReferenceDB (TLID tlid, _, _, _) ->
      tlid
  | OutReferenceEvent (TLID tlid, _, _, _) ->
      tlid


let idOfReference (r : tlReference) : id =
  match r with
  | OutReferenceDB (_, _, _, id) ->
      id
  | OutReferenceEvent (_, _, _, id) ->
      id


let shouldUpdateReferences (ops : op list) =
  List.any
    ~f:(fun op ->
      match op with
      | SetHandler _ | SetExpr _ | SetFunction _ ->
          true
      | CreateDB _
      | AddDBCol _
      | SetDBColName _
      | SetDBColType _
      | DeleteTL _
      | MoveTL _
      | TLSavepoint _
      | UndoTL _
      | RedoTL _
      | DeleteFunction _
      | ChangeDBColName _
      | ChangeDBColType _
      | DeprecatedInitDbm _
      | CreateDBMigration _
      | AddDBColToDBMigration _
      | SetDBColNameInDBMigration _
      | SetDBColTypeInDBMigration _
      | DeleteColInDBMigration _
      | AbandonDBMigration _
      | DeleteDBCol _
      | RenameDBname _
      | CreateDBWithBlankOr _
      | DeleteTLForever _
      | DeleteFunctionForever _
      | SetType _
      | DeleteType _
      | DeleteTypeForever _ ->
          false )
    ops
