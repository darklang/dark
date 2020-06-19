open Core_kernel
open Libexecution
module SF = Serialization_format

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
type 'expr_type op =
  | SetHandler of SF.tlid * SF.pos * 'expr_type SF.RuntimeT.HandlerT.handler
  | CreateDB of SF.tlid * SF.pos * string
  | AddDBCol of SF.tlid * SF.id * SF.id
  | SetDBColName of SF.tlid * SF.id * string
  | SetDBColType of SF.tlid * SF.id * string
  | DeleteTL of SF.tlid
  | MoveTL of SF.tlid * SF.pos
  | SetFunction of 'expr_type SF.RuntimeT.user_fn
  | ChangeDBColName of SF.tlid * SF.id * string
  | ChangeDBColType of SF.tlid * SF.id * string
  | UndoTL of SF.tlid
  | RedoTL of SF.tlid
  | DeprecatedInitDbm of
      SF.tlid * SF.id * SF.id * SF.id * SF.RuntimeT.DbT.migration_kind
  | SetExpr of SF.tlid * SF.id * 'expr_type
  | TLSavepoint of SF.tlid
  | DeleteFunction of SF.tlid
  | CreateDBMigration of
      SF.tlid * SF.id * SF.id * (string SF.or_blank * string SF.or_blank) list
  | AddDBColToDBMigration of SF.tlid * SF.id * SF.id
  | SetDBColNameInDBMigration of SF.tlid * SF.id * string
  | SetDBColTypeInDBMigration of SF.tlid * SF.id * string
  | AbandonDBMigration of SF.tlid
  | DeleteColInDBMigration of SF.tlid * SF.id
  | DeleteDBCol of SF.tlid * SF.id
  | RenameDBname of SF.tlid * string
  | CreateDBWithBlankOr of SF.tlid * SF.pos * SF.id * string
  | DeleteTLForever of SF.tlid
  | DeleteFunctionForever of SF.tlid
  | SetType of SF.RuntimeT.user_tipe
  | DeleteType of SF.tlid
  | DeleteTypeForever of SF.tlid
[@@deriving eq, yojson, show, bin_io]

(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

let op_to_fluid_op (op : SF.RuntimeT.expr op) : Types.fluid_expr op =
  match op with
  | SetHandler (tlid, pos, h) ->
      SetHandler (tlid, pos, Fluid.handler_to_fluid h)
  | CreateDB (tlid, pos, str) ->
      CreateDB (tlid, pos, str)
  | AddDBCol (tlid, id1, id2) ->
      AddDBCol (tlid, id1, id2)
  | SetDBColName (tlid, id, str) ->
      SetDBColName (tlid, id, str)
  | ChangeDBColName (tlid, id, str) ->
      ChangeDBColName (tlid, id, str)
  | SetDBColType (tlid, id, str) ->
      SetDBColType (tlid, id, str)
  | ChangeDBColType (tlid, id, str) ->
      ChangeDBColType (tlid, id, str)
  | DeprecatedInitDbm (tlid, id1, id2, id3, kind) ->
      DeprecatedInitDbm (tlid, id1, id2, id3, kind)
  | SetExpr (tlid, id, expr) ->
      SetExpr (tlid, id, Fluid.toFluidExpr expr)
  | TLSavepoint tlid ->
      TLSavepoint tlid
  | UndoTL tlid ->
      UndoTL tlid
  | RedoTL tlid ->
      RedoTL tlid
  | DeleteTL tlid ->
      DeleteTL tlid
  | MoveTL (tlid, pos) ->
      MoveTL (tlid, pos)
  | SetFunction f ->
      SetFunction (Fluid.user_fn_to_fluid f)
  | DeleteFunction tlid ->
      DeleteFunction tlid
  | CreateDBMigration (tlid, id1, id2, l) ->
      CreateDBMigration (tlid, id1, id2, l)
  | AddDBColToDBMigration (tlid, id1, id2) ->
      AddDBColToDBMigration (tlid, id1, id2)
  | SetDBColNameInDBMigration (tlid, id, str) ->
      SetDBColNameInDBMigration (tlid, id, str)
  | SetDBColTypeInDBMigration (tlid, id, str) ->
      SetDBColTypeInDBMigration (tlid, id, str)
  | AbandonDBMigration tlid ->
      AbandonDBMigration tlid
  | DeleteColInDBMigration (tlid, id) ->
      DeleteColInDBMigration (tlid, id)
  | DeleteDBCol (tlid, id) ->
      DeleteDBCol (tlid, id)
  | RenameDBname (tlid, str) ->
      RenameDBname (tlid, str)
  | CreateDBWithBlankOr (tlid, pos, id, str) ->
      CreateDBWithBlankOr (tlid, pos, id, str)
  | DeleteTLForever tlid ->
      DeleteTLForever tlid
  | DeleteFunctionForever tlid ->
      DeleteFunctionForever tlid
  | SetType ut ->
      SetType ut
  | DeleteType tlid ->
      DeleteType tlid
  | DeleteTypeForever tlid ->
      DeleteTypeForever tlid


let op_of_fluid_op (op : Types.fluid_expr op) :
    Serialization_format.RuntimeT.expr op =
  match op with
  | SetHandler (tlid, pos, h) ->
      SetHandler (tlid, pos, Fluid.handler_of_fluid h)
  | CreateDB (tlid, pos, str) ->
      CreateDB (tlid, pos, str)
  | AddDBCol (tlid, id1, id2) ->
      AddDBCol (tlid, id1, id2)
  | SetDBColName (tlid, id, str) ->
      SetDBColName (tlid, id, str)
  | ChangeDBColName (tlid, id, str) ->
      ChangeDBColName (tlid, id, str)
  | SetDBColType (tlid, id, str) ->
      SetDBColType (tlid, id, str)
  | ChangeDBColType (tlid, id, str) ->
      ChangeDBColType (tlid, id, str)
  | DeprecatedInitDbm (tlid, id1, id2, id3, kind) ->
      DeprecatedInitDbm (tlid, id1, id2, id3, kind)
  | SetExpr (tlid, id, expr) ->
      SetExpr (tlid, id, Fluid.fromFluidExpr expr)
  | TLSavepoint tlid ->
      TLSavepoint tlid
  | UndoTL tlid ->
      UndoTL tlid
  | RedoTL tlid ->
      RedoTL tlid
  | DeleteTL tlid ->
      DeleteTL tlid
  | MoveTL (tlid, pos) ->
      MoveTL (tlid, pos)
  | SetFunction f ->
      SetFunction (Fluid.user_fn_of_fluid f)
  | DeleteFunction tlid ->
      DeleteFunction tlid
  | CreateDBMigration (tlid, id1, id2, l) ->
      CreateDBMigration (tlid, id1, id2, l)
  | AddDBColToDBMigration (tlid, id1, id2) ->
      AddDBColToDBMigration (tlid, id1, id2)
  | SetDBColNameInDBMigration (tlid, id, str) ->
      SetDBColNameInDBMigration (tlid, id, str)
  | SetDBColTypeInDBMigration (tlid, id, str) ->
      SetDBColTypeInDBMigration (tlid, id, str)
  | AbandonDBMigration tlid ->
      AbandonDBMigration tlid
  | DeleteColInDBMigration (tlid, id) ->
      DeleteColInDBMigration (tlid, id)
  | DeleteDBCol (tlid, id) ->
      DeleteDBCol (tlid, id)
  | RenameDBname (tlid, str) ->
      RenameDBname (tlid, str)
  | CreateDBWithBlankOr (tlid, pos, id, str) ->
      CreateDBWithBlankOr (tlid, pos, id, str)
  | DeleteTLForever tlid ->
      DeleteTLForever tlid
  | DeleteFunctionForever tlid ->
      DeleteFunctionForever tlid
  | SetType ut ->
      SetType ut
  | DeleteType tlid ->
      DeleteType tlid
  | DeleteTypeForever tlid ->
      DeleteTypeForever tlid


let event_name_of_op (op : 'expr_type op) : string =
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
let required_context_to_validate (op : 'expr_type op) : required_context =
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


type 'expr_type oplist = 'expr_type op list
[@@deriving eq, yojson, show, bin_io]

let oplist_to_fluid (oplist : SF.RuntimeT.expr oplist) : Types.fluid_expr oplist
    =
  List.map oplist ~f:op_to_fluid_op


let oplist_of_fluid (oplist : Types.fluid_expr oplist) : SF.RuntimeT.expr oplist
    =
  List.map oplist ~f:op_of_fluid_op


let required_context_to_validate_oplist (oplist : 'expr_type oplist) :
    required_context =
  oplist
  |> List.map ~f:required_context_to_validate
  |> List.max_elt ~compare:compare_required_context
  |> Option.value ~default:NoContext


type 'expr_type tlid_oplists = (SF.tlid * 'expr_type oplist) list
[@@deriving eq, yojson, show, bin_io]

let is_deprecated (op : 'expr_type op) : bool = false

let has_effect (op : 'expr_type op) : bool =
  match op with TLSavepoint _ -> false | _ -> true


let tlidOf (op : 'expr_type op) : Types.tlid =
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


(* [f] is something like RuntimeT.bin_expr *)
let oplist_to_string
    ~(f : 'expr_type Bin_prot.Type_class.t) (ops : 'expr_type op list) : string
    =
  ops
  |> Core_extended.Bin_io_utils.to_line (bin_oplist f)
  |> Bigstring.to_string


(* [f] is something like RuntimeT.bin_expr *)
let oplist_of_string ~(f : 'expr_type Bin_prot.Type_class.t) (str : string) :
    'expr_type op list =
  Core_extended.Bin_io_utils.of_line str (bin_oplist f)


let oplist2tlid_oplists (oplist : 'expr_type oplist) : 'expr_type tlid_oplists =
  oplist
  |> List.map ~f:tlidOf
  |> List.stable_dedup
  |> List.map ~f:(fun tlid ->
         (tlid, List.filter oplist ~f:(fun op -> tlidOf op = tlid)))


let tlid_oplists2oplist (tos : 'expr_type tlid_oplists) : 'expr_type oplist =
  tos |> List.unzip |> Tuple.T2.get2 |> List.concat


let ast_of (op : 'expr_type op) : 'expr_type option =
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


let is_latest_op_request client_op_ctr_id op_ctr canvas_id : bool =
  let client_op_ctr_id =
    match client_op_ctr_id with
    | Some s when s = "" ->
        Uuidm.v `V4 |> Uuidm.to_string
    | None ->
        Uuidm.v `V4 |> Uuidm.to_string
    | Some s ->
        s
  in
  Db.run
    ~name:"update-browser_id-op_ctr"
    (* This is "UPDATE ... WHERE browser_id = $1 AND ctr < $2" except
             * that it also handles the initial case where there is no
             * browser_id record yet *)
    "INSERT INTO op_ctrs(browser_id,ctr,canvas_id) VALUES($1, $2, $3)
             ON CONFLICT (browser_id)
             DO UPDATE SET ctr = EXCLUDED.ctr, timestamp = NOW()
                       WHERE op_ctrs.ctr < EXCLUDED.ctr"
    ~params:
      [ Db.Uuid (client_op_ctr_id |> Uuidm.of_string |> Option.value_exn)
      ; Db.Int op_ctr
      ; Db.Uuid canvas_id ] ;
  Db.exists
    ~name:"check-if-op_ctr-is-latest"
    "SELECT 1 FROM op_ctrs WHERE browser_id = $1 AND ctr = $2"
    ~params:
      [ Db.Uuid (client_op_ctr_id |> Uuidm.of_string |> Option.value_exn)
      ; Db.Int op_ctr ]


(* filter down to only those ops which can be applied out of order
 * without overwriting previous ops' state - eg, if we have
 * SetHandler1 setting a handler's value to "aaa", and then
 * SetHandler2's value is "aa", applying them out of order (SH2,
 * SH1) will result in SH2's update being overwritten *)
(* NOTE: DO NOT UPDATE WITHOUT UPDATING THE CLIENT-SIDE LIST *)
let filter_ops_received_out_of_order (ops : 'expr_type op list) :
    'expr_type op list =
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
