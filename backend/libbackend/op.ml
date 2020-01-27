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

let event_name_of_op (op : op) : string =
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


(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

(* Note the ord *)
type required_context =
  | NoContext
  | AllDatastores
[@@deriving eq, ord]

(* Returns the 'context', ie. the other stuff on the canvas, that
 * you need to also load in order validate that this op could be added
 * to the oplist/canvas correctly *)
let required_context_to_validate (op : op) : required_context =
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


type oplist = op list [@@deriving eq, yojson, show, bin_io]

let required_context_to_validate_oplist (oplist : oplist) : required_context =
  oplist
  |> List.map ~f:required_context_to_validate
  |> List.max_elt ~compare:compare_required_context
  |> Option.value ~default:NoContext


type tlid_oplists = (tlid * oplist) list [@@deriving eq, yojson, show, bin_io]

type expr = RuntimeT.expr

let is_deprecated (op : op) : bool = false

let has_effect (op : op) : bool =
  match op with TLSavepoint _ -> false | _ -> true


let tlidOf (op : op) : tlid =
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


let oplist_to_string (ops : op list) : string =
  ops |> Core_extended.Bin_io_utils.to_line bin_oplist |> Bigstring.to_string


let oplist_of_string (str : string) : op list =
  Core_extended.Bin_io_utils.of_line str bin_oplist


let oplist2tlid_oplists (oplist : oplist) : tlid_oplists =
  oplist
  |> List.map ~f:tlidOf
  |> List.stable_dedup
  |> List.map ~f:(fun tlid ->
         (tlid, List.filter oplist ~f:(fun op -> tlidOf op = tlid)))


let tlid_oplists2oplist (tos : tlid_oplists) : oplist =
  tos |> List.unzip |> Tuple.T2.get2 |> List.concat


let ast_of (op : op) : Types.RuntimeT.expr option =
  match op with
  | SetFunction {ast; _} | SetExpr (_, _, ast) | SetHandler (_, _, {ast; _}) ->
      Some ast
  | _ ->
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
let filter_ops_received_out_of_order (ops : op list) : op list =
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
