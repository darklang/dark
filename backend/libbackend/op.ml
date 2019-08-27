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
         | RenameDBname _ ->
             false
         | CreateDB _
         | AddDBCol _
         | SetDBColType _
         | DeleteTL _
         | DeprecatedInitDbm _
         | TLSavepoint _
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
             true )
