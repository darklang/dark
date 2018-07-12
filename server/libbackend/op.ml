open Core_kernel
open Libexecution
open Types


(* DO NOT CHANGE THE ORDER ON THESE!!!! IT WILL BREAK THE SERIALIZER *)
type op = SetHandler of tlid * pos * Handler.handler
        | CreateDB of tlid * pos * string
        | AddDBCol of tlid * id * id
        | SetDBColName of tlid * id * string
        | SetDBColType of tlid * id * string
        | DeleteTL of tlid
        | MoveTL of tlid * pos
        | Deprecated0
        | Deprecated1
        | Deprecated2
        | Deprecated3
        | SetFunction of RuntimeT.user_fn
        | ChangeDBColName of tlid * id * string
        | ChangeDBColType of tlid * id * string
        | UndoTL of tlid
        | RedoTL of tlid
        | Deprecated4 of tlid list
        | InitDBMigration of tlid * id * id * id * RuntimeT.DbT.migration_kind
        | SetExpr of tlid * id * RuntimeT.expr
        | TLSavepoint of tlid
        [@@deriving eq, yojson, show, sexp, bin_io]
(* DO NOT CHANGE THE ORDER ON THESE!!!! IT WILL BREAK THE SERIALIZER *)

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]

let has_effect (op: op) : bool  =
  match op with
  | TLSavepoint _ -> false
  | Deprecated0 -> false
  | Deprecated1 -> false
  | Deprecated2 -> false
  | Deprecated3 -> false
  | Deprecated4 _ -> false
  | _ -> true

let tlidOf (op: op) : tlid option =
  match op with
  | SetHandler (tlid, _, _) -> Some tlid
  | CreateDB (tlid, _, _) -> Some tlid
  | AddDBCol (tlid, _, _) -> Some tlid
  | SetDBColName (tlid, _, _) -> Some tlid
  | ChangeDBColName (tlid, _, _) -> Some tlid
  | SetDBColType (tlid, _, _) -> Some tlid
  | ChangeDBColType (tlid, _, _) -> Some tlid
  | InitDBMigration (tlid, _, _, _, _) -> Some tlid
  | SetExpr (tlid, _, _) -> Some tlid
  | TLSavepoint tlid -> Some tlid
  | UndoTL tlid -> Some tlid
  | RedoTL tlid -> Some tlid
  | DeleteTL tlid -> Some tlid
  | MoveTL (tlid, _) -> Some tlid
  | SetFunction f -> Some f.tlid
  | Deprecated0
  | Deprecated1
  | Deprecated2
  | Deprecated3
  | Deprecated4 _ -> None


