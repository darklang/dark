open Core
open Types

(* DO NOT CHANGE THE ORDER ON THESE!!!! IT WILL BREAK THE SERIALIZER *)
type op = SetHandler of tlid * pos * Handler.handler
        | CreateDB of tlid * pos * string
        | AddDBCol of tlid * id * id
        | SetDBColName of tlid * id * string
        | SetDBColType of tlid * id * string
        | DeleteTL of tlid
        | MoveTL of tlid * pos
        | DeprecatedSavepoint
        | DeprecatedDeleteAll
        | DeprecatedUndo
        | DeprecatedRedo
        | SetFunction of RuntimeT.user_fn
        | ChangeDBColName of tlid * id * string
        | ChangeDBColType of tlid * id * string
        | UndoTL of tlid
        | RedoTL of tlid
        | Savepoint of tlid list
        [@@deriving eq, yojson, show, sexp, bin_io]
(* DO NOT CHANGE THE ORDER ON THESE!!!! IT WILL BREAK THE SERIALIZER *)

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]

let has_effect (op: op) : bool  =
  match op with
  | Savepoint _ -> false
  | DeprecatedSavepoint -> false
  | _ -> true

let tlidsOf (op: op) :  tlid list =
  match op with
  | SetHandler (tlid, _, _) -> [tlid]
  | CreateDB (tlid, _, _) -> [tlid]
  | AddDBCol (tlid, _, _) -> [tlid]
  | SetDBColName (tlid, _, _) -> [tlid]
  | ChangeDBColName (tlid, _, _) -> [tlid]
  | SetDBColType (tlid, _, _) -> [tlid]
  | ChangeDBColType (tlid, _, _) -> [tlid]
  | Savepoint tlids -> tlids
  | UndoTL tlid -> [tlid]
  | RedoTL tlid -> [tlid]
  | DeleteTL tlid -> [tlid]
  | MoveTL (tlid, _) -> [tlid]
  | SetFunction _ -> []
  | DeprecatedSavepoint -> []
  | DeprecatedUndo -> []
  | DeprecatedRedo -> []
  | DeprecatedDeleteAll -> []


