open Core
open Types

module J = Yojson.Safe.Util

type json = Yojson.Safe.json

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
type op = NoOp
        | SetHandler of int * pos * Handler.handler
        | CreateDB of int * pos * Db.db
        | DeleteTL of int
        | MoveTL of int * pos
        | Savepoint
        | DeleteAll
        | Undo
        | Redo
[@@deriving eq, yojson, show]

type oplist = op list [@@deriving eq, yojson, show]

