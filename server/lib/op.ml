open Core
open Types

module J = Yojson.Safe.Util

type json = Yojson.Safe.json

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
type op = NoOp
        | SetTL of Toplevel.toplevel
        | DeleteAll
        | DeleteTL of int
        | MoveTL of int * pos
        | CreateDB of int * pos * string
        | Savepoint
        | Undo
        | Redo
[@@deriving eq, yojson, show]

type oplist = op list [@@deriving eq, yojson, show]

