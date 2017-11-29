open Core
open Types

module J = Yojson.Safe.Util

type json = Yojson.Safe.json

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
type op = NoOp
        | SetHandler of tlid * pos * Handler.handler
        | CreateDB of tlid * pos * string
        | AddDBRow of tlid * id * id
        | SetDBRowName of tlid * id * string
        | SetDBRowType of tlid * id * string
        | DeleteTL of tlid
        | MoveTL of tlid * pos
        | Savepoint
        | DeleteAll
        | Undo
        | Redo
[@@deriving eq, yojson, show]

type oplist = op list [@@deriving eq, yojson, show]

