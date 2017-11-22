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
        | CloseThread of int * int
        | MoveTL of int * pos
        | SavePoint
        | Undo
        | Redo
[@@deriving eq, yojson, show]

type oplist = op list [@@deriving eq, yojson, show]

