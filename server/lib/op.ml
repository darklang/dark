open Core
open Types

module J = Yojson.Safe.Util

type json = Yojson.Safe.json

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
type op = NoOp
        | SetAST of Ast.toplevel
        | DeleteAll
        | DeleteAST of int
        | MoveAST of int * pos
        | SavePoint
        | Undo
        | Redo
[@@deriving eq, yojson, show]

type oplist = op list [@@deriving eq, yojson, show]

