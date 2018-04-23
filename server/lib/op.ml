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
        | Savepoint
        | DeleteAll (* does nothing, but kept for binary compatibility *)
        | Undo
        | Redo
        | SetFunction of RuntimeT.user_fn
        | ChangeDBColName of tlid * id * string
        | ChangeDBColType of tlid * id * string
        [@@deriving eq, yojson, show, sexp, bin_io]
(* DO NOT CHANGE THE ORDER ON THESE!!!! IT WILL BREAK THE SERIALIZER *)

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]

let has_effect (op: op) : bool  =
  op <> Savepoint


