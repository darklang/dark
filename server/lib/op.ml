open Core
open Types

type op = SetHandler of tlid * pos * Handler.handler
        | CreateDB of tlid * pos * string
        | AddDBCol of tlid * id * id
        | SetDBColName of tlid * id * string
        | SetDBColType of tlid * id * string
        | DeleteTL of tlid
        | MoveTL of tlid * pos
        | Savepoint
        | DeleteAll
        | Undo
        | Redo
        | SetFunction of RuntimeT.user_fn
        [@@deriving eq, yojson, show, sexp, bin_io]

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]

let has_effect (op: op) : bool  =
  op = Savepoint

let causes_any_changes (ops: oplist) : bool =
  List.exists ~f:has_effect ops

