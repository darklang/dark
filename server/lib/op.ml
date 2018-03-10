open Core
open Types

module J = Yojson.Safe.Util

type json = Yojson.Safe.json

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
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
        | Sync
[@@deriving eq, yojson, show, sexp, bin_io]

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]

let has_effect (op: op) : bool  =
  match op with
  | Sync -> false
  | Savepoint -> false
  | _ -> true

let causes_any_changes (ops: oplist) : bool =
  List.exists ~f:has_effect ops

