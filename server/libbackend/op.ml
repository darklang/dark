open Core_kernel
open Libexecution
open Types


(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
type op = SetHandler of tlid * pos * RuntimeT.HandlerT.handler
        | CreateDB of tlid * pos * string
        | AddDBCol of tlid * id * id
        | SetDBColName of tlid * id * string
        | SetDBColType of tlid * id * string
        | DeleteTL of tlid
        | MoveTL of tlid * pos
        | SetFunction of RuntimeT.user_fn
        | ChangeDBColName of tlid * id * string
        | ChangeDBColType of tlid * id * string
        | UndoTL of tlid
        | RedoTL of tlid
        | InitDBMigration of tlid * id * id * id * RuntimeT.DbT.migration_kind
        | SetExpr of tlid * id * RuntimeT.expr
        | TLSavepoint of tlid
        | DeleteFunction of RuntimeT.user_fn
        [@@deriving eq, yojson, show, sexp, bin_io]
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]
type tlid_oplists = (tlid * oplist) list
                    [@@deriving eq, yojson, show, sexp, bin_io]

type expr = RuntimeT.expr


let is_deprecated (op: op) : bool =
  false

let has_effect (op: op) : bool  =
  match op with
  | TLSavepoint _ -> false
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
  | DeleteFunction f -> Some f.tlid

let oplist_to_string (ops: op list) : string =
  ops
  |> Core_extended.Bin_io_utils.to_line bin_oplist
  |> Bigstring.to_string

let oplist_of_string (str:string) : op list =
  Core_extended.Bin_io_utils.of_line str bin_oplist

let oplist2tlid_oplists (oplist: oplist) : tlid_oplists =
  oplist
  |> List.map ~f:(fun op -> tlidOf op |> Option.value_exn)
  |> List.stable_dedup
  |> List.map ~f:(fun tlid ->
      (tlid, List.filter oplist
         ~f:(fun op -> tlidOf op = Some tlid)))

let tlid_oplists2oplist (tos: tlid_oplists) : oplist =
  tos
  |> List.unzip
  |> Tuple.T2.get2
  |> List.concat
