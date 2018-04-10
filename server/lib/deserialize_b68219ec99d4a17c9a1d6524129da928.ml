open Core

(* Some old important files were saved in an old format, fortunately as
 * JSON. Typically we can manually tweak the files, but these were too
 * complicated. *)

module T = Types
module TR = Types.RuntimeT

type pos = { x:int; y:int }[@@deriving eq, show, yojson, sexp, bin_io]

type tlid = int [@@deriving eq, show, yojson, sexp, bin_io]
type id = int [@@deriving eq, show, yojson, sexp, bin_io]

type fnname = string [@@deriving eq, yojson, show, sexp, bin_io]
type fieldname = string [@@deriving eq, yojson, show, sexp, bin_io]
type varname = string [@@deriving eq, yojson, show, sexp, bin_io]

type expr = If of id * expr * expr * expr
          | Thread of id * expr list
          | FnCall of id * fnname * expr list
          | Variable of id * varname
          | Let of id * TR.varbinding * expr * expr
          | Lambda of id * varname list * expr
          | Value of id * string
          | FieldAccess of id * expr * TR.field
          | Hole of id
          [@@deriving eq, yojson, show, sexp, bin_io]

type handler = { tlid: Types.tlid
               ; ast: expr
               ; spec : Handler.spec
               } [@@deriving eq, show, yojson, sexp, bin_io]




type op = NoOp
        | SetHandler of tlid * pos * handler
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
[@@deriving eq, yojson, show, sexp, bin_io]

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]

let rec old2new (old : expr) : TR.expr =
  match old with
  | If (id, cond, ifcase, elsecase) ->
    T.Filled (id, TR.If (old2new cond, old2new ifcase, old2new elsecase))
  | Thread (id, exprs ) ->
    T.Filled (id, TR.Thread (List.map ~f:old2new exprs))
  | FnCall (id, fnname, exprs ) ->
    T.Filled (id, TR.FnCall (fnname, List.map ~f:old2new exprs))
  | Variable (id, varname) ->
    T.Filled (id, TR.Variable (varname))
  | Let (id, varbinding, rhs, body) ->
    T.Filled (id, TR.Let (varbinding, old2new rhs, old2new body))
  | Lambda (id, varname, expr) ->
    T.Filled (id, TR.Lambda (varname, old2new expr))
  | Value (id, string) ->
    T.Filled (id, TR.Value string)
  | FieldAccess (id, expr, field) ->
    T.Filled (id, TR.FieldAccess (old2new expr, field))
  | Hole (id) ->
    T.Blank id


let convert (ops : oplist) : Op.oplist =
  ops
  |> List.filter ~f:((<>) NoOp)
  |> List.map
      ~f:(fun (op: op) : Op.op ->
          match op with
          | SetHandler (_, {x;y}, {tlid; ast; spec}) ->
            Op.SetHandler (tlid, {x;y}, {tlid; ast=old2new ast; spec})
          | CreateDB (tlid, {x;y}, string) ->
            Op.CreateDB (tlid, {x;y}, string)
          | AddDBCol (tlid, id1, id2) ->
            Op.AddDBCol (tlid, id1, id2)
          | SetDBColName (tlid, id, string) ->
            Op.SetDBColName (tlid, id, string)
          | SetDBColType (tlid, id, string) ->
            Op.SetDBColType (tlid, id, string)
          | DeleteTL (tlid) ->
            Op.DeleteTL (tlid)
          | MoveTL (tlid, {x;y}) ->
            Op.MoveTL (tlid, {x;y})
          | Savepoint -> Op.Savepoint
          | DeleteAll -> Op.DeleteAll
          | Undo -> Op.Undo
          | Redo -> Op.Redo
          | NoOp -> Exception.internal "cant noop")


let load_json ~root filename : Op.oplist =
  filename
  |> Util.readjsonfile ~root ~conv:oplist_of_yojson
  |> convert




