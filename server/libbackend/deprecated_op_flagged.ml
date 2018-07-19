open Core_kernel
open Libexecution
open Deprecated_types_flagged


(* DO NOT CHANGE THE ORDER ON THESE!!!! IT WILL BREAK THE SERIALIZER *)
type op = SetHandler of tlid * pos * RuntimeT.HandlerT.handler
        | CreateDB of tlid * pos * string
        | AddDBCol of tlid * id * id
        | SetDBColName of tlid * id * string
        | SetDBColType of tlid * id * string
        | DeleteTL of tlid
        | MoveTL of tlid * pos
        | Deprecated0
        | Deprecated1
        | Deprecated2
        | Deprecated3
        | SetFunction of RuntimeT.user_fn
        | ChangeDBColName of tlid * id * string
        | ChangeDBColType of tlid * id * string
        | UndoTL of tlid
        | RedoTL of tlid
        | Deprecated4 of tlid list
        | InitDBMigration of tlid * id * id * id * RuntimeT.DbT.migration_kind
        | SetExpr of tlid * id * RuntimeT.expr
        | TLSavepoint of tlid
        [@@deriving eq, yojson, show, sexp, bin_io]
(* DO NOT CHANGE THE ORDER ON THESE!!!! IT WILL BREAK THE SERIALIZER *)

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]
type tlid_oplists = (tlid * oplist) list
                    [@@deriving eq, yojson, show, sexp, bin_io]

type expr = Deprecated_types_flagged.RuntimeT.expr

let rec has_deprecated_expr (expr: expr) : bool =
  false

let is_deprecated (op: op) : bool =
  match op with
  | Deprecated0
  | Deprecated1
  | Deprecated2
  | Deprecated3
  | Deprecated4 _ -> true
  | SetExpr (_, _, expr) ->
    has_deprecated_expr expr
  | SetFunction (fn) ->
    has_deprecated_expr fn.ast
  | SetHandler (_, _, h) ->
    has_deprecated_expr h.ast
  | _ -> false

open RuntimeT

let convert_flagged_expr expr =
  match expr with
  | Flagged (id, msg, _, a, b) ->
    Filled (id, (FeatureFlag (msg, Blank (Util.create_id ()), a, b)))
  | e -> e

let rec traverse ~(f: expr -> expr) (expr:expr) : expr =
  let r = traverse ~f in
  let new_expr =
    match expr with
    | Blank _ -> expr
    | Flagged (id, msg, setting, a, b) ->
      Flagged (id, msg, setting, r a, r b)
    | Filled (id, nexpr) ->
      Filled (id,
              match nexpr with
               | Value _ -> nexpr
               | Variable _ -> nexpr

               | Let (lhs, rhs, body) ->
                 Let (lhs, r rhs, r body)

               | If (cond, ifbody, elsebody) ->
                 If (r cond, r ifbody, r elsebody)

               | FnCall (name, exprs) ->
                 FnCall (name, List.map ~f:r exprs)

               | Lambda (vars, lexpr) ->
                 Lambda (vars, r lexpr)

               | Thread exprs ->
                 Thread (List.map ~f:r exprs)

               | FieldAccess (obj, field) ->
                 FieldAccess (r obj, field)

               | ListLiteral exprs ->
                 ListLiteral (List.map ~f:r exprs)

               | ObjectLiteral pairs ->
                 ObjectLiteral (List.map ~f:(fun (k, v) -> (k, r v)) pairs)

               | FeatureFlag (msg, cond, a, b) ->
                 FeatureFlag (msg, r cond, r a, r b)
             )
  in
  f new_expr

let convert_flagged_ast (expr: RuntimeT.expr) : expr =
  traverse ~f:convert_flagged_expr expr

let convert_flagged (op: op) : (op) =
  match op with
  | SetExpr (tlid, id, expr) ->
    SetExpr (tlid, id, convert_flagged_ast expr)
  | SetFunction (fn) ->
    SetFunction ({ fn with ast = convert_flagged_ast fn.ast})
  | SetHandler (tlid, pos, h) ->
    SetHandler (tlid, pos, { h with ast = convert_flagged_ast h.ast })
  | op -> op


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
  | Deprecated0
  | Deprecated1
  | Deprecated2
  | Deprecated3
  | Deprecated4 _ -> None

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


