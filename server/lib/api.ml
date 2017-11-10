open Core
open Types

open Op
module RT = Runtime

(* Opcodes as sent via the API. We do this to get type checking *)
type pos = Types.pos [@@deriving yojson]

type op_expr = { if_: if_expr option [@key "if"] [@default None]
               ; value: value_expr option [@default None]
               }
and
if_expr = { cond: op_expr
          ; then_: op_expr [@key "then"]
          ; else_: op_expr [@key "else"]
          }
and
value_expr = string
[@@deriving yojson]


let rec op_expr2expr (ope: op_expr) : Ast.expr =
  let e2e = op_expr2expr in
  match ope with
  | { if_ = Some a } -> Ast.If (e2e a.cond, e2e a.then_, e2e a.else_)
  | { value = Some a } -> Ast.Value (RT.parse a)
  | _ -> Exception.internal "Unexpected opexpr"

let op_expr2ast (ope: op_expr) : Ast.ast =
  op_expr2expr ope

(* ---------------- *)
(* opcodes *)
(* ---------------- *)
type delete_all = { fake : int option [@default None]
                  } [@@deriving yojson]
type noop = { fake: int option [@default None]
            } [@@deriving yojson]

type redo = { fake: int option [@default None]
            } [@@deriving yojson]

type undo = { fake: int option [@default None]
            } [@@deriving yojson]

type savepoint = { fake: int option [@default None]
                 } [@@deriving yojson]

type set_ast = { id: int
               ; pos: pos
               ; ast: op_expr
               } [@@deriving yojson]

(* ---------------- *)
(* Read the command out *)
(* ---------------- *)
type opjson =
  { set_ast: set_ast option [@default None]
  ; redo: redo option [@default None]
  ; undo: undo option [@default None]
  ; savepoint: savepoint option [@default None]
  ; noop: noop option [@default None]
  ; delete_all: delete_all option [@default None]

  } [@@deriving yojson]
type opjsonlist = opjson list [@@deriving yojson]

let json2op (op: opjson) : op =
  match op with
  | { set_ast = Some a } -> SetAST { id = a.id; pos = a.pos; ast = op_expr2ast a.ast }
  | { noop = Some _} -> NoOp
  | { redo = Some _} -> Redo
  | { undo = Some _} -> Undo
  | { savepoint = Some _} -> SavePoint
  | { delete_all = Some a } -> DeleteAll
  | _ -> Exception.internal "Unexpected opcode"

let to_ops (payload: string) : op list =
  payload
  |> Yojson.Safe.from_string
  |> opjsonlist_of_yojson
  |> Result.ok_or_failwith
  |> List.map ~f:json2op


(*------------------*)
(* Functions *)
(*------------------*)
type param_ = { name: string
              ; tipe: string
              ; block_args : string list
              ; optional: bool
              ; description: string
              } [@@deriving yojson]

type function_ = { name: string
                 ; parameters : param_ list
                 ; description : string
                 ; return_type : string} [@@deriving yojson]
type functionlist = function_ list [@@deriving yojson]

let functions =
  Libs.fns
  |> String.Map.to_alist
  |> List.map ~f:(fun (k,(v:RT.fn))
                   -> { name = k
                      ; parameters =
                        List.map ~f:(fun p : param_ ->
                          { name = p.name
                          ; tipe = RT.tipe2str p.tipe
                          ; block_args = p.block_args
                          ; optional = p.optional
                          ; description = p.description })
                        v.parameters
                      ; description = v.description
                      ; return_type = RT.tipe2str v.return_type
                      })
  |> functionlist_to_yojson
  |> Yojson.Safe.pretty_to_string
