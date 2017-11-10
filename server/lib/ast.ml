open Core

module RT = Runtime

(* --------------------- *)
(* Types *)
(* --------------------- *)
type fnname = string [@@deriving eq, yojson, show]
type varname = string [@@deriving eq, yojson, show]

type expr = If of expr * expr * expr
          | FnCall of fnname * expr list
          | Variable of varname
          | Let of (varname * expr) list * expr
          | Lambda of varname list * expr
          | Value of RT.dval
          | Hole
          [@@deriving eq, yojson, show]

type ast = expr [@@deriving eq, yojson, show]

type toplevel = { id: Types.id
                ; pos: Types.pos
                ; ast: ast
                } [@@deriving eq, show, yojson]

(* --------------------- *)
(* API Types and Fns *)
(* --------------------- *)

type api_expr =
  { if_: api_if option [@key "if"] [@default None]
  ; value: api_value option [@default None]
  ; hole: api_hole option [@default None]
  }
and api_if = { cond: api_expr
             ; then_: api_expr [@key "then"]
             ; else_: api_expr [@key "else"]
             }
and api_value = string
and api_hole = { fake: int option [@default None] }
[@@deriving yojson]

type api_ast = api_expr [@@deriving yojson]

type api_toplevel = { id: int
                    ; pos: Types.pos
                    ; ast: api_ast
                    } [@@deriving yojson]

(* --------------------- *)
(* from API *)
(* --------------------- *)
let rec api_expr2expr (e: api_expr) : expr =
  let a2e = api_expr2expr in
  match e with
  | { if_ = Some a } ->
    If (a2e a.cond, a2e a.then_, a2e a.else_)
  | { value = Some a } ->
    Value (RT.parse a)
  | { hole = Some _ } ->
    Hole
  | _ -> Exception.internal "Unexpected opexpr"

let api_ast2ast = api_expr2expr

(* --------------------- *)
(* to API *)
(* --------------------- *)

let rec expr2api_expr (e: expr) : api_expr =
  let empty = { if_ = None
              ; value = None
              ; hole  = None } in
  let e2a = expr2api_expr in
  match e with
  | If (cond, then_, else_) ->
      { empty with if_ = Some { cond = e2a cond
                       ; then_ = e2a then_
                       ; else_ = e2a else_
                       }}
  | Value v ->
    { empty with value = Some (RT.dval_to_json_string v) }
  | Hole -> { empty with hole = Some { fake = None } }
  | _ -> Exception.internal "Unexpected opexpr"

let ast2api_ast = expr2api_expr

let toplevel2api_toplevel (tl: toplevel) : api_toplevel =
  { id = tl.id
  ; pos = tl.pos
  ; ast = ast2api_ast tl.ast }

let toplevel_to_frontend (tl: toplevel) : Yojson.Safe.json =
  tl
  |> toplevel2api_toplevel
  |> api_toplevel_to_yojson
