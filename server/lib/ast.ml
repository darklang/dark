open Core

module RT = Runtime

type fnname = string [@@deriving eq, yojson, show]
type varname = string [@@deriving eq, yojson, show]

type expr = If of expr * expr * expr
          | FnCall of fnname * expr list
          | Variable of varname
          | Let of (varname * expr) list * expr
          | Lambda of varname list * expr
          | Value of RT.dval
          [@@deriving eq, yojson, show]

type ast = expr [@@deriving eq, yojson, show]

type toplevel = { id: Types.id
                ; pos: Types.pos
                ; ast: ast
                } [@@deriving eq, show, yojson]
