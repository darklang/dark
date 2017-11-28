open Core

module RT = Runtime

(* --------------------- *)
(* Types *)
(* --------------------- *)

type handler_spec = { module_ : string Types.or_hole [@key "module"]
                    ; name : string Types.or_hole
                    ; modifier : string Types.or_hole
                    } [@@deriving eq, show, yojson]


type toplevel = { id: Types.id
                ; pos: Types.pos
                ; ast: Ast.ast
                ; handler_spec : handler_spec
                } [@@deriving eq, show, yojson]

type toplevellist = toplevel list [@@deriving eq, show, yojson]

