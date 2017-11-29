open Core

type spec = { module_ : string Types.or_hole [@key "module"]
            ; name : string Types.or_hole
            ; modifier : string Types.or_hole
            } [@@deriving eq, show, yojson]


type handler = { id: Types.id
               ; ast: Ast.ast
               ; spec : spec
               } [@@deriving eq, show, yojson]


