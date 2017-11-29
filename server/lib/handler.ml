open Core

type spec = { module_ : string Types.or_hole [@key "module"]
            ; name : string Types.or_hole
            ; modifier : string Types.or_hole
            } [@@deriving eq, show, yojson]


type handler = { tlid: Types.tlid
               ; ast: Ast.ast
               ; spec : spec
               } [@@deriving eq, show, yojson]


