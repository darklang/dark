open Core

module RT = Runtime

type spec = { module_ : string Types.or_hole [@key "module"]
            ; name : string Types.or_hole
            ; modifier : string Types.or_hole
            } [@@deriving eq, show, yojson]


type handler = { tlid: Types.tlid
               ; ast: Ast.ast
               ; spec : spec
               } [@@deriving eq, show, yojson]


let execute (eh: handler) : RT.dval =
  Ast.execute Ast.Symtable.empty eh.ast

let execute_for_analysis (global: RT.dval) (eh: handler) :
    (Types.id * RT.dval * Ast.dval_store * Ast.sym_store) list =
  let traced_symbols = Ast.symbolic_execute eh.ast in
  let (ast_value, traced_values) = Ast.execute_saving_intermediates eh.ast in
  [(eh.tlid, ast_value, traced_values, traced_symbols)]
