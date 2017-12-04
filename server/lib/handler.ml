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

type executable_handler = { tlid: Types.tlid
                          ; ast: Ast.ast
                          ; spec : spec
                          ; initial_state: Ast.symtable
                          }

let url_for (h: handler) : string option =
  match h.spec.module_, h.spec.name with
  | Full module_, Full name when String.lowercase module_ = "http" ->
    Some name
  | _ -> None

let url_for_exn (h: handler) : string =
  match (url_for h) with
  | Some s -> s
  | None ->
    Exception.internal
      "Called url_for_exn on a toplevel without a `url` param"

let make_executable (init: Ast.symtable) (h: handler) : executable_handler =
  let state =
    match url_for h with
    | Some n ->
      List.fold_left
        ~init:init
        ~f:(fun acc v ->
            Ast.Symtable.add ~key:v ~data:RT.DNull acc)
        (Http.route_variables n)
    | None -> init
  in
  { tlid = h.tlid; ast = h.ast; spec = h.spec; initial_state = state }

let to_handler (eh: executable_handler) : handler =
  { tlid = eh.tlid; ast = eh.ast; spec = eh.spec }

let execute (eh: executable_handler) : RT.dval =
  Ast.execute eh.initial_state eh.ast

let execute_for_analysis (eh: executable_handler) :
    (Types.id * RT.dval * Ast.dval_store * Ast.sym_store) list =
  let traced_symbols = Ast.symbolic_execute eh.initial_state eh.ast in
  let (ast_value, traced_values) = Ast.execute_saving_intermediates eh.initial_state eh.ast in
  [(eh.tlid, ast_value, traced_values, traced_symbols)]
