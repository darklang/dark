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

let default_env (h: handler) : RT.dval_map =
  let init = RT.DvalMap.empty in
  match url_for h with
  | Some n ->
    List.fold_left
      ~init
      ~f:(fun acc v ->
          RT.DvalMap.add ~key:v ~data:RT.DNull acc)
      (Http.route_variables n)
  | None -> init

let with_defaults (h: handler) (env: Ast.symtable) : Ast.symtable =
  Util.merge_left env (default_env h)

let execute (env: Ast.symtable) (h: handler) : RT.dval =
  Ast.execute (with_defaults h env) h.ast

let execute_for_analysis (env: Ast.symtable) (h: handler) :
    (Types.id * RT.dval * Ast.dval_store * Ast.sym_store) list =
  let traced_symbols = Ast.symbolic_execute (with_defaults h env) h.ast in
  let (ast_value, traced_values) = Ast.execute_saving_intermediates env h.ast in
  [(h.tlid, ast_value, traced_values, traced_symbols)]
