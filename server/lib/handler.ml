open Core

open Types
open Types.RuntimeT
open Types.SpecTypes
module RT = Runtime
module FF = FeatureFlag

type spec_types = { input : dark_type
                  ; output : dark_type
                  } [@@deriving eq, show, yojson, sexp, bin_io]


type spec = { module_ : string Types.or_blank [@key "module"]
            ; name : string Types.or_blank
            ; modifier : string Types.or_blank
            ; types : spec_types
            } [@@deriving eq, show, yojson, sexp, bin_io]

type handler = { tlid: Types.tlid
               ; ast: expr
               ; spec : spec
               } [@@deriving eq, show, yojson, sexp, bin_io]

let is_complete (h: handler) : bool =
  match (h.spec.module_, h.spec.name, h.spec.modifier) with
  | (Filled (_, _), Filled (_, _), Filled (_, _)) -> true
  | _ -> false

let is_http (h: handler) : bool =
  match h.spec.module_ with
  | Filled (_, m) -> String.Caseless.equal "http" m
  | _ -> true

let module_for (h: handler) : string option =
  match h.spec.module_ with
  | Filled (_, m) -> Some m
  | _ -> None

let module_for_exn (h: handler) : string =
  match (module_for h) with
  | Some s -> s
  | None ->
    Exception.internal
      "Called module_for_exn on a toplevel without a `module` param"

let event_name_for (h: handler) : string option =
  match h.spec.name with
  | Filled (_, name) ->
    Some name
  | _ -> None

let event_name_for_exn (h: handler) : string =
  match (event_name_for h) with
  | Some s -> s
  | None ->
    Exception.internal
      "Called event_name_for_exn on a toplevel without a `event_name` param"

let modifier_for (h: handler) : string option =
  match h.spec.modifier with
  | Filled (_, modifier) ->
    Some modifier
  | _ -> None

let modifier_for_exn (h: handler) : string =
  match (modifier_for h) with
  | Some s -> s
  | None ->
    Exception.internal
      "Called modifier_for_exn on a toplevel without a `modifier` param"

let default_env (h: handler) : dval_map =
  let init = DvalMap.empty in
  match event_name_for h with
  | Some n ->
    List.fold_left
      ~init
      ~f:(fun acc v ->
          DvalMap.set ~key:v ~data:DIncomplete acc)
      (Http.route_variables n)
  | None -> init

let with_defaults (h: handler) (env: Ast.symtable) : Ast.symtable =
  Util.merge_left env (default_env h)

let execute (state: Ast.exec_state) (h: handler) : dval =
  Ast.execute state (with_defaults h state.env) h.ast

let execute_for_analysis (state : Ast.exec_state) (h : handler) :
    (dval * Ast.dval_store * Ast.sym_store * Ast.symtable) =
  let default_env = with_defaults h state.env in
  let state = { state with env = default_env } in
  let traced_symbols =
    Ast.symbolic_execute state.ff state.env h.ast in
  let (ast_value, traced_values) =
    Ast.execute_saving_intermediates state h.ast in
  (ast_value, traced_values, traced_symbols, state.env)
