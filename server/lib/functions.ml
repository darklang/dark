open Core
open Types
open Types.RuntimeT

let environment_for_user_fn (ufn: user_fn) : dval_map =
  let filled =
    List.filter_map ~f:ufn_param_to_param ufn.metadata.parameters
  in
  let param_to_dval (p: param) : dval =
    DIncomplete (* TODO(ian): we should trace these correctly *)
  in
  List.fold_left
    ~f:(fun acc f ->
        Map.set ~key:f.name ~data:(param_to_dval f) acc)
    ~init:DvalMap.empty
    filled

let execute_for_analysis (state : exec_state) (f : user_fn) :
    (dval * Ast.dval_store * Ast.sym_store * symtable) =
  let traced_symbols =
    Ast.symbolic_execute state.ff state.env f.ast in
  let (ast_value, traced_values) =
    Ast.execute_saving_intermediates state f.ast in
  (ast_value, traced_values, traced_symbols, state.env)

