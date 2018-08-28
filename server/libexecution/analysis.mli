open Core_kernel

(* ----------------- *)
(* Analysis *)
(* ----------------- *)
val analyse_ast :
  tlid : Types.tlid ->
  exe_fn_ids : Types.id list ->
  execution_id : Types.tlid ->
  input_vars : Types.RuntimeT.input_vars ->
  dbs : Types.RuntimeT.DbT.db list ->
  user_fns : Types.RuntimeT.user_fn list ->
  account_id : Uuidm.t ->
  canvas_id : Uuidm.t ->
  ?load_fn_result: Types.RuntimeT.load_fn_result_type ->
  ?store_fn_result: Types.RuntimeT.store_fn_result_type ->
  ?load_fn_arguments: Types.RuntimeT.load_fn_arguments_type ->
  ?store_fn_arguments: Types.RuntimeT.store_fn_arguments_type ->
  Types.RuntimeT.expr ->
  Analysis_types.analysis

