open Core_kernel

(* ----------------- *)
(* Input vars *)
(* ----------------- *)
val input_vars_for_user_fn :
  Types.RuntimeT.user_fn ->
  Types.RuntimeT.dval_map

val dbs_as_input_vars :
  Types.RuntimeT.DbT.db list ->
  (string * Types.RuntimeT.dval) list

val http_route_input_vars :
  Types.RuntimeT.HandlerT.handler ->
  string ->
  Types.RuntimeT.input_vars

val sample_input_vars :
  Types.RuntimeT.HandlerT.handler ->
  Types.RuntimeT.input_vars

val sample_unknown_handler_input_vars :
  Types.RuntimeT.input_vars


(* ----------------- *)
(* Exec_state *)
(* ----------------- *)

val store_no_results :
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval ->
  unit

val load_no_results:
  Types.RuntimeT.function_desc ->
  Types.RuntimeT.dval list ->
  (Types.RuntimeT.dval * Time.t) option

val store_no_arguments :
  Types.RuntimeT.user_fn_desc ->
  Types.RuntimeT.dval_map ->
  unit

val load_no_arguments :
  Types.RuntimeT.user_fn_desc ->
  (Types.RuntimeT.dval_map * Time.t) list


(* ----------------- *)
(* Execution *)
(* ----------------- *)
val execute_handler :
  tlid : Types.tlid ->
  execution_id : Types.tlid ->
  input_vars : Types.RuntimeT.input_vars ->
  dbs : Types.RuntimeT.DbT.db list ->
  user_fns : Types.RuntimeT.user_fn list ->
  account_id : Uuidm.t ->
  canvas_id : Uuidm.t ->
  Types.RuntimeT.HandlerT.handler ->
  Types.RuntimeT.dval


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
