open Core_kernel

(* ----------------- *)
(* Analysis *)
(* ----------------- *)
val execute_user_fn_for_analysis :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.user_fn ->
  Analysis_types.analysis

val execute_handler_for_analysis :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.HandlerT.handler ->
  Analysis_types.analysis


