open Core_kernel

(* ----------------- *)
(* Analysis *)
(* ----------------- *)
val analyse_user_fn :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.user_fn ->
  Analysis_types.analysis

val analyse_handler :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.HandlerT.handler ->
  Analysis_types.analysis


