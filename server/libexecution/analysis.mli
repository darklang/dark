open Core_kernel

(* ----------------- *)
(* Analysis *)
(* ----------------- *)
val analyse_ast :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.expr ->
  Analysis_types.analysis


