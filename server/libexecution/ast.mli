open Core_kernel

val set_expr :
  search: Types.id ->
  replacement: Types.RuntimeT.expr ->
  Types.RuntimeT.expr ->
  Types.RuntimeT.expr

val blank_to_option :
  'a Types.or_blank ->
  'a option

val execute_ast :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.expr ->
  Types.RuntimeT.dval

val execute_userfn :
  Types.RuntimeT.exec_state ->
  string ->
  Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval

val symbolic_execute :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.expr ->
  Analysis_types.sym_store

val execute_saving_intermediates :
  input_vars: Types.RuntimeT.input_vars ->
  Types.RuntimeT.exec_state ->
  Types.RuntimeT.expr ->
  Types.RuntimeT.dval * Analysis_types.dval_store


val execute_fn :
  Types.RuntimeT.exec_state ->
  string ->
  Types.id ->
  Types.RuntimeT.dval list ->
  Types.RuntimeT.dval


