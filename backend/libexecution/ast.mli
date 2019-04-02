open Core_kernel

val set_expr :
     search:Types.id
  -> replacement:Types.RuntimeT.expr
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.expr

val blank_to_option : 'a Types.or_blank -> 'a option

val blank_map : f:('a -> 'b) -> 'a Types.or_blank -> 'b Types.or_blank

val blank_to_string : string Types.or_blank -> string

val execute_ast :
     input_vars:Types.RuntimeT.input_vars
  -> Types.RuntimeT.exec_state
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.dval * Types.tlid list

val execute_userfn :
     Types.RuntimeT.exec_state
  -> string
  -> Types.id
  -> Types.RuntimeT.dval list
  -> Types.RuntimeT.dval

val execute_saving_intermediates :
     input_vars:Types.RuntimeT.input_vars
  -> Types.RuntimeT.exec_state
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.dval * Analysis_types.dval_store * Types.tlid list

val execute_fn :
     Types.RuntimeT.exec_state
  -> string
  -> Types.id
  -> Types.RuntimeT.dval list
  -> Types.RuntimeT.dval
