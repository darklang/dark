open Core_kernel

val set_expr :
     search:Types.id
  -> replacement:Types.RuntimeT.expr
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.expr

val blank_to_option : 'a Types.or_blank -> 'a option

val blank_to_id : 'a Types.or_blank -> Types.id

val blank_map : f:('a -> 'b) -> 'a Types.or_blank -> 'b Types.or_blank

val blank_to_string : string Types.or_blank -> string

val deprecated_traverse :
     f:(Types.RuntimeT.expr -> Types.RuntimeT.expr)
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.expr

val post_traverse :
     f:(Types.RuntimeT.expr -> Types.RuntimeT.expr)
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.expr

val iter : f:(Types.RuntimeT.expr -> unit) -> Types.RuntimeT.expr -> unit

val find_db :
     Types.RuntimeT.expr Types.RuntimeT.DbT.db list
  -> string
  -> Types.RuntimeT.expr Types.RuntimeT.DbT.db

val execute_dblock :
     state:Types.RuntimeT.expr Types.RuntimeT.exec_state
  -> Types.RuntimeT.expr Types.RuntimeT.dblock_args
  -> Types.RuntimeT.expr Types.RuntimeT.dval list
  -> Types.RuntimeT.expr Types.RuntimeT.dval

val exec :
     state:Types.RuntimeT.expr Types.RuntimeT.exec_state
  -> Types.RuntimeT.expr Types.RuntimeT.dval_map
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.expr Types.RuntimeT.dval

val execute_ast :
     state:Types.RuntimeT.expr Types.RuntimeT.exec_state
  -> input_vars:Types.RuntimeT.expr Types.RuntimeT.input_vars
  -> Types.RuntimeT.expr
  -> Types.RuntimeT.expr Types.RuntimeT.dval

val execute_fn :
     state:Types.RuntimeT.expr Types.RuntimeT.exec_state
  -> string
  -> Types.id
  -> Types.RuntimeT.expr Types.RuntimeT.dval list
  -> Types.RuntimeT.expr Types.RuntimeT.dval
