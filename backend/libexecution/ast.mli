open Core_kernel

val set_expr :
     search:Types.id
  -> replacement:Types.fluid_expr
  -> Types.fluid_expr
  -> Types.fluid_expr

val blank_to_option : 'a Types.or_blank -> 'a option

val blank_to_id : 'a Types.or_blank -> Types.id

val blank_map : f:('a -> 'b) -> 'a Types.or_blank -> 'b Types.or_blank

val blank_to_string : string Types.or_blank -> string

val deprecated_traverse :
     f:(Types.fluid_expr -> Types.fluid_expr)
  -> Types.fluid_expr
  -> Types.fluid_expr

val post_traverse :
     f:(Types.fluid_expr -> Types.fluid_expr)
  -> Types.fluid_expr
  -> Types.fluid_expr

val iter : f:(Types.fluid_expr -> unit) -> Types.fluid_expr -> unit

val find_db :
     Types.fluid_expr Types.RuntimeT.DbT.db list
  -> string
  -> Types.fluid_expr Types.RuntimeT.DbT.db

val execute_dblock :
     state:Types.RuntimeT.exec_state
  -> Types.RuntimeT.dblock_args
  -> Types.RuntimeT.dval list
  -> Types.RuntimeT.dval

val exec :
     state:Types.RuntimeT.exec_state
  -> Types.RuntimeT.dval_map
  -> Types.fluid_expr
  -> Types.RuntimeT.dval

val execute_ast :
     state:Types.RuntimeT.exec_state
  -> input_vars:Types.RuntimeT.input_vars
  -> Types.fluid_expr
  -> Types.RuntimeT.dval

val execute_fn :
     state:Types.RuntimeT.exec_state
  -> string
  -> Types.id
  -> Types.RuntimeT.dval list
  -> Types.RuntimeT.dval
