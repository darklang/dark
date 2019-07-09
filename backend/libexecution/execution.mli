open Core_kernel

(* ----------------- *)
(* Input vars *)
(* ----------------- *)
val input_vars_for_user_fn : Types.RuntimeT.user_fn -> Types.RuntimeT.dval_map

val dbs_as_input_vars :
  Types.RuntimeT.DbT.db list -> (string * Types.RuntimeT.dval) list

val http_route_input_vars :
  Types.RuntimeT.HandlerT.handler -> string -> Types.RuntimeT.input_vars

val sample_route_input_vars :
  Types.RuntimeT.HandlerT.handler -> Types.RuntimeT.input_vars

val sample_input_vars :
  Types.RuntimeT.HandlerT.handler -> Types.RuntimeT.input_vars

val sample_function_input_vars :
  Types.RuntimeT.user_fn -> Types.RuntimeT.input_vars

val sample_unknown_handler_input_vars : Types.RuntimeT.input_vars

(* ----------------- *)
(* Exec_state *)
(* ----------------- *)

val store_no_results : Types.RuntimeT.store_fn_result_type

val store_no_arguments : Types.RuntimeT.store_fn_arguments_type

val load_no_results : Types.RuntimeT.load_fn_result_type

val load_no_arguments : Types.RuntimeT.load_fn_arguments_type

(* ----------------- *)
(* Execution *)
(* ----------------- *)
val execute_handler :
     tlid:Types.tlid
  -> execution_id:Types.tlid
  -> input_vars:Types.RuntimeT.input_vars
  -> dbs:Types.RuntimeT.DbT.db list
  -> user_fns:Types.RuntimeT.user_fn list
  -> user_tipes:Types.RuntimeT.user_tipe list
  -> account_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> ?load_fn_result:Types.RuntimeT.load_fn_result_type
  -> ?load_fn_arguments:Types.RuntimeT.load_fn_arguments_type
  -> ?store_fn_result:Types.RuntimeT.store_fn_result_type
  -> ?store_fn_arguments:Types.RuntimeT.store_fn_arguments_type
  -> Types.RuntimeT.HandlerT.handler
  -> Types.RuntimeT.dval * Types.tlid list

val execute_function :
     tlid:Types.tlid
  -> execution_id:Types.tlid
  -> trace_id:Uuidm.t
  -> dbs:Types.RuntimeT.DbT.db list
  -> user_fns:Types.RuntimeT.user_fn list
  -> user_tipes:Types.RuntimeT.user_tipe list
  -> account_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> caller_id:Types.id
  -> args:Types.RuntimeT.dval list
  -> ?store_fn_result:Types.RuntimeT.store_fn_result_type
  -> ?store_fn_arguments:Types.RuntimeT.store_fn_arguments_type
  -> string
  -> Types.RuntimeT.dval * Types.tlid list

(* ----------------- *)
(* Analysis *)
(* ----------------- *)
val analyse_ast :
     tlid:Types.tlid
  -> execution_id:Types.tlid
  -> input_vars:Types.RuntimeT.input_vars
  -> dbs:Types.RuntimeT.DbT.db list
  -> user_fns:Types.RuntimeT.user_fn list
  -> user_tipes:Types.RuntimeT.user_tipe list
  -> account_id:Uuidm.t
  -> canvas_id:Uuidm.t
  -> ?load_fn_result:Types.RuntimeT.load_fn_result_type
  -> ?load_fn_arguments:Types.RuntimeT.load_fn_arguments_type
  -> Types.RuntimeT.expr
  -> Analysis_types.analysis
