open Core_kernel
open Libcommon

open Analysis_types
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT

module RT = Runtime
module PReq = Parsed_request

let analyse_ast
  ~tlid
  ~exe_fn_ids
  ~execution_id
  ~input_vars
  ~dbs
  ~user_fns
  ~account_id
  ~canvas_id
  ?(load_fn_result = Execution.load_no_results)
  ?(store_fn_result = Execution.store_no_results)
  ?(load_fn_arguments = Execution.load_no_arguments)
  ?(store_fn_arguments = Execution.store_no_arguments)
  (ast : expr)
  : analysis =
  let input_vars = (Execution.dbs_as_input_vars dbs) @ input_vars  in
  let state : exec_state =
    { tlid = tlid
    ; account_id
    ; canvas_id
    ; user_fns
    ; dbs
    ; execution_id
    ; exe_fn_ids = []
    ; fail_fn = None
    ; load_fn_result
    ; store_fn_result
    ; load_fn_arguments
    ; store_fn_arguments
    }
  in
  let traced_symbols =
    Ast.symbolic_execute state ~input_vars ast in
  let (ast_value, traced_values) =
    Ast.execute_saving_intermediates state ~input_vars ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = Ast.input_values input_vars
  }

