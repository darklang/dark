open Core_kernel
open Libcommon

open Analysis_types
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT

module RT = Runtime
module PReq = Parsed_request

let analyse_ast ~(input_vars: input_vars)
    (state : exec_state) (ast : expr)
    : analysis =
  let input_vars = Execution.dbs_as_input_vars state.dbs @ input_vars in
  let traced_symbols =
    Ast.symbolic_execute state ~input_vars ast in
  let (ast_value, traced_values) =
    Ast.execute_saving_intermediates state ~input_vars ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = Ast.input_values input_vars
  }

