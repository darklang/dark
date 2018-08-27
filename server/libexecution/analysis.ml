open Core_kernel
open Libcommon

open Analysis_types
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT

module RT = Runtime
module PReq = Parsed_request


let execute_handler_for_analysis ~(input_vars: input_vars)
    (state : exec_state) (h : handler)
  : analysis =
  Log.infO "Handler for analysis"
    ~params:[ "tlid", show_tlid state.tlid
            ; "execution_id", Log.dump state.execution_id];
  let input_vars = Execution.with_defaults h input_vars in
  let traced_symbols =
    Ast.symbolic_execute state ~input_vars h.ast in
  let (ast_value, traced_values) =
    Ast.execute_saving_intermediates state ~input_vars h.ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = Ast.input_values input_vars
  }

let execute_user_fn_for_analysis ~(input_vars: input_vars)
    (state : exec_state) (f : user_fn)
    : analysis =
  Log.infO "Function for analysis"
    ~data:(show_tlid state.tlid)
    ~params:["execution_id", Log.dump state.execution_id];
  let traced_symbols =
    Ast.symbolic_execute state ~input_vars f.ast in
  let (ast_value, traced_values) =
    Ast.execute_saving_intermediates state ~input_vars f.ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = Ast.input_values input_vars
  }
