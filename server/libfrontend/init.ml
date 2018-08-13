open Libexecution

module Analysis = Libexecution.Ast_analysis


let init () =
  print_endline "libfrontend reporting in"

let state env : Libexecution.Types.RuntimeT.exec_state =
  { tlid = 0
  ; host = "test"
  ; account_id = Libexecution.Util.create_uuid ()
  ; canvas_id = Libexecution.Util.create_uuid ()
  ; user_fns = []
  ; exe_fn_ids = []
  ; env = env
  ; fail_fn = None
  ; input_cursor = 0
  ; dbs = []
  ; execution_id = 0
  ; load_fn_result = Ast_analysis.load_no_results
  ; store_fn_result = Ast_analysis.store_no_results
  ; load_fn_arguments = Ast_analysis.load_no_arguments
  ; store_fn_arguments = Ast_analysis.store_no_arguments
  }

let perform_analysis (str : string) : string =
  let env = Libexecution.Types.RuntimeT.DvalMap.empty in
  Libexecution.Ast_analysis.execute_ast (state env) env (Blank 0)
  |> Dval.dval_to_json_string


