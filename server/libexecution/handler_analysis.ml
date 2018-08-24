open Core_kernel

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request

let dbs_as_env (dbs: RTT.DbT.db list) : (string * RTT.dval) list =
  List.map dbs ~f:(fun db -> (db.name, RTT.DDB db))

let execute
  ~tlid
  ~execution_id
  ~input_vars
  ~dbs
  ~user_fns
  ~account_id
  ~canvas_id
  h
  =
  let vars = (dbs_as_env dbs) @ input_vars  in
  (* TODO: better error *)
  let env = RTT.DvalMap.of_alist_exn vars in
  let state : RTT.exec_state =
    { tlid = tlid
    ; account_id
    ; canvas_id
    ; user_fns
    ; dbs
    ; env
    ; execution_id
    ; exe_fn_ids = []
    ; fail_fn = None
    ; load_fn_result = Ast_analysis.load_no_results
    ; store_fn_result = Ast_analysis.store_no_results
    ; load_fn_arguments = Ast_analysis.load_no_arguments
    ; store_fn_arguments = Ast_analysis.store_no_arguments
    }
  in
  Ast_analysis.execute_handler state h

