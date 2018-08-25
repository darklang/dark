open Core_kernel

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request

let dbs_as_input_vars (dbs: RTT.DbT.db list) : (string * RTT.dval) list =
  List.map dbs ~f:(fun db -> (db.name, RTT.DDB db))

let execute
  ~tlid
  ~execution_id
  ~input_vars
  ~dbs
  ~user_fns
  ~account_id
  ~canvas_id
  (h : RTT.HandlerT.handler)
  : RTT.dval
  =
  let vars = (dbs_as_input_vars dbs) @ input_vars  in
  (* TODO: better error *)
  let st = RTT.DvalMap.of_alist_exn vars in
  let state : RTT.exec_state =
    { tlid = tlid
    ; account_id
    ; canvas_id
    ; user_fns
    ; dbs
    ; execution_id
    ; exe_fn_ids = []
    ; fail_fn = None
    ; load_fn_result = Ast_analysis.load_no_results
    ; store_fn_result = Ast_analysis.store_no_results
    ; load_fn_arguments = Ast_analysis.load_no_arguments
    ; store_fn_arguments = Ast_analysis.store_no_arguments
    }
  in
  let result = Ast_analysis.execute_ast state st h.ast in
  match result with
  | DErrorRail (DOption OptNothing) ->
    DResp ((Response (404, []), DStr "Not found"))
  | DErrorRail _ ->
    DResp ((Response (500, []), DStr "Invalid conversion from errorrail"))
  | dv -> dv


