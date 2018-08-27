open Core_kernel

open Types
open Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request

(* -------------------- *)
(* Input_vars *)
(* -------------------- *)
let handler_default_input_vars (h: HandlerT.handler) : input_vars =
  match Handler.event_name_for h with
  | Some n ->
    n
    |> Http.route_variables
    |> List.map ~f:(fun k -> (k, DIncomplete))
  | None -> []

let with_defaults (h: HandlerT.handler) (input_vars: input_vars) : input_vars =
  input_vars @ (handler_default_input_vars h)

let input_vars_for_user_fn (ufn: user_fn) : dval_map =
  let param_to_dval (p: param) : dval =
    DIncomplete
  in
  ufn.metadata.parameters
  |> List.filter_map ~f:ufn_param_to_param
  |> List.map ~f:(fun f -> (f.name, param_to_dval f))
  |> Analysis_types.Symtable.of_alist_exn

let dbs_as_input_vars (dbs: DbT.db list) : (string * dval) list =
  List.map dbs ~f:(fun db -> (db.name, DDB db))

let http_input_vars (h: HandlerT.handler) (path: string) : input_vars =
  let route = Handler.event_name_for_exn h in
  Http.bind_route_params_exn ~path ~route

(* -------------------- *)
(* For exec_state *)
(* -------------------- *)
let load_no_results _ _ = None
let store_no_results _ _ _ = ()
let load_no_arguments _ = []
let store_no_arguments _ _ = ()


(* -------------------- *)
(* Execution *)
(* -------------------- *)
let execute_handler
  ~tlid
  ~execution_id
  ~input_vars
  ~dbs
  ~user_fns
  ~account_id
  ~canvas_id
  (h : HandlerT.handler)
  : dval
  =
  let vars = (dbs_as_input_vars dbs) @ input_vars  in
  let state : exec_state =
    { tlid = tlid
    ; account_id
    ; canvas_id
    ; user_fns
    ; dbs
    ; execution_id
    ; exe_fn_ids = []
    ; fail_fn = None
    ; load_fn_result = load_no_results
    ; store_fn_result = store_no_results
    ; load_fn_arguments = load_no_arguments
    ; store_fn_arguments = store_no_arguments
    }
  in
  let result = Ast.execute_ast vars state h.ast in
  match result with
  | DErrorRail (DOption OptNothing) ->
    DResp ((Response (404, []), DStr "Not found"))
  | DErrorRail _ ->
    DResp ((Response (500, []), DStr "Invalid conversion from errorrail"))
  | dv -> dv




