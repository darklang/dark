open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event

type canvas = Canvas.canvas
type executable_fn_id = (tlid * id * int)

let global_vars (c: canvas) : RTT.input_vars =
  c.dbs
  |> TL.dbs
  |> Execution.dbs_as_input_vars

let sample_request =
  PReq.to_dval PReq.sample_request

let sample_event =
  RTT.DIncomplete

let default_input_vars (c: canvas) : RTT.input_vars=
  global_vars c
  @ [("request", sample_request); ("event", sample_event)]

let initial_input_vars_for_handler (c: canvas) (h: RTT.HandlerT.handler)
  : RTT.input_vars list =
  let init = global_vars c in
  let default =
     match Handler.module_type h with
     | `Http ->
       init @ [("request", sample_request)]
     | `Event ->
       init @ [("event", sample_event)]
     | `Cron -> init
     | `Unknown -> default_input_vars c
  in
  (match Handler.event_desc_for h with
  | None -> [default]
  | Some (space, path, modifier as d) ->
    let events = SE.load_events c.id d in
    if events = []
    then [default]
    else
      List.map events
        ~f:(fun e ->
            match Handler.module_type h with
            | `Http ->
              let with_r = [("request", e)] in
              let name = Handler.event_name_for_exn h in
              let bound = Http.bind_route_params_exn path name in
              init @ with_r @ bound
            | `Event ->
              init @ [("event", e)]
            | `Cron  -> init
            | `Unknown -> init (* can't happen *)
        ))

let initial_input_vars_for_user_fn (c: canvas) (fn: RTT.user_fn)
  : RTT.input_vars list =
  let init = global_vars c in
  Stored_function_arguments.load (c.id, fn.tlid)
  |> List.map ~f:(fun (m, _ts) -> init @ (RTT.DvalMap.to_alist m))


let state_for
    ~(c: canvas)
    ~(execution_id: id)
    ~(exe_fn_ids : id list)
    (tlid: tlid)
  : RTT.exec_state =
  { tlid
  ; account_id = c.owner
  ; canvas_id = c.id
  ; user_fns = c.user_functions
  ; exe_fn_ids
  ; fail_fn = None
  ; dbs = TL.dbs c.dbs
  ; execution_id
  ; load_fn_result = Ast_analysis.load_no_results
  ; store_fn_result = Ast_analysis.store_no_results
  ; load_fn_arguments = Ast_analysis.load_no_arguments
  ; store_fn_arguments = Ast_analysis.store_no_arguments
  ;
  }

let state_for_analysis
    ~(c: canvas)
    ~(execution_id: id)
    ~(exe_fn_ids: id list)
    (tlid: tlid)
  : RTT.exec_state =
  let s = state_for ~c ~execution_id ~exe_fn_ids tlid in
  { s with load_fn_result = Stored_function_result.load
         ; store_fn_result = Stored_function_result.store
         ; load_fn_arguments = Stored_function_arguments.load
         ; store_fn_arguments = Stored_function_arguments.store
  }

