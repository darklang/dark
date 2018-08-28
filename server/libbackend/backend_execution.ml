open Core_kernel
open Libexecution

open Types
open Analysis_types
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

let saved_input_vars (c: canvas) (h: RTT.HandlerT.handler) =
  match Handler.event_desc_for h with
  | None -> []
  | Some (space, path, modifier as d) ->
    List.map (SE.load_events c.id d)
      ~f:(fun e ->
          match Handler.module_type h with
          | `Http ->
            let with_r = [("request", e)] in
            let bound = Libexecution.Execution.http_route_input_vars h path in
            with_r @ bound
          | `Event ->
            [("event", e)]
          | `Cron  -> []
          | `Unknown -> [] (* can't happen *)
      )

let initial_input_vars_for_handler (c: canvas) (h: RTT.HandlerT.handler)
  : RTT.input_vars list =
  let saved = saved_input_vars c h in
  let samples = Execution.sample_input_vars h in
  if saved = []
  then [samples]
  else saved

let initial_input_vars_for_user_fn (c: canvas) (fn: RTT.user_fn)
  : RTT.input_vars list =
  Stored_function_arguments.load (c.id, fn.tlid)
  |> List.map ~f:(fun (m, _ts) -> RTT.DvalMap.to_alist m)


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
  ; load_fn_result = Libexecution.Execution.load_no_results
  ; store_fn_result = Libexecution.Execution.store_no_results
  ; load_fn_arguments = Libexecution.Execution.load_no_arguments
  ; store_fn_arguments = Libexecution.Execution.store_no_arguments
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

