open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event

type canvas = Canvas.canvas
type executable_fn_id = (tlid * id * int)

let initial_env (c: canvas) : RTT.dval_map =
  c.dbs
  |> TL.dbs
  |> User_db.dbs_as_env

let sample_request =
  PReq.to_dval PReq.sample_request

let sample_event =
  RTT.DIncomplete

let default_env (c: canvas) : RTT.dval_map =
  initial_env c
  |> RTT.DvalMap.set ~key:"request" ~data:sample_request
  |> RTT.DvalMap.set ~key:"event" ~data:sample_event

let initial_envs_for_handler (c: canvas) (h: RTT.HandlerT.handler)
  : RTT.dval_map list =
  let init = initial_env c in
  let default =
     match Handler.module_type h with
     | `Http ->
       RTT.DvalMap.set init "request" sample_request
     | `Event ->
       RTT.DvalMap.set init "event" sample_event
     | `Cron -> init
     | `Unknown -> default_env c
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
              let with_r = RTT.DvalMap.set init "request" e in
              let name = Handler.event_name_for_exn h in
              let bound = Http.bind_route_params_exn path name in
              Util.merge_left with_r bound
            | `Event ->
              RTT.DvalMap.set init "event" e
            | `Cron  -> init
            | `Unknown -> init (* can't happen *)
        ))

let initial_envs_for_user_fn (c: canvas) (fn: RTT.user_fn)
  : RTT.dval_map list =
  let init = initial_env c in
  Stored_function_arguments.load (c.id, fn.tlid)
  |> List.map ~f:(fun (m, _ts) -> Util.merge_left init m)


let state_for
    ~(c: canvas)
    ~(execution_id: int)
    ~(input_cursor: int )
    ~(exe_fn_ids : int list)
    ~(env: RTT.dval_map)
    (tlid: tlid)
  : RTT.exec_state =
  { tlid
  ; host = c.host
  ; account_id = c.owner
  ; canvas_id = c.id
  ; user_fns = c.user_functions
  ; exe_fn_ids
  ; env
  ; fail_fn = None
  ; input_cursor
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
    ~(input_cursor: int )
    ~(execution_id: int)
    ~(exe_fn_ids: int list)
    ~(env: RTT.dval_map)
    (tlid: tlid)
  : RTT.exec_state =
  let s = state_for ~c ~input_cursor ~execution_id ~exe_fn_ids ~env tlid in
  { s with load_fn_result = Stored_function_result.load
         ; store_fn_result = Stored_function_result.store
         ; load_fn_arguments = Stored_function_arguments.load
         ; store_fn_arguments = Stored_function_arguments.store
  }

let state_for_execution
    ~(c: canvas)
    ~(execution_id: int)
    ~(env: RTT.dval_map)
    (tlid: tlid)
  : RTT.exec_state =
  let s = state_for ~c ~execution_id ~input_cursor:0 ~exe_fn_ids:[] ~env tlid in
  { s with store_fn_result = Stored_function_result.store
         ; store_fn_arguments = Stored_function_arguments.store
  }

let state_for_enqueue
  ~(c: canvas)
  ~(execution_id: int)
  (tlid: tlid)
  : RTT.exec_state =
  state_for ~c ~execution_id ~input_cursor:0 ~exe_fn_ids:[] ~env:RTT.DvalMap.empty tlid

