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
  |> Handler_analysis.dbs_as_env
  |> RTT.DvalMap.of_alist_exn

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
              let bound = Http.bind_route_params_exn path name
                          |> RTT.DvalMap.of_alist_exn
              in
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
    ~(execution_id: id)
    ~(exe_fn_ids : id list)
    ~(env: RTT.dval_map)
    (tlid: tlid)
  : RTT.exec_state =
  { tlid
  ; account_id = c.owner
  ; canvas_id = c.id
  ; user_fns = c.user_functions
  ; exe_fn_ids
  ; env
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
    ~(env: RTT.dval_map)
    (tlid: tlid)
  : RTT.exec_state =
  let s = state_for ~c ~execution_id ~exe_fn_ids ~env tlid in
  { s with load_fn_result = Stored_function_result.load
         ; store_fn_result = Stored_function_result.store
         ; load_fn_arguments = Stored_function_arguments.load
         ; store_fn_arguments = Stored_function_arguments.store
  }

let state_for_enqueue
  ~(c: canvas)
  ~(execution_id: id)
  (tlid: tlid)
  : RTT.exec_state =
  state_for ~c ~execution_id ~exe_fn_ids:[] ~env:RTT.DvalMap.empty tlid

