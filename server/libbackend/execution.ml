open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event
module FF = Feature_flag

type canvas = Canvas.canvas
type executable_fn_id = (tlid * id * int)

let initial_dval_map (c: canvas) : RTT.dval_map =
  c.toplevels
  |> TL.dbs
  |> User_db.dbs_as_env

let sample_request =
  PReq.to_dval PReq.sample_request

let sample_event =
  RTT.DIncomplete

let default_env (c: canvas) : RTT.dval_map =
  initial_dval_map c
  |> RTT.DvalMap.set ~key:"request" ~data:sample_request
  |> RTT.DvalMap.set ~key:"event" ~data:sample_event

let initial_envs (c: canvas) (h: Handler.handler)
  : RTT.dval_map list =
  let initial_env = initial_dval_map c in
  let default =
     match Handler.module_type h with
     | `Http ->
       RTT.DvalMap.set initial_env "request" sample_request
     | `Event ->
       RTT.DvalMap.set initial_env "event" sample_event
     | `Cron -> initial_env
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
              let with_r = RTT.DvalMap.set initial_env "request" e in
              let name = Handler.event_name_for_exn h in
              let bound = Http.bind_route_params_exn path name in
              Util.merge_left with_r bound
            | `Event ->
              RTT.DvalMap.set initial_env "event" e
            | `Cron  -> initial_env
            | `Unknown -> initial_env (* can't happen *)
        ))

let state_for
    ~(c: canvas)
    ~(execution_id: int)
    ~(input_cursor: int )
    ~(exe_fn_ids : int list)
    ~(env: RTT.dval_map)
    (tlid: tlid)
  : RTT.exec_state =
  { ff = FF.analysis
  ; tlid
  ; host = c.host
  ; account_id = c.owner
  ; canvas_id = c.id
  ; user_fns = c.user_functions
  ; exe_fn_ids
  ; env
  ; input_cursor
  ; dbs = TL.dbs c.toplevels
  ; execution_id
  ; load_fn_result = Ast_analysis.load_nothing
  ; store_fn_result = Ast_analysis.store_nothing
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
  { s with load_fn_result = Stored_function_result.load }

let state_for_execution
    ~(c: canvas)
    ~(execution_id: int)
    ~(env: RTT.dval_map)
    (tlid: tlid)
  : RTT.exec_state =
  let s = state_for ~c ~execution_id ~input_cursor:0 ~exe_fn_ids:[] ~env tlid in
  { s with store_fn_result = Stored_function_result.store }

