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


