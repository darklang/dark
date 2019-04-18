open Core_kernel
open Libcommon
open Libexecution
open Types
open Analysis_types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event
module SA = Static_assets

type canvas = Canvas.canvas

(* ------------------------- *)
(* Non-execution analysis *)
(* ------------------------- *)

let unlocked (c : canvas) : tlid list =
  c.dbs
  |> TL.dbs
  |> User_db.unlocked c.id c.owner
  |> List.map ~f:(fun x -> x.tlid)


let get_404s ~(since : RTT.time) (c : canvas) : SE.four_oh_four list =
  let events = SE.list_events ~limit:(`Since since) ~canvas_id:c.id () in
  let handlers =
    Db.fetch
      ~name:"get_404s"
      "SELECT module, name, modifier FROM toplevel_oplists
        WHERE canvas_id = $1
          AND module IS NOT NULL
          AND name IS NOT NULL
          AND modifier IS NOT NULL
          AND tipe = 'handler'::toplevel_type"
      ~params:[Db.Uuid c.id]
    |> List.map ~f:(function
           | [modu; n; modi] ->
               (modu, n, modi)
           | _ ->
               Exception.internal "Bad DB format for get404s" )
  in
  let match_event h event : bool =
    let space, request_path, modifier, _ts, _ = event in
    let h_space, h_name, h_modifier = h in
    Http.request_path_matches_route ~route:h_name request_path
    && h_modifier = modifier
    && h_space = space
  in
  events
  |> List.filter ~f:(fun e ->
         not (List.exists handlers ~f:(fun h -> match_event h e)) )


let delete_404s
    (cid : Uuidm.t) (space : string) (path : string) (modifier : string) : unit
    =
  Db.run
    ~name:"delete_404s"
    "DELETE FROM stored_events_v2
      WHERE canvas_id = $1
      AND module = $2
      AND path = $3
      AND modifier = $4"
    ~params:[Db.Uuid cid; Db.String space; Db.String path; Db.String modifier]


(* ------------------------- *)
(* Input vars *)
(* ------------------------- *)
let saved_input_vars
    (h : RTT.HandlerT.handler) (request_path : string) (event : RTT.dval) :
    input_vars =
  match Handler.module_type h with
  | `Http ->
      let with_r = [("request", event)] in
      let bound =
        Libexecution.Execution.http_route_input_vars h request_path
      in
      with_r @ bound
  | `Event ->
      [("event", event)]
  | `Cron ->
      []
  | `Unknown ->
      []


let handler_trace (c : canvas) (h : RTT.HandlerT.handler) (trace_id : traceid)
    : trace =
  let event = SE.load_event_for_trace ~canvas_id:c.id trace_id in
  let ivs, timestamp =
    match event with
    | Some (request_path, timestamp, event) ->
        (saved_input_vars h request_path event, timestamp)
    | None ->
        (Execution.sample_input_vars h, Time.epoch)
  in
  let function_results =
    Stored_function_result.load ~trace_id ~canvas_id:c.id h.tlid
  in
  (trace_id, Some {input = ivs; timestamp; function_results})


let user_fn_trace (c : canvas) (fn : RTT.user_fn) (trace_id : traceid) : trace
    =
  let event =
    Stored_function_arguments.load_for_analysis
      ~canvas_id:c.id
      fn.tlid
      trace_id
  in
  let ivs, timestamp =
    match event with
    | Some (input_vars, timestamp) ->
        (input_vars, timestamp)
    | None ->
        (Execution.sample_function_input_vars fn, Time.epoch)
  in
  let function_results =
    Stored_function_result.load ~trace_id ~canvas_id:c.id fn.tlid
  in
  (trace_id, Some {input = ivs; timestamp; function_results})


let traceids_for_handler (c : canvas) (h : RTT.HandlerT.handler) : traceid list
    =
  h
  |> Handler.event_desc_for
  |> Option.map ~f:(SE.load_event_ids ~canvas_id:c.id)
  (* if it has no events, add a default *)
  |> (function Some [] -> None | x -> x)
  |> Option.value ~default:[Uuidm.v5 Uuidm.nil (string_of_id h.tlid)]


let traceids_for_user_fn (c : canvas) (fn : RTT.user_fn) : traceid list =
  Stored_function_arguments.load_traceids c.id fn.tlid


(* ------------------------- *)
(* function execution *)
(* ------------------------- *)
let execute_function
    (c : canvas) ~execution_id ~tlid ~trace_id ~caller_id ~args fnname =
  Execution.execute_function
    ~tlid
    ~execution_id
    ~trace_id
    ~dbs:(TL.dbs c.dbs)
    ~user_fns:(c.user_functions |> IDMap.data)
    ~user_tipes:(c.user_tipes |> IDMap.data)
    ~account_id:c.owner
    ~canvas_id:c.id
    ~caller_id
    ~args
    ~store_fn_arguments:
      (Stored_function_arguments.store ~canvas_id:c.id ~trace_id)
    ~store_fn_result:(Stored_function_result.store ~canvas_id:c.id ~trace_id)
    fnname


(* --------------------- *)
(* JSONable response *)
(* --------------------- *)

(* Response with miscellaneous stuff, and specific responses from tlids *)

type fofs = SE.four_oh_four list [@@deriving to_yojson]

type get_trace_data_rpc_result = {trace : trace} [@@deriving to_yojson]

let to_get_trace_data_rpc_result (c : canvas) (trace : trace) : string =
  {trace}
  |> get_trace_data_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


type get_unlocked_dbs_rpc_result = {unlocked_dbs : tlid list}
[@@deriving to_yojson]

let to_get_unlocked_dbs_rpc_result (unlocked_dbs : tlid list) (c : canvas) :
    string =
  {unlocked_dbs}
  |> get_unlocked_dbs_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


type new_trace_push = traceid_tlids [@@deriving to_yojson]

let to_new_trace_frontend (trace : traceid_tlids) : string =
  trace |> new_trace_push_to_yojson |> Yojson.Safe.to_string ~std:true


type new_404_push = SE.four_oh_four [@@deriving to_yojson]

let to_new_404_frontend (fof : SE.four_oh_four) : string =
  fof |> new_404_push_to_yojson |> Yojson.Safe.to_string ~std:true


let to_new_static_deploy_frontend (asset : SA.static_deploy) : string =
  asset |> SA.static_deploy_to_yojson |> Yojson.Safe.to_string ~std:true


(* Toplevel deletion:
 * The server announces that a toplevel is deleted by it appearing in
 * deleted_toplevels. The server announces it is no longer deleted by it
 * appearing in toplevels again. *)

(* A subset of responses to be merged in *)
type add_op_rpc_result =
  { toplevels : TL.toplevel list (* replace *)
  ; deleted_toplevels : TL.toplevel list (* replace, see note above *)
  ; user_functions : RTT.user_fn list (* replace *)
  ; deleted_user_functions : RTT.user_fn list
  ; user_tipes : RTT.user_tipe list
  ; deleted_user_tipes : RTT.user_tipe list
  (* replace, see deleted_toplevels *) }
[@@deriving to_yojson]

let to_add_op_rpc_result (c : canvas) : string =
  { toplevels = IDMap.data c.dbs @ IDMap.data c.handlers
  ; deleted_toplevels =
      IDMap.data c.deleted_handlers @ IDMap.data c.deleted_dbs
  ; user_functions = IDMap.data c.user_functions
  ; deleted_user_functions = IDMap.data c.deleted_user_functions
  ; user_tipes = IDMap.data c.user_tipes
  ; deleted_user_tipes = IDMap.data c.deleted_user_tipes }
  |> add_op_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


(* Initial load *)
type initial_load_rpc_result =
  { toplevels : TL.toplevel list
  ; deleted_toplevels : TL.toplevel list
  ; user_functions : RTT.user_fn list
  ; deleted_user_functions : RTT.user_fn list
  ; unlocked_dbs : tlid list
  ; fofs : SE.four_oh_four list
  ; traces : tlid_traceid list
  ; assets : SA.static_deploy list
  ; user_tipes : RTT.user_tipe list
  ; deleted_user_tipes : RTT.user_tipe list }
[@@deriving to_yojson]

let to_initial_load_rpc_result
    (c : canvas)
    (fofs : SE.four_oh_four list)
    (traces : tlid_traceid list)
    (unlocked_dbs : tlid list)
    (assets : SA.static_deploy list) : string =
  { toplevels = IDMap.data c.dbs @ IDMap.data c.handlers
  ; deleted_toplevels =
      IDMap.data c.deleted_handlers @ IDMap.data c.deleted_dbs
  ; user_functions = IDMap.data c.user_functions
  ; deleted_user_functions = IDMap.data c.deleted_user_functions
  ; user_tipes = IDMap.data c.user_tipes
  ; deleted_user_tipes = IDMap.data c.deleted_user_tipes
  ; unlocked_dbs
  ; fofs
  ; traces
  ; assets }
  |> initial_load_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


(* Execute function *)
type execute_function_rpc_result =
  { result : RTT.dval
  ; hash : string
  ; touched_tlids : tlid list }
[@@deriving to_yojson]

let to_execute_function_rpc_result hash touched_tlids dv : string =
  {result = dv; hash; touched_tlids}
  |> execute_function_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true
