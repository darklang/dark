open Core_kernel
open Libcommon
open Libexecution
open Types
open Analysis_types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event

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
    let space, request_path, modifier, _ts = event in
    let h_space, h_name, h_modifier = h in
    Http.request_path_matches_route ~route:h_name request_path
    && h_modifier = modifier
    && h_space = space
  in
  events
  |> List.filter ~f:(fun e ->
         not (List.exists handlers ~f:(fun h -> match_event h e)) )


let delete_404s
    (c : canvas) (space : string) (path : string) (modifier : string) : unit =
  Db.run
    ~name:"delete_404s"
    "DELETE FROM stored_events_v2
      WHERE canvas_id = $1
      AND module = $2
      AND path = $3
      AND modifier = $4"
    ~params:[Db.Uuid c.id; Db.String space; Db.String path; Db.String modifier]


(* ------------------------- *)
(* Input vars *)
(* ------------------------- *)
let saved_input_vars (c : canvas) (h : RTT.HandlerT.handler) :
    (Uuidm.t * input_vars) list =
  match Handler.event_desc_for h with
  | None ->
      []
  | Some d ->
      List.map (SE.load_events c.id d) ~f:(fun (request_path, id, _ts, e) ->
          match Handler.module_type h with
          | `Http ->
              let with_r = [("request", e)] in
              let bound =
                Libexecution.Execution.http_route_input_vars h request_path
              in
              (id, with_r @ bound)
          | `Event ->
              (id, [("event", e)])
          | `Cron ->
              (id, [])
          | `Unknown ->
              (id, [])
          (* can't happen *) )


let traces_for_user_fn (c : canvas) (fn : RTT.user_fn) : trace list =
  let ivs =
    match Stored_function_arguments.load_for_analysis c.id fn.tlid with
    | [] ->
        [(Uuidm.v5 Uuidm.nil (string_of_id fn.tlid), [])]
    | ivs ->
        ivs
  in
  List.map ivs ~f:(fun (trace_id, input_vars) ->
      let function_results =
        Stored_function_result.load ~trace_id ~canvas_id:c.id fn.tlid
      in
      (trace_id, Some {input = input_vars; function_results}) )


let traces_for_handler (c : canvas) (h : RTT.HandlerT.handler) : trace list =
  (* It's really awkward to do this on the client, so just do it here for now *)
  let ivs =
    match saved_input_vars c h with
    | [] ->
        [ ( Uuidm.v5 Uuidm.nil (string_of_id h.tlid)
          , Execution.sample_input_vars h ) ]
    | ivs ->
        ivs
  in
  List.map ivs ~f:(fun (trace_id, input_vars) ->
      let function_results =
        Stored_function_result.load ~trace_id ~canvas_id:c.id h.tlid
      in
      (trace_id, Some {input = input_vars; function_results}) )


let traceids_for_handler (c : canvas) (h : RTT.HandlerT.handler) : traceid list
    =
  h
  |> Handler.event_desc_for
  |> Option.map ~f:(SE.load_event_ids ~canvas_id:c.id)
  |> Option.value ~default:[]


let traceids_for_user_fn (c : canvas) (fn : RTT.user_fn) : traceid list =
  Stored_function_arguments.load_traceids c.id fn.tlid


(* ------------------------- *)
(* function execution *)
(* ------------------------- *)
let call_function
    (c : canvas) ~execution_id ~tlid ~trace_id ~caller_id ~args fnname =
  (* TODO: Should we return a trace for the userfn? *)
  let result =
    Execution.call_function
      fnname
      ~tlid
      ~execution_id
      ~trace_id
      ~dbs:(TL.dbs c.dbs)
      ~user_fns:c.user_functions
      ~account_id:c.owner
      ~canvas_id:c.id
      ~caller_id
      ~args
  in
  Stored_function_result.store
    (tlid, fnname, caller_id)
    args
    result
    ~canvas_id:c.id
    ~trace_id ;
  result


(* --------------------- *)
(* JSONable response *)
(* --------------------- *)

(* Response with miscellaneous stuff, and specific responses from tlids *)

type fofs = SE.four_oh_four list * RTT.time [@@deriving to_yojson]

type get_trace_data_rpc_result = {traces : tlid_traces list}
[@@deriving to_yojson]

let to_get_trace_data_rpc_result (traces : tlid_traces list) (c : canvas) :
    string =
  {traces}
  |> get_trace_data_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


type get_unlocked_dbs_rpc_result = {unlocked_dbs : tlid list}
[@@deriving to_yojson]

let to_get_unlocked_dbs_rpc_result (unlocked_dbs : tlid list) (c : canvas) :
    string =
  {unlocked_dbs}
  |> get_unlocked_dbs_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


type new_trace_push = tlid_traceid [@@deriving to_yojson]

let to_new_trace_frontend (traceid : tlid_traceid) : string =
  traceid |> new_trace_push_to_yojson |> Yojson.Safe.to_string ~std:true


type new_404_push = SE.four_oh_four [@@deriving to_yojson]

let to_new_404_frontend (fof : SE.four_oh_four) : string =
  fof |> new_404_push_to_yojson |> Yojson.Safe.to_string ~std:true


(* Toplevel deletion:
 * The server announces that a toplevel is deleted by it appearing in
 * deleted_toplevels. The server announces it is no longer deleted by it
 * appearing in toplevels again. *)

(* A subset of responses to be merged in *)
type add_op_rpc_result =
  { new_traces : tlid_traces list (* merge: overwrite existing analyses *)
  ; toplevels : TL.toplevel_list (* replace *)
  ; deleted_toplevels : TL.toplevel_list (* replace, see note above *)
  ; user_functions : RTT.user_fn list (* replace *)
  ; deleted_user_functions :
      RTT.user_fn list
      (* replace, see deleted_toplevels *)
  ; unlocked_dbs : tlid list
  (* replace *) }
[@@deriving to_yojson]

let to_add_op_rpc_result
    (c : canvas) (traces : tlid_traces list) (unlocked : tlid list) : string =
  { new_traces = traces
  ; toplevels = c.dbs @ c.handlers
  ; deleted_toplevels = c.deleted_toplevels
  ; user_functions = c.user_functions
  ; deleted_user_functions = c.deleted_user_functions
  ; unlocked_dbs = unlocked }
  |> add_op_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


(* Initial load *)
type initial_load_rpc_result =
  { toplevels : TL.toplevel_list
  ; deleted_toplevels : TL.toplevel_list
  ; user_functions : RTT.user_fn list
  ; deleted_user_functions : RTT.user_fn list
  ; unlocked_dbs : tlid list
  ; fofs : SE.four_oh_four list
  ; traces : tlid_traceid list }
[@@deriving to_yojson]

let to_initial_load_rpc_result
    (c : canvas)
    (fofs : SE.four_oh_four list)
    (traces : tlid_traceid list)
    (unlocked_dbs : tlid list) : string =
  { toplevels = c.dbs @ c.handlers
  ; deleted_toplevels = c.deleted_toplevels
  ; user_functions = c.user_functions
  ; deleted_user_functions = c.deleted_user_functions
  ; unlocked_dbs
  ; fofs
  ; traces }
  |> initial_load_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


(* Execute function *)
type execute_function_rpc_result =
  { result : RTT.dval
  ; hash : string }
[@@deriving to_yojson]

let to_execute_function_rpc_result hash dv : string =
  {result = dv; hash}
  |> execute_function_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true
