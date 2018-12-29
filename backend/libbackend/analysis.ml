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
  let match_event h e : bool =
    let space, path, modifier, _ts = e in
    let h_space, h_path, h_modifier = h in
    Http.path_matches_route ~path h_path
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
  | Some ((space, path, modifier) as d) ->
      List.map (SE.load_events c.id d) ~f:(fun (id, e) ->
          match Handler.module_type h with
          | `Http ->
              let with_r = [("request", e)] in
              let bound =
                Libexecution.Execution.http_route_input_vars h path
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
      {input = input_vars; function_results; id = trace_id} )


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
      {input = input_vars; function_results; id = trace_id} )


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
type get_analysis_response =
  { traces : tlid_trace list
  ; unlocked_dbs : tlid list
  ; fofs : SE.four_oh_four list * RTT.time [@key "404s"] }
[@@deriving to_yojson]

let to_getanalysis_frontend
    (req_time : RTT.time)
    (traces : tlid_trace list)
    (unlocked : tlid list)
    (f404s : SE.four_oh_four list)
    (c : canvas) : string =
  {traces; unlocked_dbs = unlocked; fofs = (f404s, req_time)}
  |> get_analysis_response_to_yojson
  |> Yojson.Safe.to_string ~std:true


type new_traces_push = tlid_traceids list [@@deriving to_yojson]

let to_new_traces_frontend (traceids : tlid_traceids list) : string =
  traceids |> new_traces_push_to_yojson |> Yojson.Safe.to_string ~std:true


(* Toplevel deletion:
 * The server announces that a toplevel is deleted by it appearing in
 * deleted_toplevels. The server announces it is no longer deleted by it
 * appearing in toplevels again. *)

(* A subset of responses to be merged in *)
type rpc_response =
  { new_traces : tlid_trace list (* merge: overwrite existing analyses *)
  ; toplevels : TL.toplevel_list (* replace *)
  ; deleted_toplevels : TL.toplevel_list (* replace, see note above *)
  ; user_functions : RTT.user_fn list (* replace *)
  ; unlocked_dbs : tlid list
  (* replace *) }
[@@deriving to_yojson]

let to_rpc_response_frontend
    (c : canvas) (traces : tlid_trace list) (unlocked : tlid list) : string =
  { new_traces = traces
  ; toplevels = c.dbs @ c.handlers
  ; deleted_toplevels = c.deleted
  ; user_functions = c.user_functions
  ; unlocked_dbs = unlocked }
  |> rpc_response_to_yojson
  |> Yojson.Safe.to_string ~std:true


type execute_function_response =
  { result : RTT.dval
  ; hash : string }
[@@deriving to_yojson]

let to_execute_function_response_frontend hash dv : string =
  {result = dv; hash}
  |> execute_function_response_to_yojson
  |> Yojson.Safe.to_string ~std:true
