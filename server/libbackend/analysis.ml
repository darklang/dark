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
(* Events *)
(* ------------------------- *)
let all_tlids (c: canvas) : tlid list =
  List.map ~f:(fun tl -> tl.tlid) (c.dbs @ c.handlers)

(* ------------------------- *)
(* Analysis types *)
(* ------------------------- *)

type executable_fn_id = tlid * id * int [@@deriving to_yojson]

(* ------------------------- *)
(* Non-execution analysis *)
(* ------------------------- *)

let unlocked (c: canvas) : tlid list =
  c.dbs
  |> TL.dbs
  |> User_db.unlocked c.id c.owner
  |> List.map ~f:(fun x -> x.tlid)

let get_404s (c: canvas) : SE.four_oh_four list =
  let events = SE.list_events ~canvas_id:c.id () in
  let handlers =
    Db.fetch
      ~name:"get_404s"
      ("SELECT module, name, modifier FROM toplevel_oplists
        WHERE canvas_id = $1
          AND module IS NOT NULL
          AND name IS NOT NULL
          AND modifier IS NOT NULL
          AND tipe = 'handler'::toplevel_type")
      ~params:[ Db.Uuid c.id]
    |> List.map ~f:(function
                    | [modu; n; modi] -> (modu, n, modi)
                    | _ -> Exception.internal "Bad DB format for get404s")
  in
  let match_event h e : bool =
    let (space, path, modifier) = e in
    let (h_space, h_path, h_modifier) = h in
    Http.path_matches_route ~path h_path
    && h_modifier = modifier
    && h_space = space
  in

  events
  |> List.filter
    ~f:(fun e ->
        not (List.exists handlers
               ~f:(fun h -> match_event h e)))
  |> List.map ~f:(fun e ->
      let events = SE.load_events c.id e
                   |> List.map ~f:Tuple.T2.get2
      in
      (e, events))

let global_vars (c: canvas) : string list =
  c.dbs
  |> TL.dbs
  |> Execution.dbs_as_input_vars
  |> (@) Execution.sample_unknown_handler_input_vars
  |> List.map ~f:Tuple.T2.get1



(* ------------------------- *)
(* Input vars *)
(* ------------------------- *)
let saved_input_vars (c: canvas) (h: RTT.HandlerT.handler) : (Uuidm.t * input_vars) list =
  match Handler.event_desc_for h with
  | None -> []
  | Some (space, path, modifier as d) ->
    List.map (SE.load_events c.id d)
      ~f:(fun (id, e) ->
          match Handler.module_type h with
          | `Http ->
            let with_r = [("request", e)] in
            let bound = Libexecution.Execution.http_route_input_vars h path in
            (id, with_r @ bound)
          | `Event ->
            (id, [("event", e)])
          | `Cron  -> (id, [])
          | `Unknown -> (id, []) (* can't happen *)
      )

let initial_input_vars_for_handler (c: canvas) (h: RTT.HandlerT.handler)
  : RTT.input_vars list =
  match saved_input_vars c h with
  | [] -> [Execution.sample_input_vars h]
  | l -> List.map ~f:Tuple.T2.get2 l

let initial_input_vars_for_user_fn (c: canvas) (fn: RTT.user_fn)
  : RTT.input_vars list =
  Stored_function_arguments.load ~canvas_id:c.id fn.tlid
  |> List.map ~f:(fun (m, _ts) -> RTT.DvalMap.to_alist m)

let traces_for_handler (c: canvas) (h: RTT.HandlerT.handler)
  : trace list =
  match saved_input_vars c h with
  | [] -> []
  | ivs ->
    List.map ivs
      ~f:(fun (trace_id, input_vars) ->
          let function_results =
            Stored_function_result.load
              ~trace_id
              ~canvas_id:c.id
              h.tlid
          in
          { input = input_vars
          ; function_results
          ; id = trace_id
          })



(* --------------------- *)
(* JSONable response *)
(* --------------------- *)

(* The full response with everything *)
type get_analysis_response =
  { traces : tlid_trace list
  ; global_varnames : string list
  ; unlocked_dbs : tlid list
  ; fofs : SE.four_oh_four list [@key "404s"]
  } [@@deriving to_yojson]

(* Toplevel deletion:
 * The server announces that a toplevel is deleted by it appearing in
 * deleted_toplevels. The server announces it is no longer deleted by it
 * appearing in toplevels again. *)

(* A subset of responses to be merged in *)
type rpc_response =
  { new_traces : tlid_trace list (* merge: overwrite existing analyses *)
  ; global_varnames : string list (* replace *)
  ; toplevels : TL.toplevel_list (* replace *)
  ; deleted_toplevels : TL.toplevel_list (* replace, see note above *)
  ; user_functions : RTT.user_fn list (* replace *)
  ; unlocked_dbs : tlid list (* replace *)
  } [@@deriving to_yojson]

type execute_function_response =
  { new_traces : tlid_trace list (* merge: overwrite existing analyses *)
  ; targets : executable_fn_id list
  } [@@deriving to_yojson]


let to_getanalysis_frontend (traces: tlid_trace list)
      (unlocked : tlid list)
      (f404s: SE.four_oh_four list)
      (c : canvas) : string =
  { traces
  ; global_varnames = global_vars c
  ; unlocked_dbs = unlocked
  ; fofs = f404s
  }
  |> get_analysis_response_to_yojson
  |> Yojson.Safe.to_string ~std:true


let to_rpc_response_frontend (c: canvas) (traces: tlid_trace list)
    (unlocked : tlid list)
  : string =
  { new_traces = traces
  ; global_varnames = global_vars c
  ; toplevels = c.dbs @ c.handlers
  ; deleted_toplevels = c.deleted
  ; user_functions = c.user_functions
  ; unlocked_dbs = unlocked
  }
  |> rpc_response_to_yojson
  |> Yojson.Safe.to_string ~std:true

let to_execute_function_response_frontend (targets : executable_fn_id list) (traces: tlid_trace list)
  : string =
  { new_traces = traces
  ; targets = targets
  }
  |> execute_function_response_to_yojson
  |> Yojson.Safe.to_string ~std:true
