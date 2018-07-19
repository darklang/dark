open Core_kernel
open Libcommon
open Libexecution

open Types
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
(* Execution/analysis *)
(* ------------------------- *)

type executable_fn_id = (tlid * id * int) [@@deriving to_yojson]
type analysis_result = tlid * Ast_analysis.analysis list

let analysis_result_to_yojson (id, results) =
  `Assoc [ ("id", `Int id)
         ; ("results", Ast_analysis.analysis_list_to_yojson results)
         ]
let global_vars (c: canvas) : string list =
  RTT.DvalMap.keys (Execution.default_env c)

let unlocked (c: canvas) : tlid list =
  c.dbs
  |> TL.dbs
  |> User_db.unlocked c.id c.owner
  |> List.map ~f:(fun x -> x.tlid)

let get_404s (c: canvas) : SE.four_oh_four list =
  let match_desc h d : bool =
    let (space, path, modifier) = d in
    match Handler.event_desc_for h with
    | Some (h_space, h_path, h_modifier) ->
      Http.path_matches_route ~path h_path
      && h_modifier = modifier
      && h_space = space
    | None -> false
  in

  let unused_descs =
    SE.list_events c.id
    |> List.filter
      ~f:(fun d ->
          not (List.exists (TL.handlers c.handlers)
                 ~f:(fun h -> match_desc h d)))
    |> List.map ~f:(fun d -> (d, SE.load_events c.id d))
  in
  unused_descs

let function_analysis
    ~(exe_fn_ids: executable_fn_id list)
    ~(execution_id: id)
    (c: canvas)
    (f: RTT.user_fn)
  : analysis_result =
  let fn_ids =
    exe_fn_ids
    |> List.filter_map
      ~f:(fun (tlid, id, _) ->
          if tlid = f.tlid
          then Some id
          else None)
  in
  let env =
    Ast_analysis.environment_for_user_fn f
    |> Util.merge_left (Execution.initial_env c)
  in
  let state =
    Execution.state_for_analysis f.tlid
      ~c ~input_cursor:0 ~execution_id ~exe_fn_ids:fn_ids ~env
  in
  (f.tlid, [Ast_analysis.execute_function_for_analysis state f])

let handler_analysis
    ~(exe_fn_ids : executable_fn_id list)
    ~(execution_id: id)
    (c: canvas)
    (h : RTT.HandlerT.handler)
  : analysis_result
   =
  Log.infO "handler_analysis"
    ~params:[ "handler", show_tlid h.tlid
            ; "host", c.host
            ; "execution_id", show_id execution_id
            ; "exe_fn_ids", Log.dump exe_fn_ids
            ];
  let fn_ids i =
    List.filter_map exe_fn_ids
      ~f:(fun (tlid, id, cursor) ->
          if tlid = h.tlid && i = cursor
          then Some id
          else None)
  in
  let state i env : RTT.exec_state =
    Execution.state_for_analysis h.tlid
      ~c ~input_cursor:i ~exe_fn_ids:(fn_ids i) ~execution_id ~env
  in
  let envs = Execution.initial_envs_for_handler c h in
  let values =
    List.mapi
      ~f:(fun i env ->
          Ast_analysis.execute_handler_for_analysis (state i env) h)
      envs
  in
  (h.tlid, values)



(* --------------------- *)
(* JSONable response *)
(* --------------------- *)

(* The full response with everything *)
type get_analysis_response =
  { analyses: analysis_result list
  ; global_varnames : string list
  ; unlocked_dbs : tlid list
  ; fofs : SE.four_oh_four list [@key "404s"]
  } [@@deriving to_yojson]

(* A subset of responses to be merged in *)
type rpc_response =
  { new_analyses: analysis_result list (* merge: overwrite existing analyses *)
  ; global_varnames : string list (* replace *)
  ; toplevels : TL.toplevel_list (* replace *)
  ; user_functions : RTT.user_fn list (* replace *)
  ; unlocked_dbs : tlid list (* replace *)
  } [@@deriving to_yojson]

type execute_function_response =
  { new_analyses: analysis_result list (* merge: overwrite existing analyses *)
  ; targets : executable_fn_id list
  } [@@deriving to_yojson]


let to_get_analysis_frontend (vals : analysis_result list)
      (unlocked : tlid list)
      (f404s: SE.four_oh_four list)
      (c : canvas) : string =
  { analyses = vals
  ; global_varnames = global_vars c
  ; unlocked_dbs = unlocked
  ; fofs = f404s
  }
  |> get_analysis_response_to_yojson
  |> Yojson.Safe.to_string ~std:true


let to_rpc_response_frontend (c : canvas) (vals : analysis_result list)
    (unlocked : tlid list)
  : string =
  { new_analyses = vals
  ; global_varnames = global_vars c
  ; toplevels = c.dbs @ c.handlers
  ; user_functions = c.user_functions
  ; unlocked_dbs = unlocked
  }
  |> rpc_response_to_yojson
  |> Yojson.Safe.to_string ~std:true

let to_execute_function_response_frontend (targets : executable_fn_id list) (vals : analysis_result list)
  : string =
  { new_analyses = vals
  ; targets = targets
  }
  |> execute_function_response_to_yojson
  |> Yojson.Safe.to_string ~std:true
