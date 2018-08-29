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
type analysis_result = tlid * analysis list

let analysis_result_to_yojson (id, results) =
  `Assoc [ ("id", id_to_yojson id)
         ; ("results", analysis_list_to_yojson results)
         ]

(* ------------------------- *)
(* Non-execution analysis *)
(* ------------------------- *)

let unlocked (c: canvas) : tlid list =
  c.dbs
  |> TL.dbs
  |> User_db.unlocked c.id c.owner
  |> List.map ~f:(fun x -> x.tlid)

let get_404s (c: canvas) : SE.four_oh_four list =
  let events = SE.list_events c.id in
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
  |> List.map ~f:(fun e -> (e, SE.load_events c.id e))

let global_vars (c: canvas) : string list =
  c.dbs
  |> TL.dbs
  |> Execution.dbs_as_input_vars
  |> (@) Execution.sample_unknown_handler_input_vars
  |> List.map ~f:Tuple.T2.get1



(* ------------------------- *)
(* Input vars *)
(* ------------------------- *)
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



(* ------------------------- *)
(* Execution-based analysis *)
(* ------------------------- *)

let ast_analysis
    ~(exe_fn_ids: executable_fn_id list)
    ~(execution_id: id)
    (all_inputs: RTT.input_vars list)
    (c: canvas)
    (ast : RTT.expr)
    tlid =
  List.mapi all_inputs
    ~f:(fun i input_vars ->
        let exe_fn_ids =
          List.filter_map exe_fn_ids
            ~f:(fun (exe_tlid, id, cursor) ->
                if exe_tlid = tlid && i = cursor
                then Some id
                else None)
        in
        Execution.analyse_ast ast
          ~tlid
          ~exe_fn_ids
          ~execution_id
          ~input_vars
          ~dbs:(TL.dbs c.dbs)
          ~user_fns:c.user_functions
          ~account_id:c.owner
          ~canvas_id:c.id
          ~load_fn_result:Stored_function_result.load
          ~store_fn_result:Stored_function_result.store
          ~load_fn_arguments:Stored_function_arguments.load
          ~store_fn_arguments:Stored_function_arguments.store
      )

let user_fn_analysis
    ~(exe_fn_ids: executable_fn_id list)
    ~(execution_id: id)
    (c: canvas)
    (f: RTT.user_fn)
  : analysis_result =
  Log.infO "user_fn_analysis"
    ~params:[ "user_fn", show_tlid f.tlid
            ; "host", c.host
            ; "execution_id", show_id execution_id
            ; "exe_fn_ids", Log.dump exe_fn_ids
            ];
  let all_inputs = initial_input_vars_for_user_fn c f in
  let values = ast_analysis all_inputs c f.ast f.tlid
      ~exe_fn_ids ~execution_id
  in
  (f.tlid, values)

let handler_analysis
    ~(exe_fn_ids : executable_fn_id list)
    ~(execution_id: id)
    (c: canvas)
    (h : RTT.HandlerT.handler)
  : analysis_result =
  Log.infO "handler_analysis"
    ~params:[ "handler", show_tlid h.tlid
            ; "host", c.host
            ; "execution_id", show_id execution_id
            ; "exe_fn_ids", Log.dump exe_fn_ids
            ];
  let all_inputs = initial_input_vars_for_handler c h in
  let values = ast_analysis all_inputs c h.ast h.tlid
      ~exe_fn_ids ~execution_id
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

(* Toplevel deletion:
 * The server announces that a toplevel is deleted by it appearing in
 * deleted_toplevels. The server announces it is no longer deleted by it
 * appearing in toplevels again. *)

(* A subset of responses to be merged in *)
type rpc_response =
  { new_analyses: analysis_result list (* merge: overwrite existing analyses *)
  ; global_varnames : string list (* replace *)
  ; toplevels : TL.toplevel_list (* replace *)
  ; deleted_toplevels : TL.toplevel_list (* replace, see note above *)
  ; user_functions : RTT.user_fn list (* replace *)
  ; unlocked_dbs : tlid list (* replace *)
  } [@@deriving to_yojson]

type execute_function_response =
  { new_analyses: analysis_result list (* merge: overwrite existing analyses *)
  ; targets : executable_fn_id list
  } [@@deriving to_yojson]


let to_getanalysis_frontend (vals : analysis_result list)
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
  ; deleted_toplevels = c.deleted
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
