module LibBackend.Analysis

// Most of this file is being merged into Api.fs

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module AT = LibExecution.AnalysisTypes
module PT = LibBackend.ProgramTypes

// open Analysis_types
// module RTT = Types.RuntimeT
// module TL = Toplevel
// module PReq = Parsed_request
// module SE = Stored_event
// module SA = Static_assets

// -------------------------
// Non-execution analysis *)
// -------------------------

// type db_stat =
//   { count : int
//   ; example : (RTT.dval * string) option }
// [@@deriving eq, show, yojson]
//
// type db_stat_map = db_stat IDMap.t [@@deriving eq, show, yojson]
//
// let db_stats (c : Canvas.canvas) (tlids : tlid list) : db_stat_map =
//   List.fold
//     ~init:IDMap.empty
//     ~f:(fun map tlid ->
//       let db = IDMap.find c.dbs tlid |> Option.bind ~f:TL.as_db in
//       match (db, IDMap.find map tlid) with
//       | Some db, None ->
//           let account_id, canvas_id = (c.owner, c.id) in
//           let count = UserDB.stats_count ~account_id ~canvas_id db in
//           let example = UserDB.stats_pluck ~account_id ~canvas_id db in
//           IDMap.add_exn ~data:{count; example} ~key:tlid map
//       | _ ->
//           map)
//     tlids
//
//
// type worker_stat = {count : int} [@@deriving show, yojson]
//
// let worker_stats (canvas_id : Uuidm.t) (tlid : tlid) : worker_stat =
//   let count =
//     Db.fetch_one
//       ~name:"count_workers"
//       ~subject:(show_tlid tlid)
//       "SELECT COUNT(1) AS num
//       FROM events E
//       INNER JOIN toplevel_oplists TL
//         ON TL.canvas_id = E.canvas_id
//         AND TL.module = E.space
//         AND TL.name = E.name
//       WHERE TL.tlid = $1
//       AND TL.canvas_id = $2
//       AND E.status IN('new', 'scheduled')"
//       ~params:[Db.ID tlid; Db.Uuid canvas_id]
//     |> List.hd_exn
//     |> int_of_string
//   in
//   {count}
//
//

// -------------------------
// Input vars
// -------------------------
let incomplete = RT.DFakeVal(RT.DIncomplete RT.SourceNone)
let sampleRequest : LibExecution.ParsedRequest.T =
  RT.Dval.obj [ ("body", incomplete)
                ("jsonBody", incomplete)
                ("formBody", incomplete)
                ("queryParams", incomplete)
                ("headers", incomplete)
                ("fullBody", incomplete)
                ("url", incomplete) ]

let sampleRequestInputVars : AT.InputVars = [ ("request", sampleRequest) ]

let sampleEventInputVars : AT.InputVars = [ ("event", RT.DFakeVal(RT.DIncomplete RT.SourceNone)) ]

let sampleModuleInputVars (h : PT.Handler.T) : AT.InputVars =
  match h.spec with
  | PT.Handler.HTTP _ -> sampleRequestInputVars
  | PT.Handler.Cron _ -> []
  | PT.Handler.REPL _ -> []
  | PT.Handler.Worker _
  | PT.Handler.OldWorker _ -> sampleEventInputVars

let sampleRouteInputVars (h : PT.Handler.T) : AT.InputVars =
  match h.spec with
  | PT.Handler.HTTP (route, _, _) ->
      route
      |> Routing.routeVariables
      |> List.map (fun k -> (k, RT.DFakeVal(RT.DIncomplete RT.SourceNone)))
  | _ -> []

let sampleInputVars (h : PT.Handler.T) : AT.InputVars =
  sampleModuleInputVars h @ sampleRouteInputVars h

let sampleFunctionInputVars (f : PT.UserFunction.T) : AT.InputVars =
  f.parameters
  |> List.map (fun p -> (p.name, incomplete))

let savedInputVars
  (h : PT.Handler.T)
  (requestPath : string)
  (event : RT.Dval)
  : AT.InputVars =
  match h.spec with
  | PT.Handler.HTTP (route, method, _) ->
      let withR = [ ("request", event) ] in

      let bound =
        if route = "" then
          []
        else
          (
          // Check the trace actually matches the route, if not the client
          // has made a mistake in matching the traceid to this handler, but
          // that might happen due to a race condition. If it does, carry
          // on, if it doesn't -- just don't do any bindings and inject the
          // sample variables. Communicating to the frontend that this
          // trace doesn't match the handler should be done in the future
          // somehow.
          if Routing.requestPathMatchesRoute route requestPath then
            Routing.routeInputVars route requestPath |> Option.unwrapUnsafe
          else
            sampleRouteInputVars h)

      withR @ bound
  | PT.Handler.Worker _ -> [ ("event", event) ]
  | PT.Handler.Cron _ -> []
  | PT.Handler.REPL _ -> []
  | PT.Handler.OldWorker _ -> []

let handlerTrace
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (h : PT.Handler.T)
  : Task<AT.Trace> =
  task {
    let! event = TraceInputs.loadEventForTrace canvasID traceID

    let input, timestamp =
      match event with
      | Some (requestPath, timestamp, event) ->
          (savedInputVars h requestPath event, timestamp)
      | None -> (sampleInputVars h, System.DateTime.UnixEpoch)

    let! functionResults = TraceFunctionResults.load canvasID traceID h.tlid

    return
      (traceID,
       Some
         { input = input; timestamp = timestamp; function_results = functionResults })
  }


let userfnTrace
  (canvasID : CanvasID)
  (traceID : AT.TraceID)
  (fn : PT.UserFunction.T)
  : Task<AT.Trace> =
  task {
    let! event = TraceFunctionArguments.loadForAnalysis canvasID traceID fn.tlid

    let ivs, timestamp =
      match event with
      | Some (inputVars, timestamp) -> (inputVars, timestamp)
      | None -> (sampleFunctionInputVars fn, System.DateTime.UnixEpoch)

    let! functionResults = TraceFunctionResults.load canvasID traceID fn.tlid

    return
      (traceID,
       Some
         { input = ivs; timestamp = timestamp; function_results = functionResults })
  }


let traceIDofTLID (tlid : tlid) : AT.TraceID =
  // This was what we originally used in OCaml, so I guess we're stuck with it.
  let nilNamespace = "00000000-0000-0000-0000-000000000000"
  // Literally the only package I could find that does v5 UUIDs
  System.GuidEx.op_Implicit (System.GuidEx(tlid.ToString(), nilNamespace))


let traceIDsForHandler (c : Canvas.T) (h : PT.Handler.T) :
    Task<List<AT.TraceID>> =
  task {
    match h.spec.toDesc() with
    | Some desc ->
        let! events = TraceInputs.loadEventIDs c.id desc in
        return
          events
          |> List.filterMap (fun (traceID, path) ->
               match h.spec with
               | PT.Handler.Spec.HTTP _ ->
                   // Ensure we only return trace_ids that would bind to this handler
                   // if the trace was executed for real now
                   c.handlers
                   (* Filter and order the handlers that would match the trace's path *)
                   |> Map.values
                   |> Routing.filterMatchingHandlers path
                   |> List.head
                   |> Option.bind (fun matching ->
                        if matching.tlid = h.tlid then Some traceID else None)
               | _ ->
                 // Don't use HTTP filtering stack for non-HTTP traces
                 Some traceID)
        // If there's no matching traces, add the default trace
        |> (function [] -> [traceIDofTLID h.tlid] | x -> x)
    | None ->
        (* If the event description isn't complete, add the default trace *)
        return [traceIDofTLID h.tlid]
    }


let traceIDsForUserFn (canvasID : CanvasID) (fnTLID : tlid) : Task<List<AT.TraceID>> =
  TraceFunctionArguments.loadTraceIDs canvasID fnTLID


// ------------------------
// function execution
// ------------------------
// let execute_function
//     (c : Canvas.canvas) ~execution_id ~tlid ~trace_id ~caller_id ~args fnname =
//   Execution.execute_function
//     ~tlid
//     ~execution_id
//     ~trace_id
//     ~dbs:(TL.dbs c.dbs)
//     ~user_fns:(c.user_functions |> IDMap.data)
//     ~userTypes:(c.userTypes |> IDMap.data)
//     ~package_fns:c.package_fns
//     ~secrets:(Secret.secrets_in_canvas c.id)
//     ~account_id:c.owner
//     ~canvas_id:c.id
//     ~caller_id
//     ~args
//     ~store_fn_arguments:
//       (Stored_function_arguments.store ~canvas_id:c.id ~trace_id)
//     ~store_fn_result:(Stored_function_result.store ~canvas_id:c.id ~trace_id)
//     fnname
//
//
// (* --------------------- *)
// (* JSONable response *)
// (* --------------------- *)
//
// (* Response with miscellaneous stuff, and specific responses from tlids *)
//
// type fofs = SE.four_oh_four list [@@deriving to_yojson]
//
// type get_trace_data_rpc_result = {trace : trace} [@@deriving to_yojson]
//
// let to_get_trace_data_rpc_result (c : Canvas.canvas) (trace : trace) : string =
//   {trace}
//   |> get_trace_data_rpc_result_to_yojson
//   |> Yojson.Safe.to_string ~std:true
//
//
// type get_unlocked_dbs_rpc_result = {unlocked_dbs : tlid list}
// [@@deriving to_yojson]
//
// let to_get_unlocked_dbs_rpc_result (unlocked_dbs : tlid list) : string =
//   {unlocked_dbs}
//   |> get_unlocked_dbs_rpc_result_to_yojson
//   |> Yojson.Safe.to_string ~std:true
//
//
// let to_db_stats_rpc_result (stats : db_stat_map) : string =
//   stats |> db_stat_map_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// let to_worker_stats_rpc_result (stats : worker_stat) : string =
//   stats |> worker_stat_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// type new_trace_push = traceid_tlids [@@deriving to_yojson]
//
// let to_new_trace_frontend (trace : traceid_tlids) : string =
//   trace |> new_trace_push_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// type new_404_push = SE.four_oh_four [@@deriving to_yojson]
//
// let to_new_404_frontend (fof : SE.four_oh_four) : string =
//   fof |> new_404_push_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// let to_new_static_deploy_frontend (asset : SA.static_deploy) : string =
//   asset |> SA.static_deploy_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// let to_worker_schedules_push (ws : Event_queue.Worker_states.t) : string =
//   ws |> Event_queue.Worker_states.to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// (* Toplevel deletion:
//  * The server announces that a toplevel is deleted by it appearing in
//  * deleted_toplevels. The server announces it is no longer deleted by it
//  * appearing in toplevels again. *)
//
// (* A subset of responses to be merged in *)
// type add_op_rpc_result =
//   { toplevels : TL.toplevel list (* replace *)
//   ; deleted_toplevels : TL.toplevel list (* replace, see note above *)
//   ; user_functions : RTT.user_fn list (* replace *)
//   ; deleted_user_functions : RTT.user_fn list
//   ; userTypes : RTT.user_tipe list
//   ; deletedUserTypes : RTT.user_tipe list (* replace, see deleted_toplevels *)
//   }
// [@@deriving to_yojson]
//
// let empty_to_add_op_rpc_result =
//   { toplevels = []
//   ; deleted_toplevels = []
//   ; user_functions = []
//   ; deleted_user_functions = []
//   ; userTypes = []
//   ; deletedUserTypes = [] }
//
//
// type add_op_stroller_msg =
//   { result : add_op_rpc_result
//   ; params : Api.add_op_rpc_params }
// [@@deriving to_yojson]
//
// let to_add_op_rpc_result (c : Canvas.canvas) : add_op_rpc_result =
//   { toplevels = IDMap.data c.dbs @ IDMap.data c.handlers
//   ; deleted_toplevels = IDMap.data c.deleted_handlers @ IDMap.data c.deleted_dbs
//   ; user_functions = IDMap.data c.user_functions
//   ; deleted_user_functions = IDMap.data c.deleted_user_functions
//   ; userTypes = IDMap.data c.userTypes
//   ; deletedUserTypes = IDMap.data c.deletedUserTypes }
//
//
// type all_traces_result = {traces : tlid_traceid list} [@@deriving to_yojson]
//
// let to_all_traces_result (traces : tlid_traceid list) : string =
//   {traces} |> all_traces_result_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// type get_404s_result = {f404s : fofs} [@@deriving to_yojson]
//
// let to_get_404s_result (f404s : fofs) : string =
//   {f404s} |> get_404s_result_to_yojson |> Yojson.Safe.to_string ~std:true
//
//
// type time = Time.t
//
// (* Warning: both to_string and date_of_string might raise; we could use _option types instead, but since we are using  this for encoding/decoding typed data, I do not think that is necessary right now *)
// let time_of_yojson (j : Yojson.Safe.t) : time =
//   j
//   (* NOTE: Safe.Util; this is "get a string from a (`String of string)", not "stringify an arbitrary Yojson object" *)
//   |> Yojson.Safe.Util.to_string
//   |> Util.date_of_isostring
//
//
// let time_to_yojson (time : time) : Yojson.Safe.t =
//   time |> Util.isostring_of_date |> fun s -> `String s
//
//
// (* Initial load *)
// type initial_load_rpc_result =
//   { toplevels : TL.toplevel list
//   ; deleted_toplevels : TL.toplevel list
//   ; user_functions : RTT.user_fn list
//   ; deleted_user_functions : RTT.user_fn list
//   ; unlocked_dbs : tlid list
//   ; assets : SA.static_deploy list
//   ; userTypes : RTT.user_tipe list
//   ; deletedUserTypes : RTT.user_tipe list
//   ; op_ctrs : (string * int) list
//   ; permission : Authorization.permission option
//   ; account : Account.user_info
//   ; canvas_list : string list
//   ; orgs : string list
//   ; org_canvas_list : string list
//   ; worker_schedules : Event_queue.Worker_states.t
//   ; secrets : RTT.secret list
//   ; creation_date : time }
// [@@deriving to_yojson]
//
// let to_initial_load_rpc_result
//     (c : Canvas.canvas)
//     (op_ctrs : (string * int) list)
//     (permission : Authorization.permission option)
//     (unlocked_dbs : tlid list)
//     (assets : SA.static_deploy list)
//     (account : Account.user_info)
//     (canvas_list : string list)
//     (orgs : string list)
//     (org_canvas_list : string list)
//     (worker_schedules : Event_queue.Worker_states.t)
//     (secrets : RTT.secret list) : string =
//   { toplevels = IDMap.data c.dbs @ IDMap.data c.handlers
//   ; deleted_toplevels = IDMap.data c.deleted_handlers @ IDMap.data c.deleted_dbs
//   ; user_functions = IDMap.data c.user_functions
//   ; deleted_user_functions = IDMap.data c.deleted_user_functions
//   ; userTypes = IDMap.data c.userTypes
//   ; deletedUserTypes = IDMap.data c.deletedUserTypes
//   ; unlocked_dbs
//   ; assets
//   ; op_ctrs
//   ; permission
//   ; account
//   ; canvas_list
//   ; orgs
//   ; org_canvas_list
//   ; worker_schedules
//   ; secrets
//   ; creation_date = c.creation_date }
//   |> initial_load_rpc_result_to_yojson
//   |> Yojson.Safe.to_string ~std:true
//
//
// (* Execute function *)
// type execute_function_rpc_result =
//   { result : RTT.dval
//   ; hash : string
//   ; hashVersion : int
//   ; touched_tlids : tlid list
//   ; unlocked_dbs : tlid list }
// [@@deriving to_yojson]
//
// let to_execute_function_rpc_result
//     hash (hashVersion : int) touched_tlids unlocked_dbs dv : string =
//   {result = dv; hash; hashVersion; touched_tlids; unlocked_dbs}
//   |> execute_function_rpc_result_to_yojson
//   |> Yojson.Safe.to_string ~std:true
//
//
// type trigger_handler_rpc_result = {touched_tlids : tlid list}
// [@@deriving to_yojson]
//
// let to_trigger_handler_rpc_result touched_tlids : string =
//   {touched_tlids}
//   |> trigger_handler_rpc_result_to_yojson
//   |> Yojson.Safe.to_string ~std:true
//
