// This file is being split between ApiServer.fs and BwdServer.fs. I'll delete from it as it's ported.


(* ------------------------------- *)
(* utils *)
(* ------------------------------- *)

type timing_header = string * float * string

let shutdown = ref false

let ready = ref false

let server_timing (times : timing_header list) =
  times
  |> List.map ~f:(fun (name, time, desc) ->
         Printf.sprintf "%s;desc=\"%s\";dur=%0.2f" name desc time)
  |> String.concat ~sep:","
  |> fun x -> [("Server-timing", x)] |> Header.of_list


let time (name : string) (fn : _ -> 'a) : timing_header * 'a =
  let start = Unix.gettimeofday () in
  let result = fn () in
  let finish = Unix.gettimeofday () in
  ((name, (finish -. start) *. 1000.0, name), result)


let get_ip_address (ch : Conduit_lwt_unix.flow) : string =
  match Conduit_lwt_unix.endp_of_flow ch with
  | `TCP (ip, port) ->
      Ipaddr.to_string ip
  | _ ->
      assert false


let request_to_rollbar (body : string) (req : CRequest.t) :
    Libservice.Rollbar.request_data =
  { body
  ; headers = req |> CRequest.headers |> Cohttp.Header.to_list
  ; url = req |> CRequest.uri |> Uri.to_string
  ; http_method = req |> CRequest.meth |> Cohttp.Code.string_of_method }


type response_or_redirect_params =
  | Respond of
      { resp_headers : Header.t
      ; execution_id : Types.id
      ; status : Cohttp.Code.status_code
      ; body : string }
  | Redirect of
      { uri : Uri.t
      ; headers : Header.t option }

let respond_or_redirect (parent : Span.t) (params : response_or_redirect_params)
    =
  match params with
  | Redirect {uri; headers} ->
      Span.set_attr parent "response.status" (`Int 302) ;
      S.respond_redirect ?headers ~uri ()
  | Respond {resp_headers; execution_id; status; body} ->
      let resp_headers =
        Header.add_list
          resp_headers
          [(Libshared.Header.execution_id, Types.string_of_id execution_id)]
      in
      (* add Content-Length if missing, e.g. when function is called directly
       * and not from `respond_or_redirect_empty_body`
       *)
      let resp_headers =
        if Header.get resp_headers "Content-Length" = None
        then
          Header.add
            resp_headers
            "Content-Length"
            (string_of_int (String.length body))
        else resp_headers
      in
      Span.set_attrs
        parent
        [ ("response.status", `Int (Cohttp.Code.code_of_status status))
        ; ("response.content_length", `Int (String.length body)) ] ;
      ( match Header.get resp_headers "content-type" with
      | Some ct ->
          Span.set_attr parent "response.content_type" (`String ct)
      | None ->
          () ) ;
      S.respond_string ~status ~body ~headers:resp_headers ()


let respond_or_redirect_empty_body
    (span : Span.t) (params : response_or_redirect_params) =
  match params with
  | Redirect _ ->
      respond_or_redirect span params
  | Respond r ->
      let headers =
        Header.add
          r.resp_headers
          "Content-Length"
          (string_of_int (String.length r.body))
      in
      respond_or_redirect
        span
        (Respond {r with body = ""; resp_headers = headers})


let respond
    ?(resp_headers = Header.init ())
    ~(execution_id : Types.id)
    (span : Span.t)
    status
    (body : string) =
  respond_or_redirect span (Respond {resp_headers; execution_id; status; body})


type host_route =
  | Canvas of string
  | Static
  | Admin

let redirect_to uri =
  let proto = uri |> Uri.scheme |> Option.value ~default:"" in
  (* If it's http and on a domain that can be served with https,
     we want to redirect to the same url but with the scheme
     replaced by "https". *)
  if proto = "http" && should_use_https uri
  then Some "https" |> Uri.with_scheme uri |> Some
  else None


(* Proxies that terminate HTTPs should give us X-Forwarded-Proto: http
   or X-Forwarded-Proto: https.

   Return the URI, adding the scheme to the URI if there is an X-Forwarded-Proto. *)
let with_x_forwarded_proto req =
  let uri = CRequest.uri req in
  match Header.get (CRequest.headers req) "X-Forwarded-Proto" with
  | Some proto ->
      Uri.with_scheme uri (Some proto)
  | None ->
    ( match Uri.scheme uri with
    | Some _ ->
        uri
    | None ->
        Uri.with_scheme uri (Some "http") )


(* Currently we just ensure that the Uri scheme is set *)
let canonicalize_request request =
  let new_uri = with_x_forwarded_proto request in
  let new_req =
    CRequest.make
      ~meth:(CRequest.meth request)
      ~version:(CRequest.version request)
      ~encoding:(CRequest.encoding request)
      ~headers:(CRequest.headers request)
      new_uri
  in
  (* Somewhat unbelievable, but CRequest.make strips the scheme (eg https)
   * from the uri, so we need to add it back in. *)
  {new_req with resource = Uri.to_string new_uri}


(* sanitize both repeated '/' and final '/'.
   "/foo//bar/" -> "/foo/bar"
   but leave "/" [root] untouched *)
let sanitize_uri_path path : string =
  path
  |> (fun str -> Re2.replace_exn (Re2.create_exn "/+") str ~f:(fun _ -> "/"))
  |> fun str -> if str = "/" then str else Util.maybe_chop_suffix "/" str



(* -------------------------------------------- *)
(* Admin server *)
(* -------------------------------------------- *)
let static_assets_upload_handler
    ~(execution_id : Types.id)
    ~(user : Account.user_info)
    (parent : Span.t)
    (canvas : string)
    req
    body : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    match try Some (Canvas.id_for_name canvas) with _ -> None with
    | None ->
        respond
          parent
          ~execution_id
          `Not_found
          "No canvas with this name exists"
    | Some canvas ->
        let ct =
          match Cohttp.Header.get (CRequest.headers req) "content-type" with
          | Some s ->
              s
          | None ->
              "error"
        in
        (* making branch a request-configurable option requires product work:
        * https://trello.com/c/pAD4uoJc/520-figure-out-branch-feature-for-static-assets
       *)
        let branch = "main" in
        let sa = Static_assets.start_static_asset_deploy ~user canvas branch in
        Stroller.push_new_static_deploy ~execution_id ~canvas_id:canvas sa ;
        let deploy_hash = sa.deploy_hash in
        let%lwt stream =
          Multipart.parse_stream (Lwt_stream.of_list [body]) ct
        in
        let%lwt upload_results =
          let%lwt parts = Multipart.get_parts stream in
          let files =
            (Multipart.StringMap.filter (fun _ v ->
                 match v with `File _ -> true | `String _ -> false))
              parts
          in
          let files =
            Multipart.StringMap.fold
              (fun _ v acc ->
                List.cons
                  ( match v with
                  | `File f ->
                      f
                  | _ ->
                      Exception.internal "didn't expect a non-`File here" )
                  acc)
              files
              ([] : Multipart.file List.t)
          in
          let processfile file =
            let filename = Multipart.file_name file in
            (* file_stream gives us a stream of strings; get a single string out
              of it *)
            let%lwt body =
              Lwt_stream.fold_s
                (fun elt acc -> Lwt.return (acc ^ elt))
                (Multipart.file_stream file)
                ""
            in
            (* Replace DARK_STATIC_ASSETS_BASE_URL with the deployed URL. This
             * will allow users to create SPAs with a sentinel value in them to
             * converts to the absolute url. In React, you would do this with
             * PUBLIC_URL. *)
            let body =
              let filetype = Magic_mime.lookup filename in
              let is_valid_text body =
                body |> Libexecution.Unicode_string.of_string |> Option.is_some
              in
              (* Other mime type prefixes are video, image, audio,
               * chemical, model, x-conference and can be ignored without
               * the expensive conversion check *)
              if String.is_prefix ~prefix:"video" filetype
                 || String.is_prefix ~prefix:"image" filetype
                 || String.is_prefix ~prefix:"audio" filetype
                 || String.is_prefix ~prefix:"chemical" filetype
                 || String.is_prefix ~prefix:"model" filetype
                 || String.is_prefix ~prefix:"x-conference" filetype
              then body
              else if String.is_prefix ~prefix:"text" filetype
                      || is_valid_text body
                      (* application/ or unknown and valid UTF-8*)
              then
                String.substr_replace_all
                  body
                  ~pattern:"DARK_STATIC_ASSETS_BASE_URL"
                  ~with_:sa.url
              else (* application/* or unknown and _not_ valid UTF-8 *)
                body
            in
            Static_assets.upload_to_bucket filename body canvas deploy_hash
          in
          Lwt.return (files |> List.map ~f:processfile)
        in
        let%lwt _, errors =
          upload_results
          |> Lwt_list.partition_p (fun r ->
                 match%lwt r with
                 | Ok _ ->
                     Lwt.return true
                 | Error _ ->
                     Lwt.return false)
        in
        let deploy =
          Static_assets.finish_static_asset_deploy canvas deploy_hash
        in
        Stroller.push_new_static_deploy ~execution_id ~canvas_id:canvas deploy ;
        ( match errors with
        | [] ->
            respond
              ~execution_id
              parent
              `OK
              ( Yojson.Safe.to_string
                  (`Assoc
                    [ ("deploy_hash", `String deploy_hash)
                    ; ( "url"
                      , `String (Static_assets.url canvas deploy_hash `Short) )
                    ; ( "long-url"
                      , `String (Static_assets.url canvas deploy_hash `Long) )
                    ])
              |> Yojson.Basic.prettify )
        | _ ->
            let err_strs =
              errors
              |> Lwt_list.map_p (fun e ->
                     match%lwt e with
                     | Error (`GcloudAuthError s) ->
                         Lwt.return s
                     | Error (`FailureUploadingStaticAsset s) ->
                         Lwt.return s
                     | Error (`FailureDeletingStaticAsset s) ->
                         Lwt.return s
                     | Ok _ ->
                         Exception.internal
                           "Can't happen, we partition error/ok above.")
            in
            err_strs
            >>= (function
            | err_strs ->
                Log.erroR
                  "Failed to deploy static assets to "
                  ~params:
                    [ ("canvas", Canvas.name_for_id canvas)
                    ; ("errs", String.concat ~sep:";" err_strs) ] ;
                Static_assets.delete_static_asset_deploy
                  ~user
                  canvas
                  branch
                  deploy_hash ;
                respond
                  ~resp_headers:(server_timing []) (* t1; t2; etc *)
                  ~execution_id
                  parent
                  `Internal_server_error
                  ( Yojson.Safe.to_string
                      (`Assoc
                        [ ( "msg"
                          , `String "We couldn't put this upload in gcloud." )
                        ; ( "execution_id"
                          , `String (Types.string_of_id execution_id) )
                        ; ( "errors"
                          , `List (List.map ~f:(fun s -> `String s) err_strs) )
                        ])
                  |> Yojson.Basic.prettify )) )
  with e -> raise e


let admin_add_op_handler
    ~(execution_id : Types.id)
    ~(user : Account.user_info)
    (parent : Span.t)
    (host : string)
    body : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let t1, (params, canvas_id) =
    time "1-read-api-ops" (fun _ ->
        let canvas_id, owner = Canvas.id_and_account_id_for_name_exn host in
        let params = Api.to_add_op_rpc_params body in
        if Serialize.is_latest_op_request
             params.clientOpCtrId
             params.opCtr
             canvas_id
        then (params, canvas_id)
        else
          ( {params with ops = params.ops |> Op.filter_ops_received_out_of_order}
          , canvas_id ))
  in
  let ops = params.ops in
  let tlids = List.map ~f:Op.tlidOf ops in
  let t2, maybe_c =
    (* NOTE: Because we run canvas-wide validation logic, it's important
     * that we load _at least_ the context (ie. datastores, functions, types etc. )
     * and not just the tlids in the API payload.
     * *)
    time "2-load-saved-ops" (fun _ ->
        match Op.required_context_to_validate_oplist ops with
        | NoContext ->
            C.load_only_tlids ~tlids host ops
        | AllDatastores ->
            C.load_with_dbs ~tlids host ops)
  in
  let params : Api.add_op_rpc_params =
    { ops = params.ops
    ; opCtr = params.opCtr
    ; clientOpCtrId = params.clientOpCtrId }
  in
  match maybe_c with
  | Ok c ->
      let t3, result =
        time "3-to-frontend" (fun _ -> !c |> Analysis.to_add_op_rpc_result)
      in
      let t4, _ =
        time "4-save-to-disk" (fun _ ->
            (* work out the result before we save it, in case it has a
              stackoverflow or other crashing bug *)
            if Api.causes_any_changes params then C.save_tlids !c tlids else ())
      in
      let t5, strollerMsg =
        (* To make this work with prodclone, we might want to have it specify
         * more ... else people's prodclones will stomp on each other ... *)
        time "5-send-ops-to-stroller" (fun _ ->
            if Api.causes_any_changes params
            then (
              let strollerMsg =
                {result; params}
                |> Analysis.add_op_stroller_msg_to_yojson
                |> Yojson.Safe.to_string
              in
              Stroller.push_new_event
                ~execution_id
                ~canvas_id
                ~event:"add_op"
                strollerMsg ;
              Some strollerMsg )
            else None)
      in
      let t6, _ =
        time "send event to heapio" (fun _ ->
            (* NB: I believe we only send one op at a time, but the type is op
             * list *)
            ops
            (* MoveTL and TLSavepoint make for noisy data, so exclude it from heapio *)
            |> List.filter ~f:(function
                   | MoveTL _ | TLSavepoint _ ->
                       false
                   | _ ->
                       true)
            |> List.iter ~f:(fun op ->
                   Lwt.async (fun () ->
                       Stroller.heapio_track
                         ~canvas_id
                         ~canvas:host
                         ~user_id:user.id
                         ~execution_id
                         Track
                         ~event:(op |> Op.event_name_of_op)
                         (* currently empty, but we could add annotations later *)
                         (`Assoc []))))
      in
      Span.set_attr parent "op_ctr" (`Int params.opCtr) ;
      respond
        ~resp_headers:(server_timing [t1; t2; t3; t4; t5; t6])
        ~execution_id
        parent
        `OK
        (Option.value
           ~default:
             ( {result = Analysis.empty_to_add_op_rpc_result; params}
             |> Analysis.add_op_stroller_msg_to_yojson
             |> Yojson.Safe.to_string )
           strollerMsg)
  | Error errs ->
      let body = String.concat ~sep:", " errs in
      respond
        ~resp_headers:(server_timing [t1; t2])
        ~execution_id
        parent
        `Bad_request
        body


let fetch_all_traces
    ~(execution_id : Types.id)
    ~(user : Account.user_info)
    ~(canvas : string)
    ~(permission : Authorization.permission option)
    (parent : Span.t)
    body : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, c =
      time "1-load-canvas" (fun _ ->
          C.load_all_from_cache canvas
          |> Result.map_error ~f:(String.concat ~sep:", ")
          |> Prelude.Result.ok_or_internal_exception "Failed to load canvas")
    in
    let t2, traces =
      time "2-load-traces" (fun _ ->
          let htraces =
            !c.handlers
            |> TL.handlers
            |> List.map ~f:(fun h ->
                   Analysis.traceids_for_handler !c h
                   |> List.map ~f:(fun traceid -> (h.tlid, traceid)))
            |> List.concat
          in
          let uftraces =
            !c.user_functions
            |> Types.IDMap.data
            |> List.map ~f:(fun uf ->
                   Analysis.traceids_for_user_fn !c uf
                   |> List.map ~f:(fun traceid -> (uf.tlid, traceid)))
            |> List.concat
          in
          htraces @ uftraces)
    in
    let t3, result =
      time "3-to-frontend" (fun _ -> Analysis.to_all_traces_result traces)
    in
    respond
      ~execution_id
      ~resp_headers:(server_timing [t1; t2; t3])
      parent
      `OK
      result
  with e -> Libexecution.Exception.reraise_as_pageable e



let execute_function
    ~(execution_id : Types.id) (parent : Span.t) (host : string) body :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let t1, params =
    time "1-read-api-ops" (fun _ -> Api.to_execute_function_rpc_params body)
  in
  let t2, c =
    time "2-load-saved-ops" (fun _ ->
        C.load_tlids_with_context_from_cache ~tlids:[params.tlid] host
        |> Result.map_error ~f:(String.concat ~sep:", ")
        |> Prelude.Result.ok_or_internal_exception "Failed to load canvas")
  in
  let t3, (result, tlids) =
    time "3-execute" (fun _ ->
        Analysis.execute_function
          !c
          params.fnname
          ~execution_id
          ~tlid:params.tlid
          ~trace_id:params.trace_id
          ~caller_id:params.caller_id
          ~args:params.args)
  in
  let t4, unlocked =
    time "4-analyze-unlocked-dbs" (fun _ ->
        Analysis.unlocked ~canvas_id:!c.id ~account_id:!c.owner)
  in
  let t5, response =
    time "5-to-frontend" (fun _ ->
        Analysis.to_execute_function_rpc_result
          (Dval.hash Dval.current_hash_version params.args)
          Dval.current_hash_version
          tlids
          unlocked
          result)
  in
  respond
    ~execution_id
    ~resp_headers:(server_timing [t1; t2; t3; t4; t5])
    parent
    `OK
    response


let upload_function
    ~(execution_id : Types.id)
    ~(user : Account.user_info)
    (parent : Span.t)
    (body : string) : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let t1, params =
    time "1-read-api" (fun _ -> Api.to_upload_function_rpc_params body)
  in
  let t2, result =
    time "2-save" (fun _ -> Package_manager.save user.username params.fn)
  in
  let t3, (response_code, response) =
    time "3-to-frontend" (fun _ ->
        match result with
        | Ok () ->
            (`OK, "\"Success\"")
        | Error msg ->
            (`Bad_request, msg))
  in
  respond
    ~execution_id
    ~resp_headers:(server_timing [t1; t2; t3])
    parent
    response_code
    response


let trigger_handler
    ~(execution_id : Types.id) (parent : Span.t) (host : string) body :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let t1, params =
    time "1-read-api-params" (fun _ -> Api.to_trigger_handler_rpc_params body)
  in
  let t2, c =
    time "2-load-saved-ops" (fun _ ->
        C.load_tlids_with_context_from_cache ~tlids:[params.tlid] host
        |> Result.map_error ~f:(String.concat ~sep:", ")
        |> Prelude.Result.ok_or_internal_exception "Failed to load canvas")
  in
  let t3, touched_tlids =
    time "3-execute" (fun _ ->
        let handler_and_desc =
          Map.find_exn !c.handlers params.tlid
          |> TL.as_handler
          |> Option.bind ~f:(fun h ->
                 Handler.event_desc_for h |> Option.map ~f:(fun d -> (h, d)))
        in
        match handler_and_desc with
        | None ->
            []
        | Some (handler, desc) ->
            let canvas_id = !c.id in
            let trace_id = params.trace_id in
            let _, touched_tlids =
              Libexecution.Execution.execute_handler
                handler
                ~execution_id
                ~tlid:params.tlid
                ~input_vars:params.input
                ~dbs:(TL.dbs !c.dbs)
                ~user_tipes:(!c.user_tipes |> Map.data)
                ~user_fns:(!c.user_functions |> Map.data)
                ~package_fns:!c.package_fns
                ~secrets:(Secret.secrets_in_canvas !c.id)
                ~account_id:!c.owner
                ~canvas_id
                ~store_fn_arguments:
                  (Stored_function_arguments.store ~canvas_id ~trace_id)
                ~store_fn_result:
                  (Stored_function_result.store ~canvas_id ~trace_id)
            in
            touched_tlids)
  in
  let t4, response =
    time "4-to-frontend" (fun _ ->
        Analysis.to_trigger_handler_rpc_result (params.tlid :: touched_tlids))
  in
  respond
    ~execution_id
    ~resp_headers:(server_timing [t1; t2; t3; t4])
    parent
    `OK
    response


let db_stats
    ~(execution_id : Types.id) (parent : Span.t) (host : string) (body : string)
    : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, params =
      time "1-read-api-tlids" (fun _ -> Api.to_db_stats_rpc_params body)
    in
    let t2, c =
      time "2-load-saved-ops" (fun _ ->
          C.load_all_dbs_from_cache host
          |> Result.map_error ~f:(String.concat ~sep:", ")
          |> Prelude.Result.ok_or_internal_exception "Failed to load canvas")
    in
    let t3, stats =
      time "3-analyze-db-stats" (fun _ -> Analysis.db_stats !c params.tlids)
    in
    let t4, result =
      time "4-to-frontend" (fun _ -> Analysis.to_db_stats_rpc_result stats)
    in
    respond
      ~execution_id
      ~resp_headers:(server_timing [t1; t2; t3; t4])
      parent
      `OK
      result
  with e -> raise e


let worker_stats
    ~(execution_id : Types.id) (parent : Span.t) (host : string) (body : string)
    : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, params =
      time "1-read-api-tlid" (fun _ -> Api.to_worker_stats_rpc_params body)
    in
    let t2, stats =
      let canvas_id = Canvas.id_for_name host in
      time "2-analyze-worker-stats" (fun _ ->
          Analysis.worker_stats canvas_id params.tlid)
    in
    let t3, result =
      time "3-to-frontend" (fun _ -> Analysis.to_worker_stats_rpc_result stats)
    in
    respond
      ~execution_id
      ~resp_headers:(server_timing [t1; t2; t3])
      parent
      `OK
      result
  with e -> raise e

let worker_schedule
    ~(execution_id : Types.id) (parent : Span.t) (host : string) (body : string)
    : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, cid =
      time "1-get-canvas-id" (fun _ ->
          let owner = Account.for_host_exn host in
          Serialize.fetch_canvas_id owner host)
    in
    let t2, params =
      time "2-read-api-tlid" (fun _ ->
          Api.to_worker_schedule_update_rpc_params body)
    in
    let t3, res =
      time "3-update-worker-schedule" (fun _ ->
          let module E = Event_queue in
          match params.schedule with
          | "pause" ->
              E.pause_worker cid params.name ;
              Ok (E.get_worker_schedules_for_canvas cid)
          | "run" ->
              Event_queue.unpause_worker cid params.name ;
              Ok (E.get_worker_schedules_for_canvas cid)
          | _ ->
              Error "unknown action")
    in
    let t4, _ =
      time "4-push-new-states" (fun _ ->
          match res with
          | Ok ws ->
              Stroller.push_worker_states ~execution_id ~canvas_id:cid ws
          | _ ->
              ())
    in
    let timing = server_timing [t1; t2; t3; t4] in
    match res with
    | Ok schedules ->
        respond
          ~execution_id
          ~resp_headers:timing
          parent
          `OK
          ( schedules
          |> Event_queue.Worker_states.to_yojson
          |> Yojson.Safe.to_string )
    | Error e ->
        respond
          ~execution_id
          ~resp_headers:timing
          parent
          `Bad_request
          ("{ \"error\" : \"" ^ e ^ "\" } ")
  with e -> raise e


let delete_404 ~(execution_id : Types.id) (parent : Span.t) (host : string) body
    : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, cid =
      time "1-get-canvas-id" (fun _ ->
          let owner = Account.for_host_exn host in
          Serialize.fetch_canvas_id owner host)
    in
    let t2, p = time "2-to-route-params" (fun _ -> Api.to_route_params body) in
    let t3, _ =
      time "3-delete-404s" (fun _ ->
          Analysis.delete_404s cid p.space p.path p.modifier)
    in
    respond
      ~execution_id
      ~resp_headers:(server_timing [t1; t2; t3])
      parent
      `OK
      "{ \"result\" : \"deleted\" } "
  with e -> raise e


let insert_secret
    ~(execution_id : Types.id) (parent : Span.t) (host : string) (body : string)
    : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, p =
      time "1-to-insert-secret-params" (fun _ ->
          Api.to_insert_secret_params body)
    in
    let t2, canvas_id =
      time "2-get-canvas-id" (fun _ ->
          let owner = Account.for_host_exn host in
          Serialize.fetch_canvas_id owner host)
    in
    let t3, _ =
      time "3-insert-secret" (fun _ ->
          Secret.insert_secret canvas_id p.secret_name p.secret_value)
    in
    let t4, secrets =
      time "4-get-secrets" (fun _ -> Secret.secrets_in_canvas canvas_id)
    in
    let t5, result =
      time "5-to-frontend" (fun _ -> Api.to_secrets_list_results secrets)
    in
    respond
      ~execution_id
      ~resp_headers:(server_timing [t1; t2; t3; t4; t5])
      parent
      `OK
      result
  with e ->
    let msg = Exception.exn_to_string e in
    if Tc.String.contains
         ~substring:"duplicate key value violates unique constraint"
         msg
    then
      Exception.raise_
        DarkStorage
        "The secret's name is already defined for this canvas"
    else raise e


(* ------------------- *)
(* Loading html pages *)
(* ------------------- *)

let hashed_filename (file : string) (hash : string) : string =
  match List.rev (String.split ~on:'.' file) with
  | [] ->
      Exception.internal "Tried splitting an empty filename key"
  | [_] ->
      Exception.internal "Tried splitting a filename key with no extension"
  | ext :: rest ->
      sprintf "%s-%s.%s" (String.concat ~sep:"." (List.rev rest)) hash ext


let to_assoc_list etags_json : (string * string) list =
  match etags_json with
  | `Assoc alist ->
      let mutated =
        List.filter_map
          ~f:(fun (fst, snd) ->
            match snd with `String inner -> Some (fst, inner) | _ -> None)
          alist
      in
      if List.length mutated <> List.length alist
      then Exception.internal "Some asset in etags.json lacked a hash value."
      else mutated
  | _ ->
      Exception.internal "etags.json must be a top-level object."


let save_test_handler ~(execution_id : Types.id) (parent : Span.t) host =
  let c = C.load_all host [] in
  match c with
  | Ok c ->
      let filename = C.save_test !c in
      respond ~execution_id parent `OK ("Saved as: " ^ filename)
  | Error errs ->
      Exception.internal
        ~info:[("errs", String.concat ~sep:", " errs)]
        "Failed to load canvas"


let check_csrf_then_handle ~execution_id ~session (parent : Span.t) handler req
    =
  if CRequest.meth req = `POST
  then
    let request_token =
      req |> CRequest.headers |> fun h -> Header.get h "X-CSRF-Token"
    in
    if Some (Auth.SessionLwt.csrf_token_for session) = request_token
    then handler parent req
    else respond ~execution_id parent `Unauthorized "Bad CSRF"
  else handler parent req


(* used to provide information to honeycomb *)
let username_header username = ("x-dark-username", username)

(* get the domain of a request *)
let domain req =
  (* For why we use 'darklang.com' and not '.darklang.com', see
   * https://www.mxsasha.eu/blog/2014/03/04/definitive-guide-to-cookie-domains/
   * tl;dr: with a leading-dot was the specified behavior prior to
   * RFC6265 (2011), and in theory is still okay because the leading
   * dot is ignored, but .darklang.localhost doesn't work and
   * darklang.localhost does, so ... no leading dot works better for
   * us. *)
  req
  |> CRequest.headers
  |> fun h ->
  Header.get h "host"
  |> Option.value ~default:"darklang.com"
  (* Host: darklang.localhost:8000 is properly set in-cookie as
                   * "darklang.localhost", the cookie domain doesn't want the
                   * port *)
  |> String.substr_replace_all ~pattern:":8000" ~with_:""


type login_page =
  { username : string
  ; password : string }
[@@deriving yojson]

let login_template = File.readfile ~root:Templates "login.html"

(* handle_local_login is used to handle GET/POST to /login for local
 * development, bypassing Auth0. *)
let handle_local_login ~execution_id (parent : Span.t) req body =
  if CRequest.meth req = `GET || CRequest.meth req = `HEAD
  then respond ~execution_id parent `OK login_template
  else
    (* Responds to a form submitted from login.html *)
    let params = Uri.query_of_encoded body in
    (*  the username form param may be username _or_ email; we get the username
     *  back from the DB when we call authenticate *)
    let username_or_email, password, redirect =
      List.fold params ~init:(None, None, None) ~f:(fun (u, p, r) (k, vs) ->
          if k = "username"
          then (List.hd vs, p, r)
          else if k = "password"
          then (u, List.hd vs, r)
          else if k = "redirect"
          then (u, p, List.hd vs)
          else (u, p, r))
    in
    let username =
      Option.map2 username_or_email password ~f:(fun u p -> (u, p))
      |> Option.bind ~f:(fun (username_or_email, password) ->
             Account.authenticate ~username_or_email ~password)
    in
    match username with
    | Some username ->
        Log.add_log_annotations
          [("username", `String username)]
          (fun _ ->
            let%lwt session = Auth.SessionLwt.new_for_username username in
            let https_only_cookie = req |> CRequest.uri |> should_use_https in
            let headers =
              username_header username
              :: Auth.SessionLwt.to_cookie_hdrs
                   ~http_only:true
                   ~secure:https_only_cookie
                   ~domain:(domain req)
                   ~path:"/"
                   Auth.SessionLwt.cookie_key
                   session
            in
            let redirect_to =
              ( match redirect with
              | None | Some "" ->
                  "/a/" ^ Uri.pct_encode username
              | Some redir ->
                  Uri.pct_decode redir )
              |> Uri.of_string
              |> Uri.path_and_query
              (* Strip the host; that also means we'll use
                                       the port of the incoming request, solving
                                       the local login bug where we redirect to
                                       darklang.localhost:80/something when we
                                       want to hit :8000 *)
            in
            over_headers_promise
              ~f:(fun h -> Header.add_list h headers)
              (S.respond_redirect ~uri:(Uri.of_string redirect_to) ()))
    | None ->
        let uri = Uri.of_string "/login" in
        let uri =
          match redirect with
          | Some redirect ->
              Uri.add_query_param' uri ("redirect", redirect)
          | None ->
              uri
        in
        let uri =
          Uri.add_query_param'
            uri
            ("error", "Invalid username or password" |> Uri.pct_encode)
        in
        S.respond_redirect ~uri ()


(* Checks for a cookie, prompts for basic auth if there isn't one,
   returns Unauthorized if basic auth doesn't work.

   Importantly this performs no authorization. Just authentication.

   It passes the username and the current CSRF token to the handler.
   Don't check against the CSRF token in the handler; use
   check_csrf_then_handle for that. Only use it to present the token
   to users in HTML.

   Also implements logout (!). *)
let authenticate_then_handle
    (parent : Span.t) ~(execution_id : Types.id) handler req body =
  let req_uri = CRequest.uri req in
  let path = Uri.path req_uri in
  let live_login, login_uri, logout_uri =
    if Config.use_login_darklang_com_for_login
    then
      ( `Live
      , Uri.of_string "https://login.darklang.com"
      , Uri.of_string "https://login.darklang.com/logout" )
    else (`Local, Uri.of_string "/login", Uri.of_string "/logout")
  in
  let%lwt session = Auth.SessionLwt.of_request req in
  match (live_login, path, session) with
  | `Live, "/login", _ ->
      (* old, pre-Auth0 login route. support it for a while, just in case *)
      S.respond_redirect ~uri:login_uri ()
  | `Live, "/logout", _ ->
      (* login.darklang.com will clear the cookie and the postgres session *)
      S.respond_redirect ~uri:logout_uri ()
  | `Local, "/login", _ ->
      (* locally, support a login form to bypass Auth0 *)
      handle_local_login ~execution_id parent req body
  | `Local, "/logout", Ok (Some session) ->
      (* Fallback for local env, where we don't have
       * login.darklang.com/logout, we just need to clear the session *)
      Auth.SessionLwt.clear Auth.SessionLwt.backend session ;%lwt
      let headers =
        Header.of_list (Auth.SessionLwt.clear_hdrs Auth.SessionLwt.cookie_key)
      in
      let uri = Uri.of_string "https://darklang.com" in
      S.respond_redirect ~headers ~uri ()
  | _, _, Ok (Some session) ->
      (* all other authenticated requsets should run the handler *)
      let username = Auth.SessionLwt.username_for session in
      let csrf_token = Auth.SessionLwt.csrf_token_for session in
      Span.set_attr parent "username" (`String username) ;
      let headers = [username_header username] in
      over_headers_promise
        ~f:(fun h -> Header.add_list h headers)
        (handler ~session ~csrf_token parent req)
  | _ when Re2.matches (Re2.create_exn "^/api/") path ->
      (* If it's an api, don't try to redirect *)
      respond ~execution_id parent `Unauthorized "Bad credentials"
  | _ ->
      (* anything else requires authentication first *)
      let uri =
        Uri.add_query_param'
          login_uri
          ("redirect", req_uri |> Uri.to_string |> Uri.pct_encode)
      in
      S.respond_redirect ~uri ()


let is_canvas_name_valid (canvas : string) : bool =
  Re2.matches (Re2.create_exn "^([a-z0-9]+[_-]?)*[a-z0-9]$") canvas


let admin_ui_handler
    ~(execution_id : Types.id)
    ~(path : string list)
    ~(canvasname : string)
    ~(body : string)
    ~(account_created : Core_kernel.Time.t)
    ~(user : Account.user_info)
    ~(csrf_token : string)
    (parent : Span.t)
    (req : CRequest.t) =
  let verb = req |> CRequest.meth in
  let uri = req |> CRequest.uri in
  let query_param_set name =
    match Uri.get_query_param uri name with
    | Some v when v <> "0" && v <> "false" ->
        true
    | _ ->
        false
  in
  let integration_test =
    query_param_set "integration-test" && Config.allow_test_routes
  in
  let local = Uri.get_query_param uri "localhost-assets" in
  let html_hdrs =
    [ ("Content-type", "text/html; charset=utf-8")
      (* Don't allow any other websites to put this in an iframe;
       this prevents "clickjacking" attacks.
       https://www.owasp.org/index.php/Clickjacking_Defense_Cheat_Sheet#Content-Security-Policy:_frame-ancestors_Examples
       It would be nice to use CSP to limit where we can load scripts etc from,
       but right now we load from CDNs, <script> tags, etc. So the only thing
       we could do is script-src: 'unsafe-inline', which doesn't offer us
       any additional security. *)
    ; ("Content-security-policy", "frame-ancestors 'none';") ]
  in
  let html_hdrs =
    if local = None
    then html_hdrs
    else ("Access-Control-Allow-Origin", "*") :: html_hdrs
  in
  let html_hdrs = Header.of_list html_hdrs in
  (* this could be more middleware like in the future *if and only if* we
     only make changes in promises .*)
  let when_can_view ~canvas f =
    let auth_domain = Account.auth_domain_for canvas in
    if Authorization.can_view_canvas ~canvas ~username:user.username
    then
      match Account.owner ~auth_domain with
      | Some owner ->
          Log.add_log_annotations
            [("canvas", `String canvas)]
            (fun _ -> f (Serialize.fetch_canvas_id owner canvasname))
      | None ->
          respond ~execution_id parent `Not_found "Not found"
    else respond ~execution_id parent `Unauthorized "Unauthorized"
  in
  match (verb, path) with
  | `GET, ["a"; canvas] when is_canvas_name_valid canvas ->
      when_can_view ~canvas (fun canvas_id ->
          if integration_test then Canvas.load_and_resave_from_test_file canvas ;
          let html =
            admin_ui_html
              ~canvas_id
              ~canvas
              ~csrf_token
              ~local
              ~account_created
              user
          in
          respond ~resp_headers:html_hdrs ~execution_id parent `OK html)
  | `GET, ["a"; canvas] ->
      respond
        ~execution_id
        parent
        `Bad_request
        "Your canvas name must:\n - Consist of lowercase alphanumeric characters, '-', and '_'\n - Start and end with an alphanumeric character (no initial/final '-' or '_')"
  | _ ->
      respond ~execution_id parent `Not_found "Not found"


let admin_api_handler
    ~(execution_id : Types.id)
    ~(path : string list)
    ~(body : string)
    ~(user : Account.user_info)
    (parent : Span.t)
    (req : CRequest.t) =
  let verb = req |> CRequest.meth in
  (* this could be more middleware like in the future *if and only if* we
     only make changes in promises .*)
  let when_can_edit ~canvas f =
    let p =
      Authorization.permission
        ~auth_domain:(Account.auth_domain_for canvas)
        ~username:user.username
    in
    if p = Some Authorization.ReadWrite
    then f p
    else respond ~execution_id parent `Unauthorized "Unauthorized"
  in
  let when_can_view ~canvas f =
    let p =
      Authorization.permission
        ~auth_domain:(Account.auth_domain_for canvas)
        ~username:user.username
    in
    if p >= Some Authorization.Read
    then f p
    else respond ~execution_id parent `Unauthorized "Unauthorized"
  in
  let client_version =
    req
    |> CRequest.headers
    |> (fun hs -> Cohttp.Header.get hs Libshared.Header.client_version)
    |> Tc.Option.withDefault ~default:""
  in
  Span.set_attr
    parent
    ("request.header." ^ Libshared.Header.client_version)
    (`String client_version) ;
  match (verb, path) with
  (* Operational APIs.... maybe these shouldn't be here, but
     they start with /api so they need to be. *)
  | `POST, ["api"; "clear-benchmarking-data"] ->
      Db.delete_benchmarking_data () ;
      respond ~execution_id parent `OK "Cleared"
  | `POST, ["api"; canvas; "save_test"] when Config.allow_test_routes ->
      save_test_handler ~execution_id parent canvas
  (* Canvas API *)
  | `POST, ["api"; canvas; "rpc"] (* old name, remove later *)
  | `POST, ["api"; canvas; "add_op"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (admin_add_op_handler ~execution_id ~user parent canvas body))
  | `POST, ["api"; canvas; "initial_load"] ->
      when_can_view ~canvas (fun permission ->
          wrap_editor_api_headers
            (initial_load ~execution_id ~user ~canvas ~permission parent body))
  | `POST, ["api"; canvas; "all_traces"] ->
      when_can_view ~canvas (fun permission ->
          wrap_editor_api_headers
            (fetch_all_traces
               ~execution_id
               ~user
               ~canvas
               ~permission
               parent
               body))
  | `POST, ["api"; canvas; "execute_function"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (execute_function ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "packages"; "upload_function"] when user.admin ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (upload_function ~execution_id ~user parent body))
  | `POST, ["api"; canvas; "packages"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers (get_all_packages ~execution_id parent ()))
  | `POST, ["api"; canvas; "trigger_handler"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (trigger_handler ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "get_trace_data"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers
            (get_trace_data ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "get_404s"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers (get_404s ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "get_db_stats"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers (db_stats ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "get_worker_stats"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers
            (worker_stats ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "get_unlocked_dbs"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers
            (get_unlocked_dbs ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "worker_schedule"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (worker_schedule ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "delete_404"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers (delete_404 ~execution_id parent canvas body))
  | `POST, ["api"; canvas; "static_assets"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (static_assets_upload_handler
               ~execution_id
               ~user
               parent
               canvas
               req
               body))
  | `POST, ["api"; canvas; "insert_secret"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (insert_secret ~execution_id parent canvas body))
  | _ ->
      respond ~execution_id parent `Not_found "Not found"


let admin_handler
    ~(execution_id : Types.id)
    ~(uri : Uri.t)
    ~(body : string)
    ~session
    ~(csrf_token : string)
    (parent : Span.t)
    (req : CRequest.t) =
  let username = Auth.SessionLwt.username_for session in
  let user = Account.get_user username in
  let path =
    uri
    |> Uri.path
    |> String.lstrip ~drop:(( = ) '/')
    |> String.rstrip ~drop:(( = ) '/')
    |> String.split ~on:'/'
  in
  (* routing *)
  match (user, path) with
  | Some user, "api" :: canvas :: _ ->
      Span.set_attrs
        parent
        [ ("is_admin", `Bool user.admin)
        ; ("username", `String user.username)
        ; ("canvas", `String canvas) ] ;
      check_csrf_then_handle
        ~execution_id
        ~session
        parent
        (admin_api_handler ~execution_id ~path ~body ~user)
        req
  | Some user, "a" :: canvasname :: _ ->
      Span.set_attrs
        parent
        [ ("is_admin", `Bool user.admin)
        ; ("username", `String user.username)
        ; ("canvas", `String canvasname) ] ;
      let account_created = Account.get_user_created_at_exn username in
      admin_ui_handler
        ~execution_id
        ~path
        ~canvasname
        ~body
        ~account_created
        ~user
        ~csrf_token
        parent
        req
  | _ ->
      respond ~execution_id parent `Not_found "Not found"


(* -------------------------------------------- *)
(* The server *)
(* -------------------------------------------- *)

let static_etag_for =
  (* Read the etags.json JSON document here so that it reads
     at load-time, not at each call-time.

     The server gets restarted after we get new etags, so we
     don't need to worry about updates here. *)
  let etags_json =
    File.readfile ~root:Config.Webroot "etags.json" |> Yojson.Basic.from_string
  in
  fun uri ->
    try
      etags_json
      (* Get the JSON field that corresponds to the filename,
         stripped of the leftmost /. *)
      |> Yojson.Basic.Util.member
           (uri |> Uri.path |> String.lstrip ~drop:(( = ) '/'))
      |> Yojson.Basic.Util.to_string
      |> fun x -> [("etag", x)]
    with e -> []


let static_handler uri =
  let fname = S.resolve_file ~docroot:(Config.dir Config.Webroot) ~uri in
  S.respond_file
    ~headers:(Header.of_list (cors :: static_etag_for uri))
    ~fname
    ()


let db_conn_readiness_check () : string option =
  match Dbconnection.status () with
  | `Healthy ->
      None
  | `Disconnected ->
      Some "Dbconnection.status = `Disconnected"


let stroller_readiness_check () : string option Lwt.t =
  match%lwt Stroller.status () with
  | `Healthy ->
      Lwt.return None
  | `Unconfigured ->
      Lwt.return (Some "Stroller.status = `Unconfigured")
  | `Unhealthy s ->
      Lwt.return (Some ("Stroller.status = `Unhealthy: " ^ s))


let k8s_handler
    (parent : Span.t)
    (req : CRequest.t)
    ~(execution_id : Types.id)
    ~(stopper : unit Lwt.u) : (CResponse.t * Cl.Body.t) Lwt.t =
  let%lwt stroller_readiness_check = stroller_readiness_check () in
  match req |> CRequest.uri |> Uri.path with
  (* For GKE health check *)
  | "/" ->
    ( match Dbconnection.status () with
    | `Healthy ->
        if not !ready
           (* ie. liveness check has found a service with 2 minutes of failing readiness checks *)
        then (
          Log.infO "Liveness check found unready service, returning unavailable" ;
          respond ~execution_id parent `Service_unavailable "Service not ready"
          )
        else respond ~execution_id parent `OK "Hello internal overlord"
    | `Disconnected ->
        respond
          ~execution_id
          parent
          `Service_unavailable
          "Sorry internal overlord" )
  | "/ready" ->
      let checks =
        [db_conn_readiness_check (); stroller_readiness_check]
        |> List.filter_map ~f:(fun x -> x)
      in
      ( match checks with
      | [] ->
          if !ready
          then respond ~execution_id parent `OK "Hello internal overlord"
          else (
            Log.infO "Service ready" ;
            ready := true ;
            respond ~execution_id parent `OK "Hello internal overlord" )
      | _ ->
          Log.erroR
            ("Failed readiness check(s): " ^ String.concat checks ~sep:": ") ;
          respond
            ~execution_id
            parent
            `Service_unavailable
            "Sorry internal overlord" )
  (* For GKE graceful termination *)
  | "/pkill" ->
      if not !shutdown (* note: this is a ref, not a boolean `not` *)
      then (
        shutdown := true ;
        Log.infO
          "shutdown"
          ~data:"Received shutdown request - shutting down"
          ~params:[("execution_id", Types.string_of_id execution_id)] ;
        (* k8s gives us 30 seconds, so ballpark 2s for overhead *)
        Lwt_unix.sleep 28.0
        >>= fun _ ->
        Lwt.wakeup stopper () ;
        respond ~execution_id parent `OK "Terminated" )
      else (
        Log.infO
          "shutdown"
          ~data:"Received redundant shutdown request - already shutting down" ;
        respond ~execution_id parent `OK "Terminated" )
  | _ ->
      respond ~execution_id parent `Not_found ""




let callback
    ~k8s_callback
    (parent : Span.t)
    (ip : string)
    (req : CRequest.t)
    (body : string)
    (execution_id : Types.id) : (CResponse.t * Cl.Body.t) Lwt.t =
  let req = canonicalize_request req in
  let uri = CRequest.uri req in
  let handle_error ~(include_internals : bool) (e : exn) =
    try
      let bt = Exception.get_backtrace () in
      let%lwt _ =
        match e with
        | Exception.DarkException e when e.tipe = EndUser ->
            Lwt.return `Disabled
        | _ ->
            Rollbar.report_lwt
              e
              bt
              (Remote (request_to_rollbar body req))
              (Types.show_id execution_id)
      in
      let real_err =
        try
          match e with
          | Pageable.PageableExn (Exception.DarkException e)
          | Exception.DarkException e ->
              e
              |> Exception.exception_data_to_yojson
              |> Yojson.Safe.pretty_to_string
          | Yojson.Json_error msg ->
              "Not a valid JSON value: '" ^ msg ^ "'"
          | _ ->
              "Dark Internal Error: " ^ Exn.to_string e
        with _ -> "UNHANDLED ERROR: real_err"
      in
      let real_err =
        real_err
        (* Commented out because API handlers need to be JSON decoded *)
        (* ^ (Exception.get_backtrace () *)
        (*             |> Exception.backtrace_to_string) *)
      in
      Log.erroR
        real_err
        ~bt
        ~params:[("execution_id", Types.string_of_id execution_id)] ;
      match e with
      | Exception.DarkException e when e.tipe = EndUser ->
          respond ~execution_id parent `Bad_request e.short
      | Exception.DarkException e when e.tipe = DarkClient ->
          respond ~execution_id parent `Bad_request real_err
      | _ ->
          let body =
            if include_internals || Config.show_stacktrace
            then real_err
            else
              "Dark Internal Error: Dark - the service running this application - encountered an error. This problem is a bug in Dark, we're sorry! Our automated systems have noted this error and we are working to resolve it. The author of this application can post in our slack (darkcommunity.slack.com) for more information."
          in
          respond ~execution_id parent `Internal_server_error body
    with e ->
      let bt = Exception.get_backtrace () in
      Rollbar.last_ditch e ~bt "handle_error" (Types.show_id execution_id) ;
      respond ~execution_id parent `Internal_server_error "unhandled error"
  in
  try
    (* first: if this isn't https and should be, redirect *)
    match redirect_to (CRequest.uri req) with
    | Some x ->
        S.respond_redirect ~uri:x ()
    | None ->
      ( match
          ( CRequest.meth req
          , Cohttp.Header.get (CRequest.headers req) "content-type" )
        with
      | `POST, Some s when String.lowercase s = "text/ping" ->
          (* I'm a teapot! We don't support text/ping:
           * https://dev.w3.org/html5/pf-summary/structured-client-side-storage.html#hyperlink-auditing *)
          (* Impl'd here since it doesn't matter which canvas we hit, or if it
           * exists at all *)
          respond ~execution_id parent (Cohttp.Code.status_of_code 418) ""
      | _ ->
        ( match Uri.to_string uri with
        | "/sitemap.xml" | "/favicon.ico" ->
            respond ~execution_id parent `OK ""
        | _ ->
          (* figure out what handler to dispatch to... *)
          ( match route_host req with
          | Some (Canvas canvas) ->
              canvas_handler ~execution_id ~canvas ~ip ~uri ~body parent req
          | Some Static ->
              static_handler uri
          | Some Admin ->
            ( try
                authenticate_then_handle
                  parent
                  ~execution_id
                  (fun ~session ~csrf_token span r ->
                    try
                      admin_handler
                        ~execution_id
                        ~uri
                        ~body
                        ~session
                        ~csrf_token
                        span
                        r
                    with e -> handle_error ~include_internals:true e)
                  req
                  body
              with e -> handle_error ~include_internals:false e )
          | None ->
              k8s_callback req ~execution_id ) ) )
  with e -> handle_error ~include_internals:false e


let server () =
  let stop, stopper = Lwt.wait () in
  let cbwb (conn : S.conn) (req : CRequest.t) (req_body : Cl.Body.t) :
      (CResponse.t * Cl.Body.t) Lwt.t =
    let%lwt body_string = Cl.Body.to_string req_body in
    let execution_id = Util.create_id () in
    let uri = CRequest.uri req in
    (* use the x-forwarded-for ip, falling back to the raw ip in the request *)
    let ip =
      let ch, _ = conn in
      Header.get (CRequest.headers req) "X-forwarded-for"
      |> Option.bind ~f:(fun str -> str |> String.split ~on:';' |> List.hd)
      |> Option.map ~f:String.strip
      |> Option.value ~default:(get_ip_address ch)
    in
    let headers = CRequest.headers req in
    Telemetry.with_root "http.request" (fun span ->
        (* attribute names copied from honeycomb's beeline-go *)
        Span.set_attrs
          span
          [ ("meta.type", `String "http_request")
          ; ("meta.server_version", `String Config.build_hash)
          ; ("execution_id", `String (Types.string_of_id execution_id))
          ; ("request.remote_addr", `String ip)
          ; ( "request.method"
            , `String (req |> CRequest.meth |> Cohttp.Code.string_of_method) )
          ; ("request.url", `String (Uri.to_string uri))
          ; ("request.path", `String (Uri.path uri)) ] ;

        [ ("request.header.user_agent", Cohttp.Header.get headers "user-agent")
        ; ("request.host", Uri.host uri)
        ; ("request.method", Uri.verbatim_query uri) ]
        |> List.iter ~f:(function
               | k, Some v ->
                   Span.set_attr span k (`String v)
               | _ ->
                   ()) ;
        callback
          ~k8s_callback:(k8s_handler span ~stopper)
          span
          ip
          req
          body_string
          execution_id)
  in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback:cbwb ())


