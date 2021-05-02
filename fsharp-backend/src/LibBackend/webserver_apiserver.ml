// This file is being split between ApiServer.fs and BwdServer.fs. I'll delete from it as it's ported.


(* ------------------------------- *)
(* utils *)
(* ------------------------------- *)

type timing_header = string * float * string

let shutdown = ref false

let ready = ref false


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



let admin_api_handler
  Span.set_attr
    parent
    ("request.header." ^ Libshared.Header.client_version)
    (`String client_version) ;
  match (verb, path) with
  | `POST, ["api"; canvas; "save_test"] when Config.allow_test_routes ->
      save_test_handler ~execution_id parent canvas
  | `POST, ["api"; canvas; "packages"; "upload_function"] when user.admin ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (upload_function ~execution_id ~user parent body))
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


