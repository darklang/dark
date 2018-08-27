open Core_kernel
open Libcommon

open Lwt
module Clu = Cohttp_lwt_unix
module S = Clu.Server
module CRequest = Clu.Request
module Header = Cohttp.Header
module Cookie = Cohttp.Cookie

module C = Canvas

module Exception = Libexecution.Exception
module Util = Libexecution.Util
module Dval = Libexecution.Dval
module PReq = Libexecution.Parsed_request
module Types = Libexecution.Types
module Http = Libexecution.Http
module RTT = Types.RuntimeT
module Handler = Libexecution.Handler
module TL = Libexecution.Toplevel

module Dbconnection = Libservice.Dbconnection

(* ------------------------------- *)
(* utils *)
(* ------------------------------- *)
type timing_header = string * float * string

let shutdown = ref false

let ready = ref false

let server_timing (times: timing_header list) =
  times
  |> List.map ~f:(fun (name, time, desc) ->
      name
      ^ ";desc=\"" ^ desc ^ "\""
      ^ ";dur=" ^ (time |> Float.to_string_hum ~decimals:3))

  |> String.concat ~sep:","
  |> fun x -> [("Server-timing", x)]
  |> Header.of_list

let time (name: string) (fn: (_ -> 'a)) :
  (timing_header * 'a) =
  let start = Unix.gettimeofday () in
  let result = fn () in
  let finish = Unix.gettimeofday () in
  ((name, (finish -. start) *. 1000.0, name), result)

let get_ip_address ch : string =
  match Conduit_lwt_unix.endp_of_flow ch with
  | `TCP (ip, port) -> Ipaddr.to_string ip
  | _ -> assert false

let request_to_rollbar (body: string) (req: CRequest.t) : Libservice.Rollbar.request_data =
  { body = body
  ; headers = req |> CRequest.headers |> Cohttp.Header.to_list
  ; url = req |> CRequest.uri |> Uri.to_string
  ; http_method = req |> CRequest.meth |> Cohttp.Code.string_of_method
  }

let respond ?(resp_headers=Header.init ()) ~(execution_id: Types.id) status (body: string) =
  let resp_headers =
    Header.add resp_headers "X-Darklang-Execution-ID" (Log.dump execution_id)
  in
  Log.infO "response"
    ~params:[ "execution_id", Log.dump execution_id
            ; "headers", Log.dump resp_headers
            ; "status", Log.dump status
            ; "body", Log.dump body
            ];
  S.respond_string ~status ~body ~headers:resp_headers ()

(* -------------------------------------------- *)
(* handlers for end users *)
(* -------------------------------------------- *)
let cors = ("Access-Control-Allow-Origin", "*")

let options_handler ~(execution_id: Types.id) (c: C.canvas) (req: CRequest.t) =
  (*       allow (from the route matching) *)
  (*       Access-Control-Request-Method: POST  *)
  (* Access-Control-Request-Headers: X-PINGOTHER, Content-Type *)
  (* This is just enough to fix conduit. Here's what we should do:
   * https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/OPTIONS *)
  let req_headers = Cohttp.Header.get (CRequest.headers req) "access-control-request-headers" in
  let allow_headers =
    match req_headers with
    | Some h -> h
    | None -> "*"
  in
  let resp_headers = [( "Access-Control-Allow-Methods"
                      , "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS")
                     ; ("Access-Control-Allow-Origin"
                       , "*")
                     ; ("Access-Control-Allow-Headers"
                       , allow_headers)]
  in
  respond ~resp_headers:(Cohttp.Header.of_list resp_headers) ~execution_id `OK ""


let user_page_handler ~(execution_id: Types.id) ~(host: string) ~(ip: string) ~(uri: Uri.t)
    ~(body: string) (req: CRequest.t) =
  let verb = req |> CRequest.meth |> Cohttp.Code.string_of_method in
  let headers = req |> CRequest.headers |> Header.to_list in
  let query = req |> CRequest.uri |> Uri.query in
  let c = C.load_http host ~verb ~path:(Uri.path uri) in
  let pages = !c.handlers |> TL.http_handlers in
  let pages =
    if List.length pages > 1
    then List.filter pages
        ~f:(fun h ->
            not (Http.has_route_variables
                   (Handler.event_name_for_exn h)))
    else pages in

  match pages with
  | [] when String.Caseless.equal verb "OPTIONS" ->
    options_handler ~execution_id !c req
  | [] ->
    PReq.from_request headers query body
    |> PReq.to_dval
    |> Stored_event.store_event !c.id ("HTTP", Uri.path uri, verb) ;
    let resp_headers = Cohttp.Header.of_list [cors] in
    respond ~resp_headers ~execution_id `Not_found "404: No page matches"
  | a :: b :: _ ->
    let resp_headers = Cohttp.Header.of_list [cors] in
    respond `Internal_server_error ~resp_headers ~execution_id
      "500: More than one page matches"
  | [page] ->
    let input = PReq.from_request headers query body in
    (match (Handler.module_for page, Handler.modifier_for page) with
    | (Some m, Some mo) ->
      (* Store the event with the input path not the event name, because we
       * want to be able to
       *    a) use this event if this particular handler changes
       *    b) use the input url params in the analysis for this handler
       *)
      let desc = (m, Uri.path uri, mo) in
      Stored_event.store_event !c.id desc (PReq.to_dval input)
    | _-> ());

    let route = Handler.event_name_for_exn page in
    let bound = Http.bind_route_params_exn ~path:(Uri.path uri) ~route in
    let result = Libexecution.Execution.execute_handler page
        ~execution_id
        ~account_id:!c.owner
        ~canvas_id:!c.id
        ~user_fns:!c.user_functions
        ~tlid:page.tlid
        ~dbs:(TL.dbs !c.dbs)
        ~input_vars:([("request", PReq.to_dval input)] @ bound)
    in
    let maybe_infer_headers resp_headers value =
      if List.Assoc.mem resp_headers ~equal:(=) "Content-Type"
      then
        resp_headers
      else
        match value with
        | RTT.DObj _ | RTT.DList _ ->
          List.Assoc.add
            resp_headers
            ~equal:(=)
            "Content-Type"
            "application/json; charset=utf-8"
        | _ ->
          List.Assoc.add
            resp_headers
            ~equal:(=)
            "Content-Type"
            "text/plain; charset=utf-8"
    in
    (match result with
    | RTT.DResp (http, value) ->
      (match http with
       | Redirect url ->
         S.respond_redirect (Uri.of_string url) ()
       | Response (code, resp_headers) ->
         let body =
           if List.exists resp_headers ~f:(fun (name, value) ->
              String.lowercase name = "content-type"
              && String.is_prefix value ~prefix:"text/html")
           then Dval.to_human_repr value
           (* TODO: only pretty print for a webbrowser *)
           else
             Dval.dval_to_pretty_json_string value
         in
         let resp_headers = maybe_infer_headers resp_headers value in
         let status = Cohttp.Code.status_of_code code in
         let resp_headers = Cohttp.Header.of_list ([cors]
                                                   @ resp_headers)
         in
         respond ~resp_headers ~execution_id status body)
    | _ ->
      (match result with
      | DIncomplete ->
        respond ~execution_id `Internal_server_error
          "Program error: program was incomplete"
      | _ ->
        let body = Dval.dval_to_pretty_json_string result in
        let ct_headers = maybe_infer_headers [] result in
        let resp_headers = Cohttp.Header.of_list ([cors] @ ct_headers) in
        (* for demonstrations sake, let's return 200 Okay when
         * no HTTP response object is returned *)
        respond ~resp_headers ~execution_id `OK body))


(* -------------------------------------------- *)
(* Admin server *)
(* -------------------------------------------- *)
let admin_rpc_handler ~(execution_id: Types.id) (host: string) body : (Cohttp.Header.t * string) =
  try
    let (t1, params) = time "1-read-api-ops"
      (fun _ -> Api.to_rpc_params body) in

    let exe_fn_ids = [] in
    let tlids = List.filter_map ~f:Op.tlidOf params.ops in

    let (t2, c) = time "2-load-saved-ops"
      (fun _ ->
        C.load_only ~tlids host params.ops)
    in

    let (t3, hvals) = time "3-handler-analyses"
      (fun _ ->
         !c.handlers
         |> List.filter_map ~f:TL.as_handler
         |> List.map
           ~f:(Analysis.handler_analysis ~exe_fn_ids ~execution_id !c))
    in

    let (t4, fvals) = time "4-user-fn-analyses"
      (fun _ ->
        !c.user_functions
        |> List.filter ~f:(fun f -> List.mem ~equal:(=) tlids f.tlid)
        |> List.map
          ~f:(Analysis.user_fn_analysis ~exe_fn_ids ~execution_id !c))
    in
    let (t5, unlocked) = time "5-analyze-unlocked-dbs"
      (fun _ -> Analysis.unlocked !c) in


    let (t6, result) = time "6-to-frontend"
      (fun _ -> Analysis.to_rpc_response_frontend !c (hvals @ fvals) unlocked) in

    let (t7, _) = time "7-save-to-disk"
      (fun _ ->
        (* work out the result before we save it, incase it has a
         stackoverflow or other crashing bug *)
        if Api.causes_any_changes params
        then C.save_tlids !c tlids
        else ()
      ) in

  (server_timing [t1; t2; t3; t4; t5; t6; t7], result)
  with
  | e ->
    raise e

let initial_load ~(execution_id: Types.id) (host: string) body : (Cohttp.Header.t * string) =
  try
    let (t1, c) = time "1-load-saved-ops"
      (fun _ ->
        C.load_all host []) in

    let (t2, unlocked) = time "2-analyze-unlocked-dbs"
      (fun _ -> Analysis.unlocked !c) in

    let (t3, result) = time "3-to-frontend"
        (fun _ -> Analysis.to_rpc_response_frontend !c [] unlocked) in

  (server_timing [t1; t2; t3], result)
  with
  | e ->
    raise e



let execute_function ~(execution_id: Types.id) (host: string) body : (Cohttp.Header.t * string) =
  try
    let (t1, params) = time "1-read-api-ops"
      (fun _ -> Api.to_execute_function_params body) in

    let exe_fn_ids = params.executable_fns in
    let tlids = List.map exe_fn_ids ~f:Tuple.T3.get1 in

    let (t2, c) = time "2-load-saved-ops"
      (fun _ -> C.load_only ~tlids host []) in

    let (t3, hvals) = time "3-handler-analyses"
      (fun _ ->
         !c.handlers
         |> List.filter_map ~f:TL.as_handler
         |> List.map
           ~f:(Analysis.handler_analysis ~exe_fn_ids ~execution_id !c))
    in

    let (t4, fvals) = time "4-user-fn-analyses"
      (fun _ ->
        !c.user_functions
        |> List.filter ~f:(fun f -> List.mem ~equal:(=) tlids f.tlid)
        |> List.map
          ~f:(Analysis.user_fn_analysis ~exe_fn_ids ~execution_id !c))
    in

    let (t5, result) = time "5-to-frontend"
      (fun _ ->
        Analysis.to_execute_function_response_frontend exe_fn_ids (hvals @ fvals)) in

  (server_timing [t1; t2; t3; t4; t5], result)
  with
  | e ->
    raise e

let get_analysis ~(execution_id: Types.id) (host: string) (body: string) : (Cohttp.Header.t * string) =
  try
    let (t1, tlids) = time "1-read-api-tlids"
      (fun _ -> Api.to_analysis_params body) in

    let (t2, c) = time "2-load-saved-ops"
      (fun _ -> C.load_only ~tlids host []) in

    let (t3, f404s) = time "3-get-404s"
      (fun _ -> Analysis.get_404s !c) in

    let (t4, hvals) = time "4-handler-analyses"
      (fun _ ->
         !c.handlers
         |> List.filter_map ~f:TL.as_handler
         |> List.map
           ~f:(Analysis.handler_analysis ~exe_fn_ids:[] ~execution_id !c))
    in

    let (t5, fvals) = time "5-user-fn-analyses"
      (fun _ ->
        !c.user_functions
        |> List.filter ~f:(fun f -> List.mem ~equal:(=) tlids f.tlid)
        |> List.map
          ~f:(Analysis.user_fn_analysis ~exe_fn_ids:[] ~execution_id !c))
    in

    let (t6, unlocked) = time "6-analyze-unlocked-dbs"
      (fun _ -> Analysis.unlocked !c) in

    let (t7, result) = time "7-to-frontend"
      (fun _ -> Analysis.to_getanalysis_frontend (hvals @ fvals) unlocked f404s !c) in

  (server_timing [t1; t2; t3; t4; t5; t6; t7], result)
  with
  | e ->
    raise e



let admin_ui_handler ~(debug:bool) () =
  let template = File.readfile_lwt ~root:Templates "ui.html" in
  template
  >|= Util.string_replace "{ALLFUNCTIONS}" (Api.functions ())
  >|= Util.string_replace "{ROLLBARCONFIG}" (Config.rollbar_js)
  >|= Util.string_replace "{ELMDEBUG}" (if debug
                                      then "-debug"
                                      else "")

let save_test_handler ~(execution_id: Types.id) host =
  let g = C.load_all host [] in
  let filename = C.save_test !g in
  respond ~execution_id `OK ("Saved as: " ^ filename)


let auth_then_handle ~(execution_id: Types.id) req host handler =
  let path = req |> CRequest.uri |> Uri.path in
  let headers = req |> CRequest.headers in
  if not (String.is_prefix ~prefix:"/admin" path)
  then
    handler (Header.init ())
  else
    let run_handler ~auth_domain ~username headers =
      let permission = Account.get_permissions ~auth_domain ~username () in
      let permission_needed =
        if (String.is_prefix ~prefix:"/admin/ops/" path)
        then `Operations
        else if (String.is_prefix ~prefix:"/admin" path)
        then `Edit
        else `None
      in
      match (permission_needed, permission) with
      | (_, Account.CanAccessOperations)
      | (`None, _)
      | (`Edit, Account.CanEdit) ->
        handler headers
      | _ -> respond ~execution_id `Unauthorized "Unauthorized"
    in

    (* only handle auth for admin routes *)
    (* let users use their domain as a prefix for scratch work *)
    let auth_domain = Account.auth_domain_for host in
    match%lwt Auth.Session.of_request req with
    | Ok (Some session) ->
      if path = "/admin/logout"
      then
        (Auth.Session.clear Auth.Session.backend session;%lwt
        let headers =
          (Header.of_list
             (Auth.Session.clear_hdrs Auth.Session.cookie_key)) in
        S.respond_redirect ~headers ~uri:(Uri.of_string "/admin/ui") ())
      else
        let username = Auth.Session.username_for session in
        run_handler ~auth_domain ~username (Header.init ())
    | _ ->
      match Header.get_authorization headers with
      | (Some (`Basic (username, password))) ->
        (if Account.authenticate ~username ~password
         then
           let%lwt session = Auth.Session.new_for_username username in
           let headers =
             Header.of_list
               (Auth.Session.to_cookie_hdrs Auth.Session.cookie_key session)
           in
           run_handler ~auth_domain ~username headers
         else
          respond ~execution_id `Unauthorized "Bad credentials")
      | None ->
        S.respond_need_auth ~auth:(`Basic "dark") ()
      | _ ->
        respond ~execution_id `Unauthorized "Invalid session"

let admin_handler ~(execution_id: Types.id) ~(host: string) ~(uri: Uri.t) ~stopper
  ~(body: string) (req: CRequest.t) resp_headers =
  let verb = req |> CRequest.meth in
  let json_hdrs hdrs =
    Header.add_list resp_headers
      (hdrs
       |> Header.to_list
       |> List.cons ("Content-type",  "application/json; charset=utf-8"))
  in
  let html_hdrs =
    Header.add_list resp_headers
      [ ("Content-type", "text/html; charset=utf-8")
      (* Don't allow any other websites to put this in an iframe;
         this prevents "clickjacking" at tacks.
         https://www.owasp.org/index.php/Clickjacking_Defense_Cheat_Sheet#Content-Security-Policy:_frame-ancestors_Examples
         It would be nice to use CSP to limit where we can load scripts etc from,
         but right now we load from CDNs, <script> tags, etc. So the only thing
         we could do is script-src: 'unsafe-inline', which doesn't offer us
         any additional security. *)
      ; ("Content-security-policy", "frame-ancestors 'none';")
      ]
  in
  match (verb, Uri.path uri) with
  | (`POST, "/admin/api/rpc") ->
    let (resp_headers, response_body) = admin_rpc_handler ~execution_id host body in
    respond ~resp_headers:(json_hdrs resp_headers) ~execution_id `OK response_body
  | (`POST, "/admin/api/initial_load") ->
    let (resp_headers, response_body) = initial_load ~execution_id host body in
    respond ~resp_headers:(json_hdrs resp_headers) ~execution_id `OK response_body
  | (`POST, "/admin/api/execute_function") ->
    let (resp_headers, response_body) = execute_function ~execution_id host body in
    respond ~resp_headers:(json_hdrs resp_headers) ~execution_id `OK response_body
  | (`POST, "/admin/api/get_analysis") ->
    let (resp_headers, response_body) = get_analysis ~execution_id host body in
    respond ~resp_headers:(json_hdrs resp_headers) ~execution_id `OK response_body
  | (`POST, "/admin/api/shutdown") when Config.allow_server_shutdown ->
    Lwt.wakeup stopper ();
    respond ~execution_id `OK "Disembowelment"
  | (`POST, "/admin/api/clear-benchmarking-data") ->
    Db.delete_benchmarking_data ();
    respond ~execution_id `OK "Cleared"
  | (`POST, "/admin/api/save_test") when Config.allow_test_routes ->
    save_test_handler ~execution_id host
  | (`GET, "/admin/ui-debug") ->
    let%lwt body = admin_ui_handler ~debug:true () in
    respond
      ~resp_headers:html_hdrs
      ~execution_id
      `OK body
  | (`GET, "/admin/ui") ->
    let%lwt body = admin_ui_handler ~debug:false () in
    respond
      ~resp_headers:html_hdrs
      ~execution_id
      `OK body
  | (`GET, "/admin/integration_test") when Config.allow_test_routes ->
    Canvas.load_and_resave_from_test_file host;
    let%lwt body = admin_ui_handler ~debug:false () in
    respond
      ~resp_headers:html_hdrs
      ~execution_id
      `OK body
  | (`POST, "/admin/ops/migrate-all-canvases") ->
    Canvas.migrate_all_hosts ();
    respond ~execution_id `OK "Migrated"
  | (`POST, "/admin/ops/check-all-canvases") ->
    Canvas.check_all_hosts ();
    respond ~execution_id `OK "Checked"
  | (`GET, "/admin/ops/check-all-canvases") ->
    respond ~execution_id `OK "<html><body><form action='/admin/ops/check-all-canvases' method='post'><input type='submit' value='Check all canvases'></form><form action='/admin/ops/migrate-all-canvases' method='post'><input type='submit' value='Migrate all canvases'></form></body></html>"
  | _ ->
    respond ~execution_id `Not_found "Not found"

(* -------------------------------------------- *)
(* The server *)
(* -------------------------------------------- *)

let static_handler uri =
  let fname = S.resolve_file ~docroot:(Config.dir Config.Webroot) ~uri in
  S.respond_file ~fname ()


(* Proxies that terminate HTTPs should give us X-Forwarded-Proto: http
   or X-Forwarded-Proto: https.

   Return the URI, adding the scheme to the URI if there is an X-Forwarded-Proto. *)
let with_x_forwarded_proto req =
  match Header.get (CRequest.headers req) "X-Forwarded-Proto" with
  | Some proto -> Uri.with_scheme
                   (CRequest.uri req)
                   (Some proto)
  | None -> CRequest.uri req

let redirect_to uri =
  let proto = uri
              |> Uri.scheme
              |> Option.value ~default:"" in
  let parts = uri
              |> Uri.host
              |> Option.value ~default:""
              |> (fun h -> String.split h '.')
  in
  match (proto, parts) with
  | ("http", ["builtwithdark"; "com"; ])
  | ("http", [_; "builtwithdark"; "com"; ]) ->
     (* If it's http and on a domain that can be served with https,
        we want to redirect to the same url but with the scheme
        replaced by "https". *)
     Some "https" |> Uri.with_scheme uri |> Some
  | _ -> None

let server () =
  let stop,stopper = Lwt.wait () in

  let callback (ch, conn) req body =
    let execution_id = Util.create_id () in
    if !shutdown then respond ~execution_id `Service_unavailable "Shutting down" else

    let handle_error ~(include_internals:bool) (e:exn) =
      try
        let bt = Exception.get_backtrace () in
        let%lwt _ =
          Rollbar.report_lwt
            e
            bt
            (Remote (request_to_rollbar body req))
            (Types.show_id execution_id)
        in
        let real_err =
          try
            match e with
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
        Log.erroR real_err ~bt ~params:["execution_id", Log.dump execution_id];
        let resp_headers = Cohttp.Header.of_list [cors] in
        match e with
        | Exception.DarkException e when e.tipe = EndUser ->
          respond ~resp_headers ~execution_id `Bad_request e.short
        | _ ->
          let body =
            if include_internals
            then real_err
            else "Dark Internal Error"
          in
          respond ~resp_headers ~execution_id `Internal_server_error body
      with e ->
        let bt = Exception.get_backtrace () in
        Rollbar.last_ditch e ~bt "handle_error" (Types.show_id execution_id);
        respond ~execution_id `Internal_server_error "unhandled error"
    in


    try
      let host =
        req
        |> CRequest.uri
        |> Uri.host
        |> Option.bind
          ~f:(fun host ->
              match String.split host '.' with
              (* For production *)
              | ["darksingleinstance"; "com"] -> Some "darksingleinstance"
              | ["builtwithdark"; "com"] -> Some "builtwithdark"
              | [a; "darksingleinstance"; "com"] -> Some a
              | [a; "builtwithdark"; "com"] -> Some a

              (* For customers *)
              | [a; "dabblefox"; "com"] -> Some ("dabblefox-" ^ a)

              (* For development and testing *)
              | ["localhost"] -> Some "localhost"
              | [a; "integration-tests"] -> Some a
              | [a; "localhost"] -> Some a

              | _ -> None)
      in

      let handler resp_headers =
        let ip = get_ip_address ch in
        let uri = req |> CRequest.uri in

        Log.infO "request"
          ~params:[ "host", Option.value ~default:"none" host
                  ; "ip", ip
                  ; "method", req
                              |> CRequest.meth
                              |> Cohttp.Code.string_of_method
                  ; "uri", Uri.to_string uri
                  ; "execution_id", Log.dump execution_id
                  ];

        match (Uri.path uri, host) with
        | (_, None) ->
          respond ~execution_id `Not_found "Not found"
        | ("/sitemap.xml", _)
        | ("/favicon.ico", _) ->
         respond ~execution_id `OK ""
        | (p, _) when (String.is_prefix ~prefix:"/static/" p) ->
          static_handler uri
        | (p, Some host) ->
          if String.is_prefix ~prefix:"/admin/" p
          then
            try
              admin_handler ~execution_id ~host ~uri ~body ~stopper req resp_headers
            with e -> handle_error ~include_internals:true e
          else
            (* caught by a handle_error a bit lower *)
            user_page_handler ~execution_id ~host ~ip ~uri ~body req
      in
      match redirect_to (with_x_forwarded_proto req) with
      | Some x -> S.respond_redirect ~uri:x ()
      | _ -> match (req |> CRequest.uri |> Uri.path, host) with
            (* This seems like it should be moved closer to the admin handler,
             * but don't do that - that makes Lwt swallow our exceptions. *)
            | (_, Some host) ->
               auth_then_handle ~execution_id req host handler
            | ("/", None) -> (* for GKE health check *)
               (match Dbconnection.status () with
                | `Healthy ->
                   if (not !ready) (* ie. liveness check has found a service with 2 minutes of failing readiness checks *)
                   then begin
                       Log.infO "Liveness check found unready service, returning unavailable";
                       respond ~execution_id `Service_unavailable "Service not ready"
                     end
                   else
                     respond ~execution_id `OK "Hello internal overlord"
                | `Disconnected -> respond ~execution_id `Service_unavailable "Sorry internal overlord")
            | ("/ready", None) ->
               (match Dbconnection.status () with
                | `Healthy ->
                   if !ready
                   then
                     respond ~execution_id `OK "Hello internal overlord"
                   else begin
                       (* exception here caught by handle_error *)
                       Canvas.check_tier_one_hosts ();
                       Log.infO "All canvases loaded correctly - Service ready";
                       ready := true;
                       respond ~execution_id `OK "Hello internal overlord"
                     end
                | `Disconnected -> respond ~execution_id `Service_unavailable "Sorry internal overlord")
            | ("/pkill", None) -> (* for GKE graceful termination *)
               if !shutdown (* note: this is a ref, not a boolean `not` *)
               then
                 (shutdown := true;
                  Log.infO "shutdown"
                    ~data:"Received shutdown request - shutting down"
                    ~params:["execution_id", Types.string_of_id execution_id];
                  (* k8s gives us 30 seconds, so ballpark 2s for overhead *)
                  Lwt_unix.sleep 28.0 >>= fun _ ->
                  Lwt.wakeup stopper ();
                  respond ~execution_id `OK "Terminated")
               else
                 (Log.infO "shutdown"
                    ~data:"Received redundant shutdown request - already shutting down"
                    ~params:["execution_id", Types.string_of_id execution_id];
                  respond ~execution_id `OK "Terminated")
            | (_, None) -> (* for GKE health check *)
               respond ~execution_id `Not_found "Not found"
    with e -> handle_error ~include_internals:false e

  in
  let cbwb conn req req_body =
    let%lwt body_string = Cohttp_lwt__Body.to_string req_body in
    callback conn req body_string in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback:cbwb ())

let run () =
  ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
