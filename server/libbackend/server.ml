open Core_kernel
open Libcommon

open Lwt
module Clu = Cohttp_lwt_unix
module S = Clu.Server
module CRequest = Clu.Request
module CResponse = Clu.Response
module Header = Cohttp.Header
module Cookie = Cohttp.Cookie
module Client = Clu.Client

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
    ~params:[ "status", Log.dump (Cohttp.Code.code_of_status status)
            ; "execution_id", Log.dump execution_id
            ; "headers", Log.dump resp_headers
            ; "body", Log.dump body
            ];
  S.respond_string ~status ~body ~headers:resp_headers ()

let should_use_https uri =
  let parts = uri
              |> Uri.host
              |> Option.value ~default:""
              |> (fun h -> String.split h '.')
  in
  match parts with
  | ["darklang"; "com"; ]
  | ["builtwithdark"; "com"; ]
  | [_; "builtwithdark"; "com"; ] -> true
  | _ -> false


let redirect_to uri =
  let proto = uri
              |> Uri.scheme
              |> Option.value ~default:"" in
  (* If it's http and on a domain that can be served with https,
     we want to redirect to the same url but with the scheme
     replaced by "https". *)
  if proto = "http" && should_use_https uri
  then Some "https" |> Uri.with_scheme uri |> Some
  else None

(* there might be some better way to do this... *)
let over_headers (r : CResponse.t) ~(f : Header.t -> Header.t) : CResponse.t  =
  CResponse.make
    ~version:(CResponse.version r)
    ~status:(CResponse.status r)
    ~flush:(CResponse.flush r)
    ~encoding:(CResponse.encoding r)
    ~headers:(r |> CResponse.headers |> f)
    ()

let over_headers_promise
      (resp_promise: (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t)
      ~(f : Header.t -> Header.t)
    : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let%lwt (resp, body) = resp_promise in
  return (over_headers ~f resp, body)

let wrap_json_headers =
  let json_headers = [("Content-type",  "application/json; charset=utf-8") ] in
  over_headers_promise ~f:(fun h -> Header.add_list h json_headers)

(* Proxies that terminate HTTPs should give us X-Forwarded-Proto: http
   or X-Forwarded-Proto: https.

   Return the URI, adding the scheme to the URI if there is an X-Forwarded-Proto. *)
let with_x_forwarded_proto req =
  match Header.get (CRequest.headers req) "X-Forwarded-Proto" with
  | Some proto -> Uri.with_scheme
                   (CRequest.uri req)
                   (Some proto)
  | None -> CRequest.uri req

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

let user_page_handler ~(execution_id: Types.id) ~(canvas: string) ~(ip: string) ~(uri: Uri.t)
      ~(body: string) (req: CRequest.t) =
  (* HACK temporarily redirect /admin/ui to the right url.
     remove this once customers understand it's the right place. *)
  Log.infO "user_page_handler" ~params:["uri", Uri.to_string uri];
  if Uri.path uri = "/admin/ui"
  then
    (* change the domain to the admin host and the
       path to /a/canvas. *)
    let (host, port) =
      match String.lsplit2 ~on:':' Config.admin_host with
      | None -> (Config.admin_host, None)
      | Some (a, b) -> (a, Some (int_of_string b))
    in
    let uri = with_x_forwarded_proto req
              |> (fun u -> Uri.with_host u (Some host))
              |> (fun u -> Uri.with_port u port)
              |> (fun u -> Uri.with_path u ("/a/" ^ Uri.pct_encode canvas))
    in S.respond_redirect ~uri ()
  else
    let verb = req |> CRequest.meth |> Cohttp.Code.string_of_method in
    let headers = req |> CRequest.headers |> Header.to_list in
    let query = req |> CRequest.uri |> Uri.query in
    let c = C.load_http canvas ~verb ~path:(Uri.path uri) in
    let pages = !c.handlers |> TL.http_handlers in
    let pages =
      if List.length pages > 1
      then List.filter pages
             ~f:(fun h ->
               not (Http.has_route_variables
                    (Handler.event_name_for_exn h)))
      else pages in

    let trace_id = Util.create_uuid () in
    let canvas_id = !c.id in
    match pages with
    | [] when String.Caseless.equal verb "OPTIONS" ->
       options_handler ~execution_id !c req
    | [] ->
       PReq.from_request headers query body
       |> PReq.to_dval
       |> Stored_event.store_event ~trace_id ~canvas_id ("HTTP", Uri.path uri, verb) ;
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
           Stored_event.store_event ~trace_id ~canvas_id desc (PReq.to_dval input)
        | _-> ());

       let bound = Libexecution.Execution.http_route_input_vars
                     page (Uri.path uri)
       in
       let result = Libexecution.Execution.execute_handler page
                      ~execution_id
                      ~account_id:!c.owner
                      ~canvas_id
                      ~user_fns:!c.user_functions
                      ~tlid:page.tlid
                      ~dbs:(TL.dbs !c.dbs)
                      ~input_vars:([("request", PReq.to_dval input)] @ bound)
                      ~store_fn_arguments:(Stored_function_arguments.store ~canvas_id ~trace_id)
                      ~store_fn_result:(Stored_function_result.store ~canvas_id ~trace_id)
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
       match result with
       | DIncomplete ->
          respond ~execution_id `Internal_server_error
            "Program error: program was incomplete"
       | RTT.DResp (Redirect url, value) ->
          S.respond_redirect (Uri.of_string url) ()
       | RTT.DResp (Response (code, resp_headers), value) ->
          let body =
            if List.exists resp_headers ~f:(fun (name, value) ->
                   String.lowercase name = "content-type"
                   && String.is_prefix value ~prefix:"text/html")
            then Dval.to_human_repr value
                                    (* TODO: only pretty print for a webbrowser *)
            else
              Dval.unsafe_dval_to_pretty_json_string value
          in
          let resp_headers = maybe_infer_headers resp_headers value in
          let status = Cohttp.Code.status_of_code code in
          let resp_headers = Cohttp.Header.of_list ([cors]
                                                    @ resp_headers)
          in
          respond ~resp_headers ~execution_id status body
       | _ ->
          let body = Dval.unsafe_dval_to_pretty_json_string result in
          let ct_headers = maybe_infer_headers [] result in
          let resp_headers = Cohttp.Header.of_list ([cors] @ ct_headers) in
          (* for demonstrations sake, let's return 200 Okay when
           * no HTTP response object is returned *)
          respond ~resp_headers ~execution_id `OK body

(* -------------------------------------------- *)
(* Admin server *)
(* -------------------------------------------- *)
let admin_rpc_handler ~(execution_id: Types.id) (host: string) body
    :  (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let (t1, params) = time "1-read-api-ops"
      (fun _ -> Api.to_rpc_params body) in

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
           ~f:(fun h -> (h.tlid, Analysis.traces_for_handler !c h)))
    in

    let (t4, fvals) = time "4-user-fn-analyses"
      (fun _ ->
        []
        (* !c.user_functions *)
        (* |> List.filter ~f:(fun f -> List.mem ~equal:(=) tlids f.tlid) *)
        (* |> List.map *)
        (*   ~f:(fun f -> (f.tlid, Analysis.initial_input_vars_for_user_fn !c f)) *)
        )
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

    respond ~resp_headers:(server_timing [t1; t2; t3; t4; t5; t6; t7]) ~execution_id `OK result
  with
  | e ->
    raise e

let initial_load ~(execution_id: Types.id) (host: string) body
  : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let (t1, c) = time "1-load-saved-ops"
    (fun _ ->
      C.load_all host []) in

  let (t2, unlocked) = time "2-analyze-unlocked-dbs"
    (fun _ -> Analysis.unlocked !c) in

  let (t3, result) = time "3-to-frontend"
      (fun _ -> Analysis.to_rpc_response_frontend !c [] unlocked) in

  respond ~execution_id ~resp_headers:(server_timing [t1; t2; t3]) `OK result

let execute_function ~(execution_id: Types.id) (host: string) body
  : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let (t1, params) = time "1-read-api-ops"
    (fun _ -> Api.to_execute_function_params body)
  in

  let (t2, c) = time "2-load-saved-ops"
    (fun _ -> C.load_only ~tlids:[params.tlid] host [])
  in

  let (t3, result) = time "3-execute"
    (fun _ ->
       Analysis.call_function !c params.fnname
         ~execution_id
         ~tlid:params.tlid
         ~trace_id:params.trace_id
         ~caller_id:params.caller_id
         ~args:params.args)
  in
  let (t4, response) = time "4-to-frontend"
    (fun _ ->
      Analysis.to_execute_function_response_frontend
        (Dval.hash params.args)
        result)
  in
  respond ~execution_id ~resp_headers:(server_timing [t1; t2; t3; t4]) `OK response

let get_analysis ~(execution_id: Types.id) (host: string) (body: string)
        : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
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
           ~f:(fun h -> (h.tlid, Analysis.traces_for_handler !c h)))
    in

    let (t5, fvals) = time "5-user-fn-analyses"
      (fun _ ->
        []
        (* !c.user_functions *)
        (* |> List.filter ~f:(fun f -> List.mem ~equal:(=) tlids f.tlid) *)
        (* |> List.map *)
        (*   ~f:(fun f -> (f.tlid, Analysis.initial_input_vars_for_user_fn !c f)) *)
        )
    in

    let (t6, unlocked) = time "6-analyze-unlocked-dbs"
      (fun _ -> Analysis.unlocked !c) in

    let (t7, result) = time "7-to-frontend"
      (fun _ -> Analysis.to_getanalysis_frontend (hvals @ fvals) unlocked f404s !c) in

    respond ~execution_id ~resp_headers:(server_timing [t1; t2; t3; t4; t5; t6; t7]) `OK result
  with
  | e ->
    raise e


let delete_404 ~(execution_id: Types.id) (host: string) body
        : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let (t1, c) = time "1-get-canvas"
      (fun _ -> C.load_all host []) in
    let (t2, p) = time "2-to-route-params"
      (fun _ -> Api.to_route_params body) in
    let (t3, _) = time "3-delete-404s"
      (fun _ -> Analysis.delete_404s !c p.space p.path p.modifier) in
    let (t4, f404s) = time "4-get-404s"
      (fun _ -> Analysis.get_404s !c) in
    let (t5, result) = time "5-to-frontend"
      (fun _ -> Analysis.to_getanalysis_frontend [] [] f404s !c) in
    respond ~execution_id ~resp_headers:(server_timing [t1; t2; t3; t4; t5]) `OK result
  with
  | e ->
    raise e


let admin_ui_html ~(debug:bool) () =
  let template = File.readfile_lwt ~root:Templates "ui.html" in
  template
  >|= Util.string_replace "{ALLFUNCTIONS}" (Api.functions ())
  >|= Util.string_replace "{LIVERELOADJS}"
    (if String.equal "dev" Config.env_display_name
      then "<script type=\"text/javascript\" src=\"//localhost:35729/livereload.js\"> </script>"
      else "")
  >|= Util.string_replace "{STATIC}" Config.static_host
  >|= Util.string_replace "{ROLLBARCONFIG}" (Config.rollbar_js)
  >|= Util.string_replace "{USER_CONTENT_HOST}" Config.user_content_host
  >|= Util.string_replace "{ELMDEBUG}" (if debug then "-debug" else "")
  >|= Util.string_replace "{ENVIRONMENT_NAME}" Config.env_display_name


let save_test_handler ~(execution_id: Types.id) host =
  let g = C.load_all host [] in
  let filename = C.save_test !g in
  respond ~execution_id `OK ("Saved as: " ^ filename)

(* Checks for a cookie, prompts for basic auth if there isn't one,
   returns Unauthorized if basic auth doesn't work.

   Importantly this performs no authorization. Just authentication.

   Also implements logout (!). *)
let authenticate_then_handle ~(execution_id: Types.id) handler req =
  let path = req |> CRequest.uri |> Uri.path in
  let headers = req |> CRequest.headers in

  match%lwt Auth.Session.of_request req with
  | Ok (Some session) ->
     let username = Auth.Session.username_for session in
     if path = "/logout"
     then
       (Auth.Session.clear Auth.Session.backend session;%lwt
        let headers =
          (Header.of_list
             (Auth.Session.clear_hdrs Auth.Session.cookie_key)) in
            let uri = Uri.of_string ("/a/" ^ Uri.pct_encode username) in
            S.respond_redirect ~headers ~uri ())
     else
       handler ~username req
  | _ ->
     match Header.get_authorization headers with
     | (Some (`Basic (username, password))) ->
        (if Account.authenticate ~username ~password
         then
           let%lwt session = Auth.Session.new_for_username username in
           let https_only_cookie = req |> CRequest.uri |> should_use_https in
           let headers = Auth.Session.to_cookie_hdrs
                           ~http_only:true
                           ~secure:https_only_cookie
                           ~path:"/"
                           Auth.Session.cookie_key session
           in
           over_headers_promise ~f:(fun h -> Header.add_list h headers)
             (handler ~username req)
         else
           respond ~execution_id `Unauthorized "Bad credentials")
     | None ->
        S.respond_need_auth ~auth:(`Basic "dark") ()
     | _ ->
        respond ~execution_id `Unauthorized "Invalid session"


let admin_ui_handler ~(execution_id: Types.id) ~(path: string list) ~stopper
      ~(body: string) ~(username:string) (req: CRequest.t) =
  let verb = req |> CRequest.meth in
  let html_hdrs =
    Header.of_list
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
  (* this could be more middleware like in the future *if and only if* we
     only make changes in promises .*)
  let when_can_edit ~canvas f =
    if Account.can_edit_canvas ~auth_domain:(Account.auth_domain_for canvas) ~username
    then f ()
    else respond ~execution_id `Unauthorized "Unauthorized"
  in
  match (verb, path) with
  (* Canvas webpages... *)
  | (`GET, [ "a" ; canvas ; "integration_test" ]) when Config.allow_test_routes ->
     when_can_edit ~canvas
       (fun _ ->
         Canvas.load_and_resave_from_test_file canvas;
         let%lwt body = admin_ui_html ~debug:false () in
         respond
           ~resp_headers:html_hdrs
           ~execution_id
           `OK body)
  | (`GET, [ "a"; canvas; "ui-debug" ]) ->
     when_can_edit ~canvas
       (fun _ ->
         let%lwt body = admin_ui_html ~debug:true () in
         respond
           ~resp_headers:html_hdrs
           ~execution_id
           `OK body)
  | (`GET, [ "a" ; canvas; ]) ->
     when_can_edit ~canvas
       (fun _ ->
         let%lwt body = admin_ui_html ~debug:false () in
         respond
           ~resp_headers:html_hdrs
           ~execution_id
           `OK body)
  | _ -> respond ~execution_id `Not_found "Not found"

let admin_api_handler ~(execution_id: Types.id) ~(path: string list) ~stopper
      ~(body: string) ~(username:string) (req: CRequest.t) =
  let verb = req |> CRequest.meth in
  (* this could be more middleware like in the future *if and only if* we
     only make changes in promises .*)
  let when_can_edit ~canvas f =
    if Account.can_edit_canvas ~auth_domain:(Account.auth_domain_for canvas) ~username
    then f ()
    else respond ~execution_id `Unauthorized "Unauthorized"
  in
  match (verb, path) with
  (* Operational APIs.... maybe these shouldn't be here, but
     they start with /api so they need to be. *)
  | (`POST, [ "api" ; "shutdown" ]) when Config.allow_server_shutdown ->
     Lwt.wakeup stopper ();
     respond ~execution_id `OK "Disembowelment"
  | (`POST, [ "api" ; "clear-benchmarking-data" ] ) ->
     Db.delete_benchmarking_data ();
     respond ~execution_id `OK "Cleared"
  | (`POST, [ "api" ; canvas; "save_test" ]) when Config.allow_test_routes ->
     save_test_handler ~execution_id canvas

  (* Canvas API *)
  | (`POST, [ "api" ; canvas ;  "rpc" ]) ->
     when_can_edit ~canvas
       (fun _ ->
         wrap_json_headers (admin_rpc_handler ~execution_id canvas body))
  | (`POST, [ "api" ; canvas ; "initial_load" ]) ->
     when_can_edit ~canvas
       (fun _ ->
         wrap_json_headers (initial_load ~execution_id canvas body))
  | (`POST, [ "api" ; canvas ; "execute_function" ]) ->
     when_can_edit ~canvas
       (fun _ ->
         wrap_json_headers (execute_function ~execution_id canvas body))
  | (`POST, [ "api" ; canvas ; "get_analysis" ]) ->
     when_can_edit ~canvas
       (fun _ ->
         wrap_json_headers (get_analysis ~execution_id canvas body))
  | (`POST, [ "api" ; canvas ; "delete_404" ]) ->
     when_can_edit ~canvas
       (fun _ ->
         wrap_json_headers (delete_404 ~execution_id canvas body))
  | _ -> respond ~execution_id `Not_found "Not found"

let ops_api_handler ~(execution_id: Types.id) ~(path: string list) ~stopper
      ~(body: string) ~(username:string) (req: CRequest.t) =
  let verb = req |> CRequest.meth in
  (* this could be more middleware like in the future *if and only if* we
     only make changes in promises .*)
  let when_can_ops f =
    if Account.can_access_operations username
    then f ()
    else respond ~execution_id `Unauthorized "Unauthorized"
  in
  match (verb, List.drop path 1) with
  | (`POST, [ "migrate-all-canvases" ]) ->
     when_can_ops
       (fun _ ->
         Canvas.migrate_all_hosts ();
         respond ~execution_id `OK "Migrated")
  | (`POST, [ "check-all-canvases" ]) ->
     when_can_ops
       (fun _ ->
         Canvas.check_all_hosts ();
         respond ~execution_id `OK "Checked")
  | (`POST, [ "cleanup-old-traces" ]) ->
     when_can_ops
       (fun _ ->
         Canvas.cleanup_old_traces ();
         respond ~execution_id `OK "Cleanedup")
    | (`GET, [ "check-all-canvases" ]) ->
       when_can_ops
         (fun _ ->
           respond ~execution_id `OK "<html>
           <body>
           <form action='/admin/ops/check-all-canvases' method='post'>
           <input type='submit' value='Check all canvases'>
           </form>
           <form action='/admin/ops/migrate-all-canvases' method='post'>
           <input type='submit' value='Migrate all canvases'>
           </form>
           <form action='/admin/ops/cleanup-old-traces' method='post'>
           <input type='submit' value='Cleanup old traces (done nightly by cron)'>
           </form>
           </body></html>")
  | _ ->
     respond ~execution_id `Not_found "Not found"

let admin_handler ~(execution_id: Types.id) ~(uri: Uri.t) ~stopper
      ~(body: string) ~(username:string) (req: CRequest.t) =
  let path = uri
             |> Uri.path
             |> String.lstrip ~drop:((=) '/')
             |> String.rstrip ~drop:((=) '/')
             |> String.split ~on:'/' in

  (* routing *)
  match path with
  | "ops" :: _ ->  ops_api_handler ~execution_id ~path ~stopper ~body ~username req
  | "api" :: _ ->  admin_api_handler ~execution_id ~path ~stopper ~body ~username req
  | "a" :: _ ->  admin_ui_handler ~execution_id ~path ~stopper ~body ~username req
  | _ -> respond ~execution_id `Not_found "Not found"

(* -------------------------------------------- *)
(* The server *)
(* -------------------------------------------- *)

let static_etag_for =
  (* Read the etags.json JSON document here so that it reads
     at load-time, not at each call-time.

     The server gets restarted after we get new etags, so we
     don't need to worry about updates here. *)
  let etags_json =
    File.readfile ~root:Config.Webroot "etags.json"
    |> Yojson.Basic.from_string
  in
  (fun uri ->
    try
      etags_json
      (* Get the JSON field that corresponds to the filename,
         stripped of the leftmost /. *)
      |> Yojson.Basic.Util.member
           (uri |> Uri.path |> String.lstrip ~drop:((=) '/'))
      |> Yojson.Basic.Util.to_string
      |> (fun x -> [("etag", x)])
    with e ->
      [])

let static_handler uri =
  let fname = S.resolve_file ~docroot:(Config.dir Config.Webroot) ~uri in
  S.respond_file
    ~headers:(Header.of_list (cors :: static_etag_for uri))
    ~fname
    ()


type host_route =
  | Canvas of string
  | Static
  | Admin

let route_host req =
  match req
        |> CRequest.uri
        |> Uri.host
        |> Option.value ~default:""
        |> (fun h -> String.split h '.') with
  | [ "static"; "darklang"; "localhost" ]
  | [ "static" ; "darklang"; "com" ]
  | [ "static" ; "integration-tests" ]
  -> Some Static

  (* Dark canvases *)
  | [a; "builtwithdark"; "com" ; ]
  | [a; "builtwithdark"; "localhost" ; ]
  | [a; "integration-tests"]
  | [a; "darksingleinstance"; "com"]
    -> Some (Canvas a)

  (* Specific Dark canvas: builtwithdark *)
  | ["builtwithdark"; "localhost" ; ]
  | ["builtwithdark"; "com" ; ]
    -> Some (Canvas "builtwithdark")
  (* Specific Dark canvas: darksingleinstance *)
  | ["darksingleinstance"; "com"]
    -> Some (Canvas "darksingleinstance")
  | [a; "dabblefox"; "com" ]
    -> Some (Canvas ("dabblefox-" ^ a))

  (* admin interface + outer site, conditionally *)
  | ["integration-tests"]
  | ["darklang" ; "com" ]
  | ["darklang" ; "localhost" ]
  | ["dark_dev" ; "com" ]
    -> Some Admin

  (* Not a match... *)
  | _ -> None

let k8s_handler req ~execution_id ~stopper =
  match req |> CRequest.uri |> Uri.path with
  (* For GKE health check *)
  | "/" ->
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
  | "/ready" ->
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
  (* For GKE graceful termination *)
  | "/pkill" ->
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
  | _ -> respond ~execution_id `Not_found ""

let server () =
  let stop,stopper = Lwt.wait () in

  let callback (ch, conn) req body =
    let execution_id = Util.create_id () in
    let uri = CRequest.uri req in
    let ip = get_ip_address ch in

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
        match e with
        | Exception.DarkException e when e.tipe = EndUser ->
           respond ~execution_id `Bad_request e.short
        | _ ->
           let body =
             if include_internals
             then real_err
             else "Dark Internal Error"
           in
           respond ~execution_id `Internal_server_error body
      with e ->
        let bt = Exception.get_backtrace () in
        Rollbar.last_ditch e ~bt "handle_error" (Types.show_id execution_id);
        respond ~execution_id `Internal_server_error "unhandled error"
    in

    try
     Log.infO "request"
       ~params:[ "ip", ip
               ; "method", req
                           |> CRequest.meth
                           |> Cohttp.Code.string_of_method
               ; "uri", Uri.to_string uri
               ; "execution_id", Log.dump execution_id
       ];
     (* first: if this isn't https and should be, redirect *)
     match redirect_to (with_x_forwarded_proto req) with
       Some x -> S.respond_redirect ~uri:x ()
     | None ->

        match Uri.to_string uri with
        | "/sitemap.xml"
        | "/favicon.ico" ->
           respond ~execution_id `OK ""

        | _ ->
         (* figure out what handler to dispatch to... *)
         match route_host req with
         | Some (Canvas canvas) ->
            user_page_handler ~execution_id ~canvas ~ip ~uri ~body req

         | Some Static -> static_handler uri

         | Some Admin ->
            (try
               authenticate_then_handle ~execution_id
                 (fun ~username r ->
                    try
                      admin_handler ~execution_id ~uri ~body ~stopper ~username r
                    with e ->
                      handle_error ~include_internals:true e)
                 req
             with e ->
               handle_error ~include_internals:false e)
         | None -> k8s_handler req ~execution_id ~stopper

    with e ->
      handle_error ~include_internals:false e
  in
  let cbwb conn req req_body =
    let%lwt body_string = Cohttp_lwt__Body.to_string req_body in
    callback conn req body_string in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback:cbwb ())

let run () =
  ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
