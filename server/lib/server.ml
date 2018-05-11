open Core
open Lwt

module Clu = Cohttp_lwt_unix
module S = Clu.Server
module CRequest = Clu.Request
module Header = Cohttp.Header
module C = Canvas
module RT = Runtime
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module FF = Feature_flag


(* ------------------------------- *)
(* utils *)
(* ------------------------------- *)
type timing_header = string * float * string

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

let respond ?(headers=Header.init ()) status (body: string) =
  S.respond_string ~status ~body ~headers ()


(* ------------------------------- *)
(* handlers for dark developers *)
(* ------------------------------- *)

(* -------------------------------------------- *)
(* handlers for end users *)
(* -------------------------------------------- *)
let cors = ("Access-Control-Allow-Origin", "*")

let options_handler (c: C.canvas) (req: CRequest.t) =
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
  let headers = [( "Access-Control-Allow-Methods"
                 , "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS")
                ; ("Access-Control-Allow-Origin", "*")
                ; ("Access-Control-Allow-Headers", allow_headers)] in
  respond ~headers:(Cohttp.Header.of_list headers) `OK ""


let user_page_handler ~(host: string) ~(ip: string) ~(uri: Uri.t)
    ~(body: string) (req: CRequest.t) =
  let c = C.load host [] in
  let verb = req |> CRequest.meth |> Cohttp.Code.string_of_method in
  let pages = C.pages_matching_route ~uri ~verb !c in
  let pages =
    if List.length pages > 1
    then List.filter ~f:(fun (has_vars, _) -> not has_vars) pages
    else pages in
  let pages = List.map ~f:Tuple.T2.get2 pages in

  match pages with
  | [] when String.Caseless.equal verb "OPTIONS" ->
    options_handler !c req
  | [] ->
    let input = PReq.from_request req body in
    Stored_event.store_event host ("HTTP", Uri.path uri, verb) (PReq.to_dval input);
    let headers = Cohttp.Header.of_list [cors] in
    respond ~headers `Not_found "404: No page matches"
  | [page] ->
    let route = Handler.event_name_for_exn page in
    let input = PReq.from_request req body in
    let bound = Http.bind_route_params_exn ~path:(Uri.path uri) ~route in
    let dbs = TL.dbs !c.toplevels in
    let dbs_env = Db.dbs_as_exe_env (dbs) in
    (match (Handler.module_for page, Handler.modifier_for page) with
    | (Some m, Some mo) ->
      (* Store the event with the input path not the event name, because we
       * want to be able to
       *    a) use this event if this particular handler changes
       *    b) use the input url params in the analysis for this handler
       *)
      let desc = (m, Uri.path uri, mo) in
      Stored_event.store_event host desc (PReq.to_dval input)
    | _-> ());
    let env = Util.merge_left bound dbs_env in
    let env = Map.set ~key:"request" ~data:(PReq.to_dval input) env in

    let headers = CRequest.headers req in
    let ff = FF.fingerprint_user ip headers in
    let session_headers = FF.session_headers headers ff in
    let state : RTT.exec_state =
      { ff = ff
      ; tlid = page.tlid
      ; host = !c.host
      ; user_fns = !c.user_functions
      ; exe_fn_ids = []
      ; env = env
      ; dbs = dbs
      ; id = Util.create_id ()
      } in
    let result = Handler.execute state page in
    let maybe_infer_headers headers value =
      if List.Assoc.mem headers ~equal:(=) "Content-Type"
      then
        headers
      else
        match value with
        | RTT.DObj _ | RTT.DList _ ->
          List.Assoc.add
            headers
            ~equal:(=)
            "Content-Type"
            "application/json"
        | _ ->
          List.Assoc.add
            headers
            ~equal:(=)
            "Content-Type"
            "text/plain"
    in
    (match result with
    | DResp (http, value) ->
      (match http with
       | Redirect url ->
         Event_queue.finalize ~host state.id ~status:`OK;
         S.respond_redirect (Uri.of_string url) ()
       | Response (code, resp_headers) ->
         let body =
           if List.exists resp_headers ~f:(fun (name, value) ->
              String.lowercase name = "content-type"
              && String.lowercase value = "text/html")
           then Dval.to_human_repr value
           (* TODO: only pretty print for a webbrowser *)
           else
             Dval.dval_to_pretty_json_string value
         in
         let resp_headers =
           maybe_infer_headers resp_headers value
         in
         Event_queue.finalize ~host state.id ~status:`OK;
         let status = Cohttp.Code.status_of_code code in
         let headers = Cohttp.Header.of_list
                        ([cors] @ resp_headers @ session_headers) in
         respond ~headers status body)
    | _ ->
      Event_queue.finalize ~host state.id ~status:`OK;
      let body = Dval.dval_to_pretty_json_string result in
      let ct_headers =
        maybe_infer_headers [] result
      in
      let headers = Cohttp.Header.of_list ([cors] @ ct_headers @session_headers) in
      (* for demonstrations sake, let's return 200 Okay when
       * no HTTP response object is returned *)
      respond ~headers `OK body)
  | _ ->
    let headers = Cohttp.Header.of_list [cors] in
    respond `Internal_server_error ~headers "500: More than one page matches"

(* -------------------------------------------- *)
(* Admin server *)
(* -------------------------------------------- *)
let rec admin_rpc_handler body (host: string) : (Cohttp.Header.t * string) =
  let execution_id = Util.create_id () in
  try
    let (t1, params) = time "1-read-api-ops"
      (fun _ -> Api.to_rpc_params body) in

    let (t2, c) = time "2-load-saved-ops"
      (fun _ -> C.load host params.ops) in

    let (t3, (envs, f404s)) = time "3-create-envs"
      (fun _ ->
        C.create_environments !c host) in

    let (t4, result) = time "4-to-frontend"
      (fun _ -> C.to_frontend_string envs f404s execution_id params.executable_fns !c) in

    let (t5, _) = time "5-save-to-disk"
      (fun _ ->
        (* work out the result before we save it, incase it has a
         stackoverflow or other crashing bug *)
        if Api.causes_any_changes params
        then C.save !c
        else ()
      ) in

  Event_queue.finalize ~host execution_id ~status:`OK;
  (server_timing [t1; t2; t3; t4; t5], result)
  with
  | Postgresql.Error e when C.is_uninitialized_db_error host e ->
    C.rerun_all_db_ops host;
    admin_rpc_handler body host
  | e ->
    Event_queue.finalize ~host execution_id ~status:`Err;
    raise e

let admin_ui_handler ~(debug:bool) () =
  let template = Util.readfile_lwt ~root:Templates "ui.html" in
  template
  >|= Util.string_replace "{ALLFUNCTIONS}" (Api.functions)
  >|= Util.string_replace "{ROLLBARCONFIG}" (Config.rollbar_js)
  >|= Util.string_replace "{ELMDEBUG}" (if debug
                                      then "-debug"
                                      else "")

let save_test_handler host =
  let g = C.load host [] in
  let filename = C.save_test !g in
  respond `OK ("Saved as: " ^ filename)


let auth_then_handle req subdomain handler =
  let path = req |> CRequest.uri |> Uri.path in
  if not (String.is_prefix ~prefix:"/admin" path)
  then
    handler (Header.init ())
  else
    (* only handle auth for admin routes *)
    (* let users use their domain as a prefix for scratch work *)
    let auth_domain =
      match String.split subdomain '-' with
      | d :: scratch -> d
      | _ -> subdomain
    in
    Auth.Session.of_request req
    >>= function
    | Ok (Some session) ->
      if path = "/admin/logout"
      then
        Auth.Session.clear Auth.Session.backend session
        >>= fun _ ->
        let headers =
          (Header.of_list
             (Auth.Session.clear_hdrs Auth.Session.cookie_key)) in
        S.respond_redirect ~headers ~uri:(Uri.of_string "/admin/ui") ()
      else
        (match Auth.Session.user_for session with
         | Some user ->
           if Auth.has_access ~host:auth_domain ~user
           then handler (Header.init ())
           else respond `Unauthorized "Unauthorized"
         | None ->
           respond `Unauthorized "Invalid Session")
    | _ ->
      let auth =
        req
        |> CRequest.headers
        |> Header.get_authorization
      in
      match auth with
      | (Some (`Basic (user, pass))) ->
        (match Auth.authenticate ~host:auth_domain ~username:user ~password:pass with
         | Some user ->
           Auth.Session.new_for_user user >>=
           (fun session ->
              let headers = Header.of_list (Auth.Session.to_cookie_hdrs Auth.Session.cookie_key session) in
              handler headers)
         | None ->
          respond `Unauthorized "Bad credentials")
      | None ->
        S.respond_need_auth ~auth:(`Basic "dark") ()
      | _ ->
        respond `Unauthorized "Invalid session"

let admin_handler ~(host: string) ~(uri: Uri.t) ~stopper ~(body: string) (req: CRequest.t) headers =
  let empty_body = "{ ops: [], executable_fns: []}" in
  let rpc ?(body=empty_body) () =
    let (headers, response_body) = admin_rpc_handler body host in
    respond ~headers `OK response_body
  in

  match Uri.path uri with
  | "/admin/api/rpc" -> rpc ~body ()
  | "/admin/api/get_analysis" ->
    (* Reuse the RPC handler because it basically does the same
     * thing. It shouldn't save because there are no ops sent.
     * It also sends too much data back, but we just ignore it
     * in the client. *)
    rpc ()
  | "/admin/api/shutdown" when Config.allow_server_shutdown ->
    Lwt.wakeup stopper ();
    respond `OK "Disembowelment"
  | "/admin/ui-debug" ->
    admin_ui_handler ~debug:true () >>= fun body -> respond ~headers `OK body
  | "/admin/ui" ->
    admin_ui_handler ~debug:false () >>= fun body -> respond ~headers `OK body
  | "/admin/integration_test" ->
    admin_ui_handler ~debug:false () >>= fun body -> respond `OK body
  | "/admin/api/save_test" ->
    save_test_handler host
  | _ ->
    respond `Not_found "Not found"

(* -------------------------------------------- *)
(* The server *)
(* -------------------------------------------- *)

let static_handler uri =
  let fname = S.resolve_file ~docroot:(Config.dir Config.Webroot) ~uri in
  S.respond_file ~fname ()


let server () =
  let stop,stopper = Lwt.wait () in

  let callback (ch, conn) req body =
    let host =
      req
      |> CRequest.uri
      |> Uri.host
      |> Option.bind
        ~f:(fun host ->
            match String.split host '.' with
            | ["localhost"] -> Some "localhost"
            | [_] -> None
            | a :: rest -> Some a
            | _ -> None)
    in
    let handler headers =
      try
        let ip = get_ip_address ch in
        let uri = req |> CRequest.uri in

        Log.infO "request"
          ( host |> Option.value ~default:"Unsupported host"
          , ip
          , req |> CRequest.meth |> Cohttp.Code.string_of_method
          , "http:" ^ Uri.to_string uri);

        match (Uri.path uri) with
        | "/sitemap.xml"
        | "/favicon.ico" ->
         respond `OK ""
        | p when (String.is_prefix ~prefix:"/static/" p) ->
          static_handler uri
        | p when  (String.is_prefix ~prefix:"/admin/" p) ->
          (match host with
           | Some host ->
             admin_handler ~host ~uri ~body ~stopper req headers
           | None ->
             respond `Not_found "Not found")
        | _ ->
          (match host with
           | Some host ->
             user_page_handler ~host ~ip ~uri ~body req
           | None ->
             respond `Not_found "Not found")
      with
      | e ->
        let bt = Backtrace.Exn.most_recent () in
        Rollbar.report_lwt e bt (Remote (req, body)) >>= fun _ ->
        let err_body = (match e with
          | Exception.DarkException e ->
            e
            |> Exception.exception_data_to_yojson
            |> Yojson.Safe.pretty_to_string
          | Yojson.Json_error msg ->
            "Not a value: " ^ msg
          | Postgresql.Error e ->
            "Postgres error: " ^ Postgresql.string_of_error e
          | _ ->
            "Dark Internal Error: " ^ Exn.to_string e)
        in
        Lwt_io.printl ("Error: " ^ err_body) >>= fun () ->
        Lwt_io.printl (Backtrace.to_string bt) >>= fun () ->
        let headers = Cohttp.Header.of_list [cors] in
        respond ~headers `Internal_server_error err_body
    in
    match host with
    (* This seems like it should be moved closer to the admin handler,
     * but don't do that - that makes Lwt swallow our exceptions. *)
    | Some host ->
      auth_then_handle req host handler
    | None ->
      respond `Not_found "Not found"
  in
  let cbwb conn req req_body =
    (* extract a string out of the body *)
    req_body |> Cohttp_lwt__Body.to_string >>= callback conn req in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback:cbwb ())

let run () =
  ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
