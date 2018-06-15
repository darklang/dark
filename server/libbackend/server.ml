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

let respond ?(resp_headers=Header.init ()) status (body: string) =
  S.respond_string ~status ~body ~headers:resp_headers ()


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
  let resp_headers = [( "Access-Control-Allow-Methods"
                      , "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS")
                     ; ("Access-Control-Allow-Origin"
                       , "*")
                     ; ("Access-Control-Allow-Headers"
                       , allow_headers)]
  in
  respond ~resp_headers:(Cohttp.Header.of_list resp_headers) `OK ""


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
    Stored_event.store_event !c.id ("HTTP", Uri.path uri, verb) (PReq.to_dval input);
    let resp_headers = Cohttp.Header.of_list [cors] in
    respond ~resp_headers `Not_found "404: No page matches"
  | [page] ->
    let route = Handler.event_name_for_exn page in
    let input = PReq.from_request req body in
    let bound = Http.bind_route_params_exn ~path:(Uri.path uri) ~route in
    let dbs = TL.dbs !c.toplevels in
    let dbs_env = User_db.dbs_as_exe_env (dbs) in
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
    let env = Util.merge_left bound dbs_env in
    let env = Map.set ~key:"request" ~data:(PReq.to_dval input) env in

    let resp_headers = CRequest.headers req in
    let ff = FF.fingerprint_user ip resp_headers in
    let session_headers = FF.session_headers resp_headers ff in
    let state : RTT.exec_state =
      { ff = ff
      ; tlid = page.tlid
      ; host = !c.host
      ; account_id = !c.owner
      ; canvas_id = !c.id
      ; user_fns = !c.user_functions
      ; exe_fn_ids = []
      ; env = env
      ; dbs = dbs
      ; id = Util.create_id ()
      } in
    let result = Handler.execute state page in
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
    | DResp (http, value) ->
      (match http with
       | Redirect url ->
         Event_queue.finalize state.id ~status:`OK;
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
         let resp_headers =
           maybe_infer_headers resp_headers value
         in
         Event_queue.finalize state.id ~status:`OK;
         let status = Cohttp.Code.status_of_code code in
         let resp_headers = Cohttp.Header.of_list ([cors]
                                                   @ resp_headers
                                                   @ session_headers)
         in
         respond ~resp_headers status body)
    | _ ->
      Event_queue.finalize state.id ~status:`OK;
      let body = Dval.dval_to_pretty_json_string result in
      let ct_headers =
        maybe_infer_headers [] result
      in
      let resp_headers = Cohttp.Header.of_list ([cors] @ ct_headers @session_headers) in
      (* for demonstrations sake, let's return 200 Okay when
       * no HTTP response object is returned *)
      respond ~resp_headers `OK body)
  | _ ->
    let resp_headers = Cohttp.Header.of_list [cors] in
    respond `Internal_server_error ~resp_headers
      "500: More than one page matches"

(* -------------------------------------------- *)
(* Admin server *)
(* -------------------------------------------- *)
let admin_rpc_handler ~(host: string) body : (Cohttp.Header.t * string) =
  let execution_id = Util.create_id () in
  try
    let (t1, params) = time "1-read-api-ops"
      (fun _ -> Api.to_rpc_params body) in

    let (t2, c) = time "2-load-saved-ops"
      (fun _ -> C.load host params.ops) in

    let (t3, (envs, f404s)) = time "3-create-envs"
      (fun _ -> C.create_environments !c) in

    let (t4, unlocked) = time "4-analyze-unlocked-dbs"
      (fun _ -> C.unlocked !c) in

    let (t5, fvals) = time "5-function-values"
      (fun _ ->
        C.function_values !c params.executable_fns execution_id) in

    let (t6, tlvals) = time "6-toplevel-values"
      (fun _ ->
        C.toplevel_values !c envs params.executable_fns execution_id) in

    let (t7, result) = time "7-to-frontend"
      (fun _ -> C.to_frontend tlvals fvals unlocked envs f404s !c) in

    let (t8, _) = time "8-save-to-disk"
      (fun _ ->
        (* work out the result before we save it, incase it has a
         stackoverflow or other crashing bug *)
        if Api.causes_any_changes params
        then C.save !c
        else ()
      ) in

  Event_queue.finalize execution_id ~status:`OK;
  (server_timing [t1; t2; t3; t4; t5; t6; t7; t8], result)
  with
  | e ->
    Event_queue.finalize execution_id ~status:`Err;
    raise e

let get_analysis (host: string) : (Cohttp.Header.t * string) =
  let execution_id = Util.create_id () in
  try
    let (t2, c) = time "2-load-saved-ops"
      (fun _ -> C.load host []) in

    let (t3, (envs, f404s)) = time "3-create-envs"
      (fun _ -> C.create_environments !c) in

    let (t4, unlocked) = time "4-analyze-unlocked-dbs"
      (fun _ -> C.unlocked !c) in

    let (t5, fvals) = time "5-function-values"
      (fun _ ->
        C.function_values !c [] execution_id) in

    let (t6, tlvals) = time "6-toplevel-values"
      (fun _ ->
        C.toplevel_values !c envs [] execution_id) in

    let (t7, result) = time "7-to-frontend"
      (fun _ -> C.to_frontend tlvals fvals unlocked envs f404s !c) in

  Event_queue.finalize execution_id ~status:`OK;
  (server_timing [t2; t3; t4; t5; t6; t7], result)
  with
  | e ->
    Event_queue.finalize execution_id ~status:`Err;
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


let auth_then_handle req host handler =
  let path = req |> CRequest.uri |> Uri.path in
  if not (String.is_prefix ~prefix:"/admin" path)
  then
    handler (Header.init ())
  else
    (* only handle auth for admin routes *)
    (* let users use their domain as a prefix for scratch work *)
    let auth_domain = Account.auth_domain_for host in
    Auth.Session.of_request req
    >>= (function
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
        (let username = Auth.Session.username_for session in
         if Account.can_edit ~auth_domain ~username
         then
           handler (Header.init ())
         else
           respond `Unauthorized "Unauthorized")
    | _ ->
      let auth =
        req
        |> CRequest.headers
        |> Header.get_authorization
      in
      match auth with
      | (Some (`Basic (username, password))) ->
        (if Account.authenticate ~username ~password
            && Account.can_edit ~auth_domain ~username
         then
           Auth.Session.new_for_username username >>=
           (fun session ->
              let headers = Header.of_list (Auth.Session.to_cookie_hdrs Auth.Session.cookie_key session) in
              handler headers)
         else
          respond `Unauthorized "Bad credentials")
      | None ->
        S.respond_need_auth ~auth:(`Basic "dark") ()
      | _ ->
        respond `Unauthorized "Invalid session")

let admin_handler ~(host: string) ~(uri: Uri.t) ~stopper ~(body: string)
    (req: CRequest.t) req_headers =
  let text_plain_resp_headers =
    Header.init_with "Content-type" "text/html; charset=utf-8"
  in

  match Uri.path uri with
  | "/admin/api/rpc" ->
    let (resp_headers, response_body) = admin_rpc_handler host body in
    let utf8 = "application/json; charset=utf-8" in
    let resp_headers = Header.add resp_headers "Content-type" utf8 in
    respond ~resp_headers `OK response_body
  | "/admin/api/get_analysis" ->
    let (resp_headers, response_body) = get_analysis host in
    let utf8 = "application/json; charset=utf-8" in
    let resp_headers = Header.add resp_headers "Content-type" utf8 in
    respond ~resp_headers `OK response_body
  | "/admin/api/shutdown" when Config.allow_server_shutdown ->
    Lwt.wakeup stopper ();
    respond `OK "Disembowelment"
  | "/admin/api/clear-benchmarking-data" ->
    Db.delete_benchmarking_data ();
    respond `OK "Cleared"
  | "/admin/ui-debug" ->
    admin_ui_handler ~debug:true () >>=
    fun body -> respond ~resp_headers:text_plain_resp_headers `OK body
  | "/admin/ui" ->
    admin_ui_handler ~debug:false () >>=
    fun body -> respond ~resp_headers:text_plain_resp_headers `OK body
  | "/admin/integration_test" ->
    admin_ui_handler  ~debug:false () >>=
    fun body -> respond ~resp_headers:text_plain_resp_headers `OK body
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
    let handle_error ~(include_internals:bool) (e:exn) =
      let bt = Backtrace.Exn.most_recent () in
      (* TODO: if this raises an error we're hosed *)
      let%lwt _ = Rollbar.report_lwt e bt (Remote (req, body)) in
      let real_err =
        try
          match e with
           | Exception.DarkException e ->
             e
             |> Exception.exception_data_to_yojson
             |> Yojson.Safe.pretty_to_string
           | Yojson.Json_error msg ->
             "Not a valid JSON value: '" ^ msg ^ "'"
           | Postgresql.Error e ->
             "Postgres error: " ^ Postgresql.string_of_error e
           | _ ->
             "Dark Internal Error: " ^ Exn.to_string e
        with _ -> "ERROR FETCHING ERROR" (* TODO: monitor this *)
      in
      let user_err =
        try
          match e with
           | Exception.DarkException e ->
             real_err
           | Yojson.Json_error msg ->
             real_err
           | Postgresql.Error e when include_internals ->
             real_err
           | _ ->
             if include_internals
             then real_err
             else "Dark Internal Error"
        with _ -> "Error fetching error"
      in
      Lwt_io.printl ("Error: " ^ real_err);%lwt
      Lwt_io.printl (Backtrace.to_string bt);%lwt
      let resp_headers = Cohttp.Header.of_list [cors] in
      respond ~resp_headers `Internal_server_error user_err
    in


    try
      let host =
        req
        |> CRequest.uri
        |> Uri.host
        |> Option.bind
          ~f:(fun host ->
              match String.split host '.' with
              | ["localhost"] -> Some "localhost"
              | ["darksingleinstance"; "com"] -> Some "darksingleinstance"
              | [_] -> None
              | a :: _ when int_of_string_opt a = None ->
                (* only for letters (ignore ip addresses) *)
                Some a
              | _ -> None)
      in

      let handler req_headers =
        let ip = get_ip_address ch in
        let uri = req |> CRequest.uri in

        Log.infO "request"
          ( host |> Option.value ~default:"Unsupported host"
          , ip
          , req |> CRequest.meth |> Cohttp.Code.string_of_method
          , "http:" ^ Uri.to_string uri);

        match (Uri.path uri, host) with
        | ("/sitemap.xml", _)
        | ("/favicon.ico", _) ->
         respond `OK ""
        | (p, _) when (String.is_prefix ~prefix:"/static/" p) ->
          static_handler uri
        | (_, None) ->
          respond `Not_found "Not found"
        | (p, Some host) ->
          if String.is_prefix ~prefix:"/admin/" p
          then
            try
              admin_handler ~host ~uri ~body ~stopper req req_headers
            with e -> handle_error ~include_internals:true e
          else
            user_page_handler ~host ~ip ~uri ~body req
      in

      match (req |> CRequest.uri |> Uri.path, host) with
      (* This seems like it should be moved closer to the admin handler,
       * but don't do that - that makes Lwt swallow our exceptions. *)
      | (_, Some host) ->
         auth_then_handle req host handler
      | ("/", None) -> (* for GKE health check *)
        respond `OK "Hello internal overlord"
      | (_, None) -> (* for GKE health check *)
        respond `Not_found "Not found"
    with e -> handle_error ~include_internals:false e

  in
  let cbwb conn req req_body =
    (* extract a string out of the body *)
    req_body |> Cohttp_lwt__Body.to_string >>= callback conn req in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback:cbwb ())

let run () =
  ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
