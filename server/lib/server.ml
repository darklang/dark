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
module FF = FeatureFlag


(* ------------------------------- *)
(* utils *)
(* ------------------------------- *)
type timing_header = string * float * string

let server_timing (times: timing_header list) =
  times
  |> List.map ~f:(fun (name, time, desc) ->
      (* chrome 64 *)
      name
      ^ "=" ^ (time |> Float.to_string_hum ~decimals:3)
      ^ "; \"" ^ desc ^ "\"")

  (* chrome 65 *)
  (* name *)
  (* ^ ";desc=\"" ^ desc ^ "\"" *)
  (* ^ ";dur=" ^ (time |> Float.to_string_hum ~decimals:3) *)

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

(* ------------------------------- *)
(* handlers for dark developers *)
(* ------------------------------- *)
let admin_rpc_handler body (host: string) : (Cohttp.Header.t * string) =
  try
    let (t1, params) = time "1-read-api-ops"
      (fun _ -> Api.to_rpc_params body) in

    let (t2, c) = time "2-load-saved-ops"
      (fun _ -> C.load host params.ops) in

    let (t3, envs) = time "3-create-envs"
      (fun _ ->
        Event_queue.set_scope !c.name;
        C.create_environments !c host) in

    let (t4, result) = time "4-to-frontend"
      (fun _ -> C.to_frontend_string envs params.executable_fns !c) in

    let (t5, _) = time "5-save-to-disk"
      (fun _ ->
        (* work out the result before we save it, incase it has a
         stackoverflow or other crashing bug *)
        if Api.causes_any_changes params
        then C.save !c
        else ()
      ) in

  Event_queue.unset_scope ~status:`OK;
  (server_timing [t1; t2; t3; t4; t5], result)
  with
  | e ->
    Event_queue.unset_scope ~status:`Err;
    let bt = Backtrace.Exn.most_recent () in
    let msg = Exn.to_string e in
    print_endline ("Exception: " ^ msg);
    print_endline (Backtrace.to_string bt);
    raise e

let admin_ui_handler () =
  let template = Util.readfile_lwt (Config.templates_dir ^ "ui.html") in
  template >|= Util.string_replace "ALLFUNCTIONS" (Api.functions)

let static_handler uri =
  let fname = S.resolve_file ~docroot:(Config.webroot_dir) ~uri in
  S.respond_file ~fname ()

let save_test_handler host =
  let g = C.load host [] in
  let filename = C.save_test !g in
  S.respond_string ~status:`OK ~body:("Saved as: " ^ filename) ()


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
  S.respond_string ~status:`OK
                   ~body:""
                   ~headers:(Cohttp.Header.of_list headers)
                   ()


let user_page_handler (host: string) (ip: string) (uri: Uri.t) (req: CRequest.t) (body: string) =
  let c = C.load host [] in
  Event_queue.set_scope !c.name;
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
    S.respond_string ~status:`Not_found ~headers:(Cohttp.Header.of_list [cors]) ~body:"404: No page matches" ()
  | [page] ->
    let route = Handler.event_name_for_exn page in
    let input = PReq.from_request req body in
    Stored_event.store host page.tlid (PReq.to_dval input);
    let bound = Http.bind_route_params_exn ~uri ~route in
    let dbs = TL.dbs !c.toplevels in
    let dbs_env = Db.dbs_as_exe_env (dbs) in
    Db.cur_dbs := dbs;
    let env = Util.merge_left bound dbs_env in
    let env = Map.set ~key:"request" ~data:(PReq.to_dval input) env in

    let headers = CRequest.headers req in
    let ff = FF.fingerprint_user ip headers in
    let session_headers = FF.session_headers headers ff in
    let state : Ast.exec_state =
      { ff = ff
      ; tlid = page.tlid
      ; hostname = !c.name
      ; user_fns = !c.user_functions
      ; exe_fn_ids = []
      ; env = env} in
    let result = Handler.execute state page in
    (match result with
    | DResp (http, value) ->
      (match http with
       | Redirect url ->
         Event_queue.unset_scope ~status:`OK;
         S.respond_redirect (Uri.of_string url) ()
       | Response (code, resp_headers) ->
         let body =
           if List.exists resp_headers ~f:(fun (name, value) ->
              String.lowercase name = "content-type"
              && String.lowercase value = "text/html")
           then Dval.to_simple_repr "<" ">" value
           (* TODO: only pretty print for a webbrowser *)
           else Dval.dval_to_pretty_json_string value
         in
         Event_queue.unset_scope ~status:`OK;
         S.respond_string
           ~status:(Cohttp.Code.status_of_code code)
           ~headers:(Cohttp.Header.of_list
                       ([cors] @ resp_headers @ session_headers))
           ~body:body
           ())
    | _ ->
      Event_queue.unset_scope ~status:`OK;
      let body = Dval.dval_to_pretty_json_string result in
      (* for demonstrations sake, let's return 200 Okay when
       * no HTTP response object is returned *)
      let code = 200 in
      S.respond_string
        ~status:(Cohttp.Code.status_of_code code)
        ~headers:(Cohttp.Header.of_list ([cors] @ session_headers))
        ~body:body
        ())
  | _ ->
    Event_queue.unset_scope ~status:`Err;
    S.respond_string ~status:`Internal_server_error ~headers:(Cohttp.Header.of_list [cors]) ~body:"500: More than one page matches" ()

(* -------------------------------------------- *)
(* The server, with auth and routing *)
(* -------------------------------------------- *)

let server () =
  let stop,stopper = Lwt.wait () in

  let callback (ch, conn) req req_body =
    let subdomain =
      let domain = Uri.host (req |> CRequest.uri) |> Option.value ~default:"" in
      match String.split domain '.' with
      | ["localhost"] -> "localhost"
      | a :: rest -> a
      | _ -> ""
    in

    let auth_then_handle handler =
      let path = req |> CRequest.uri |> Uri.path in
      (* only handle auth for admin routes *)
      if String.is_prefix ~prefix:"/admin" path
      then
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
            S.respond_redirect ~headers:(Header.of_list (Auth.Session.clear_hdrs Auth.Session.cookie_key)) ~uri:(Uri.of_string "/admin/ui") ()
          else
            (match Auth.Session.user_for session with
             | Some user ->
               if Auth.has_access ~domain:auth_domain ~user
               then handler (Header.init ())
               else S.respond_string ~status:`Unauthorized ~body:"Unauthorized" ()
             | None ->
               S.respond_string ~status:`Unauthorized ~body:"Invalid Session" ())
        | _ ->
          let auth =
            req
            |> CRequest.headers
            |> Header.get_authorization
          in
          match auth with
          | (Some (`Basic (user, pass))) ->
            (match Auth.authenticate ~domain:auth_domain ~username:user ~password:pass with
             | Some user ->
               Auth.Session.new_for_user user >>=
               (fun session ->
                  let headers = Header.of_list (Auth.Session.to_cookie_hdrs Auth.Session.cookie_key session) in
                  handler headers)
             | None ->
              S.respond_string ~status:`Unauthorized ~body:"Bad credentials" ())
          | None ->
            S.respond_need_auth ~auth:(`Basic "dark") ()
          | _ ->
            S.respond_string ~status:`Unauthorized ~body:"Invalid session" ()
      else
        handler (Header.init ())
    in

    let route_handler in_headers  =
      req_body |> Cohttp_lwt__Body.to_string >>=
      (fun req_body ->
         try
           let uri = req |> CRequest.uri in
           let verb = req |> CRequest.meth in

           let domain = match subdomain with
             | "" ->  failwith @@ "Unsupported domain: " ^ subdomain
             | _ -> subdomain
           in

           let rpc body =
             let (headers, response_body) = admin_rpc_handler body domain in
             S.respond_string ~status:`OK ~body:response_body ~headers ()
           in

           Log.infO "request" (domain, Cohttp.Code.string_of_method verb, ("http:" ^Uri.to_string uri));
           match (Uri.path uri) with
           | "/admin/api/rpc" -> rpc req_body
           | "/admin/api/get_analysis" ->
             (* Reuse the RPC handler because it basically does the same
              * thing. It shouldn't save because there are no ops sent.
              * It also sends too much data back, but we just ignore it
              * in the client. *)
             let empty_body = "{ ops: [], executable_fns: []}" in
             rpc empty_body
           | "/admin/api/shutdown" ->
             Lwt.wakeup stopper ();
             S.respond_string ~status:`OK ~body:"Disembowelment" ()
           | "/admin/ui" ->
             admin_ui_handler () >>= fun body -> S.respond_string ~status:`OK ~headers:in_headers ~body ()
           | "/admin/integration_test" ->
             admin_ui_handler () >>= fun body -> S.respond_string ~status:`OK ~body ()
           | "/admin/api/save_test" ->
             save_test_handler domain
           | "/sitemap.xml"
           | "/favicon.ico" ->
             S.respond_string ~status:`OK ~body:"" ()
           | p when (String.is_prefix ~prefix:"/static/" p) ->
             static_handler uri
           | _ ->
             user_page_handler domain (get_ip_address ch) uri req req_body
         with
         | e ->
           let bt = Backtrace.Exn.most_recent () in
           let body = match e with
             | Exception.DarkException e ->
               Exception.exception_data_to_yojson e |> Yojson.Safe.pretty_to_string
             | Yojson.Json_error msg -> "Not a value: " ^ msg
             | Postgresql.Error e -> "Postgres error: " ^ Postgresql.string_of_error e
             | _ -> "Dark Internal Error: " ^ Exn.to_string e
           in
           Lwt_io.printl ("Error: " ^ body) >>= fun () ->
           Lwt_io.printl (Backtrace.to_string bt) >>= fun () ->
           S.respond_string ~status:`Internal_server_error ~headers:(Cohttp.Header.of_list [cors]) ~body ())
    in
    auth_then_handle route_handler
  in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback ())

let run () =
  ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
