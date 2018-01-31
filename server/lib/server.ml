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
module DReq = Dark_request

let server =
  let stop,stopper = Lwt.wait () in

  let callback _ req req_body =

    let admin_rpc_handler body (host: string) : string =
      let _ = Log.tS "req start" in
      let time = Unix.gettimeofday () in
      let body = Log.pp "request body" body ~f:ident in
      try
        let _ = Log.tS "before ops" in
        let ops = Api.to_ops body in
        let c = C.load host ops in
        let _ = Log.tS "after ops" in
        let global = DReq.sample |> DReq.to_dval in
        let _ = Log.tS "db before" in
        let dbs = TL.dbs !c.toplevels in
        let dbs_env = Db.dbs_as_env dbs in
        let _ = Log.tS "db after" in
        Db.cur_dbs := dbs;
        let env = RTT.DvalMap.set dbs_env "request" global in
        let _ = Log.tS "frontend before" in
        let result = C.to_frontend_string env !c in
        let _ = Log.tS "frontend after" in
        let _ = Log.tS "req near end" in
        let total = string_of_float (1000.0 *. (Unix.gettimeofday () -. time)) in
        Log.pP ~stop:10000 ~f:ident ("response (" ^ total ^ "ms):") result;
        (* work out the result before we save it, incase it has a stackoverflow
         * or other crashing bug *)
        let _ = Log.tS "before save " in
        C.save !c;
        let _ = Log.tS "after save" in
        result
      with
      | e ->
        let bt = Backtrace.Exn.most_recent () in
        let msg = Exn.to_string e in
        print_endline ("Exception: " ^ msg);
        print_endline (Backtrace.to_string bt);
        raise e
    in

    let admin_ui_handler () =
      let template = Util.readfile_lwt "templates/ui.html" in
      template >|= Util.string_replace "ALLFUNCTIONS" (Api.functions)
    in

    let static_handler uri =
      let fname = S.resolve_file ~docroot:"." ~uri in
      S.respond_file ~fname ()
    in

    let save_test_handler host =
      let g = C.load host [] in
      let filename = C.save_test !g in
      S.respond_string ~status:`OK ~body:("Saved as: " ^ filename) ()
    in

    let options_handler (c: C.canvas) (req: CRequest.t) =
      (*       allow (from the route matching) *)
      (*       Access-Control-Request-Method: POST  *)
      (* Access-Control-Request-Headers: X-PINGOTHER, Content-Type *)
      (* This is just enough to fix conduit. Here's what we should do: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/OPTIONS *)
      let req_headers = Cohttp.Header.get (CRequest.headers req) "access-control-request-headers" in
      let allow_headers =
        match req_headers with
        | Some h -> h
        | None -> "*"
      in
      let headers = [("Access-Control-Allow-Methods", "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS"); ("Access-Control-Allow-Origin", "*"); ("Access-Control-Allow-Headers", allow_headers)] in
      S.respond_string ~status:`OK
                       ~body:""
                       ~headers:(Cohttp.Header.of_list headers)
                       ()
    in

    let cors = ("Access-Control-Allow-Origin", "*") in

    let user_page_handler (host: string) (uri: Uri.t) (req: CRequest.t) (body: string) =
      let c = C.load host [] in
      let verb = req |> CRequest.meth |> Cohttp.Code.string_of_method in
      let pages = C.pages_matching_route ~uri ~verb !c in
      match pages with
      | [] when String.Caseless.equal verb "OPTIONS" ->
        options_handler !c req
      | [] ->
        S.respond_string ~status:`Not_found ~headers:(Cohttp.Header.of_list [cors]) ~body:"404: No page matches" ()
      | [page] ->
        let route = Handler.url_for_exn page in
        let input = DReq.from_request req body uri in
        let bound = Http.bind_route_params_exn ~uri ~route in
        let dbs = TL.dbs !c.toplevels in
        let dbs_env = Db.dbs_as_exe_env (dbs) in
        Db.cur_dbs := dbs;
        let env = Util.merge_left bound dbs_env in
        let env = Map.set ~key:"request" ~data:(DReq.to_dval input) env in
        let result = Handler.execute env page in
        (match result with
        | DResp (http, value) ->
          let body = Dval.dval_to_json_string value in
          (match http with
           | Redirect url ->
             S.respond_redirect (Uri.of_string url) ()
           | Response (code, headers) ->
             S.respond_string
               ~status:(Cohttp.Code.status_of_code code)
               ~headers:(Cohttp.Header.of_list (List.cons cors headers))
               ~body:body
               ())
        | _ ->
          let body = Dval.dval_to_json_string result in
          (* for demonstrations sake, let's return 200 Okay when
           * no HTTP response object is returned *)
          let code = 200 in
          S.respond_string
            ~status:(Cohttp.Code.status_of_code code)
            ~headers:(Cohttp.Header.of_list [cors])
            ~body:body
            ())
      | _ ->
        S.respond_string ~status:`Internal_server_error ~headers:(Cohttp.Header.of_list [cors]) ~body:"500: More than one page matches" ()
    in

    (* let auth_handler handler *)
    (*   = match auth with *)
    (*   | (Some `Basic ("dark", "eapnsdc")) *)
    (*     -> handler *)
    (*   | _ *)
    (*     -> Cohttp_lwt_unix.Server.respond_need_auth ~auth:(`Basic "dark") () *)
    (* in *)
    (*  *)
    let route_handler _ =
      req_body |> Cohttp_lwt__Body.to_string >>=
      (fun req_body ->
         try
           let uri = req |> CRequest.uri in
           let verb = req |> CRequest.meth in
           (* let auth = req |> Request.headers |> Header.get_authorization in *)

           let domain = Uri.host uri |> Option.value ~default:"" in
           let domain = match String.split domain '.' with
           | ["localhost"] -> "localhost"
           | a :: rest -> a
           | _ -> failwith @@ "Unsupported domain: " ^ domain in

           Log.pP "req: " (domain, Cohttp.Code.string_of_method verb, uri);

           match (Uri.path uri) with
           | "/admin/api/rpc" ->
             S.respond_string ~status:`OK
                              ~body:(admin_rpc_handler req_body domain) ()
           | "/sitemap.xml" ->
             S.respond_string ~status:`OK ~body:"" ()
           | "/favicon.ico" ->
             S.respond_string ~status:`OK ~body:"" ()
           | "/admin/api/shutdown" ->
             Lwt.wakeup stopper ();
             S.respond_string ~status:`OK ~body:"Disembowelment" ()
           | "/admin/ui" ->
             admin_ui_handler () >>= fun body -> S.respond_string ~status:`OK ~body ()
           | "/admin/integration_test" ->
             admin_ui_handler () >>= fun body -> S.respond_string ~status:`OK ~body ()
           | "/admin/api/save_test" ->
             save_test_handler domain
           | p when (String.length p) < 8 ->
             user_page_handler domain uri req req_body
           | p when (String.equal (String.sub p ~pos:0 ~len:8) "/static/") ->
             static_handler uri
           | _ ->
             user_page_handler domain uri req req_body
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
    ()
    |> route_handler
    (* |> auth_handler *)
  in
  S.create ~stop ~mode:(`TCP (`Port 8000)) (S.make ~callback ())

let run () = ignore (Lwt_main.run server)
