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

let server =
  let stop,stopper = Lwt.wait () in

  let callback _ req req_body =
    let admin_rpc_handler body (host: string) : ((string * float * string) list * string) =
      let time name desc fn =
        let start = Unix.gettimeofday () in
        let result = fn () in
        let finish = Unix.gettimeofday () in
        ((name, (finish -. start) *. 1000.0, desc), result)
      in

      try
        let (t1, ops) = time "1-read-api-ops" "1. read ops from api" (fun _ ->
          Api.to_ops body
        ) in

        let (t2, c) = time "2-load-saved-ops" "2. read saved ops from disk" (fun _ ->
          C.load host ops
        ) in

        let (t3, envs) = time "3-create-envs" "3. create the environment" (fun _ ->
          let dbs = TL.dbs !c.toplevels in
          let dbs_env = Db.dbs_as_env dbs in
          Db.cur_dbs := dbs;
          let global = PReq.sample |> PReq.to_dval in
          let env = RTT.DvalMap.set dbs_env "request" global in
          let env_map acc (h : Handler.handler) =
            let h_env =
              try
                let (body, c_req) =
                  let reqs = Stored_request.load_all host h.tlid in
                  let cursor = (List.length reqs) - 1 (* just get the last for now *) in
                  List.nth_exn reqs cursor
                in
                let d_req = PReq.from_request c_req body in
                PReq.to_dval d_req
              with
              | _ ->
                global
             in
             let new_env = RTT.DvalMap.set dbs_env "request" h_env in
             RTT.EnvMap.set acc h.tlid new_env
          in
          let tls_map =
            List.fold_left ~init:RTT.EnvMap.empty ~f:env_map (TL.handlers !c.toplevels)
          in
          (* TODO(ian): using 0 as a default, come up with better idea
           * later *)
          RTT.EnvMap.set tls_map 0 env
        ) in


        let (t4, result) = time "4-to-frontend" "4. serialize canvas to frontend" (fun _ ->
          C.to_frontend_string envs !c
        ) in

        let (t5, _) = time "5-save-to-disk" "5. serialize ops to disk" (fun _ ->
          (* work out the result before we save it, incase it has a
           stackoverflow or other crashing bug *)
          C.save !c;
        ) in

      ([t1; t2; t3; t4; t5], result)
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
        let route = Handler.url_for_exn page in
        Stored_request.store host body page.tlid req;
        let input = PReq.from_request req body in
        let bound = Http.bind_route_params_exn ~uri ~route in
        let dbs = TL.dbs !c.toplevels in
        let dbs_env = Db.dbs_as_exe_env (dbs) in
        Db.cur_dbs := dbs;
        let env = Util.merge_left bound dbs_env in
        let env = Map.set ~key:"request" ~data:(PReq.to_dval input) env in
        let result = Handler.execute env page in
        (match result with
        | DResp (http, value) ->
          (match http with
           | Redirect url ->
             S.respond_redirect (Uri.of_string url) ()
           | Response (code, headers) ->
             let body =
               if List.exists headers ~f:(fun (name, value) ->
                  String.lowercase name = "content-type"
                  && String.lowercase value = "text/html")
               then Dval.to_simple_repr "<" ">" value
               (* TODO: only pretty print for a webbrowser *)
               else Dval.dval_to_pretty_json_string value
             in
             S.respond_string
               ~status:(Cohttp.Code.status_of_code code)
               ~headers:(Cohttp.Header.of_list (List.cons cors headers))
               ~body:body
               ())
        | _ ->
          let body = Dval.dval_to_pretty_json_string result in
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
             let (timing_headers, body) =
               admin_rpc_handler req_body domain in
             let header =
               ("Server-timing"
               , timing_headers
                 |> List.map ~f:(fun (name, time, desc) ->
                      (* chrome 64 *)
                      name
                      ^ "=" ^ (time |> Float.to_string_hum ~decimals:3)
                      ^ "; \"" ^ desc ^ "\"")

                      (* chrome 65 *)
                      (* name *)
                      (* ^ ";desc=\"" ^ desc ^ "\"" *)
                      (* ^ ";dur=" ^ (time |> Float.to_string_hum ~decimals:3) *)

                 |> String.concat ~sep:",") in
             let headers = Cohttp.Header.of_list [header] in
             S.respond_string ~status:`OK ~body:body ~headers:headers ()
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
