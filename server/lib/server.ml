open Core
open Lwt

module Clu = Cohttp_lwt_unix
module C = Cohttp
module S = Clu.Server
module Request = Clu.Request
module Header = C.Header
module G = Graph
module RT = Runtime

let server =
  let stop,stopper = Lwt.wait () in

  let callback _ req req_body =
    let uri = req |> Request.uri in
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    (* let auth = req |> Request.headers |> Header.get_authorization in *)

    let admin_rpc_handler body (host: string) (save: bool) : string =
      let time = Unix.gettimeofday () in
      let body = Log.pp "request body" body ~f:ident in
     let g = G.load host [] in
      try
        let ops = Api.to_ops body in
        g := !(G.load host ops);
        G.verify !g;
        let result = !g
                     |> Graph.to_frontend_string in
        let total = string_of_float (1000.0 *. (Unix.gettimeofday () -. time)) in
        Log.pP ~stop:2000 ~f:ident ("response (" ^ total ^ "ms):") result;
        (* work out the result before we save it, incase it has a stackoverflow
         * or other crashing bug *)
        if save then G.save !g;
        result
      with
      | e ->
          let bt = Exn.backtrace () in
             let msg = Exn.to_string e in
             print_endline (G.show_graph !g);
             print_endline ("Exception: " ^ msg);
             print_endline bt;
             raise e
    in

    let admin_ui_handler () =
      let template = Util.readfile_lwt "templates/ui.html" in
      template >|= Util.string_replace "ALLFUNCTIONS" (Api.functions) in

    let static_handler uri =
      let fname = S.resolve_file ~docroot:"." ~uri in
      S.respond_file ~fname ()
    in

    let save_test_handler host =
      let g = G.load host [] in
      let filename = G.save_test !g in
      S.respond_string ~status:`OK ~body:("Saved as: " ^ filename) ()
    in



    let user_page_handler host body uri =
      let g = G.load host [] in
      let pages = G.get_pages !g in
      match List.filter ~f:(fun p -> p#get_arg_value "url" (G.gfns !g) = RT.DStr (Uri.path uri)) pages with
      | [] ->
        S.respond_string ~status:`Not_found ~body:"404: No page matches" ()
      | [page] ->
        S.respond_string ~status:`OK ~body:(G.execute_node page !g |> RT.to_url_string) ()
      | _ ->
        S.respond_string ~status:`Internal_server_error ~body:"500: More than one page matches" ()


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
      req_body |> Cohttp_lwt_body.to_string >>=
      (fun req_body ->
         try
           let domain = Uri.host uri |> Option.value ~default:"" in
           let domain = match String.split domain '.' with
           | ["localhost"] -> "localhost"
           | [a; "localhost"] -> a
           | _ -> failwith @@ "Unsupported domain: " ^ domain in

           match (Uri.path uri) with
           | "/admin/api/rpc" ->
             S.respond_string ~status:`OK
                              ~body:(admin_rpc_handler req_body domain true) ()
           | "/admin/api/phantom" ->
             S.respond_string ~status:`OK
                              ~body:(admin_rpc_handler req_body domain false) ()
           | "/sitemap.xml" ->
             S.respond_string ~status:`OK ~body:"" ()
           | "/favicon.ico" ->
             S.respond_string ~status:`OK ~body:"" ()
           | "/admin/api/shutdown" ->
             Lwt.wakeup stopper ();
             S.respond_string ~status:`OK ~body:"Disembowelment" ()
           | "/admin/ui" ->
             admin_ui_handler () >>= fun body -> S.respond_string ~status:`OK ~body ()
           | "/admin/test" ->
             static_handler (Uri.of_string "/templates/test.html")
           | "/admin/api/save_test" ->
             save_test_handler domain
           | p when (String.length p) < 8 ->
             user_page_handler domain req_body uri
           | p when (String.equal (String.sub p ~pos:0 ~len:8) "/static/") ->
             static_handler uri
           | _ ->
             user_page_handler domain req_body uri
         with
         | e ->
           let backtrace = Exn.backtrace () in
           let body = match e with
             | Exception.DarkException e ->
                 Exception.exception_data_to_yojson e |> Yojson.Safe.pretty_to_string
             | Yojson.Json_error msg -> "Not a value: " ^ msg
             | _ -> "Dark Internal Error: " ^ Exn.to_string e
           in
           Lwt_io.printl ("Error: " ^ body) >>= fun () ->
           Lwt_io.printl backtrace >>= fun () ->
           S.respond_string ~status:`Internal_server_error ~body ())
    in
    ()
    |> route_handler
    (* |> auth_handler *)
  in
  S.create ~stop ~mode:(`TCP (`Port 8000)) (S.make ~callback ())

let run () = ignore (Lwt_main.run server)
