open Core
open Lwt

module Clu = Cohttp_lwt_unix
module C = Cohttp
module S = Clu.Server
module Request = Clu.Request
module Header = C.Header
module G = Graph


let server =
  let callback _ req req_body =
    let uri = req |> Request.uri in
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    let auth = req |> Request.headers |> Header.get_authorization in

    let admin_rpc_handler body : string =
      let body = Util.inspect "request body" body ~formatter:ident in
      let g = G.load "blog" in
      try
        Api.apply_ops g body;
        G.save !g;
        !g
        |> Graph.to_frontend_string
        |> Util.inspect "response: " ~formatter:ident
      with
      | e -> print_endline (G.show_graph !g);
        raise e
    in

    let admin_ui_handler () =
      let template = Util.readfile "templates/ui.html" in
      Util.string_replace "ALLFUNCTIONS" (Api.functions) template
    in

    let static_handler f : string =
      (* TODO: mimetypes *)
      let l = String.length f in
      let f = String.sub f ~pos:1 ~len:(l-1) in
      match f with
      | "static/base.css" -> Util.readfile f
      | "static/reset-normalize.css" -> Util.readfile f
      | "static/elm.js" -> Util.readfile2 f
      | "templates/test.html" -> Util.readfile2 f
      | _ -> failwith "File not found"
    in

    let auth_handler handler
      = match auth with
      | (Some `Basic ("dark", "2DqMHguUfsAGCPerWgyHRxPi"))
        -> handler
      | _
        -> Cohttp_lwt_unix.Server.respond_need_auth ~auth:(`Basic "dark") ()
    in

    let route_handler _ =
      req_body |> Cohttp_lwt_body.to_string >>=
      Lwt.wrap1
      (fun req_body ->
         try
           match (Uri.path uri) with
           | "/admin/api/rpc" -> `OK, admin_rpc_handler req_body
           | "/sitemap.xml" -> `OK, ""
           | "/favicon.ico" -> `OK, ""
           | "/admin/ui" -> `OK, (admin_ui_handler ())
           | "/admin/test" -> `OK, static_handler "/templates/test.html"
           | p when (String.length p) < 8 -> `Not_implemented, "app routing"
           | p when (String.equal (String.sub p ~pos:0 ~len:8) "/static/")
             -> `OK, static_handler p
           | _ -> `Not_implemented, "app routing"
         with
         | e ->
           let backtrace = Exn.backtrace () in
           let msg = match e with
             | (Exception.UserException msg) -> "UserException: " ^ msg
             | (Yojson.Json_error msg) -> "Not a value: " ^ msg
             | _ -> Exn.to_string e
           in
           print_endline ("Error: " ^ msg);
           print_endline backtrace;
           `Internal_server_error, msg)
      >>= (fun (status, body) -> S.respond_string ~status ~body ())
    in
    ()
    |> route_handler
    |> auth_handler
  in
  S.create ~mode:(`TCP (`Port 8000)) (S.make ~callback ())

let run () = ignore (Lwt_main.run server)
