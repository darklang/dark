open Lwt

module Clu = Cohttp_lwt_unix
module C = Cohttp
module S = Clu.Server
module Request = Clu.Request
module Header = C.Header
module J = Yojson.Basic.Util

let p s = Printf.printf s;; flush stdout;;

let server =
  let callback _ req req_body =
    let uri = req |> Request.uri in
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    let auth = req |> Request.headers |> Header.get_authorization in

    let admin_rpc_handler body : string =
      let () = "payload: " ^ body |> print_endline in
      let payload = body |> Yojson.Basic.from_string in

      let g = Graph.load "blog" in
      let command = J.member "command" payload |> J.to_string in
      let args = J.member "args" payload in
      let cursor : Node.node option = match command with
        | "load_initial_graph" -> None
        | _ -> failwith ("TODO: " ^ command)
      in
      Graph.to_frontend g cursor |> Yojson.Basic.to_string
    in

    let admin_ui_handler () =
      Util.slurp "templates/ui.html"
    in

    let static_handler f : string =
      let l = String.length f in
      let f = String.sub f 1 (l-1) in
      match f with
      | "static/base.css" -> Util.slurp f
      | "static/reset-normalize.css" -> Util.slurp f
      | "static/elm.js" -> Util.slurp f
      | _ -> failwith "File not found"
    in

    let auth_handler handler
      = match auth with
      | (Some `Basic ("dark", "2DqMHguUfsAGCPerWgyHRxPi"))
        -> handler
      | _
        -> Cohttp_lwt_unix.Server.respond_need_auth (`Basic "dark") ()
    in

    let route_handler handler =
      req_body |> Cohttp_lwt_body.to_string >|=
      (fun req_body ->
         match (Uri.path uri) with
         | "/admin/api/rpc" -> admin_rpc_handler req_body
         | "/sitemap.xml" -> ""
         | "/favicon.ico" -> ""
         | "/admin/ui" -> admin_ui_handler ()
         | p when (String.equal (String.sub p 0 8) "/static/")
           -> static_handler p
         | _ -> "app routing")
      >>= (fun body -> S.respond_string ~status:`OK ~body ())
    in
    ()
    |> route_handler
    |> auth_handler
  in
  S.create ~mode:(`TCP (`Port 8000)) (S.make ~callback ())

let run () = ignore (Lwt_main.run server)
