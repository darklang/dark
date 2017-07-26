open Core
open Lwt

module Clu = Cohttp_lwt_unix
module C = Cohttp
module S = Clu.Server
module Request = Clu.Request
module Header = C.Header
module J = Yojson.Basic.Util
module G = Graph

let inspect = Util.inspect

let server =
  let callback _ req req_body =
    let uri = req |> Request.uri in
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    let auth = req |> Request.headers |> Header.get_authorization in

    let admin_rpc_handler body : string =
      let body = inspect ~f:ident "request body" body in
      let payload = Yojson.Basic.from_string body in

      let g = G.load "blog" in
      let () = match payload with
      | `List [`Assoc [("load_initial_graph", `Assoc [])]] -> ()
      | `List (head::rest) ->
        let rest = List.map ~f:Api.json2op rest in
        let ops = Api.backfill_id (Api.json2op head) rest in
        List.iter ~f:(fun op -> G.add_op op g) ops
      | _ -> Exception.raise "Unexpected request structure"
      in
      G.save !g;
      !g
      |> Graph.to_frontend_string
      |> Util.inspect ~f:ident "response: "
    in

    let admin_ui_handler () =
      Util.slurp "templates/ui.html"
    in

    let static_handler f : string =
      (* TODO: mimetypes *)
      let l = String.length f in
      let f = String.sub f ~pos:1 ~len:(l-1) in
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
           | p when (String.length p) < 8 -> `Not_implemented, "app routing"
           | p when (String.equal (String.sub p ~pos:0 ~len:8) "/static/")
             -> `OK, static_handler p
           | _ -> `Not_implemented, "app routing"
         with
         | e -> Printexc.print_backtrace stderr;
           raise e)
      >>= (fun (status, body) -> S.respond_string ~status ~body ())
    in
    ()
    |> route_handler
    |> auth_handler
  in
  S.create ~mode:(`TCP (`Port 8000)) (S.make ~callback ())

let run () = ignore (Lwt_main.run server)
