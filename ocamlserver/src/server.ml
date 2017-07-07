open Lwt
open Cohttp
open Cohttp_lwt_unix
open Cohttp.Auth
open Logs

let server =
  let callback _ req body =
    let uri = req |> Request.uri in
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    let auth = req |> Request.headers |> Header.get_authorization in

    let auth_handler handler
      = match auth with
      | (Some `Basic ("dark", "2DqMHguUfsAGCPerWgyHRxPi"))
        -> handler
      | _
        -> Server.respond_need_auth (`Basic "dark") () in

    let route_handler handler =
      let body =
        match (Uri.path uri) with
        | "/admin/api/rpc" -> "rpc"
        | "/sitemap.xml" -> "sitemap"
        | "/favicon.ico" -> "favicon"
        | "/admin/ui" -> "admin ui"
        | _ -> "app routing" in

      let debug = Uri.path uri in
      Server.respond_string ~status:`OK ~body:(Printf.sprintf "%s - '%s'" body debug) () in

    ()
    |> route_handler
    |> auth_handler
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
