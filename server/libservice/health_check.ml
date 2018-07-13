open Core_kernel
open Lwt
open Cohttp
open Cohttp_lwt_unix

let run () =
  (* simply respond with 200 for now, we'll make this more usable later *)
  let callback _conn req body =
    Server.respond_string ~status:`OK ~body:"Hello from queue worker" ()
  in
  Server.create ~mode:(`TCP (`Port Config.health_check_port)) (Server.make ~callback ())
