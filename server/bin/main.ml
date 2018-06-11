open Core

let () =
  print_endline "Starting server";
  (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  let () = Lwt.async_exception_hook := ignore in
  Backend.Init.init ();
  Backend.Server.run ()
