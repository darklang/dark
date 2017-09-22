open Core


let () =
  let () = Printexc.record_backtrace true in
  let () = Exn.initialize_module () in
  print_endline "Starting server";
  let () = Lwt.async_exception_hook := ignore in
  (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  Server.run ()
