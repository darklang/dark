open Core



let () =
  let () = Printexc.record_backtrace true in
  let () = Exn.initialize_module () in
  print_endline "Starting server";
  (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  let () = Lwt.async_exception_hook := ignore in
  (* init the Random module, will be seeded from /dev/urandom on Linux *)
  let () = Random.self_init () in
  Dark.Server.run ()
