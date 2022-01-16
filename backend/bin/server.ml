open Core_kernel

let () =
  try
    print_endline "Starting server" ;
    (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
    let () = Lwt.async_exception_hook := ignore in
    Libbackend.Init.init ~run_migrations:false ~run_side_effects:true ;
    Libbackend.Webserver.run ()
  with e ->
    let bt = Libexecution.Exception.get_backtrace () in
    Libbackend.Rollbar.last_ditch e ~bt "server" "no execution id"
