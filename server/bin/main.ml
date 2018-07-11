open Core_kernel

let () =
  try
    print_endline "Starting server";
    (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
    let () = Lwt.async_exception_hook := ignore in
    Libbackend.Init.init ();
    Libbackend.Server.run ()
  with e ->
    Libbackend.Rollbar.last_ditch e "main" "no execution id"

