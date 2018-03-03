open Core

let () =
  print_endline "Starting server";
  (* see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  let () = Lwt.async_exception_hook := ignore in
  Dark.Init.init ();
  Dark.Serialize.write_shape_data ();
  Dark.Server.run ()
