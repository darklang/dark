open Core


let () =
  let () = Printexc.record_backtrace true in
  let () = Exn.initialize_module () in
  print_endline "Starting server";
  Server.run ()
