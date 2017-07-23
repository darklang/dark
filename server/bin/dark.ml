open Core


let () =
  let () = Printexc.record_backtrace true in
  print_endline "Starting server";
  Server.run ()
