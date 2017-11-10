open Core

module C = Canvas

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let filename = Sys.argv.(1) in
  let c = C.load ~filename:(Some filename) "testcanvas" [] in
  print_endline (C.to_frontend_string !c)


