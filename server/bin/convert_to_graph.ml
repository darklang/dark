open Core

module G = Graph

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let filename = Sys.argv.(1) in
  let g = G.load ~filename:(Some filename) "testgraph" [] in
  print_endline (G.to_frontend_string !g)


