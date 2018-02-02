open Core

module C = Dark.Canvas

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let in_file = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  in_file
  |> C.read_ops_binary
  |> C.write_ops_json out_file
