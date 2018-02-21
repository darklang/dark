open Core

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let in_file = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  in_file
  |> Dark.Serialize.load_json
  |> Dark.Serialize.save_binary out_file

