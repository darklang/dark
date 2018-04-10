open Core

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let in_file = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  in_file
  |> Dark.Serialize.load_binary ~root:Dark.Config.No_check
  |> Dark.Serialize.save_json ~root:Dark.Config.No_check out_file
