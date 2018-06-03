open Core

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let host = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  match Dark.Serialize.load_binary_from_db host with
  | Some ops ->
    Dark.Serialize.save_json_to_db host ops;
    Dark.Serialize.save_json_to_disk ~root:Dark.Config.No_check out_file ops;
    ()
  | None ->
    failwith ("Can't read from DB for host: " ^ host)

