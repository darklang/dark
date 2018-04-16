open Core

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let domain = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  match Dark.Serialize.load_from_db domain with
  | Some ops ->
    Dark.Serialize.save_json ~root:Dark.Config.No_check out_file ops
  | None ->
    failwith ("Can't read from DB for domain: " ^ domain)

