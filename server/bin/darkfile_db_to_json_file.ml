open Core_kernel

let _ =
  Printexc.record_backtrace true;
  Exn.initialize_module ();
  let host = Sys.argv.(1) in
  match Libbackend.Serialize.load_binary_from_db host with
  | Some ops ->
    Libbackend.Serialize.save_json_to_db host ops
  | None ->
    failwith ("Can't read from DB for host: " ^ host)

