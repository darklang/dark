open Core_kernel

let _ =
  try
    Libbackend.Init.init ();
    let host = Sys.argv.(1) in
    let owner = Libbackend.Account.for_host host in
    let canvas_id = Libbackend.Serialize.fetch_canvas_id owner host in
    match Libbackend.Serialize.load_binary_from_db ~host ~canvas_id
            ~digest:Libbackend.Serialize.digest ()
    with
    | Some ops ->
      Libbackend.Serialize.save_json_to_db host ops
    | None ->
      failwith ("Can't read from DB for host: " ^ host)
  with e ->
    Libbackend.Rollbar.last_ditch e "darkfile_db_to_json_file" "no execution id"

