open Core_kernel

let _ =
  try
    Libbackend.Init.init ();
    let host = Sys.argv.(1) in
    let c = Libbackend.Canvas.load_all host [] in
    Libbackend.Canvas.save_as_json !c
  with e ->
    Libbackend.Rollbar.last_ditch e "darkfile_db_to_json_file" "no execution id"

