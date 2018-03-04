open Core

let run_dir : string =
  (Sys.getenv_exn "DARK_CONFIG_VAR_RUN_DIR") ^ "/"

let appdata_dir : string =
  run_dir ^ "appdata/"

let log_dir : string =
  run_dir ^ "logs/"

let serialization_dir: string =
  run_dir ^ "serialization/"

let () =
  Unix.mkdir run_dir;
  Unix.mkdir serialization_dir;
  Unix.mkdir log_dir;
  Unix.mkdir appdata_dir
