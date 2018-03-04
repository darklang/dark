open Core

let run_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_VAR_RUN_DIR" in
  if not (Filename.is_absolute dir)
  then Exception.internal "FAIL: DARK_CONFIG_VAR_RUN_DIR is not absolute"
  else dir ^ "/"

let appdata_dir : string =
  run_dir ^ "appdata/"

let log_dir : string =
  run_dir ^ "logs/"

let serialization_dir: string =
  "serialization/"
