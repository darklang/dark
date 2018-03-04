open Core

let run_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_VAR_RUN_DIR" in
  if not (Filename.is_absolute dir)
  then Exception.internal "FAIL: DARK_CONFIG_VAR_RUN_DIR is not absolute"
  else dir ^ "/"

let persist_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_PERSIST_DIR" in
  if not (Filename.is_absolute dir)
  then Exception.internal "FAIL: DARK_CONFIG_PERSIST_DIR is not absolute"
  else dir ^ "/"

let root_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_ROOT_DIR" in
  if not (Filename.is_absolute dir)
  then Exception.internal "FAIL: DARK_CONFIG_ROOT_DIR is not absolute"
  else dir ^ "/"


let server_dir : string =
  root_dir ^ "server/"

let appdata_dir : string =
  persist_dir ^ "appdata/"

let events_dir : string =
  persist_dir ^ "events/"

let log_dir : string =
  run_dir ^ "logs/"

let cache_dir : string =
  run_dir ^ "cache/"

let serialization_dir: string =
  server_dir ^ "serialization/"

let templates_dir : string =
  server_dir ^ "templates/"
