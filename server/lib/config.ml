open Core

let run_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_RUN_DIR" in
  if not (Filename.is_absolute dir)
  then Exception.internal "FAIL: DARK_CONFIG_RUN_DIR is not absolute"
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

let testdata_dir : string =
  server_dir ^ "test_appdata/"

let events_dir : string =
  persist_dir ^ "events/"

let function_results_dir : string =
  persist_dir ^ "function_results/"

let log_dir : string =
  run_dir ^ "logs/"

let serialization_dir: string =
  server_dir ^ "serialization/"

let templates_dir : string =
  Sys.getenv "DARK_CONFIG_TEMPLATES_DIR"
  |> Option.value ~default:(server_dir ^ "templates")
  |> fun x -> x ^ "/"

let completed_test_dir : string =
  run_dir ^ "completed_tests/"

let webroot_dir : string =
  Sys.getenv "DARK_CONFIG_WEBROOT_DIR"
  |> Option.value ~default:server_dir
  |> fun x -> x ^ "/"

let bin_root_dir : string =
  Sys.getenv "DARK_CONFIG_BIN_ROOT_DIR"
  |> Option.value ~default:(server_dir ^ "_build/default/bin")
  |> fun x -> x ^ "/"

let port : int =
  Sys.getenv "DARK_CONFIG_HTTP_PORT"
  |> Option.value ~default:"8000"
  |> int_of_string
