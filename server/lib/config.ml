open Core

(* ------------------------- *)
(* Root directories *)
(* ------------------------- *)

let run_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_RUN_DIR" in
  if not (Filename.is_absolute dir)
  then failwith "FAIL: DARK_CONFIG_RUN_DIR is not absolute"
  else dir ^ "/"

let persist_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_PERSIST_DIR" in
  if not (Filename.is_absolute dir)
  then failwith "FAIL: DARK_CONFIG_PERSIST_DIR is not absolute"
  else dir ^ "/"

let root_dir : string =
  let dir = Sys.getenv_exn "DARK_CONFIG_ROOT_DIR" in
  if not (Filename.is_absolute dir)
  then failwith "FAIL: DARK_CONFIG_ROOT_DIR is not absolute"
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

let completed_test_dir : string =
  run_dir ^ "completed_tests/"

(* ------------------------- *)
(* Important dirs *)
(* ------------------------- *)
let templates_dir : string =
  Sys.getenv "DARK_CONFIG_TEMPLATES_DIR"
  |> Option.value ~default:(server_dir ^ "templates")
  |> fun x -> x ^ "/"

let webroot_dir : string =
  Sys.getenv "DARK_CONFIG_WEBROOT_DIR"
  |> Option.value ~default:server_dir
  |> fun x -> x ^ "/"

let swagger_dir : string =
  Sys.getenv "DARK_CONFIG_SWAGGER_DIR"
  |> Option.value ~default:(server_dir ^ "swagger")
  |> fun x -> x ^ "/"

let migrations_dir : string =
  Sys.getenv "DARK_CONFIG_MIGRATIONS_DIR"
  |> Option.value ~default:(server_dir ^ "migrations")
  |> fun x -> x ^ "/"

let bin_root_dir : string =
  Sys.getenv "DARK_CONFIG_BIN_ROOT_DIR"
  |> Option.value ~default:(server_dir ^ "_build/default/bin")
  |> fun x -> x ^ "/"

let __unused_bin_scripts_dir : string =
  Sys.getenv "DARK_CONFIG_SCRIPTS_DIR"
  |> Option.value ~default:"todo"
  |> fun x -> x ^ "/"



(* -------------------- *)
(* For use in Util *)
(* -------------------- *)
type root = Events
          | Function_results
          | Log
          | Serialization
          | Templates
          | Webroot
          | Completed_test
          | Testdata
          | Bin_root
          | Appdata
          | Swagger
          | Migrations
          | No_check


let dir root =
  match root with
  | Events -> events_dir
  | Function_results -> function_results_dir
  | Log -> log_dir
  | Serialization -> serialization_dir
  | Templates -> templates_dir
  | Webroot -> webroot_dir
  | Completed_test -> completed_test_dir
  | Bin_root -> bin_root_dir
  | Appdata -> appdata_dir
  | Swagger -> swagger_dir
  | Testdata -> testdata_dir
  | Migrations -> migrations_dir
  | No_check -> ""


(* ------------------------- *)
(* Running the server *)
(* ------------------------- *)
let port : int =
  Sys.getenv "DARK_CONFIG_HTTP_PORT"
  |> Option.value ~default:"8000"
  |> int_of_string

let allow_server_shutdown : bool =
  Sys.getenv "DARK_CONFIG_ALLOW_SERVER_SHUTDOWN"
  |> Option.value ~default:"N"
  |> (=) "Y"

let __unused_trigger_queue_workers : bool =
  Sys.getenv "DARK_CONFIG_TRIGGER_QUEUE_WORKERS"
  |> Option.value ~default:"N"
  |> (=) "Y"

(* ------------------------- *)
(* Logs *)
(* ------------------------- *)
let should_use_stackdriver_logging : bool =
  match Sys.getenv "DARK_CONFIG_LOGGING_FORMAT" with
  | Some format when String.lowercase format = "stackdriver" -> true
  | _ -> false


let log_level : Log.level =
  let level = Sys.getenv "DARK_CONFIG_LOGLEVEL"
              |> Option.value ~default:"all"
              |> String.uppercase
  in
  match level with
  | "OFF" -> `Off
  | "FATAL" -> `Fatal
  | "ERROR" -> `Error
  | "WARN" -> `Warn
  | "INFO" -> `Info
  | "DEBUG" -> `Debug
  | "ALL" -> `All
  | _ -> `All


let should_write_shape_data =
  Sys.getenv "DARK_CONFIG_SAVE_SERIALIZATION_DIGEST" <> None

(* ------------------------- *)
(* Rollbar *)
(* ------------------------- *)

let rollbar_url =
  "https://api.rollbar.com/api/1/item/"

let rollbar_enabled =
  Sys.getenv "DARK_CONFIG_ROLLBAR_ENABLED"
  |> Option.value ~default:"N"
  |> String.uppercase
  |> (=) "Y"

let rollbar_environment =
  Sys.getenv "DARK_CONFIG_ROLLBAR_ENVIRONMENT"
  |> Option.value ~default:"development"
  |> String.lowercase

let rollbar_client_access_token =
  Sys.getenv "DARK_CONFIG_ROLLBAR_POST_CLIENT_ITEM"

let rollbar_server_access_token =
  let tok = Sys.getenv "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM" in
  if rollbar_enabled
  then Option.value_exn tok
  else Option.value ~default:"unused" tok

let rollbar_js =
  match rollbar_client_access_token with
  | Some token ->
    Printf.sprintf
      "{captureUncaught:true,enabled:%s,accessToken:'%s',payload:{environment: '%s'}}"
      (string_of_bool rollbar_enabled)
      token
      rollbar_environment
  | _ ->
    "{enabled:false}"

(* -------------------- *)
(* db *)
(* -------------------- *)


type postgres_config = { host: string
                       ; dbname: string
                       ; user: string
                       ; password: string
                       }

let default_pg = { host = "localhost"
                 ; dbname = "proddb"
                 ; user = "dark"
                 ; password = "eapnsdc"
                 }



let postgres_settings : postgres_config =
  let host = Sys.getenv "DARK_CONFIG_DB_HOST" in
  let dbname = Sys.getenv "DARK_CONFIG_DB_DBNAME" in
  let user = Sys.getenv "DARK_CONFIG_DB_USER" in
  let password = Sys.getenv "DARK_CONFIG_DB_PASSWORD" in
  match (host, dbname, user, password) with
  | (None, None, None, None) -> default_pg
  | (Some host, Some dbname, Some user, Some password) ->
    { host = host
    ; dbname = dbname
    ; user = user
    ; password = password
    }
  | _ -> failwith "Inconsistent DB setup, so refusing to start. Either set all DB configs, or none."
