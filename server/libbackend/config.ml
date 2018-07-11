open Core
open Libservice.Config_dsl
(* ------------------------- *)
(* Note: if you add an env-var in development, you'll probably need to
 * restart the dev container. *)
(* ------------------------- *)

(* ------------------------- *)
(* Root directories *)
(* ------------------------- *)

let run_dir = absolute_dir "DARK_CONFIG_RUN_DIR"
let persist_dir = absolute_dir "DARK_CONFIG_PERSIST_DIR"
let root_dir = absolute_dir "DARK_CONFIG_ROOT_DIR"

let server_dir = root_dir ^ "server/"
let appdata_dir = persist_dir ^ "appdata/"
let testdata_dir = server_dir ^ "test_appdata/"
let testresult_dir = run_dir ^ "test_results/"
let log_dir = run_dir ^ "logs/"
let serialization_dir = server_dir ^ "serialization/"
let completed_test_dir = run_dir ^ "completed_tests/"

(* ------------------------- *)
(* Configurable dirs *)
(* ------------------------- *)
let templates_dir = absolute_dir "DARK_CONFIG_TEMPLATES_DIR"
let webroot_dir = absolute_dir "DARK_CONFIG_WEBROOT_DIR"
let swagger_dir = absolute_dir "DARK_CONFIG_SWAGGER_DIR"
let migrations_dir = absolute_dir "DARK_CONFIG_MIGRATIONS_DIR"
let bin_root_dir = absolute_dir "DARK_CONFIG_BIN_ROOT_DIR"
let __unused_bin_scripts_dir = absolute_dir "DARK_CONFIG_SCRIPTS_DIR"


(* -------------------- *)
(* For use in Util *)
(* -------------------- *)
type root = Log
          | Serialization
          | Templates
          | Webroot
          | Completed_test
          | Testdata
          | Testresults
          | Bin_root
          | Appdata
          | Swagger
          | Migrations
          | No_check


let dir root =
  match root with
  | Log -> log_dir
  | Serialization -> serialization_dir
  | Templates -> templates_dir
  | Webroot -> webroot_dir
  | Completed_test -> completed_test_dir
  | Bin_root -> bin_root_dir
  | Appdata -> appdata_dir
  | Swagger -> swagger_dir
  | Testdata -> testdata_dir
  | Testresults -> testresult_dir
  | Migrations -> migrations_dir
  | No_check -> ""


(* ------------------------- *)
(* Running the server *)
(* ------------------------- *)
let port = int "DARK_CONFIG_HTTP_PORT"

let allow_server_shutdown = bool "DARK_CONFIG_ALLOW_SERVER_SHUTDOWN"
let __unused_trigger_queue_workers = bool "DARK_CONFIG_TRIGGER_QUEUE_WORKERS"

(* ------------------------- *)
(* Logs *)
(* ------------------------- *)
let log_format : [ `Stackdriver | `Regular | `Decorated ] =
  let as_str =
    string_option "DARK_CONFIG_LOGGING_FORMAT"
      ["stackdriver"; "regular"; "decorated"]
  in
  match as_str with
  | "stackdriver" -> `Stackdriver
  | "regular" -> `Regular
  | "decorated" -> `Decorated
  | _ -> failwith ("Invalid logging format: " ^ as_str)


let log_level =
  let as_str =
    string_option
      "DARK_CONFIG_LOGLEVEL"
      [ "off"; "inspect"; "fatal"; "error"
       ; "warn"; "info"; "success"; "debug"; "all"]
  in
  match as_str with
  | "off" -> `Off
  | "inspect" -> `Inspect
  | "fatal" -> `Fatal
  | "error" -> `Error
  | "warn" -> `Warn
  | "info" -> `Info
  | "success" -> `Success
  | "debug" -> `Debug
  | "all" -> `All
  | _ -> failwith ("Invalid level name:" ^ as_str)


let should_write_shape_data =
  bool "DARK_CONFIG_SAVE_SERIALIZATION_DIGEST"

(* ------------------------- *)
(* Rollbar *)
(* ------------------------- *)

let rollbar_client_access_token =
  (* This is what the rollbar UI calls it *)
  match string "DARK_CONFIG_ROLLBAR_POST_CLIENT_ITEM" with
  | "none" -> None
  | item -> Some item

let rollbar_enabled = Libservice.Config.rollbar_enabled
let rollbar_environment = Libservice.Config.rollbar_environment

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

let pghost = string "DARK_CONFIG_DB_HOST"
let pgdbname = string "DARK_CONFIG_DB_DBNAME"
let pguser = string "DARK_CONFIG_DB_USER"
let pgpassword = password "DARK_CONFIG_DB_PASSWORD"

let postgres_settings : postgres_config =
  { host = pghost
  ; dbname = pgdbname
  ; user = pguser
  ; password = pgpassword
  }
