open Core
open Config_dsl

let env_display_name = string "DARK_CONFIG_ENV_DISPLAY_NAME"

let rollbar_server_access_token =
  (* This is what the rollbar UI calls it *)
  string "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM"


let rollbar_url = "https://api.rollbar.com/api/1/item/"

let rollbar_enabled = bool "DARK_CONFIG_ROLLBAR_ENABLED"

let rollbar_environment = string "DARK_CONFIG_ROLLBAR_ENVIRONMENT"

let health_check_port = int "DARK_CONFIG_HEALTH_CHECK_PORT"

let legacy_fuzzing_server_port = int "DARK_CONFIG_LEGACY_FUZZING_SERVER_PORT"

let legacy_serialization_server_port =
  int "DARK_CONFIG_LEGACY_SERIALIZATION_SERVER_PORT"


let legacy_serialization_server_host =
  string "DARK_CONFIG_LEGACY_SERIALIZATION_SERVER_HOST"


(* -------------------- *)
(* db *)
(* -------------------- *)

type postgres_config =
  { host : string
  ; dbname : string
  ; user : string
  ; password : string }

let pghost = string "DARK_CONFIG_DB_HOST"

let pgdbname = string "DARK_CONFIG_DB_DBNAME"

let pguser = string "DARK_CONFIG_DB_USER"

let pgpassword = password "DARK_CONFIG_DB_PASSWORD"

let postgres_settings : postgres_config =
  {host = pghost; dbname = pgdbname; user = pguser; password = pgpassword}


let getting_started_canvas_name =
  string "DARK_CONFIG_GETTING_STARTED_CANVAS_NAME"


let getting_started_canvas_source =
  string "DARK_CONFIG_GETTING_STARTED_CANVAS_SOURCE"


(* ------------------------- *)
(* Logs *)
(* ------------------------- *)
let log_format : [`Json | `DecoratedJson] =
  let as_str =
    string_choice "DARK_CONFIG_LOGGING_FORMAT" ["json"; "decorated_json"]
  in
  match as_str with
  | "json" ->
      `Json
  | "decorated_json" ->
      `DecoratedJson
  | _ ->
      failwith ("Invalid logging format: " ^ as_str)


let log_level =
  let as_str =
    string_choice
      "DARK_CONFIG_LOGLEVEL"
      [ "off"
      ; "inspect"
      ; "fatal"
      ; "error"
      ; "warn"
      ; "info"
      ; "success"
      ; "debug"
      ; "all" ]
  in
  match as_str with
  | "off" ->
      `Off
  | "inspect" ->
      `Inspect
  | "fatal" ->
      `Fatal
  | "error" ->
      `Error
  | "warn" ->
      `Warn
  | "info" ->
      `Info
  | "success" ->
      `Success
  | "debug" ->
      `Debug
  | "all" ->
      `All
  | _ ->
      failwith ("Invalid level name:" ^ as_str)
