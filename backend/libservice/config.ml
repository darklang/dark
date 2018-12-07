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
