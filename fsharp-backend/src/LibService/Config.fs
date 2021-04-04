module LibService.Config

open ConfigDsl

let envDisplayName = string "DARK_CONFIG_ENV_DISPLAY_NAME"

let rollbarServerAccessToken =
  (* This is what the rollbar UI calls it *)
  string "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM"


let rollbarEnabled = bool "DARK_CONFIG_ROLLBAR_ENABLED"

let rollbarEnvironment = string "DARK_CONFIG_ROLLBAR_ENVIRONMENT"

let healthCheckPort = int "DARK_CONFIG_HEALTH_CHECK_PORT"

// --------------------
// db
// --------------------

type PostgresConfig =
  { host : string
    dbname : string
    user : string
    password : string }

let pghost = string "DARK_CONFIG_DB_HOST"

let pgdbname = string "DARK_CONFIG_DB_DBNAME"

let pguser = string "DARK_CONFIG_DB_USER"

let pgpassword = password "DARK_CONFIG_DB_PASSWORD"

let postgresSettings : PostgresConfig =
  { host = pghost; dbname = pgdbname; user = pguser; password = pgpassword }

let gettingStartedCanvasName = string "DARK_CONFIG_GETTING_STARTED_CANVAS_NAME"

let gettingStartedCanvasSource = string "DARK_CONFIG_GETTING_STARTED_CANVAS_SOURCE"
