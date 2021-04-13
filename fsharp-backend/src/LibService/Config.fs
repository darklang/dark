module LibService.Config

open ConfigDsl

let envDisplayName = string "DARK_CONFIG_ENV_DISPLAY_NAME"

// --------------------
// rollbar
// --------------------
let rollbarServerAccessToken =
  (* This is what the rollbar UI calls it *)
  string "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM"

let rollbarEnabled = bool "DARK_CONFIG_ROLLBAR_ENABLED"

let rollbarEnvironment = string "DARK_CONFIG_ROLLBAR_ENVIRONMENT"


// --------------------
// honeycomb
// --------------------
let honeycombKey = stringOption "DARK_CONFIG_HONEYCOMB_API_KEY"

let honeycombDataset = string "DARK_CONFIG_HONEYCOMB_DATASET_NAME"
let honeycombEndpoint = string "DARK_CONFIG_HONEYCOMB_API_ENDPOINT"


// --------------------
// k8s
// --------------------
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

// --------------------
// getting started canvas
// --------------------
let gettingStartedCanvasName = string "DARK_CONFIG_GETTING_STARTED_CANVAS_NAME"

let gettingStartedCanvasSource = string "DARK_CONFIG_GETTING_STARTED_CANVAS_SOURCE"
