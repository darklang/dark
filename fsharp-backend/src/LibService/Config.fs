module LibService.Config

open ConfigDsl

let envDisplayName = string "DARK_CONFIG_ENV_DISPLAY_NAME"

// If the GIT_COMMIT is in the environment, use that as the build hash.
// Otherwise, set it to the env name so that it's constant.
//
// We intentionally bypass our DSL here as `GIT_COMMIT` is not set by our
// config system but as part of the production container build process, and is
// not available in dev mode.
let buildHash : string =
  match getEnv "GIT_COMMIT" with
  | Some s -> s
  | None -> envDisplayName

// --------------------
// rollbar
// --------------------
let rollbarServerAccessToken =
  (* This is what the rollbar UI calls it *)
  string "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM"

let rollbarEnabled = bool "DARK_CONFIG_ROLLBAR_ENABLED"

let rollbarEnvironment = string "DARK_CONFIG_ROLLBAR_ENVIRONMENT"

// -------------------------
// Heap
// -------------------------
let heapioId = string "DARK_CONFIG_HEAPIO_ID"


// --------------------
// honeycomb
// --------------------
type TelemetryExporter =
  | NoExporter
  | Honeycomb
  | Console

// "Where do you send the logs?"
let telemetryExporter : TelemetryExporter =
  stringChoice
    "DARK_CONFIG_TELEMETRY_EXPORTER"
    [ ("none", NoExporter); ("honeycomb", Honeycomb); ("console", Console) ]

let honeycombApiKey = string "DARK_CONFIG_HONEYCOMB_API_KEY"
let honeycombDataset = string "DARK_CONFIG_HONEYCOMB_DATASET_NAME"
let honeycombEndpoint = string "DARK_CONFIG_HONEYCOMB_API_ENDPOINT"


// --------------------
// k8s
// --------------------
// Don't use DARK_CONFIG_HEALTH_CHECK_PORT as that's part of the ocaml service
let bwdServerHealthCheckPort = int "DARK_CONFIG_BWDSERVER_HEALTHCHECK_PORT"

let apiServerHealthCheckPort = int "DARK_CONFIG_APISERVER_HEALTHCHECK_PORT"

let apiServerPort = int "DARK_CONFIG_APISERVER_PORT"
let bwdServerPort = int "DARK_CONFIG_BWDSERVER_PORT"
let legacyServerPort = int "DARK_CONFIG_LEGACYSERVER_PORT"

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
