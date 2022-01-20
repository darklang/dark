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

let rootDir = absoluteDir "DARK_CONFIG_ROOT_DIR"

// Provided by k8s, used in rollbar
let hostName = getEnv "HOSTNAME" |> Option.defaultValue "none"

// --------------------
// rollbar
// --------------------
let rollbarServerAccessToken =
  // This is what the rollbar UI calls it
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
  | Honeycomb
  | Console

// "Where do you send the logs?"
let telemetryExporters : List<TelemetryExporter> =
  "DARK_CONFIG_TELEMETRY_EXPORTER"
  |> string
  |> Tablecloth.String.split ","
  |> Tablecloth.List.filterMap (function
    | "honeycomb" -> Some Honeycomb
    | "console" -> Some Console
    | "none" -> None
    | name -> failwith $"Unexpected Telemetry exporter {name}")

let honeycombApiKey = string "DARK_CONFIG_HONEYCOMB_API_KEY"
let honeycombDataset = string "DARK_CONFIG_HONEYCOMB_DATASET_NAME"
let honeycombEndpoint = string "DARK_CONFIG_HONEYCOMB_API_ENDPOINT"

// Don't use DARK_CONFIG_HEALTH_CHECK_PORT as that's part of the ocaml service
let apiServerPort = int "DARK_CONFIG_APISERVER_BACKEND_PORT"
let apiServerNginxPort = int "DARK_CONFIG_APISERVER_NGINX_PORT"
let apiServerKubernetesPort = int "DARK_CONFIG_APISERVER_KUBERNETES_PORT"

// Don't use DARK_CONFIG_HEALTH_CHECK_PORT as that's part of the ocaml service
let bwdServerPort = int "DARK_CONFIG_BWDSERVER_BACKEND_PORT"
let bwdServerNginxPort = int "DARK_CONFIG_BWDSERVER_NGINX_PORT"
let bwdServerKubernetesPort = int "DARK_CONFIG_BWDSERVER_KUBERNETES_PORT"

let legacyFuzzingServerPort = int "DARK_CONFIG_LEGACY_FUZZING_SERVER_PORT"
let legacySerializtionServerPort = int "DARK_CONFIG_LEGACY_SERIALIZATION_SERVER_PORT"

let croncheckerKubernetesPort = int "DARK_CONFIG_CRONCHECKER_KUBERNETES_PORT"
let queueWorkerKubernetesPort = int "DARK_CONFIG_QUEUEWORKER_KUBERNETES_PORT"

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
