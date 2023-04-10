/// Provides central place to fetch known configuration values for LibService
module LibService.Config

open ConfigDsl

let envDisplayName = string "DARK_CONFIG_ENV_DISPLAY_NAME"

/// <summary>
/// If the GIT_COMMIT is in the environment, use that as the build hash.
/// Otherwise, set it to the env name so that it's constant.
/// </summary>
///
/// <remarks>
/// We intentionally bypass our DSL here as `GIT_COMMIT` is not set by our
/// config system but as part of the production container build process, and is
/// not available in dev mode.
/// </remarks>
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
  |> Tablecloth.List.filterMap (fun telemExporter ->
    match telemExporter with
    | "honeycomb" -> Some Honeycomb
    | "console" -> Some Console
    | "none" -> None
    | name -> failwith $"Unexpected Telemetry exporter {name}")

let honeycombApiKey = string "DARK_CONFIG_HONEYCOMB_API_KEY"
let honeycombDataset = string "DARK_CONFIG_HONEYCOMB_DATASET_NAME"
let honeycombEndpoint = string "DARK_CONFIG_HONEYCOMB_API_ENDPOINT"

// --------------------
// ports
// --------------------
let bwdServerPort = int "DARK_CONFIG_BWDSERVER_BACKEND_PORT"
let bwdServerKubernetesPort = int "DARK_CONFIG_BWDSERVER_KUBERNETES_PORT"

let bwdDangerServerPort = int "DARK_CONFIG_BWDSERVER_DANGER_BACKEND_PORT"

let bwdDangerServerKubernetesPort =
  int "DARK_CONFIG_BWDSERVER_DANGER_KUBERNETES_PORT"

let croncheckerKubernetesPort = int "DARK_CONFIG_CRONCHECKER_KUBERNETES_PORT"
let queueWorkerKubernetesPort = int "DARK_CONFIG_QUEUEWORKER_KUBERNETES_PORT"

// --------------------
// Launchdarkly
// --------------------

/// If none, use test values
let launchDarklyApiKey = stringOption "DARK_CONFIG_LAUNCHDARKLY_SDK_API_KEY"


// --------------------
// Feature flag defaults
// Sometimes we want different flag defaults in different environments
// --------------------

let traceSamplingRuleDefault = string "DARK_CONFIG_TRACE_SAMPLING_RULE_DEFAULT"

// --------------------
// db
// --------------------
let pgHost = string "DARK_CONFIG_DB_HOST"

let pgDBName = string "DARK_CONFIG_DB_DBNAME"

let pgUser = string "DARK_CONFIG_DB_USER"

let pgPassword = password "DARK_CONFIG_DB_PASSWORD"

let pgPoolSize = int "DARK_CONFIG_DB_POOL_SIZE"

// This is just until the base migration file stabalizes
let clearDBOnStartup = bool "DARK_CONFIG_DB_CLEAR_ON_STARTUP"
