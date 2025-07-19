module LibService.Config

open LibConfig.ConfigDsl

// Re-expose everything from LibService.Config
let envDisplayName = LibConfig.Config.envDisplayName
let buildHash = LibConfig.Config.buildHash
let rootDir = LibConfig.Config.rootDir

// -------------------------
// Logging
// -------------------------
type Logger =
  | NoLogger
  | ConsoleLogger

let defaultLogger =
  match string "DARK_CONFIG_DEFAULT_LOGGER" with
  | "none" -> NoLogger
  | "console" -> ConsoleLogger
  | name -> failwith $"Unexpected default logger: {name}"

// Cloud-specific config
// Provided by k8s, used in rollbar
let hostName = getEnv "HOSTNAME" |> Option.defaultValue "none"


// --------------------
// rollbar
// --------------------
let rollbarServerAccessToken =
  // This is what the rollbar UI calls it
  credentials "DARK_CONFIG_ROLLBAR_POST_SERVER_ITEM"

let rollbarEnabled = bool "DARK_CONFIG_ROLLBAR_ENABLED"

let rollbarEnvironment = string "DARK_CONFIG_ROLLBAR_ENVIRONMENT"



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
  |> String.split ","
  |> List.filterMap (fun telemExporter ->
    match telemExporter with
    | "honeycomb" -> Some Honeycomb
    | "console" -> Some Console
    | "none" -> None
    | name -> failwith $"Unexpected Telemetry exporter {name}")

let honeycombApiKey = credentials "DARK_CONFIG_HONEYCOMB_API_KEY"
let honeycombDataset = string "DARK_CONFIG_HONEYCOMB_DATASET_NAME"
let honeycombEndpoint = string "DARK_CONFIG_HONEYCOMB_API_ENDPOINT"

// --------------------
// ports
// --------------------
let bwdServerPort = int "DARK_CONFIG_BWDSERVER_BACKEND_PORT"
let bwdServerKubernetesPort = int "DARK_CONFIG_BWDSERVER_KUBERNETES_PORT"

let croncheckerKubernetesPort = int "DARK_CONFIG_CRONCHECKER_KUBERNETES_PORT"
let queueWorkerKubernetesPort = int "DARK_CONFIG_QUEUEWORKER_KUBERNETES_PORT"

// --------------------
// Launchdarkly
// --------------------

/// If none, use test values
let launchDarklyApiKey = credentialsOption "DARK_CONFIG_LAUNCHDARKLY_SDK_API_KEY"


// --------------------
// Feature flag defaults
// Sometimes we want different flag defaults in different environments
// --------------------

let traceSamplingRuleDefault = string "DARK_CONFIG_TRACE_SAMPLING_RULE_DEFAULT"
