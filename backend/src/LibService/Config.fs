module LibService.Config

open LibConfig.ConfigDsl

// Re-expose everything from LibConfig.Config
let envDisplayName = LibConfig.Config.envDisplayName
let buildHash = LibConfig.Config.buildHash
let runDir = LibConfig.Config.runDir
let logDir = LibConfig.Config.logDir

// sourceRootDir is for referencing things from our source code - requires env var
let sourceRootDir = LibConfig.ConfigDsl.absoluteDir "DARK_CONFIG_ROOT_DIR"

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
let hostName = getEnv "HOSTNAME" |> Option.defaultValue "none"


// --------------------
// ports
// --------------------
let bwdServerPort = int "DARK_CONFIG_BWDSERVER_BACKEND_PORT"
let bwdServerKubernetesPort = int "DARK_CONFIG_BWDSERVER_KUBERNETES_PORT"


// --------------------
// Feature flag defaults
// Sometimes we want different flag defaults in different environments
// --------------------

let traceSamplingRuleDefault = string "DARK_CONFIG_TRACE_SAMPLING_RULE_DEFAULT"
