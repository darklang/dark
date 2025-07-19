module LibConfig.Config

open ConfigDsl

let envDisplayName =
  let envName = "DARK_CONFIG_ENV_DISPLAY_NAME"
  match getEnv envName with
  | Some env -> lowercase envName env
  | None -> "unknown" // TODO something better

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

let rootDir = absoluteDirOrCurrent "DARK_CONFIG_ROOT_DIR"

let runDir = $"{rootDir}rundir/"

let dbName =
  match getEnv "DARK_CONFIG_DB_NAME" with
  | Some s -> s
  | None -> "data.db"

let dbPath = $"{runDir}/{dbName}"
