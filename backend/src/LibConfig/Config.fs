module LibConfig.Config

open ConfigDsl

let envDisplayName =
  let envName = "DARK_CONFIG_ENV_DISPLAY_NAME"
  match getEnv envName with
  | Some env -> lowercase envName env
  | None -> "unknown" // TODO something better

/// <summary>
/// Read the git commit hash from the embedded resource.
/// Falls back to "dev" for local development builds.
/// </summary>
let buildHash : string =
  try
    use stream =
      System.Reflection.Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("LibConfig.build-hash.txt")
    if stream <> null then
      use reader = new System.IO.StreamReader(stream)
      reader.ReadToEnd().Trim()
    else
      "dev"
  with _ ->
    "dev"

/// Identifies this compilation, including builds from the same git commit.
/// Without embedded metadata, a process-local ID prevents persisted cache reuse.
let buildId : string =
  use stream =
    System.Reflection.Assembly
      .GetExecutingAssembly()
      .GetManifestResourceStream("LibConfig.build-id.txt")
  if stream = null then
    System.Guid.NewGuid().ToString("N")
  else
    use reader = new System.IO.StreamReader(stream)
    match System.Guid.TryParse(reader.ReadToEnd().Trim()) with
    | true, id -> id.ToString("N")
    | false, _ -> System.Guid.NewGuid().ToString("N")

// runDir is for runtime data (DB, logs, etc.) - separate from source code paths
let runDir = absoluteDirOrCurrent "DARK_CONFIG_RUNDIR"

let logDir = $"{runDir}logs/"

let dbName =
  match getEnv "DARK_CONFIG_DB_NAME" with
  | Some s -> s
  | None -> "data.db"

// `runDir` already ends in a separator, so no slash here. This path gets printed.
let dbPath = $"{runDir}{dbName}"
