module LibConfig.Config

open ConfigDsl

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

/// The directory the CLI chose for its store, if it chose one. Read once, here.
let private chosenRunDir = StoreLocation.take ()

// runDir is for runtime data (DB, logs, etc.) - separate from source code paths
let runDir =
  match chosenRunDir with
  | Some dir -> $"{dir}/"
  | None -> absoluteDirOrCurrent "DARK_HOME"

let logDir = $"{runDir}logs/"

/// A store the CLI chose is always `data.db`: that is the file `extract` seeds
/// and upgrades, so a name from the environment would open an empty one beside it.
let dbName =
  match chosenRunDir, getEnv "DARK_CONFIG_DB_NAME" with
  | None, Some s -> s
  | _ -> "data.db"

// `runDir` already ends in a separator, so no slash here. This path gets printed.
let dbPath = $"{runDir}{dbName}"
