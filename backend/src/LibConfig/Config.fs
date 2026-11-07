module LibConfig.Config

open ConfigDsl

/// The commit this CLI was built from, as 7 characters, or "dev" when nothing says.
/// A Release CLI embeds it (see Cli.fsproj). A Debug build doesn't, so a commit doesn't
/// recompile anything, and reads the GIT_COMMIT that `scripts/run-cli` exports.
let buildHash : string =
  let embedded =
    try
      match System.Reflection.Assembly.GetEntryAssembly() with
      | null -> None
      | entry ->
        use stream = entry.GetManifestResourceStream("build-hash.txt")
        if isNull stream then
          None
        else
          use reader = new System.IO.StreamReader(stream)
          Some(reader.ReadToEnd().Trim())
    with _ ->
      None
  match embedded, getEnv "GIT_COMMIT" with
  | Some hash, _ when hash <> "" -> hash
  | _, Some hash when hash <> "" -> hash.Substring(0, min 7 hash.Length)
  | _ -> "dev"

/// The directory the CLI chose for its store, if it chose one. Read once, here.
let private chosenRunDir = StoreLocation.take ()

// runDir is for runtime data (DB, logs, etc.) - separate from source code paths
let runDir =
  match chosenRunDir with
  | Some dir -> $"{dir}/"
  | None -> $"{StoreLocation.defaultDirectory ()}/"

let logDir = $"{runDir}logs/"

/// A store the CLI chose is always `data.db`: that is the file `extract` seeds
/// and upgrades, so a name from the environment would open an empty one beside it.
let dbName =
  match chosenRunDir, getEnv "DARK_CONFIG_DB_NAME" with
  | None, Some s -> s
  | _ -> "data.db"

// `runDir` already ends in a separator, so no slash here. This path gets printed.
let dbPath = $"{runDir}{dbName}"
