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

/// A published CLI carries its own seed store, which `Cli.EmbeddedResources.extract` keeps up to date.
let private publishedStore : bool =
  match System.Reflection.Assembly.GetEntryAssembly() with
  | null -> false
  | entry -> entry.GetManifestResourceNames() |> Array.contains "data.db.br"

// runDir is for runtime data (DB, logs, etc.) - separate from source code paths
let runDir = $"{StoreLocation.defaultDirectory ()}/"

let logDir = $"{runDir}logs/"

/// A published CLI's store is always `data.db`: that is the file `extract` seeds
/// and upgrades, so a name from the environment would open an empty one beside it.
let dbName =
  match publishedStore, getEnv "DARK_CONFIG_DB_NAME" with
  | false, Some s -> s
  | _ -> "data.db"

// `runDir` already ends in a separator, so no slash here. This path gets printed.
let dbPath = $"{runDir}{dbName}"
