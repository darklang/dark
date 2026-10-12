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

/// Which release this binary is, and which it comes after, as `scripts/build/_release-tag` found them at
/// build: `(Some "v0.0.74", Some "v0.0.74")` for a release, `(None, Some "v0.0.74")` for a build after
/// it, `(None, None)` where the build saw no tags.
let releaseTags : Option<string> * Option<string> =
  let line (s : string) = if s.Trim() = "" then None else Some(s.Trim())
  try
    use stream =
      System.Reflection.Assembly
        .GetExecutingAssembly()
        .GetManifestResourceStream("LibConfig.release-tag.txt")
    if stream <> null then
      use reader = new System.IO.StreamReader(stream)
      match reader.ReadToEnd().Split('\n') |> List.ofArray with
      | tag :: after :: _ -> (line tag, line after)
      | [ tag ] -> (line tag, None)
      | [] -> (None, None)
    else
      (None, None)
  with _ ->
    (None, None)

// runDir is for runtime data (DB, logs, etc.) - separate from source code paths
let runDir = absoluteDirOrCurrent "DARK_CONFIG_RUNDIR"

let logDir = $"{runDir}logs/"

let dbName =
  match getEnv "DARK_CONFIG_DB_NAME" with
  | Some s -> s
  | None -> "data.db"

// `runDir` already ends in a separator, so no slash here. This path gets printed.
let dbPath = $"{runDir}{dbName}"
