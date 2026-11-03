/// Where this instance's store lives, when the CLI worked it out rather than
/// the environment.
///
/// A published CLI picks its own store directory at startup
/// (`Cli.EmbeddedResources.extract`). Handing it to `LibConfig.Config` through
/// the process environment would hand it to every process this one starts as
/// well, and a second `dark` among them would open this store instead of its
/// own. Handing it over here reaches `Config` and nothing else.
module LibConfig.StoreLocation

/// `~/.darklang`, for every build. A process wanting its own store gets its own HOME. The CLR's
/// `UserProfile` ignores the environment on Windows, so `USERPROFILE` is read first there.
let defaultDirectory () : string =
  let fromEnv name =
    match System.Environment.GetEnvironmentVariable name with
    | null
    | "" -> None
    | value -> Some value
  let home =
    (if System.OperatingSystem.IsWindows() then fromEnv "USERPROFILE" else None)
    |> Option.defaultWith (fun () ->
      System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile))
  System.IO.Path.Combine(home, ".darklang")

let mutable private chosen : Option<string> = None
let mutable private taken = false

/// Must run before anything reads `LibConfig.Config`, which fixes its paths the
/// first time it is touched. Too late is an error rather than a no-op, since the
/// alternative is a process whose logs and policy sit in one directory and whose
/// store sits in another.
let set (dir : string) : unit =
  if taken then
    failwith
      $"LibConfig.StoreLocation.set {dir} after LibConfig.Config was already read"
  chosen <- Some dir

/// For `LibConfig.Config` only.
let take () : Option<string> =
  taken <- true
  chosen
