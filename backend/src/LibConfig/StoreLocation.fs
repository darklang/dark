/// Where this instance's store lives.
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
