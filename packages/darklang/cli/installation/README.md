Relates to locally installing the `dark` CLI tool
(as opposed to running it without any install).

To run the CLI executable against the local package manager:



- [ ] TODO: Bring back self-updating
  ```fsharp

      // Check if we should skip self update
      // (if we've updated in the last 24 hours, don't update)
      let hasUpdatedInLastDay (configPath: String) : Bool =
        match Config.read configPath with
        | Ok config ->
          match config |> Stdlib.Dict.get "lastUpdateTimestamp" with
          | None -> false
          | Some timestamp ->
            let secondsSinceLastUpdate =
              ((Stdlib.DateTime.now ()) |> Stdlib.DateTime.toSeconds)
              - (timestamp |> Stdlib.Int64.parse |> Builtin.unwrap)

            let oneDayInSeconds = 86400L // TODO: show math

            secondsSinceLastUpdate < oneDayInSeconds
        | Error e -> false



      let selfUpdateIfRelevant () : Stdlib.Result.Result<Unit, String> =
        // if not installed, ignore -- nothing to self-update

        let configPath =
          match (Stdlib.Cli.OS.getOS ()) with
          | Ok Windows ->
            let homeDirectory =
              (Stdlib.Cli.PowerShell.getHomeDirectory ()) |> Builtin.unwrap

            $"{homeDirectory}\\.darklang\\config.json"
          | Ok Linux | Ok MacOS -> "$HOME/.darklang/config.json"


        if Stdlib.Bool.not (Builtin.fileExists configPath) then
          Stdlib.Result.Result.Ok()
        // don't update _too_ often
        else if hasUpdatedInLastDay configPath then
          Stdlib.printLine
            "Skipping self-update because we've updated in the last 24 hours"

          Stdlib.Result.Result.Ok()
        else
          match isAtLatestVersion configPath with
          | Ok true -> Stdlib.Result.Result.Ok()
          | Ok false ->
            (Stdlib.Cli.Host.getRuntimeHost ())
            |> Stdlib.Result.andThen (fun host ->
              Installation.Install.installOrUpdateLatestRelease host)
          | Error e -> Stdlib.Result.Result.Error e

  if Stdlib.List.member_v0 args "--skip-self-update" then
    let newArgs =
      args |> Stdlib.List.filter (fun arg -> arg != "--skip-self-update")

    processNormally newArgs
  else
    match Installation.selfUpdateIfRelevant () with
    | Ok _ -> processNormally args
    | Error e ->
      Stdlib.printLine $"Failed to run self-update: {e}\nProceeding anyway."
      processNormally args
  ```