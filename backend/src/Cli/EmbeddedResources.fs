module Cli.EmbeddedResources

open System
open System.IO
open System.Reflection

/// Determines if CLI is running in "installed" mode (in ~/.darklang/bin/) vs portable mode
let private isInstalledMode () : bool =
  let exePath =
    let location = Assembly.GetExecutingAssembly().Location
    if String.IsNullOrEmpty(location) then
      System.Environment.ProcessPath
    else
      location

  match exePath with
  | null
  | "" -> false
  | path ->
    let exeDir = Path.GetDirectoryName(path)
    exeDir.EndsWith("/.darklang/bin") || exeDir.EndsWith("\\.darklang\\bin")

/// Gets the appropriate .darklang directory path
let private getDarklangDirectory () : string =
  if isInstalledMode () then
    // Installed mode: use the central ~/.darklang directory
    let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    Path.Combine(home, ".darklang")
  else
    // Portable mode: use adjacent .darklang directory
    let exePath =
      let location = Assembly.GetExecutingAssembly().Location
      if String.IsNullOrEmpty(location) then
        System.Environment.ProcessPath
      else
        location

    let exeDir =
      if String.IsNullOrEmpty(exePath) then
        Environment.CurrentDirectory
      else
        Path.GetDirectoryName(exePath)

    Path.Combine(exeDir, ".darklang")

let private extractResource (resourceName : string) (targetPath : string) : unit =
  let assembly = Assembly.GetExecutingAssembly()

  let targetDir = Path.GetDirectoryName(targetPath)
  if not (Directory.Exists(targetDir)) then
    Directory.CreateDirectory(targetDir) |> ignore

  use stream = assembly.GetManifestResourceStream(resourceName)

  if stream = null then
    // Resource not found - acceptable in debug builds
    ()
  else
    use fileStream = File.Create(targetPath)
    stream.CopyTo(fileStream)

let private hasEmbeddedResource (resourceName : string) : bool =
  let assembly = Assembly.GetExecutingAssembly()
  assembly.GetManifestResourceNames() |> Array.contains resourceName

let extract () : unit =
  if hasEmbeddedResource "data.db" then
    let darklangDir = getDarklangDirectory ()

    Environment.SetEnvironmentVariable("DARK_CONFIG_RUNDIR", darklangDir)

    // Only extract if .darklang directory doesn't exist yet
    if not (Directory.Exists(darklangDir)) then
      printfn $"Setting up Darklang CLI data directory at {darklangDir}"

      // Extract database
      let dbPath = Path.Combine(darklangDir, "data.db")
      extractResource "data.db" dbPath

      // Extract README
      let readmePath = Path.Combine(darklangDir, "README.md")
      extractResource "README.md" readmePath

      // Create logs directory
      let logsDir = Path.Combine(darklangDir, "logs")
      Directory.CreateDirectory(logsDir) |> ignore

      printfn "CLI data directory setup complete"
