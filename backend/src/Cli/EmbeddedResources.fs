module Cli.EmbeddedResources

open System
open System.IO
open System.Reflection

// Resolve the running executable's directory.
// Assembly.Location returns "" for assemblies embedded in a single-file or AOT
// bundle (and emits IL3000). AppContext.BaseDirectory is the AOT-clean replacement
// for "where is the published binary"; ProcessPath stays as a final fallback.
let private exeDirectory () : string =
  let baseDir = AppContext.BaseDirectory
  if not (String.IsNullOrEmpty(baseDir)) then
    baseDir.TrimEnd('/', '\\')
  else
    let path = System.Environment.ProcessPath
    if String.IsNullOrEmpty(path) then
      Environment.CurrentDirectory
    else
      Path.GetDirectoryName(path)

/// Determines if CLI is running in "installed" mode (in ~/.darklang/bin/) vs portable mode
let private isInstalledMode () : bool =
  let dir = exeDirectory ()
  dir.EndsWith("/.darklang/bin") || dir.EndsWith("\\.darklang\\bin")

/// Gets the appropriate .darklang directory path
let private getDarklangDirectory () : string =
  if isInstalledMode () then
    // Installed mode: use the central ~/.darklang directory
    let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    Path.Combine(home, ".darklang")
  else
    // Portable mode: use adjacent .darklang directory
    Path.Combine(exeDirectory (), ".darklang")

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

/// Extract a resource that was gzip-compressed at build time.
/// SQLite databases compress ~3-4× with gzip; we ship `data.db.gz`
/// embedded and decompress on first extract. Saves ~7 MB on the binary.
let private extractGzippedResource
  (resourceName : string)
  (targetPath : string)
  : unit =
  let assembly = Assembly.GetExecutingAssembly()

  let targetDir = Path.GetDirectoryName(targetPath)
  if not (Directory.Exists(targetDir)) then
    Directory.CreateDirectory(targetDir) |> ignore

  use stream = assembly.GetManifestResourceStream(resourceName)
  if stream = null then
    ()
  else
    use gzip =
      new System.IO.Compression.GZipStream(
        stream,
        System.IO.Compression.CompressionMode.Decompress
      )
    use fileStream = File.Create(targetPath)
    gzip.CopyTo(fileStream)

let private hasEmbeddedResource (resourceName : string) : bool =
  let assembly = Assembly.GetExecutingAssembly()
  assembly.GetManifestResourceNames() |> Array.contains resourceName

let extract () : unit =
  // The embedded resource is `data.db.gz` (the seed db, gzip-compressed at
  // build time to save ~7 MB on binary size). On first run, decompress to
  // `~/.darklang/data.db`; on subsequent runs the file already exists and
  // grow/init proceeds against the local copy.
  if hasEmbeddedResource "data.db.gz" then
    let darklangDir = getDarklangDirectory ()

    Environment.SetEnvironmentVariable("DARK_CONFIG_RUNDIR", darklangDir)

    let dbPath = Path.Combine(darklangDir, "data.db")

    if not (File.Exists(dbPath)) then
      printfn $"Setting up Darklang CLI data directory at {darklangDir}"

      if not (Directory.Exists(darklangDir)) then
        Directory.CreateDirectory(darklangDir) |> ignore

      extractGzippedResource "data.db.gz" dbPath

      let readmePath = Path.Combine(darklangDir, "README.md")
      extractResource "README.md" readmePath

      let logsDir = Path.Combine(darklangDir, "logs")
      Directory.CreateDirectory(logsDir) |> ignore

      printfn "CLI data directory setup complete"
