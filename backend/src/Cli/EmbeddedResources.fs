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

/// The .darklang directory to use when nothing says otherwise.
let private getDefaultDarklangDirectory () : string =
  if isInstalledMode () then
    // Installed mode: use the central ~/.darklang directory
    let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    Path.Combine(home, ".darklang")
  else
    // Portable mode: use adjacent .darklang directory
    Path.Combine(exeDirectory (), ".darklang")

/// Where this instance keeps its store, logs and local config.
///
/// An explicit `DARK_CONFIG_RUNDIR` wins over the default. Overwriting it instead would
/// mean every process on a machine shares one store, so a second instance -- a throwaway
/// store to try something in, or two binaries measured against the same data -- could not
/// be asked for at all.
let private getDarklangDirectory () : string =
  match Environment.GetEnvironmentVariable "DARK_CONFIG_RUNDIR" with
  | null
  | "" -> getDefaultDarklangDirectory ()
  | explicit -> explicit

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

/// The embedded schema, or None in a debug build that did not embed it.
let embeddedSchema () : Option<string> =
  LibDB.CatchUpStore.schemaFrom (Assembly.GetExecutingAssembly())


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



/// Copy the store aside before an upgrade changes anything in it, and say where.
///
/// Through SQLite's backup API rather than a file copy: the store runs in WAL mode, so `data.db` alone
/// can be missing what was last committed. One per build it upgrades TO, and an existing one is kept:
/// a start that retries after a failed step must not replace the store as it was with the store as
/// the failure left it.
let private backupBeforeUpgrade
  (dbPath : string)
  (build : string)
  : Result<string, string> =
  let dir = Path.Combine(Path.GetDirectoryName(dbPath), "backups")
  let tag = if build.Length > 12 then build.Substring(0, 12) else build
  let target = Path.Combine(dir, $"data.db.before-upgrade-to-{tag}")
  if File.Exists target then
    Ok target
  else
    let partial = target + ".partial"
    try
      Directory.CreateDirectory(dir) |> ignore<DirectoryInfo>
      if File.Exists partial then File.Delete partial
      (use source =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={dbPath};Pooling=False"
        )
       source.Open()
       use destination =
         new Microsoft.Data.Sqlite.SqliteConnection(
           $"Data Source={partial};Pooling=False"
         )
       destination.Open()
       source.BackupDatabase destination)
      File.Move(partial, target)
      eprintfn $"Backed up your store to {target} before upgrading it."
      Ok target
    with e ->
      Error e.Message


/// Stop rather than run against a store an upgrade could not finish, and say how to get back.
///
/// Carrying on is what this replaces: one line on stderr, the command ran against a half-migrated
/// store, and the build stamp went in anyway, so the same binary never tried again.
let private refuseToOpen
  (dbPath : string)
  (backup : string option)
  (what : string)
  (reason : string)
  : 'a =
  let e = System.Console.Error
  e.WriteLine ""
  e.WriteLine "dark could not finish upgrading your store, so it has not opened it."
  e.WriteLine $"  {what}"
  // The first line: a database error carries its whole stack trace in its message.
  let firstLine = (reason.Split '\n' |> Array.head).Trim()
  e.WriteLine $"  error: {firstLine}"
  match backup with
  | Some backup ->
    e.WriteLine $"  your store from before this upgrade: {backup}"
    e.WriteLine ""
    e.WriteLine
      "Nothing was skipped: the next start of this dark tries the upgrade again from where it stopped."
    e.WriteLine "To go back instead, stop every dark, then:"
    e.WriteLine $"  rm -f '{dbPath}-wal' '{dbPath}-shm' && cp '{backup}' '{dbPath}'"
    e.WriteLine "and use the dark you had before."
  | None ->
    e.WriteLine "  no backup was taken, so nothing in the store has been changed."
  exit 3


/// One dark at a time through an upgrade, and through the grow that folds what it brought. SQLite has
/// no `ADD COLUMN IF NOT EXISTS`, so each step looks before it acts, and two processes that both look
/// before either acts both try to add the column; and sixteen processes folding one upgrade's ops at
/// once failed six of them with "database is locked".
///
/// An exclusive open of a file beside the store, which .NET holds as an OS lock (`flock` on Unix). The
/// kernel drops it when the process exits however it exits, so a crash leaves no stale lock, and a
/// leftover FILE is not a held lock. A waiter re-reads what it was about to do once it gets in, and
/// gives up after five minutes.
let upgradeLock (dbPath : string) : System.IDisposable =
  let path = dbPath + ".upgrade-lock"
  let deadline = System.DateTime.UtcNow.AddMinutes 5.0
  let mutable said = false
  let rec acquire () : System.IDisposable =
    try
      new FileStream(
        path,
        FileMode.OpenOrCreate,
        FileAccess.ReadWrite,
        FileShare.None
      )
      :> System.IDisposable
    with :? IOException as e ->
      if System.DateTime.UtcNow > deadline then
        refuseToOpen
          dbPath
          None
          "another dark has been upgrading this store for five minutes"
          e.Message
      if not said then
        said <- true
        eprintfn "Waiting for another dark to finish upgrading the store..."
      System.Threading.Thread.Sleep 100
      acquire ()
  acquire ()


/// What this start's catch-up found authored here rather than by a build, for `Cli.fs` to put back
/// after the fold (`LibDB.CatchUpStore.LocallyAuthored`).
let mutable locallyAuthored : LibDB.CatchUpStore.LocallyAuthored = []


/// Catch an existing store up to this binary's embedded package ops (`LibDB.CatchUpStore.fromRelease`).
///
/// Failure is not fatal on purpose: a store that could not be topped up is no worse off than before. It
/// answers whether it worked, so a failure leaves the store unstamped and the next start tries again.
let private reseedFromEmbedded (dbPath : string) : bool =
  let temp =
    Path.Combine(Path.GetTempPath(), $"dark-seed-{System.Guid.NewGuid()}.db")

  try
    try
      extractGzippedResource "data.db.gz" temp

      if File.Exists temp then
        locallyAuthored <- LibDB.CatchUpStore.fromRelease dbPath temp
      true
    with e ->
      System.Console.Error.WriteLine(
        $"could not top up the package store: {e.Message}"
      )
      false
  finally
    try
      if File.Exists temp then File.Delete temp
    with _ ->
      ()


/// Sub-timings for `extract`, in Stopwatch ticks. Collected rather than logged because `extract` runs
/// before telemetry has an output path: it's what sets DARK_CONFIG_RUNDIR, where the log lives.
/// `Cli.Main` drains this once telemetry is up.
let timings : ResizeArray<string * int64> = ResizeArray()

let inline private timed (label : string) (f : unit -> 'a) : 'a =
  let t0 = System.Diagnostics.Stopwatch.GetTimestamp()
  let r = f ()
  timings.Add(label, System.Diagnostics.Stopwatch.GetTimestamp() - t0)
  r

let private storeStamp = LibDB.CatchUpStore.storeStamp
let private recordStoreStamp = LibDB.CatchUpStore.recordStoreStamp

let extract () : unit =
  // On first run, decompress the embedded seed db to `~/.darklang/data.db`; afterwards the
  // file exists and grow/init proceeds against the local copy.
  if timed "extract.hasResource" (fun () -> hasEmbeddedResource "data.db.gz") then
    let darklangDir = getDarklangDirectory ()

    Environment.SetEnvironmentVariable("DARK_CONFIG_RUNDIR", darklangDir)

    let dbPath = Path.Combine(darklangDir, "data.db")

    // Asked ONCE, and it decides everything below.
    //
    // The stamp says which build last reconciled this store with its own embedded seed. The
    // schema, the release steps and the seed are all fixed per binary, so a store this same
    // build has already reconciled cannot need any of them again -- and nothing outside can
    // create that need, because an older binary running here records ITS hash and we come back
    // and do the work.
    //
    // It used to guard only the seed top-up, and the schema pass ran on every single command:
    // `CREATE TABLE IF NOT EXISTS` for every table, then the release list, then every index.
    // Measured on the published binary, that was 31 ms of a 203 ms startup, paid by `dark ps`
    // and `dark eval 1L` alike.
    let build = LibConfig.Config.buildHash
    // A build with no hash of its own cannot claim anything, so it does the work every time,
    // which is what every build did before.
    // `File.Exists` first: reading the stamp opens the db, which CREATES it, and an empty file
    // here reads as a store that needs no seed.
    let reconciled =
      build <> "dev" && File.Exists(dbPath) && storeStamp dbPath = Some build

    // An EXISTING store keeps whatever shape the seed it was born from had: the schema never runs
    // against it, so a table or column added since is simply absent, and the top-up below is the first
    // thing to trip over it -- as a raw SQLite error ("table locations has no column named previous"),
    // on a store that is otherwise fine. Bring the shape forward first, in the order the statements
    // require.
    //
    // In this order, and each one only if the one before it worked: back up, run the steps, top up,
    // stamp. A step that fails stops dark here (`refuseToOpen`), unstamped, so the next start tries
    // again; carrying on would run every command against a half-upgraded store.
    if File.Exists(dbPath) && not reconciled then
      use _upgrading = upgradeLock dbPath
      // Another dark may have done all of this while this one waited for the lock.
      if not (build <> "dev" && storeStamp dbPath = Some build) then
        let backup =
          match backupBeforeUpgrade dbPath build with
          | Ok backup -> backup
          | Error reason ->
            refuseToOpen
              dbPath
              None
              "the backup taken before upgrading failed"
              reason

        try
          match embeddedSchema () with
          | Some sql ->
            timed "extract.schema" (fun () -> LibDB.Releases.applySchemaTables sql)
            timed "extract.releases" (fun () -> LibDB.Releases.runPending ())
            timed "extract.indexes" (fun () -> LibDB.Releases.applySchemaIndexes sql)
          | None -> ()
        with
        | LibDB.Releases.StepFailed(step, inner) ->
          refuseToOpen dbPath (Some backup) $"release step: {step}" inner.Message
        | e ->
          refuseToOpen
            dbPath
            (Some backup)
            "bringing the store's tables up to date"
            e.Message

        // Top up an existing store with this binary's own package code (see
        // `reseedFromEmbedded`: additive, content-addressed), then `growIfNeeded` folds
        // it; without this, upgrading the binary would mean wiping the store.
        let toppedUp =
          timed "extract.topUpStore" (fun () -> reseedFromEmbedded dbPath)
        if toppedUp && build <> "dev" then recordStoreStamp dbPath build

    if not (File.Exists(dbPath)) then
      eprintfn $"Setting up Darklang CLI data directory at {darklangDir}"

      if not (Directory.Exists(darklangDir)) then
        Directory.CreateDirectory(darklangDir) |> ignore

      extractGzippedResource "data.db.gz" dbPath

      // Everything in a store this fresh came from the seed, so the ledger can be filled with
      // certainty exactly once. Without it the first upgrade after an install has no provenance.
      try
        use conn =
          new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <-
          "CREATE TABLE IF NOT EXISTS seed_ops (op_id TEXT PRIMARY KEY);
           INSERT OR IGNORE INTO seed_ops (op_id) SELECT id FROM package_ops;"
        cmd.ExecuteNonQuery() |> ignore<int>
      with e ->
        System.Console.Error.WriteLine(
          $"could not record which ops came from this build: {e.Message}"
        )

      let readmePath = Path.Combine(darklangDir, "README.md")
      extractResource "README.md" readmePath

      let logsDir = Path.Combine(darklangDir, "logs")
      Directory.CreateDirectory(logsDir) |> ignore

      // A store just written from this binary's own seed is by definition reconciled with it.
      recordStoreStamp dbPath LibConfig.Config.buildHash

      eprintfn "CLI data directory setup complete"
