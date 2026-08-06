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
/// The built-in choice, when nothing says otherwise.
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
/// An explicit `DARK_CONFIG_RUNDIR` WINS. It used to be overwritten unconditionally a few lines below, which
/// meant the shipped CLI silently ignored it and every process on a machine shared one store. That made two
/// instances on one box impossible -- a relay running next to your own client, say, which is the obvious
/// thing for a small team to do -- and it made the store-mismatch error elsewhere in the CLI recommend
/// exactly the variable that did nothing.
///
/// Pointing it at an empty directory gives you a fresh instance: the embedded seed extracts there like it
/// would on a new machine.
let private getDarklangDirectory () : string =
  match Environment.GetEnvironmentVariable "DARK_CONFIG_RUNDIR" with
  | null -> getDefaultDarklangDirectory ()
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



/// Top up an existing store with this binary's embedded package ops.
///
/// The seed is a SQLite database, so this attaches it and copies rows across rather than deserializing
/// anything: the op blobs are opaque here, and the fold that runs afterwards is what gives them meaning.
///
/// `INSERT OR IGNORE` is doing the real work. An op's id IS its content hash, so every definition this
/// build shares with the store collides and is skipped, and what lands is exactly what changed. The ops
/// go in unapplied, which is the signal `Seed.growIfNeeded` already looks for.
///
/// Failure here is not fatal on purpose. A store that could not be topped up is no worse off than before
/// this existed, and taking the CLI down over it would turn a recoverable upgrade into a brick.
/// Copy the store aside before an upgrade touches it.
///
/// Topping a store up is additive and does not delete, but it is still the moment a working store meets
/// code that has never seen it. A copy costs a few seconds and a few hundred MB, and it is the difference
/// between "the upgrade went strangely" being an inconvenience and being your work.
///
/// Kept next to the store rather than in a temp directory, because the point is that you can find it
/// without being told where to look. One per calendar day: upgrading three times in an afternoon should
/// not leave three copies, and the useful thing to roll back to is the state before today's changes.
///
/// Best-effort. If the copy fails the upgrade still proceeds, because refusing to start a CLI over a
/// failed BACKUP would be a worse outcome than the risk it is insuring against.
let private backupBeforeUpgrade (dbPath : string) : unit =
  try
    if File.Exists dbPath then
      let dir = Path.Combine(Path.GetDirectoryName(dbPath), "backups")
      Directory.CreateDirectory(dir) |> ignore<DirectoryInfo>

      let stamp = System.DateTime.UtcNow.ToString("yyyy-MM-dd")
      let target = Path.Combine(dir, $"data.db.before-upgrade-{stamp}")

      if not (File.Exists target) then
        File.Copy(dbPath, target)
        printfn $"Backed up your store to {target} before upgrading it."
  with e ->
    System.Console.Error.WriteLine($"could not back up the store: {e.Message}")


let private reseedFromEmbedded (dbPath : string) : unit =
  let temp =
    Path.Combine(Path.GetTempPath(), $"dark-seed-{System.Guid.NewGuid()}.db")

  try
    try
      extractGzippedResource "data.db.gz" temp

      if File.Exists temp then
        use conn =
          new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
        conn.Open()

        // Whether this seed has anything for this store, asked BEFORE touching it -- so the backup below
        // is taken only when the store is actually about to change. Copying the whole store on the way
        // past cost a fresh install an ~80MB copy on its first run, to protect it from an upgrade that
        // had nothing to do, under a message announcing an upgrade that was not happening.
        use probe = conn.CreateCommand()
        probe.CommandText <-
          "ATTACH DATABASE $seed AS seed;
           SELECT (SELECT COUNT(*) FROM seed.package_ops s
                     WHERE NOT EXISTS (SELECT 1 FROM package_ops o WHERE o.id = s.id))
                + (SELECT COUNT(*) FROM package_ops o
                     WHERE o.effective = 0 AND o.id IN (SELECT id FROM seed.package_ops));"
        probe.Parameters.AddWithValue("$seed", temp) |> ignore<obj>
        let pending = probe.ExecuteScalar() |> string |> int
        use detach = conn.CreateCommand()
        detach.CommandText <- "DETACH DATABASE seed;"
        detach.ExecuteNonQuery() |> ignore<int>

        if pending > 0 then
          backupBeforeUpgrade dbPath

        use cmd = conn.CreateCommand()
        // The seed's ops arrive COMMITTED, under its baseline commit, and they have to stay that way.
        // Dropping `commit_hash` leaves them in the draft, so the first `dark status` after an upgrade
        // reports several thousand items changed -- which is both alarming and false, since nobody
        // changed anything. The commit rows come across first so the reference has something to point at.
        cmd.CommandText <-
          "ATTACH DATABASE $seed AS seed;
           INSERT OR IGNORE INTO commits (hash, message, author, origin_ts)
             SELECT hash, message, author, origin_ts FROM seed.commits;
           INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts, commit_hash)
             SELECT id, op_blob, 0, 1, origin_ts, commit_hash FROM seed.package_ops;
           -- An op already PRESENT but INERT has to be woken up, and `INSERT OR IGNORE` cannot do it.
           --
           -- A relay stores what its clients push at `effective = 0`: hosted data, never folded into its
           -- own projections. Ops are content-addressed, so a client pushing its package tree lands the
           -- SAME ids this seed carries. The insert above then skips them as already-present, the rows
           -- stay inert, and the fold never applies them -- so a name binds to a hash whose content was
           -- never folded, and the binary dies with `FnNotFound` on its own router.
           --
           -- Found by taking a real relay down. Only ops the seed actually contains are touched, and only
           -- ones that are inert, so a client op the seed knows nothing about stays hosted data.
           UPDATE package_ops
             SET effective = 1, applied = 0
             WHERE effective = 0
               AND id IN (SELECT id FROM seed.package_ops);
           DETACH DATABASE seed;"
        cmd.Parameters.AddWithValue("$seed", temp) |> ignore<obj>
        cmd.ExecuteNonQuery() |> ignore<int>
    with e ->
      System.Console.Error.WriteLine(
        $"could not top up the package store: {e.Message}"
      )
  finally
    try
      if File.Exists temp then File.Delete temp
    with _ ->
      ()


/// Sub-timings for `extract`, in Stopwatch ticks. Collected rather than logged because `extract` runs
/// before telemetry has an output path: it's what sets DARK_CONFIG_RUNDIR, which is where the log lives.
/// `Cli.Main` drains this once telemetry is up.
///
/// Worth splitting out because on a shipped single-file binary this phase reads as "startup" when it's
/// really a manifest-resource enumeration plus a throwaway SQLite connection, which are different
/// things to fix.
let timings : ResizeArray<string * int64> = ResizeArray()

let inline private timed (label : string) (f : unit -> 'a) : 'a =
  let t0 = System.Diagnostics.Stopwatch.GetTimestamp()
  let r = f ()
  timings.Add(label, System.Diagnostics.Stopwatch.GetTimestamp() - t0)
  r

let extract () : unit =
  // The embedded resource is `data.db.gz` (the seed db, gzip-compressed at
  // build time to save ~7 MB on binary size). On first run, decompress to
  // `~/.darklang/data.db`; on subsequent runs the file already exists and
  // grow/init proceeds against the local copy.
  if timed "extract.hasResource" (fun () -> hasEmbeddedResource "data.db.gz") then
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
    // An existing store gets this binary's OWN package code topped up, then `growIfNeeded` folds it.
    //
    // Without this, upgrading the binary meant wiping the store: the new build pins package refs by hash,
    // the old store holds the previous build's hashes, and nothing reconciles them. Survivable on a laptop
    // (re-grow and carry on) and not survivable on a relay, which is a place people leave things -- and
    // under CI, where every deploy is an upgrade, it would wipe on every push.
    //
    // Topping up is additive and cheap to be wrong about: ops are content-addressed, so everything this
    // build shares with the store already dedups, and only genuinely-new ops are inserted. It does not
    // delete, it does not rebind by itself, and a store that is already current does no work.
    //
    // Upstream reconciles by RELEASE NUMBER here (`planCliUpgrade`: migrate in place, refuse a newer
    // store, or clean-break). That footing is gone on this branch: `package_ops` is canonical and every
    // projection re-folds, so an op-format change needs no numbered migration -- the ops are
    // content-addressed and the binary tops its own store up. Keeping upstream's `timed` wrapper, because
    // this runs before telemetry has an output path and its cost is worth seeing.
    else
      // The backup happens inside the top-up, once it knows there is something to top up.
      timed "extract.topUpStore" (fun () -> reseedFromEmbedded dbPath)
