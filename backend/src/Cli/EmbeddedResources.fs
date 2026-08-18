module Cli.EmbeddedResources

open System
open System.IO
open System.Reflection

// Resolve the running executable's directory. Assembly.Location returns "" in a single-file or AOT
// bundle, so AppContext.BaseDirectory is the AOT-clean replacement; ProcessPath is the fallback.
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
/// An explicit `DARK_CONFIG_RUNDIR` WINS and must keep winning over the default: overwrite it and every
/// process on a machine shares one store, so two instances on one box (a relay next to your own client)
/// become impossible. Pointing it at an empty directory gives a fresh instance.
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

/// Extract a resource that was gzip-compressed at build time. We ship `data.db.gz`
/// embedded and decompress it on first extract.
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



/// Copy the store aside before an upgrade touches it. Kept next to the store, one per calendar day:
/// three upgrades in an afternoon leave one copy, of the state before the day's first change.
///
/// Best-effort. A failed backup does not stop the upgrade.
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


/// Bindings this store holds that the embedded seed did not write, captured before an upgrade folds.
///
/// `locations.op_id` is the op the fold credited with each binding, so an op the seed does not carry is one
/// authored here or pulled from a peer. Upgrading must not silently take those back: the seed's version of a
/// name you edited is newer by stamp and would win LWW.
///
/// (owner, modules, name, item_type, item_hash, source). `source` separates a name you edited from one
/// that merely followed it through propagation, which matters only for what gets reported: one edit to
/// a core function repoints hundreds of callers.
let mutable locallyAuthored :
  List<string * string * string * string * string * string> = []


/// Top up an existing store with this binary's embedded package ops.
///
/// The seed is a SQLite database, so this attaches it and copies rows across rather than deserializing:
/// the op blobs are opaque here, and the fold afterwards is what gives them meaning. An op's id IS its
/// content hash, so `INSERT OR IGNORE` skips everything this build shares with the store, and what lands
/// is exactly what changed. The ops go in unapplied, the signal `Seed.growIfNeeded` looks for.
///
/// Failure is not fatal on purpose: a store that could not be topped up is no worse off than before.
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

        // Whether this seed has anything for this store, asked BEFORE touching it, so the backup
        // below is taken only when the store is actually about to change.
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

          // Captured while `seed` is still attached and before anything folds, because afterwards the
          // seed's own SetName may already have taken the name.
          //
          // Against the LEDGER, not against this seed: comparing against the current seed alone would
          // call the entire previous package set locally authored.
          //
          // `extract` runs before migrations, so on an older store this table does not exist yet and
          // every query against it is a startup crash. Declaring it here is the only place that can
          // be true of both a fresh store and one that predates the ledger.
          use ensure = conn.CreateCommand()
          ensure.CommandText <-
            "CREATE TABLE IF NOT EXISTS seed_ops (op_id TEXT PRIMARY KEY)"
          ensure.ExecuteNonQuery() |> ignore<int>

          use ledger = conn.CreateCommand()
          ledger.CommandText <- "SELECT COUNT(*) FROM seed_ops"
          let known = ledger.ExecuteScalar() |> string |> int

          use mine = conn.CreateCommand()

          if known = 0 then
            // First run on a store that predates the ledger: nothing here can say which ops came
            // from a build, so claim none rather than guess. Every upgrade after this has provenance.
            locallyAuthored <- []
          else
            mine.CommandText <-
              "SELECT owner, modules, name, item_type, item_hash, source
               FROM locations
               WHERE unlisted_at IS NULL
                 AND op_id NOT IN (SELECT op_id FROM seed_ops)"
            let held = ResizeArray()
            use r = mine.ExecuteReader()
            while r.Read() do
              held.Add(
                r.GetString 0,
                r.GetString 1,
                r.GetString 2,
                r.GetString 3,
                r.GetString 4,
                r.GetString 5)
            r.Close()
            locallyAuthored <- List.ofSeq held

          // Record what THIS seed carries, whichever branch ran above.
          use remember = conn.CreateCommand()
          remember.CommandText <-
            "ATTACH DATABASE $seed AS seed2;
             INSERT OR IGNORE INTO seed_ops (op_id) SELECT id FROM seed2.package_ops;
             DETACH DATABASE seed2;"
          remember.Parameters.AddWithValue("$seed", temp) |> ignore<obj>
          remember.ExecuteNonQuery() |> ignore<int>

        use cmd = conn.CreateCommand()
        // The seed's ops arrive COMMITTED, under its baseline commit, and have to stay that way:
        // dropping `commit_hash` leaves them in the draft, so the first `dark status` after an upgrade
        // reports thousands of items changed. Commit rows come first so the reference has a target.
        cmd.CommandText <-
          "ATTACH DATABASE $seed AS seed;
           INSERT OR IGNORE INTO commits (hash, message, author, origin_ts)
             SELECT hash, message, author, origin_ts FROM seed.commits;
           INSERT OR IGNORE INTO package_ops (id, op_blob, applied, effective, origin_ts, commit_hash)
             SELECT id, op_blob, 0, 1, origin_ts, commit_hash FROM seed.package_ops;
           -- An op already PRESENT but INERT has to be woken up, and `INSERT OR IGNORE` cannot do it.
           -- A relay stores what its clients push at `effective = 0`, and ops are content-addressed, so
           -- a client pushing its package tree lands the SAME ids this seed carries: the insert above
           -- skips them, the rows stay inert, and the binary dies with `FnNotFound` on its own router.
           -- Only ops the seed contains, and only inert ones, are touched, so a client op the seed
           -- knows nothing about stays hosted data.
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
/// before telemetry has an output path: it's what sets DARK_CONFIG_RUNDIR, where the log lives.
/// `Cli.Main` drains this once telemetry is up.
let timings : ResizeArray<string * int64> = ResizeArray()

let inline private timed (label : string) (f : unit -> 'a) : 'a =
  let t0 = System.Diagnostics.Stopwatch.GetTimestamp()
  let r = f ()
  timings.Add(label, System.Diagnostics.Stopwatch.GetTimestamp() - t0)
  r

let extract () : unit =
  // On first run, decompress the embedded seed db to `~/.darklang/data.db`; afterwards the
  // file exists and grow/init proceeds against the local copy.
  if timed "extract.hasResource" (fun () -> hasEmbeddedResource "data.db.gz") then
    let darklangDir = getDarklangDirectory ()

    Environment.SetEnvironmentVariable("DARK_CONFIG_RUNDIR", darklangDir)

    let dbPath = Path.Combine(darklangDir, "data.db")

    if not (File.Exists(dbPath)) then
      printfn $"Setting up Darklang CLI data directory at {darklangDir}"

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

      printfn "CLI data directory setup complete"
    // An existing store gets this binary's OWN package code topped up, then `growIfNeeded` folds it.
    // Without it, upgrading the binary would mean wiping the store: the new build pins package refs
    // by hash and the old store holds the previous build's hashes, with nothing to reconcile them.
    //
    // Topping up is additive: ops are content-addressed, so only genuinely-new ops are inserted. It
    // does not delete, it does not rebind by itself, and a store already current does no work. No
    // numbered migration is needed either, since `package_ops` is canonical and projections re-fold.
    else
      // The backup happens inside the top-up, once it knows there is something to top up.
      timed "extract.topUpStore" (fun () -> reseedFromEmbedded dbPath)
