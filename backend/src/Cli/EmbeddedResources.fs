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


// ── Release reconciliation on an EXISTING store ──────────────────────────────────────────────────────
// The CLI seeds a fresh store from the embedded db, but a store left over from a PREVIOUS CLI release is
// not otherwise migrated: the schema/op-format/hashing can differ, and the shipped CLI doesn't run the
// LocalExec migrator. So on startup we reconcile the store's Release stamp with this binary's
// `LibDB.Releases.currentRelease` before any LibDB connection is opened.

/// Read the store's Release stamp via a throwaway, NON-POOLED read-only connection, so LibDB's own (pooled)
/// shared connection is never opened here — a reseed moves the db file aside, and a pooled handle would keep
/// pointing at the moved inode. `Some r` = stamped; `None` = opened fine but no stamp (treated as a stale
/// store to re-seed). If the db can't be opened/read at all, hard-fail — a lock or corruption must not be
/// mistaken for "stale" and trigger a data-destroying reseed.
let private readStoredRelease (dbPath : string) : int option =
  try
    use conn =
      new Microsoft.Data.Sqlite.SqliteConnection(
        $"Data Source={dbPath};Mode=ReadOnly;Pooling=false"
      )
    conn.Open()
    use tableCmd = conn.CreateCommand()
    tableCmd.CommandText <-
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'release_state_v0'"
    match tableCmd.ExecuteScalar() with
    | null -> None
    | _ ->
      use cmd = conn.CreateCommand()
      cmd.CommandText <- "SELECT \"release\" FROM release_state_v0 WHERE id = 0"
      match cmd.ExecuteScalar() with
      | null -> None
      | v -> Some(Convert.ToInt32 v)
  with ex ->
    eprintfn $"Cannot open the Darklang data store at {dbPath}: {ex.Message}"
    eprintfn
      "Close any other Darklang processes and retry, or move that file aside to start fresh."
    exit 1

/// The peer URLs configured in a store, or [] if the table is absent / unreadable. Peers are sync CONFIG
/// (owned by `sync_peers_v0` in packages/darklang/sync.dark), not disposable package content.
let private readPeerUrls (path : string) : List<string> =
  try
    use conn =
      new Microsoft.Data.Sqlite.SqliteConnection(
        $"Data Source={path};Mode=ReadOnly;Pooling=false"
      )
    conn.Open()
    use cmd = conn.CreateCommand()
    cmd.CommandText <- "SELECT url FROM sync_peers_v0"
    use reader = cmd.ExecuteReader()
    [ while reader.Read() do
        yield reader.GetString 0 ]
  with _ ->
    []

/// Best-effort: carry the peer URLs from the backed-up store into the fresh one, so a clean-break upgrade
/// doesn't make you re-add your peers. Cursors are deliberately NOT carried — a clean break means the fresh
/// store must re-pull from scratch, so a stale cursor would skip ops. Returns how many peers were carried.
let private carryForwardPeers (backupPath : string) (dbPath : string) : int =
  let urls = readPeerUrls backupPath
  if List.isEmpty urls then
    0
  else
    try
      use conn =
        new Microsoft.Data.Sqlite.SqliteConnection(
          $"Data Source={dbPath};Pooling=false"
        )
      conn.Open()
      use create = conn.CreateCommand()
      create.CommandText <-
        "CREATE TABLE IF NOT EXISTS sync_peers_v0 (url TEXT PRIMARY KEY)"
      create.ExecuteNonQuery() |> ignore
      for url in urls do
        use ins = conn.CreateCommand()
        ins.CommandText <- "INSERT OR IGNORE INTO sync_peers_v0 (url) VALUES ($u)"
        ins.Parameters.AddWithValue("$u", url) |> ignore
        ins.ExecuteNonQuery() |> ignore
      List.length urls
    with _ ->
      0

/// Move the stale store (and its WAL/SHM sidecars, so a leftover WAL can't attach to the new db) aside to a
/// timestamped backup, then re-extract the embedded current-Release seed and carry the peer list forward.
/// Returns (backup path, number of peers carried).
let private reseedStore (dbPath : string) (storedLabel : string) : string * int =
  let stamp = DateTime.UtcNow.ToString("yyyyMMddHHmmss")
  let backup = $"{dbPath}.{storedLabel}-{stamp}.bak"
  File.Move(dbPath, backup)
  for suffix in [ "-wal"; "-shm" ] do
    let side = dbPath + suffix
    if File.Exists side then File.Move(side, backup + suffix)
  extractGzippedResource "data.db.gz" dbPath
  let peers = carryForwardPeers backup dbPath
  (backup, peers)

let private reseedAndReport
  (dbPath : string)
  (current : int)
  (storedLabel : string)
  : unit =
  let (backup, peers) = reseedStore dbPath storedLabel
  eprintfn
    $"Upgraded this data directory to Release {current} (was {storedLabel}); previous store backed up to {backup}."
  if peers > 0 then
    eprintfn
      $"Kept your {peers} peer connection(s) — run `dark sync` to re-pull and repopulate."
  else
    eprintfn "Re-connect and pull from your peers, or re-author, to repopulate it."

/// Reconcile an existing store's Release with this binary's. Called only when data.db already exists.
let private reconcileExistingStore (dbPath : string) : unit =
  let current = LibDB.Releases.currentRelease
  let stored = readStoredRelease dbPath
  let label =
    match stored with
    | Some r -> $"release{r}"
    | None -> "an untracked store"
  match LibDB.Releases.planCliUpgrade LibDB.Releases.releases stored current with
  | LibDB.Releases.CliUpgrade.Proceed -> () // up to date
  | LibDB.Releases.CliUpgrade.RefuseNewer r ->
    // A newer store must not be opened by older code (it could corrupt the newer format). Refuse.
    eprintfn
      $"This Darklang data directory is from a newer release (Release {r}); this binary is Release {current}."
    eprintfn
      $"Upgrade the CLI, or move {dbPath} aside to start fresh — refusing to open it with older code."
    exit 1
  | LibDB.Releases.CliUpgrade.MigrateInPlace ->
    // Every pending step is durable — migrate the store forward in place, PRESERVING the data (schema
    // copy-swap + op-format re-serialize + refold). No source needed, so the CLI can run it directly.
    LibDB.Releases.applyPending current
    eprintfn
      $"Upgraded this data directory from {label} to Release {current} (data preserved)."
  | LibDB.Releases.CliUpgrade.Reseed ->
    // A clean-break Release (content hashing changed) or an untracked store of unknown format: the old
    // package data can't be reused in place, so back it up and re-seed from the embedded current-Release
    // store.
    reseedAndReport dbPath current label

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
    else
      timed "extract.reconcileStore" (fun () -> reconcileExistingStore dbPath)
