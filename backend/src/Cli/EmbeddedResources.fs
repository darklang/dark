module Cli.EmbeddedResources

open System
open System.IO
open System.Reflection

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
///
/// One resource per file under `migrations/schema/`, concatenated in NAME order -- the same order
/// `LocalExec.Migrations` reads them from disk in, and the order the statements need (FK targets
/// before FK sources, across files as well as within one).
let embeddedSchema () : Option<string> =
  let assembly = Assembly.GetExecutingAssembly()

  let names =
    assembly.GetManifestResourceNames()
    |> Array.filter (fun n -> n.StartsWith("schema/") && n.EndsWith(".sql"))
    |> Array.sort

  if Array.isEmpty names then
    None
  else
    names
    |> Array.map (fun name ->
      use stream = assembly.GetManifestResourceStream(name)
      use reader = new StreamReader(stream)
      reader.ReadToEnd())
    |> String.concat "\n"
    |> Some


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
/// `LibDB.StoreBackups` takes it, so `dark backups` lists, restores and prunes it with the rest:
/// through SQLite's backup API, one per build it upgrades TO, an existing one kept.
let private backupBeforeUpgrade
  (dbPath : string)
  (build : string)
  : Result<string, string> =
  LibDB.StoreBackups.beforeUpgrade dbPath build
  |> Result.map (fun (target, takenNow) ->
    if takenNow then
      eprintfn $"Backed up your store to {target} before upgrading it."
    target)


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
  let rec acquire (said : bool) : System.IDisposable =
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
        eprintfn "Waiting for another dark to finish upgrading the store..."
      System.Threading.Thread.Sleep 100
      acquire true
  acquire false


/// (owner, modules, name, item_type, item_hash, source) of one binding in `locations`.
type HeldBinding = string * string * string * string * string * string


/// Top up an existing store with this binary's embedded package ops.
///
/// The seed is a SQLite database, so this attaches it and copies rows across rather than deserializing:
/// the op blobs are opaque here, and the fold afterwards is what gives them meaning. An op's id IS its
/// content hash, so `INSERT OR IGNORE` skips everything this build shares with the store, and what lands
/// is exactly what changed. The ops go in unapplied, the signal `Seed.growIfNeeded` looks for.
///
/// Failure is not fatal on purpose: a store that could not be topped up is no worse off than before. It
/// answers whether it worked, so a failure leaves the store unstamped and the next start tries again.
///
/// It also answers which bindings were locally authored (see `Extracted.locallyAuthored`), captured
/// before the top-up and kept even when the top-up itself then fails.
let private reseedFromEmbedded (dbPath : string) : bool * List<HeldBinding> =
  let temp =
    Path.Combine(Path.GetTempPath(), $"dark-seed-{System.Guid.NewGuid()}.db")

  let failed (e : exn) : bool =
    System.Console.Error.WriteLine(
      $"could not top up the package store: {e.Message}"
    )
    false

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
           SELECT COUNT(*) FROM seed.package_ops s
           WHERE NOT EXISTS
             (SELECT 1 FROM package_ops o WHERE o.id = s.id AND o.branch_id = s.branch_id);"
        probe.Parameters.AddWithValue("$seed", temp) |> ignore<obj>
        let pending = probe.ExecuteScalar() |> string |> int
        use detach = conn.CreateCommand()
        detach.CommandText <- "DETACH DATABASE seed;"
        detach.ExecuteNonQuery() |> ignore<int>

        // Captured while `seed` is still attached and before anything folds, because afterwards the
        // seed's own SetName may already have taken the name.
        //
        // Against the LEDGER, not against this seed: comparing against the current seed alone would
        // call the entire previous package set locally authored.
        //
        // `extract` runs before migrations, so on an older store this table does not exist yet and
        // every query against it is a startup crash. Declaring it here is the only place that can
        // be true of both a fresh store and one that predates the ledger.
        let held : List<HeldBinding> =
          if pending = 0 then
            []
          else
            use ensure = conn.CreateCommand()
            ensure.CommandText <-
              "CREATE TABLE IF NOT EXISTS seed_ops (op_id TEXT PRIMARY KEY)"
            ensure.ExecuteNonQuery() |> ignore<int>

            use ledger = conn.CreateCommand()
            ledger.CommandText <- "SELECT COUNT(*) FROM seed_ops"
            let known = ledger.ExecuteScalar() |> string |> int

            if known = 0 then
              // First run on a store that predates the ledger: nothing here can say which ops came
              // from a build, so claim none rather than guess. Every upgrade after this has provenance.
              []
            else
              use mine = conn.CreateCommand()
              mine.CommandText <-
                $"SELECT owner, modules, name, item_type, item_hash, source
                 FROM locations
                 WHERE live = 1 AND item_hash <> '' AND branch_id = '{string LibExecution.ProgramTypes.BranchId.Main}'
                   AND op_id NOT IN (SELECT op_id FROM seed_ops)"
              use r = mine.ExecuteReader()
              let rows =
                [ while r.Read() do
                    yield
                      (r.GetString 0,
                       r.GetString 1,
                       r.GetString 2,
                       r.GetString 3,
                       r.GetString 4,
                       r.GetString 5) ]
              r.Close()
              rows

        let toppedUp =
          try
            if pending > 0 then
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
               INSERT OR IGNORE INTO commits (hash, branch_id, message, origin_ts, parent, housekeeping)
                 SELECT hash, branch_id, message, origin_ts, parent, housekeeping FROM seed.commits;
               INSERT OR IGNORE INTO package_ops
                 (id, branch_id, authored_on, op_blob, op_kind, applied, origin_ts, commit_hash, source)
                 SELECT id, branch_id, authored_on, op_blob, op_kind, 0, origin_ts, commit_hash, source
                 FROM seed.package_ops;
               DETACH DATABASE seed;"
            cmd.Parameters.AddWithValue("$seed", temp) |> ignore<obj>
            cmd.ExecuteNonQuery() |> ignore<int>
            true
          with e ->
            failed e

        (toppedUp, held)
      else
        (true, [])
    with e ->
      (failed e, [])
  finally
    try
      if File.Exists temp then File.Delete temp
    with _ ->
      ()


/// What `extract` found that `Cli.Main` acts on later.
type Extracted =
  {
    /// Sub-timings for `extract`, in Stopwatch ticks. Returned rather than logged because `extract` runs
    /// before telemetry has an output path: it's what chooses the store directory, where the log lives.
    /// `Cli.Main` logs them once telemetry is up.
    timings : List<string * int64>

    /// Bindings this store holds that the embedded seed did not write, captured before an upgrade folds.
    ///
    /// `locations.op_id` is the op the fold credited with each binding, so an op the seed does not carry is one
    /// authored here or pulled from a peer. Upgrading must not silently take those back: the seed's version of a
    /// name you edited is newer by stamp and would win LWW.
    ///
    /// `source` separates a name you edited from one that merely followed it through propagation, which
    /// matters only for what gets reported: one edit to a core function repoints hundreds of callers.
    locallyAuthored : List<HeldBinding>
  }

let inline private timed (label : string) (f : unit -> 'a) : 'a * (string * int64) =
  let t0 = System.Diagnostics.Stopwatch.GetTimestamp()
  let r = f ()
  (r, (label, System.Diagnostics.Stopwatch.GetTimestamp() - t0))

/// Which build last reconciled this store with its own embedded seed.
///
/// `reseedFromEmbedded` decompresses the whole embedded store to a temp file and diffs its ops
/// against this one, to answer a question whose answer is almost always "nothing". On a
/// NativeAOT build, where there is no JIT to hide behind, that is most of what `dark` spends
/// before it does anything at all, on every command.
///
/// The seed is fixed per binary and the top-up only ever adds the binary's own ops, so a store
/// this same build has already topped up cannot need topping up again. Nothing external can
/// create that need.
///
/// The stamp lives IN the store rather than beside it, so it travels with the file: a store
/// copied elsewhere reads as unstamped, which is the safe answer.
let private stampTable =
  "CREATE TABLE IF NOT EXISTS store_stamp_v0 (id INTEGER PRIMARY KEY CHECK (id = 0), build TEXT NOT NULL)"

let private storeStamp (dbPath : string) : string option =
  try
    use conn = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
    conn.Open()
    use cmd = conn.CreateCommand()
    cmd.CommandText <- stampTable
    cmd.ExecuteNonQuery() |> ignore<int>
    use read = conn.CreateCommand()
    read.CommandText <- "SELECT build FROM store_stamp_v0 WHERE id = 0"
    match read.ExecuteScalar() with
    | null -> None
    | v -> Some(string v)
  with _ ->
    None // an unreadable store is not one we should claim is up to date

let private recordStoreStamp (dbPath : string) (build : string) : unit =
  try
    use conn = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
    conn.Open()
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      stampTable
      + "; INSERT OR REPLACE INTO store_stamp_v0 (id, build) VALUES (0, $build)"
    cmd.Parameters.AddWithValue("$build", build)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    cmd.ExecuteNonQuery() |> ignore<int>
  with _ ->
    () // failing to record it costs the next run the top-up it just did, nothing worse

let extract () : Extracted =
  // On first run, decompress the embedded seed db to `~/.darklang/data.db`; afterwards the
  // file exists and grow/init proceeds against the local copy.
  let hasResource, hasResourceTiming =
    timed "extract.hasResource" (fun () -> hasEmbeddedResource "data.db.gz")
  if not hasResource then
    { timings = [ hasResourceTiming ]; locallyAuthored = [] }
  else
    let darklangDir = LibConfig.StoreLocation.defaultDirectory ()

    // Straight to `LibConfig`, not through the environment: a variable set here is inherited
    // by every process this one starts, and another `dark` among them would open THIS store.
    LibConfig.StoreLocation.set darklangDir

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
    let upgradeTimings, locallyAuthored =
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

          let schemaTimings =
            try
              match embeddedSchema () with
              | Some sql ->
                let (_ : Option<string>), reset =
                  timed "extract.reset" (fun () ->
                    LibDB.Releases.resetIfOlderShape dbPath)
                let (), schema =
                  timed "extract.schema" (fun () ->
                    LibDB.Releases.applySchemaTables sql)
                let (), releases =
                  timed "extract.releases" (fun () -> LibDB.Releases.runPending ())
                let (), indexes =
                  timed "extract.indexes" (fun () ->
                    LibDB.Releases.applySchemaIndexes sql)
                [ reset; schema; releases; indexes ]
              | None -> []
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
          let (toppedUp, held), topUp =
            timed "extract.topUpStore" (fun () -> reseedFromEmbedded dbPath)
          if toppedUp && build <> "dev" then recordStoreStamp dbPath build
          (schemaTimings @ [ topUp ], held)
        else
          ([], [])
      else
        ([], [])

    // Only one process should create the store. After taking the lock, check again in case
    // another process finished setting it up while we were waiting.
    if not (File.Exists(dbPath)) then
      if not (Directory.Exists(darklangDir)) then
        Directory.CreateDirectory(darklangDir) |> ignore

      use _settingUp = upgradeLock dbPath

      if not (File.Exists(dbPath)) then
        eprintfn $"Setting up Darklang CLI data directory at {darklangDir}"

        // Extract to a temporary file, then move it into place. If setup is interrupted,
        // the next run can start over instead of opening an incomplete database.
        let partial = $"{dbPath}.partial-{System.Guid.NewGuid()}"
        try
          extractGzippedResource "data.db.gz" partial
          File.Move(partial, dbPath)
        finally
          try
            if File.Exists partial then File.Delete partial
          with _ ->
            ()

        // Record that all existing ops came from the embedded seed. Future upgrades use this
        // record to distinguish bundled ops from the user's own changes.
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

    { timings = hasResourceTiming :: upgradeTimings
      locallyAuthored = locallyAuthored }
