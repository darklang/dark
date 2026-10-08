/// Catching an existing store up to a newer release: what a newer build does to the store an older one
/// left, adding the release's package ops and keeping what the person authored. Two callers, one
/// implementation: the desktop CLI catches up from the package store embedded in its binary (its "seed",
/// `Cli/EmbeddedResources.fs`), and Dark in the browser catches up a store it restored from the
/// browser's own storage from the store it fetches (`Wasm/Host.fs`).
///
/// Not its settled shape: this is meant to become one stage of a single store-upgrade pipeline that
/// every host passes through, each with its own backup and failure policy (design:
/// `store-upgrade-map-2026-10-09.md`).
module LibDB.CatchUpStore

open System.IO
open System.Reflection

open Prelude


/// The schema embedded in <param assembly>, or None when it embedded none.
///
/// One resource per file under `migrations/schema/`, named `schema/<file>`, concatenated in NAME order
/// -- the same order `LocalExec.Migrations` reads them from disk in, and the order the statements need
/// (FK targets before FK sources, across files as well as within one).
let schemaFrom (assembly : Assembly) : Option<string> =
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


/// Which build last reconciled this store with its own embedded seed.
///
/// `reseedFromEmbedded` decompresses the whole embedded store to a temp file and diffs its ops
/// against this one, to answer a question whose answer is almost always "nothing". On a
/// NativeAOT build, where there is no JIT to hide behind, that is most of what `dark` spends
/// before it does anything at all, on every command.
///
/// The seed is fixed per binary and catching up only ever adds the binary's own ops, so a store
/// this same build has already caught up cannot need catching up again. Nothing external can
/// create that need.
///
/// The stamp lives IN the store rather than beside it, so it travels with the file: a store
/// copied elsewhere reads as unstamped, which is the safe answer.
let private stampTable =
  "CREATE TABLE IF NOT EXISTS store_stamp_v0 (id INTEGER PRIMARY KEY CHECK (id = 0), build TEXT NOT NULL)"

let storeStamp (dbPath : string) : string option =
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

let recordStoreStamp (dbPath : string) (build : string) : unit =
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
    () // failing to record it costs the next run the catch-up it just did, nothing worse


/// Bindings a store holds that no seed wrote, captured before an upgrade folds:
/// (owner, modules, name, item_type, item_hash, source).
///
/// `locations.op_id` is the op the fold credited with each binding, so an op no seed carried is one
/// authored here or pulled from a peer. An upgrade must not silently take those back: the seed's
/// version of a name you edited is newer by stamp and would win LWW, so `UpgradeKeep.restore` puts
/// them back after the fold. `source` separates a name you edited from one that merely followed it
/// through propagation, which matters only for what gets reported: one edit to a core function
/// repoints hundreds of callers.
type LocallyAuthored = List<string * string * string * string * string * string>

/// Catch the store at <param dbPath> up to the release whose package store is at <param releasePath>.
///
/// The seed is a SQLite database, so this attaches it and copies rows across rather than deserializing:
/// the op blobs are opaque here, and the fold afterwards is what gives them meaning. An op's id IS its
/// content hash, so `INSERT OR IGNORE` skips everything the two share, and what lands is exactly what
/// changed. The ops go in unapplied, the signal `Seed.growIfNeeded` looks for.
///
/// Takes no backup: a caller that wants one takes it before any step runs, since the release steps
/// change the store before this does. Raises on failure: whether a store that could not be caught up is
/// still usable is the caller's call, not this one's.
let fromRelease (dbPath : string) (releasePath : string) : LocallyAuthored =
  use conn = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
  conn.Open()

  // Whether this seed has anything for this store, asked BEFORE touching it, so the ledger
  // below is read and written only when the store is actually about to change.
  use probe = conn.CreateCommand()
  probe.CommandText <-
    "ATTACH DATABASE $seed AS seed;
     SELECT (SELECT COUNT(*) FROM seed.package_ops s
               WHERE NOT EXISTS (SELECT 1 FROM package_ops o WHERE o.id = s.id))
          + (SELECT COUNT(*) FROM package_ops o
               WHERE o.effective = 0 AND o.id IN (SELECT id FROM seed.package_ops));"
  probe.Parameters.AddWithValue("$seed", releasePath) |> ignore<obj>
  let pending = probe.ExecuteScalar() |> string |> int
  use detach = conn.CreateCommand()
  detach.CommandText <- "DETACH DATABASE seed;"
  detach.ExecuteNonQuery() |> ignore<int>

  let authored : LocallyAuthored =
    if pending = 0 then
      []
    else
      // Captured while `seed` is still attached and before anything folds, because afterwards the
      // seed's own SetName may already have taken the name.
      //
      // Against the LEDGER, not against this seed: comparing against the current seed alone would
      // call the entire previous package set locally authored.
      //
      // The CLI tops up before migrations (`extract`), so on an older store this table does not
      // exist yet and every query against it is a startup crash. Declaring it here is the only
      // place that can be true of both a fresh store and one that predates the ledger.
      use ensure = conn.CreateCommand()
      ensure.CommandText <-
        "CREATE TABLE IF NOT EXISTS seed_ops (op_id TEXT PRIMARY KEY)"
      ensure.ExecuteNonQuery() |> ignore<int>

      use ledger = conn.CreateCommand()
      ledger.CommandText <- "SELECT COUNT(*) FROM seed_ops"
      let known = ledger.ExecuteScalar() |> string |> int

      let held =
        if known = 0 then
          // First run on a store that predates the ledger: nothing here can say which ops came
          // from a build, so claim none rather than guess. Every upgrade after this has provenance.
          []
        else
          use mine = conn.CreateCommand()
          mine.CommandText <-
            "SELECT owner, modules, name, item_type, item_hash, source
             FROM locations
             WHERE unlisted_at IS NULL
               AND op_id NOT IN (SELECT op_id FROM seed_ops)"
          let rows = ResizeArray()
          use r = mine.ExecuteReader()
          while r.Read() do
            rows.Add(
              r.GetString 0,
              r.GetString 1,
              r.GetString 2,
              r.GetString 3,
              r.GetString 4,
              r.GetString 5
            )
          r.Close()
          List.ofSeq rows

      // Record what THIS seed carries, whichever branch ran above.
      use remember = conn.CreateCommand()
      remember.CommandText <-
        "ATTACH DATABASE $seed AS seed2;
         INSERT OR IGNORE INTO seed_ops (op_id) SELECT id FROM seed2.package_ops;
         DETACH DATABASE seed2;"
      remember.Parameters.AddWithValue("$seed", releasePath) |> ignore<obj>
      remember.ExecuteNonQuery() |> ignore<int>
      held

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
     -- Main runs it now, so no branch may still claim it: an effective op is never tagged (see
     -- `Branches.storeDeltaOpsStamped`). A review queue holding a peer's op that this build ships
     -- has nothing left to review for it, and a tag left behind hid the op from main's draft.
     DELETE FROM op_branches WHERE op_id IN (SELECT id FROM seed.package_ops);
     DETACH DATABASE seed;"
  cmd.Parameters.AddWithValue("$seed", releasePath) |> ignore<obj>
  cmd.ExecuteNonQuery() |> ignore<int>
  authored


/// Record every package op the store at <param dbPath> holds as a seed op, for a store that IS a
/// shipped seed and nothing else. Without it the first `fromRelease` finds an empty ledger and, rightly,
/// claims nothing as locally authored, which leaves an edit unprotected on that first upgrade.
let markAllAsShipped (dbPath : string) : unit =
  use conn = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "CREATE TABLE IF NOT EXISTS seed_ops (op_id TEXT PRIMARY KEY);
     INSERT OR IGNORE INTO seed_ops (op_id) SELECT id FROM package_ops;"
  cmd.ExecuteNonQuery() |> ignore<int>
