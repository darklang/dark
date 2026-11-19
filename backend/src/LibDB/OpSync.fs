/// Moving the op log from one store to another: the first piece of sync.
///
/// Three steps, each a file, so the transport is anything that can move a file (a copy, `scp`, a
/// shared directory):
///
///   1. `writeHave` on the RECEIVING store: which ops, commits and blobs it already holds.
///   2. `export` on the SENDING store, against that file: every row the receiver lacks, as a bundle.
///   3. `import` on the receiving store: append the bundle's rows.
///
/// Op ids are content hashes, so "already holds" is an exact question and importing a bundle twice
/// changes nothing the second time. An op is keyed by (id, branch, commit): an op the receiver holds
/// in its draft that the sender has since committed travels again, and the import moves the
/// receiver's copy out of its draft into that commit.
///
/// Receiving is APPENDING, then folding. The ops land past the fold's mark, and the ordinary fold
/// (`PackageOpPlayback`) applies them in the import's own transaction, so a catch-up is one commit.
/// There is no second write path for ops that came from somewhere else, and last-writer-wins by each
/// op's own stamp decides a name the two stores bound differently, exactly as it does on one
/// machine. A value arrives with what the sender evaluated it to, and a receiver of the same build
/// takes that; anything left unevaluated is evaluated at the next `Seed.growIfNeeded`.
///
/// Removals travel as entries of the sync journal (`sync_journal`, filled by triggers): a discarded
/// draft, a merge moving a branch's ops to its parent, an archive, a branch renamed. An anti-join
/// cannot see a row that is gone, so the journal is what says it went. Each entry says what one row
/// is now, and for each row the latest entry wins, by the entry's stamp and then its id, the same
/// way in every store. A bundle carries the entries the receiver lacks, and the import applies
/// those that are now the latest for their row. An op whose latest entry here says it is gone is
/// not taken back from a store that still holds it.
///
/// Once two stores are talking, `exportSince` sends only what changed after a `Cursor`: the rows the
/// journal entries past it name. The import ignores whatever it already holds, so a cursor that is
/// behind costs bytes, never correctness, and a bundle that holds nothing new is not written at all.
///
/// Conflict records do not travel: the import finds them. An incoming `SetName`, `Unbind` or `Rename`
/// that did not build on what this store holds there, while this store moved the name too, is the
/// same divergence the other store sees from its side, and both record it under the same
/// content-derived id (`ScmDetect.conflictId`). The decision that settles it is an op and travels
/// with the rest; one that arrives before the conflict it settles leaves the record settled, and the
/// import keeps it so. The name bases a merge or an archive removes stay local, and a replayed merge
/// moves rows; it does not run the merge's conflict detection again.
///
/// Each store remembers how far into each other store it holds everything (`sync_peers`), so a
/// session after the first starts from there (`receivedFrom`, `exportSince`) instead of comparing
/// whole stores, and journal entries every other store holds, once replaced, are dropped
/// (`recordAcked`).
module LibDB.OpSync


open Microsoft.Data.Sqlite

open Prelude

module PT = LibExecution.ProgramTypes


/// The shape of the have and bundle files. Each side refuses a file of another version.
let formatVersion = "6"

/// How far into a store another store has been sent: its journal's `seq` and its blobs' `rowid`.
/// Both only grow; blobs are never removed except by a purge, which empties the journal too.
type Cursor = { journal : int64; blobs : int64 }

/// How many rows a step moved, per kind. `committed` and `dropped` are the import's alone: ops it
/// moved out of the receiver's draft into a commit the sender made, and ops it removed because the
/// sender's journal says they went. `conflicts` are the ones the import recorded for the first time.
///
/// `upTo` is where the store stood when the step finished: for an export, how far the bundle
/// reaches, so the next `exportSince` starts there; for an import, the receiver's own journal after
/// the bundle's entries joined it. `journalBefore` is the receiver's journal before they did.
/// `sentUpTo` is the import's alone: how far into the SENDER the bundle reaches, as it says.
/// `folded` is how many ops the import folded, and `values` how many values took the sender's result.
type Counts =
  { ops : int64
    folded : int64
    values : int64
    committed : int64
    dropped : int64
    commits : int64
    branches : int64
    bases : int64
    blobs : int64
    journal : int64
    conflicts : List<ScmDetect.Conflict>
    journalBefore : int64
    upTo : Cursor
    sentUpTo : Cursor }

let private noCounts : Counts =
  { ops = 0L
    folded = 0L
    values = 0L
    committed = 0L
    dropped = 0L
    commits = 0L
    branches = 0L
    bases = 0L
    blobs = 0L
    journal = 0L
    conflicts = []
    journalBefore = 0L
    upTo = { Cursor.journal = 0L; blobs = 0L }
    sentUpTo = { Cursor.journal = 0L; blobs = 0L } }


/// A connection to the store at <param path>, which must exist: a mistyped path is an error, not a
/// new empty store.
///
/// `ReadWriteCreate` all the same, because a database ATTACHed to the connection opens with its
/// flags, and the have and bundle files are created that way.
let private openStore (path : string) : SqliteConnection =
  if not (System.IO.File.Exists path) then
    Exception.raiseInternal "no store at this path" [ "path", path ]
  let conn =
    new SqliteConnection($"Data Source={path};Mode=ReadWriteCreate;Pooling=false")
  conn.Open()
  use cmd = conn.CreateCommand()
  // As every other connection to the store (`Sqlite`): in WAL mode a commit then waits for no
  // fsync, which on a busy disk is most of what a small import costs.
  cmd.CommandText <- "PRAGMA busy_timeout=5000; PRAGMA synchronous=NORMAL;"
  let _ = cmd.ExecuteNonQuery()
  conn

let private run
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<string * obj>)
  : int64 =
  use cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  for (name, value) in ps do
    cmd.Parameters.AddWithValue(name, value) |> ignore<SqliteParameter>
  let n = cmd.ExecuteNonQuery()
  int64 n

let private scalar (conn : SqliteConnection) (sql : string) : string =
  use cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  let v = cmd.ExecuteScalar()
  match v with
  | null -> ""
  | :? System.DBNull -> ""
  | v -> string v

let private pairs (conn : SqliteConnection) (sql : string) : List<string * string> =
  use cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  use reader = cmd.ExecuteReader()
  Sqlite.readRows (fun r -> (r.GetString 0, r.GetString 1)) reader

let private count (conn : SqliteConnection) (table : string) : int64 =
  let n = scalar conn $"SELECT COUNT(*) FROM {table}"
  int64 n

/// None when the store on <param conn> keeps a sync journal; otherwise why not. A store that predates
/// it can say what it holds but not what it removed, and syncing with it would bring removed ops back.
let private checkJournal (conn : SqliteConnection) (path : string) : Option<string> =
  let has =
    scalar
      conn
      "SELECT COUNT(*) FROM main.sqlite_master WHERE type = 'table' AND name = 'sync_journal'"
  if has = "1" then None else Some $"the store at {path} keeps no sync journal"

/// A fresh file at <param path>: an earlier one there is replaced, never added to.
let private fresh (path : string) : unit =
  for suffix in [ ""; "-wal"; "-shm"; "-journal" ] do
    if System.IO.File.Exists(path + suffix) then
      System.IO.File.Delete(path + suffix)

/// Attach a have or bundle file. Each is scratch, written whole by one exchange and read back by it,
/// so it is written with no rollback journal and no fsync: on a busy disk the syncs alone took
/// seconds. The store itself is written as always.
let private attach (conn : SqliteConnection) (path : string) (alias : string) =
  let _ = run conn $"ATTACH DATABASE $path AS {alias}" [ "$path", box path ]
  let _ = run conn $"PRAGMA {alias}.journal_mode = OFF" []
  run conn $"PRAGMA {alias}.synchronous = OFF" []

/// What the store on <param conn> calls itself to the person on the other side: its
/// `instance.name`, else the machine's name.
let private label (conn : SqliteConnection) : string =
  let hasConfig =
    scalar
      conn
      "SELECT COUNT(*) FROM main.sqlite_master WHERE type = 'table' AND name = 'config_v0'"
  let named =
    if hasConfig = "1" then
      scalar conn "SELECT value FROM main.config_v0 WHERE key = 'instance.name'"
    else
      ""
  if named.Trim() = "" then System.Environment.MachineName else named

/// Who the store on <param conn> is to the stores it syncs with: its `instance.id`, else its name.
let private identity (conn : SqliteConnection) : string =
  let hasConfig =
    scalar
      conn
      "SELECT COUNT(*) FROM main.sqlite_master WHERE type = 'table' AND name = 'config_v0'"
  let id =
    if hasConfig = "1" then
      scalar conn "SELECT value FROM main.config_v0 WHERE key = 'instance.id'"
    else
      ""
  if id.Trim() = "" then label conn else id

let private writeMeta (conn : SqliteConnection) (alias : string) : unit =
  let _ =
    run
      conn
      $"CREATE TABLE {alias}.sync_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
      []
  let from = label conn
  let id = identity conn
  let _ =
    run
      conn
      $"INSERT INTO {alias}.sync_meta
        VALUES ('format', $v), ('from', $from), ('id', $id), ('build', $build)"
      [ "$v", box formatVersion
        "$from", box from
        "$id", box id
        "$build", box LibConfig.Config.buildHash ]
  ()

/// None when the attached file is one this build reads; otherwise why not.
let private checkMeta
  (conn : SqliteConnection)
  (alias : string)
  (what : string)
  : Option<string> =
  let hasMeta =
    scalar
      conn
      $"SELECT COUNT(*) FROM {alias}.sqlite_master WHERE name = 'sync_meta'"
  if hasMeta <> "1" then
    Some $"not a sync {what} file"
  else
    let version =
      scalar conn $"SELECT value FROM {alias}.sync_meta WHERE key = 'format'"
    if version = formatVersion then
      None
    else
      Some
        $"a sync {what} file in format {version}; this build reads format {formatVersion}"


/// Write what the store at <param storePath> holds to <param havePath>, for the other side's
/// `export`: each op's (id, branch, commit), each commit's hash and branch, the blob hashes, each
/// branch's name and marks, and the ids of the journal entries it holds. Reads the store only.
let writeHave (storePath : string) (havePath : string) : Result<unit, string> =
  use conn = openStore storePath
  match checkJournal conn storePath with
  | Some reason -> Error reason
  | None ->
    fresh havePath
    let _ = attach conn havePath "have"
    writeMeta conn "have"
    let _ =
      run
        conn
        "CREATE TABLE have.have_ops (
           id TEXT NOT NULL, branch_id TEXT NOT NULL, commit_hash TEXT,
           PRIMARY KEY (id, branch_id)) WITHOUT ROWID"
        []
    let _ =
      run
        conn
        "INSERT INTO have.have_ops SELECT id, branch_id, commit_hash FROM main.package_ops"
        []
    let _ =
      run
        conn
        "CREATE TABLE have.have_commits (
           hash TEXT PRIMARY KEY, branch_id TEXT NOT NULL) WITHOUT ROWID"
        []
    let _ =
      run
        conn
        "INSERT INTO have.have_commits SELECT hash, branch_id FROM main.commits"
        []
    let _ =
      run
        conn
        "CREATE TABLE have.have_branches (
           id TEXT PRIMARY KEY, name TEXT NOT NULL, merged_at TEXT, archived_at TEXT)
         WITHOUT ROWID"
        []
    let _ =
      run
        conn
        "INSERT INTO have.have_branches
         SELECT id, name, merged_at, archived_at FROM main.branches"
        []
    let _ =
      run
        conn
        "CREATE TABLE have.have_blobs (hash TEXT PRIMARY KEY) WITHOUT ROWID"
        []
    let _ =
      run conn "INSERT INTO have.have_blobs SELECT hash FROM main.package_blobs" []
    let _ =
      run
        conn
        "CREATE TABLE have.have_journal (id TEXT PRIMARY KEY) WITHOUT ROWID"
        []
    let _ =
      run conn "INSERT INTO have.have_journal SELECT id FROM main.sync_journal" []
    let _ = run conn "DETACH DATABASE have" []
    Ok()


/// Which rows a bundle carries: those a have file says the receiver lacks, or every row the journal
/// names past a cursor.
type private Wanted =
  | Against of havePath : string
  | Since of Cursor * peer : string

/// Whose drafts a bundle may carry. A server shared by several people sends each person their own
/// drafts and everyone's commits: a draft is work in progress, and two people's drafts of one name
/// would otherwise overwrite each other. A draft's owner is the person whose store it came from
/// (`sync_peers.person`); one this store made itself, or took from the seed, is everyone's.
type DraftScope =
  | AllDrafts
  | DraftsOf of person : string

/// A bundle of the rows <param wanted> picks from the store at <param storePath>, written to
/// <param bundlePath>, in one read transaction so it is one moment of the store. Reads the store only.
let private exportWith
  (storePath : string)
  (scope : DraftScope)
  (wanted : Wanted)
  (bundlePath : string)
  : Result<Counts, string> =
  use conn = openStore storePath
  let refused =
    match checkJournal conn storePath with
    | Some reason -> Some reason
    | None ->
      match wanted with
      | Against havePath ->
        let _ = attach conn havePath "have"
        checkMeta conn "have" "have"
      | Since _ -> None
  match refused with
  | Some reason -> Error reason
  | None ->
    fresh bundlePath
    let _ = attach conn bundlePath "bundle"
    // A bundle since a cursor is usually a few rows, and with SQLite's usual 4 KB pages its empty
    // tables alone came to 36 KB on every save; with 1 KB pages, 9 KB.
    match wanted with
    | Since _ ->
      let _ = run conn "PRAGMA bundle.page_size = 1024" []
      ()
    | Against _ -> ()
    let since =
      match wanted with
      | Since(cursor, _) -> cursor
      | Against _ -> { Cursor.journal = 0L; blobs = 0L }
    let peer =
      match wanted with
      | Since(_, peer) -> peer
      | Against _ -> ""
    let person =
      match scope with
      | AllDrafts -> ""
      | DraftsOf person -> person
    let ps =
      [ "$journal", box since.journal
        "$blobs", box since.blobs
        "$peer", box peer
        "$person", box person ]
    // A draft goes only to its owner's stores; anything committed goes to everyone.
    let othersStores =
      "SELECT id FROM main.sync_peers WHERE person <> '' AND person <> $person"
    let opVisible =
      $"($person = '' OR p.commit_hash IS NOT NULL OR NOT EXISTS (
          SELECT 1 FROM main.sync_journal o
          WHERE o.key = 'op ' || p.branch_id || ' ' || p.id
            AND o.came_from IN ({othersStores})))"
    let entryVisible =
      $"($person = '' OR j.kind NOT IN ('op', 'op-gone') OR j.commit_hash IS NOT NULL
         OR j.came_from NOT IN ({othersStores}))"
    // What each table sends: the rows the receiver lacks, or the rows the journal names past the
    // cursor. Ops and commits a later entry changed in place (a commit adopting a draft op, a merge
    // moving a commit) are named by that entry, so they go again.
    let opsFrom, commitsWhere, blobsWhere, journalWhere =
      match wanted with
      | Against _ ->
        "FROM main.package_ops p
         WHERE NOT EXISTS (
           SELECT 1 FROM have.have_ops h
           WHERE h.id = p.id AND h.branch_id = p.branch_id AND h.commit_hash IS p.commit_hash)",
        "NOT EXISTS (SELECT 1 FROM have.have_commits h WHERE h.hash = c.hash)",
        "NOT EXISTS (SELECT 1 FROM have.have_blobs h WHERE h.hash = b.hash)",
        // A fresh install holds the same seed rows under journal entries of its own, so an entry
        // goes only when it says something the receiver doesn't already hold, and only the latest
        // one for its row: on first contact, what a row is now is all that matters.
        "NOT EXISTS (SELECT 1 FROM have.have_journal h WHERE h.id = j.id)
         AND NOT EXISTS (
           SELECT 1 FROM main.sync_journal k
           WHERE k.key = j.key AND (k.at > j.at OR (k.at = j.at AND k.id > j.id)))
         AND NOT CASE j.kind
           WHEN 'op' THEN EXISTS (
             SELECT 1 FROM have.have_ops h
             WHERE h.id = j.op AND h.branch_id = j.branch AND h.commit_hash IS j.commit_hash)
           WHEN 'op-gone' THEN NOT EXISTS (
             SELECT 1 FROM have.have_ops h WHERE h.id = j.op AND h.branch_id = j.branch)
           WHEN 'commit' THEN EXISTS (
             SELECT 1 FROM have.have_commits h
             WHERE h.hash = j.commit_hash AND h.branch_id = j.branch)
           WHEN 'branch' THEN EXISTS (
             SELECT 1 FROM have.have_branches h
             WHERE h.id = j.branch AND h.name IS j.name AND h.merged_at IS j.merged_at
               AND h.archived_at IS j.archived_at)
           ELSE 0 END"
      // Nothing goes back to the store it came from.
      | Since _ ->
        "FROM (SELECT DISTINCT branch, op FROM main.sync_journal
               WHERE seq > $journal AND kind = 'op' AND came_from <> $peer) n
         JOIN main.package_ops p ON p.id = n.op AND p.branch_id = n.branch",
        "c.hash IN (SELECT commit_hash FROM main.sync_journal
                    WHERE seq > $journal AND commit_hash IS NOT NULL AND came_from <> $peer)",
        "b.rowid > $blobs",
        "j.seq > $journal AND j.came_from <> $peer"
    use tx = conn.BeginTransaction(deferred = true)
    writeMeta conn "bundle"
    let _ =
      run
        conn
        $"CREATE TABLE bundle.sync_ops AS
          SELECT p.rowid AS seq, p.id, p.branch_id, p.authored_on, p.op_blob, p.op_kind,
                 p.commit_hash, p.source, p.origin_ts
          {opsFrom}
          AND {opVisible}
          ORDER BY p.rowid"
        ps
    // What the values among them evaluated to here, so a receiver of the same build takes the
    // result rather than evaluating each again. Values evaluate with no effects allowed, so the
    // result is a function of the value and the build.
    let valueOps =
      use cmd = conn.CreateCommand()
      cmd.CommandText <-
        "SELECT id, op_blob FROM bundle.sync_ops WHERE op_kind = 'add-value'"
      use reader = cmd.ExecuteReader()
      Sqlite.readRows
        (fun r -> (System.Guid.Parse(r.GetString 0), r.GetFieldValue<byte[]>(1)))
        reader
    let valueHashes =
      valueOps
      |> List.choose (fun (id, blob) ->
        match
          LibSerialization.Binary.Serialization.PT.PackageOp.tryDeserialize id blob
        with
        | Some(PT.PackageOp.AddValue value) ->
          let (PT.Hash h) =
            match value.hash with
            | PT.Hash "" ->
              LibSerialization.Hashing.Hashing.computeValueHash
                LibSerialization.Hashing.Hashing.Normal
                value
            | h -> h
          Some h
        | _ -> None)
    let _ =
      run
        conn
        "CREATE TABLE bundle.sync_values AS
         SELECT hash, rt_dval, value_type FROM main.package_values
         WHERE rt_dval IS NOT NULL AND hash IN (SELECT value FROM json_each($hashes))"
        [ "$hashes", box (Sqlite.jsonStrings valueHashes) ]
    let _ =
      run
        conn
        $"CREATE TABLE bundle.sync_commits AS
          SELECT c.hash, c.branch_id, c.message, c.origin_ts, c.created_at, c.parent,
                 c.housekeeping
          FROM main.commits c
          WHERE {commitsWhere}"
        ps
    let _ =
      run
        conn
        "CREATE TABLE bundle.sync_branches AS
         SELECT id, name, parent_id, created_at FROM main.branches"
        []
    let _ =
      run
        conn
        "CREATE TABLE bundle.sync_bases AS
         SELECT branch_id, owner, modules, name, base_hash FROM main.branch_name_bases
         WHERE branch_id IN (SELECT DISTINCT branch_id FROM bundle.sync_ops)"
        []
    let _ =
      run
        conn
        $"CREATE TABLE bundle.sync_blobs AS
          SELECT b.hash, b.length, b.bytes FROM main.package_blobs b
          WHERE {blobsWhere}"
        ps
    let _ =
      run
        conn
        $"CREATE TABLE bundle.sync_journal AS
          SELECT j.seq, j.id, j.at, j.kind, j.key, j.branch, j.op, j.commit_hash, j.name,
                 j.merged_at, j.archived_at
          FROM main.sync_journal j
          WHERE {journalWhere} AND {entryVisible}
          ORDER BY j.seq"
        ps
    let upToJournal =
      scalar conn "SELECT COALESCE(MAX(seq), 0) FROM main.sync_journal"
    let upToBlobs =
      scalar conn "SELECT COALESCE(MAX(rowid), 0) FROM main.package_blobs"
    // How far into this store the bundle reaches, for the receiver to say where it stands with us.
    let _ =
      run
        conn
        "INSERT INTO bundle.sync_meta VALUES ('journal', $journal), ('blobs', $blobs)"
        [ "$journal", box upToJournal; "$blobs", box upToBlobs ]
    // `CREATE TABLE AS` reports no row count, so the counts are read back.
    let ops = count conn "bundle.sync_ops"
    let commits = count conn "bundle.sync_commits"
    let branches = count conn "bundle.sync_branches"
    let bases = count conn "bundle.sync_bases"
    let blobs = count conn "bundle.sync_blobs"
    let journal = count conn "bundle.sync_journal"
    tx.Commit()
    let _ = run conn "DETACH DATABASE bundle" []
    match wanted with
    | Against _ ->
      let _ = run conn "DETACH DATABASE have" []
      ()
    | Since _ -> ()
    Ok
      { noCounts with
          ops = ops
          commits = commits
          branches = branches
          bases = bases
          blobs = blobs
          journal = journal
          upTo = { Cursor.journal = int64 upToJournal; blobs = int64 upToBlobs } }

/// Write to <param bundlePath> every row of the store at <param storePath> that the store described
/// by <param havePath> lacks: ops (in the order this store logged them), commits, blobs and journal
/// entries (in the order this store learned them), plus the branch registry and the name bases of
/// every branch an exported op is on. Reads the store only. The answer's `upTo` is where the store
/// stood, for the `exportSince` after it.
let exportFor
  (scope : DraftScope)
  (storePath : string)
  (havePath : string)
  (bundlePath : string)
  : Result<Counts, string> =
  exportWith storePath scope (Against havePath) bundlePath

let export (storePath : string) (havePath : string) (bundlePath : string) =
  exportFor AllDrafts storePath havePath bundlePath

/// Write to <param bundlePath> what changed in the store at <param storePath> after <param cursor>,
/// for the store <param peer> (an `instance.id`): the journal entries past it, the ops and commits
/// they name, and the blobs added since, except what arrived from that store. A store that already
/// has some of it ignores that part, so this needs no have file and no round trip.
let exportSinceFor
  (scope : DraftScope)
  (storePath : string)
  (cursor : Cursor)
  (peer : string)
  (bundlePath : string)
  : Result<Counts, string> =
  exportWith storePath scope (Since(cursor, peer)) bundlePath

let exportSince
  (storePath : string)
  (cursor : Cursor)
  (peer : string)
  (bundlePath : string)
  =
  exportSinceFor AllDrafts storePath cursor peer bundlePath

/// What the store at <param storePath> calls itself to the other side: its `instance.name`, else the
/// machine's name.
let nameOf (storePath : string) : string =
  use conn = openStore storePath
  label conn

/// Who the store at <param storePath> is to the stores it syncs with: its `instance.id` (its name,
/// when it has none), and its name.
let identityOf (storePath : string) : string * string =
  use conn = openStore storePath
  let id = identity conn
  let name = label conn
  id, name

/// Where the store at <param storePath> stands now, for a peer that has just been sent everything.
let cursorOf (storePath : string) : Cursor =
  use conn = openStore storePath
  let journal = scalar conn "SELECT COALESCE(MAX(seq), 0) FROM main.sync_journal"
  let blobs = scalar conn "SELECT COALESCE(MAX(rowid), 0) FROM main.package_blobs"
  { Cursor.journal = int64 journal; blobs = int64 blobs }


/// One incoming change to a name, as an incoming `SetName`, `Unbind` or `Rename` says it. `hash` is
/// "" where the name ends, and `renamedTo` names where a rename took it.
type private Bound =
  {
    branch : string
    owner : string
    modules : string
    name : string
    /// "" when the op does not say: an `Unbind` names no kind.
    kind : string
    hash : string
    previous : Option<string>
    originTs : string
    renamedTo : string
  }

/// What this store holds at a name on a branch, nearest layer first. `hash` is "" where the name was
/// removed here, and `renamedTo` names where, when a rename removed it.
type private Held =
  { hash : string
    originTs : string
    previous : Option<string>
    kind : string
    renamedTo : string }

/// The conflicts in what arrived: for each name the bundle's new `SetName`, `Unbind` and `Rename`
/// ops change, the incoming line of versions, and what this store holds there on the same branch.
/// They diverged when this store's version is neither where the incoming line ends nor anywhere on
/// it: the other store never saw it, and this store moved the name while the other did. The other
/// store, receiving this one's ops, finds the same pair, so both record the same id.
///
/// Three shapes: both changed it (`same-name-different-hash`, or `independent-creation` when both
/// made it from nothing); this side changed it and that side removed or renamed it; or the other way
/// round. The last two are `removed-vs-edited`, under the id a merge gives the same pair.
///
/// Recorded pending on the branch, with the winner the fold will pick (`Lww`), and returned only
/// when new here. A record already settled, because the decision that settles it arrived first,
/// stays settled and only gains the details.
let private recordDivergences
  (conn : SqliteConnection)
  (localLabel : string)
  (incomingLabel : string)
  : List<ScmDetect.Conflict> =
  let raw =
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      "SELECT b.branch_id, b.id, b.op_blob, b.origin_ts FROM bundle.sync_ops b
       WHERE b.op_kind IN ('set-name', 'unbind', 'rename')
         AND NOT EXISTS (
           SELECT 1 FROM main.package_ops p WHERE p.id = b.id AND p.branch_id = b.branch_id)
         AND NOT EXISTS (
           SELECT 1 FROM temp.sync_latest l
           WHERE l.kind = 'op-gone' AND l.branch = b.branch_id AND l.op = b.id)
       ORDER BY b.seq"
    use reader = cmd.ExecuteReader()
    Sqlite.readRows
      (fun r ->
        (r.GetString 0,
         System.Guid.Parse(r.GetString 1),
         r.GetFieldValue<byte[]>(2),
         r.GetString 3))
      reader
  let bound
    (branch : string)
    (ts : string)
    (loc : PT.PackageLocation)
    (kind : string)
    (hash : string)
    (previous : Option<PT.Hash>)
    (renamedTo : string)
    : Bound =
    { branch = branch
      owner = loc.owner
      modules = String.concat "." loc.modules
      name = loc.name
      kind = kind
      hash = hash
      previous = previous |> Option.map (fun (PT.Hash h) -> h)
      originTs = ts
      renamedTo = renamedTo }
  let fqnOf (loc : PT.PackageLocation) =
    ScmDetect.fqn loc.owner (String.concat "." loc.modules) loc.name
  let incoming =
    raw
    |> List.collect (fun (branch, id, blob, ts) ->
      match
        LibSerialization.Binary.Serialization.PT.PackageOp.tryDeserialize id blob
      with
      | Some(PT.PackageOp.SetName(loc, target, previous)) ->
        let (PT.Hash hash) = target.hash
        [ bound branch ts loc (target.kind.toString ()) hash previous "" ]
      | Some(PT.PackageOp.Unbind(loc, previous)) ->
        [ bound branch ts loc "" "" previous "" ]
      | Some(PT.PackageOp.Rename(oldLocation, newLocation, target, _, _)) ->
        let (PT.Hash hash) = target.hash
        let kind = target.kind.toString ()
        [ bound
            branch
            ts
            oldLocation
            kind
            ""
            (Some target.hash)
            $"renamed to {fqnOf newLocation}"
          bound branch ts newLocation kind hash None "" ]
      | _ -> [])
  let lines =
    incoming
    |> List.groupBy (fun b -> (b.branch, b.owner, b.modules, b.name))
    |> Map.values
    |> List.ofSeq
    |> List.map (fun line ->
      line
      |> List.sortWith (fun a b ->
        match compare a.originTs b.originTs with
        | 0 -> System.String.CompareOrdinal(a.hash, b.hash)
        | c -> c))
  let localHeld (b : Bound) (kind : string) : Option<Held> =
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      "SELECT l.item_hash, COALESCE(l.origin_ts, ''), l.previous, l.item_type,
              COALESCE(p.op_kind, ''), p.op_blob, l.op_id
       FROM main.locations l
       LEFT JOIN main.package_ops p ON p.id = l.op_id AND p.branch_id = l.branch_id
       WHERE l.owner = $owner AND l.modules = $modules AND l.name = $name
         AND ($kind = '' OR l.item_type = $kind)
         AND l.branch_id = $branch AND l.live = 1
       ORDER BY l.draft DESC LIMIT 1"
    for (k, v) in
      [ "$owner", b.owner
        "$modules", b.modules
        "$name", b.name
        "$kind", kind
        "$branch", b.branch ] do
      cmd.Parameters.AddWithValue(k, v) |> ignore<SqliteParameter>
    use reader = cmd.ExecuteReader()
    Sqlite.readRows
      (fun r ->
        let renamedTo =
          if r.GetString 4 = "rename" && not (r.IsDBNull 5) then
            match
              LibSerialization.Binary.Serialization.PT.PackageOp.tryDeserialize
                (System.Guid.Parse(r.GetString 6))
                (r.GetFieldValue<byte[]>(5))
            with
            | Some(PT.PackageOp.Rename(_, newLocation, _, _, _)) ->
              $"renamed to {fqnOf newLocation}"
            | _ -> ""
          else
            ""
        { hash = r.GetString 0
          originTs = r.GetString 1
          previous = (if r.IsDBNull 2 then None else Some(r.GetString 2))
          kind = r.GetString 3
          renamedTo = renamedTo })
      reader
    |> List.tryHead
  let candidate side hash removed text originTs author : ScmDetect.Candidate =
    { side = side
      hash = hash
      removed = removed
      text = text
      originTs = originTs
      author = author }
  // One side ended the name (removed or renamed it) and the other moved it to <param editHash>.
  let removedVsEdited
    (last : Bound)
    (kind : string)
    (local : ScmDetect.Candidate)
    (incoming : ScmDetect.Candidate)
    : ScmDetect.Conflict =
    let ended, edited = if local.removed then local, incoming else incoming, local
    let removalWins =
      LibExecution.Lww.unbindBeatsBinding ended.originTs edited.originTs
    { id = ScmDetect.conflictId last.owner last.modules last.name [ edited.hash; "" ]
      owner = last.owner
      modules = last.modules
      name = last.name
      itemType = kind
      part = ""
      kind = "removed-vs-edited"
      candidates = [ local; incoming ]
      autoResolvedTo = if removalWins then "" else edited.hash
      reason =
        if not removalWins then "the edit is newer"
        elif ended.text <> "" then "the rename is newer"
        else "removal is newer"
      status = "pending"
      resolvedBy = "" }
  let divergence
    (line : List<Bound>)
    (held : Option<Held>)
    : Option<ScmDetect.Conflict> =
    let first = line.Head
    let last = (List.rev line).Head
    let onLine (h : string) =
      line |> List.exists (fun b -> b.hash = h || b.previous = Some h)
    let kind =
      line
      |> List.tryPick (fun b -> if b.kind = "" then None else Some b.kind)
      |> Option.orElse (held |> Option.map (fun h -> h.kind))
      |> Option.defaultValue ""
    match held with
    | None -> None
    | Some local when local.hash = last.hash -> None
    // The incoming line built on what this store holds.
    | Some local when local.hash <> "" && onLine local.hash -> None
    // Removed on both.
    | Some local when local.hash = "" && last.hash = "" -> None
    | Some local when local.hash = "" ->
      // Removed here, changed there: a conflict when the change could not have seen the removal.
      // It built on what this store removed, or it is older than the removal: the other store
      // stamps after everything it holds, so an edit made there after the removal arrived is
      // newer. A newer edit that names no predecessor could be either, and stands on both
      // stores; the other store, which still held the name, records that one.
      match local.previous with
      | Some removed when
        last.hash <> removed
        && (line |> List.exists (fun b -> b.previous = Some removed)
            || last.originTs < local.originTs)
        ->
        Some(
          removedVsEdited
            last
            kind
            (candidate "local" "" true local.renamedTo local.originTs localLabel)
            (candidate "incoming" last.hash false "" last.originTs incomingLabel)
        )
      | _ -> None
    | Some local when last.hash = "" ->
      // Changed here, removed there.
      Some(
        removedVsEdited
          last
          kind
          (candidate "local" local.hash false "" local.originTs localLabel)
          (candidate "incoming" "" true last.renamedTo last.originTs incomingLabel)
      )
    | Some local ->
      let incomingWins =
        LibExecution.Lww.incomingWins
          last.originTs
          last.hash
          local.originTs
          local.hash
      Some
        { ScmDetect.Conflict.id =
            ScmDetect.conflictId
              last.owner
              last.modules
              last.name
              [ local.hash; last.hash ]
          owner = last.owner
          modules = last.modules
          name = last.name
          itemType = kind
          part = ""
          kind =
            if first.previous = None && local.previous = None then
              "independent-creation"
            else
              "same-name-different-hash"
          candidates =
            [ candidate "local" local.hash false "" local.originTs localLabel
              candidate "incoming" last.hash false "" last.originTs incomingLabel ]
          autoResolvedTo = if incomingWins then last.hash else local.hash
          reason =
            if local.originTs = "" then "no-local-stamp"
            elif local.originTs = last.originTs then "tie-break-by-hash"
            else "later-origin-ts"
          status = "pending"
          resolvedBy = "" }
  let recorded = ResizeArray<ScmDetect.Conflict>()
  for line in lines do
    let last = (List.rev line).Head
    let lineKind =
      line
      |> List.tryPick (fun b -> if b.kind = "" then None else Some b.kind)
      |> Option.defaultValue ""
    let held = localHeld last lineKind
    match divergence line held with
    | None -> ()
    | Some c ->
      let ps =
        [ "$id", box c.id
          "$owner", box c.owner
          "$modules", box c.modules
          "$name", box c.name
          "$itemType", box c.itemType
          "$kind", box c.kind
          "$candidates", box (ScmDetect.candidatesJson c.candidates)
          "$resolvedTo", box c.autoResolvedTo
          "$reason", box c.reason
          "$branch", box last.branch ]
      let n =
        run
          conn
          "INSERT OR IGNORE INTO main.conflicts
             (id, owner, modules, name, item_type, part, kind, candidates, auto_resolved_to,
              reason, status, origin_ts, branch_id)
           VALUES ($id, $owner, $modules, $name, $itemType, '', $kind, $candidates, $resolvedTo,
                   $reason, 'pending', strftime('%Y-%m-%dT%H:%M:%fZ','now'), $branch)"
          ps
      if n > 0L then
        recorded.Add c
      else
        // Settled before this store saw what it settles (`PackageOpPlayback`'s decision fold
        // leaves such a record): it stays settled, and now says what the two versions were.
        let _ =
          run
            conn
            "UPDATE main.conflicts
             SET kind = $kind, item_type = $itemType, candidates = $candidates
             WHERE id = $id AND candidates = '[]' AND status <> 'pending'"
            ps
        ()
  List.ofSeq recorded


/// Fold every op on <param conn> that is not yet applied, on the caller's transaction, the way
/// `Seed.foldRead` does: each run of consecutive ops on one branch into that branch, in log order,
/// and the mark moved past them. An op this build cannot read stays unapplied, for a build
/// that can. Answers how many folded.
let private foldPending (conn : SqliteConnection) : int64 =
  let pending =
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      AppliedOps.pending "p.rowid AS seq, p.branch_id, p.id, p.op_blob"
      + " ORDER BY seq"
    use reader = cmd.ExecuteReader()
    Sqlite.readRows
      (fun r ->
        (r.GetInt64 0,
         r.GetString 1,
         System.Guid.Parse(r.GetString 2),
         r.GetFieldValue<byte[]>(3)))
      reader
  let readable =
    pending
    |> List.choose (fun (rowid, branch, id, blob) ->
      LibSerialization.Binary.Serialization.PT.PackageOp.tryDeserialize id blob
      |> Option.map (fun op -> (rowid, PT.BranchId.ParseUnsafe branch, op)))
  let runs =
    readable
    |> List.fold
      (fun (acc : List<PT.BranchId * List<PT.PackageOp>>) (_, branch, op) ->
        match acc with
        | (b, ops) :: rest when b = branch -> (b, op :: ops) :: rest
        | _ -> (branch, [ op ]) :: acc)
      []
    |> List.rev
    |> List.map (fun (b, ops) -> (b, List.rev ops))
  for (branch, ops) in runs do
    PackageOpPlayback.applyOpsOnConnection conn branch ops
  if not (List.isEmpty pending) then
    let readRows = readable |> List.map (fun (rowid, _, _) -> rowid) |> Set.ofList
    let skipped =
      pending
      |> List.map (fun (rowid, _, _, _) -> rowid)
      |> List.filter (fun rowid -> not (Set.contains rowid readRows))
    let _ =
      run
        conn
        AppliedOps.foldedThrough
        [ "$last",
          box (pending |> List.map (fun (rowid, _, _, _) -> rowid) |> List.max)
          "$skipped",
          box ("[" + (skipped |> List.map string |> String.concat ",") + "]") ]
    ()
  int64 (List.length readable)


/// Append the bundle at <param bundlePath> to the store at <param storePath>, and fold it, in one
/// transaction, and say what changed. Rows the store already holds are left as they are. The ops
/// fold as any op does (`PackageOpPlayback`), in the same transaction, so a catch-up of a thousand
/// ops is one commit, and the values among them take the result the sender evaluated when the two
/// run the same build; a value that came without one is evaluated by the next `Seed.growIfNeeded`.
/// A branch is registered by its id with the name and parent it was created with. The authoring clock moves past the newest stamp received, so what is written here next
/// sorts after it. A bundle that holds nothing this store lacks writes nothing, not even an empty
/// transaction.
///
/// The bundle's journal entries join this store's, keeping their ids and stamps, and every row whose
/// latest entry is now one of them is made to match it: an op gone from a branch is dropped with
/// what it wrote, a commit moves to the branch it names, a branch takes the name and marks it names.
/// The triggers are held off meanwhile, so none of this is journalled a second time. Names both
/// stores moved apart are recorded as conflicts (see `recordDivergences`).
/// Join each branch the bundle on <param conn> brings under an id this store doesn't know to this
/// store's live branch of the same name, if it has exactly one, and remember it. Then rewrite the
/// bundle so whatever it carries on a joined branch lands on this store's: by its new alias, or by
/// one an earlier first contact made.
let private joinBranches (conn : SqliteConnection) (firstContact : bool) : unit =
  if firstContact then
    let _ =
      run
        conn
        "INSERT OR IGNORE INTO main.sync_branch_aliases (foreign_id, local_id)
         SELECT b.id, (SELECT l.id FROM main.branches l
                       WHERE l.name = b.name AND l.merged_at IS NULL AND l.archived_at IS NULL)
         FROM bundle.sync_branches b
         WHERE NOT EXISTS (SELECT 1 FROM main.branches k WHERE k.id = b.id)
           AND (SELECT COUNT(*) FROM main.branches l
                WHERE l.name = b.name AND l.merged_at IS NULL AND l.archived_at IS NULL) = 1"
        []
    ()
  let aliased (column : string) =
    $"COALESCE((SELECT a.local_id FROM main.sync_branch_aliases a WHERE a.foreign_id = {column}), {column})"
  let branchId = aliased "branch_id"
  let parentId = aliased "parent_id"
  let branch = aliased "branch"
  for sql in
    [ $"UPDATE bundle.sync_ops SET branch_id = {branchId}"
      $"UPDATE bundle.sync_commits SET branch_id = {branchId}"
      $"UPDATE bundle.sync_bases SET branch_id = {branchId}"
      $"UPDATE bundle.sync_branches SET parent_id = {parentId}"
      "DELETE FROM bundle.sync_branches
       WHERE id IN (SELECT foreign_id FROM main.sync_branch_aliases)"
      $"UPDATE bundle.sync_journal SET
          key = CASE kind
            WHEN 'op' THEN 'op ' || {branch} || ' ' || op
            WHEN 'op-gone' THEN 'op ' || {branch} || ' ' || op
            WHEN 'branch' THEN 'branch ' || {branch}
            ELSE key END,
          branch = {branch}
        WHERE branch IN (SELECT foreign_id FROM main.sync_branch_aliases)" ] do
    let _ = run conn sql []
    ()

let private importWith
  (firstContact : bool)
  (storePath : string)
  (bundlePath : string)
  : Result<Counts, string> =
  use conn = openStore storePath
  let _ = attach conn bundlePath "bundle"
  let refused =
    match checkJournal conn storePath with
    | Some reason -> Some reason
    | None -> checkMeta conn "bundle" "bundle"
  match refused with
  | Some reason -> Error reason
  | None ->
    let sentJournal =
      scalar
        conn
        "SELECT COALESCE(MAX(value), 0) FROM bundle.sync_meta WHERE key = 'journal'"
    let sentBlobs =
      scalar
        conn
        "SELECT COALESCE(MAX(value), 0) FROM bundle.sync_meta WHERE key = 'blobs'"
    let sentUpTo = { Cursor.journal = int64 sentJournal; blobs = int64 sentBlobs }
    joinBranches conn firstContact
    // The import looks rows of the bundle up by key, once per row of the store or of the bundle.
    for index in
      [ "CREATE INDEX IF NOT EXISTS bundle.sync_ops_key ON sync_ops (id, branch_id)"
        "CREATE INDEX IF NOT EXISTS bundle.sync_journal_id ON sync_journal (id)"
        "CREATE INDEX IF NOT EXISTS bundle.sync_journal_key ON sync_journal (key)" ] do
      let _ = run conn index []
      ()
    let news =
      scalar
        conn
        "SELECT
           (SELECT COUNT(*) FROM bundle.sync_journal b
            WHERE NOT EXISTS (SELECT 1 FROM main.sync_journal j WHERE j.id = b.id))
         + (SELECT COUNT(*) FROM bundle.sync_ops b
            WHERE NOT EXISTS (
              SELECT 1 FROM main.package_ops p
              WHERE p.id = b.id AND p.branch_id = b.branch_id
                AND (b.commit_hash IS NULL OR p.commit_hash IS b.commit_hash)))
         + (SELECT COUNT(*) FROM bundle.sync_commits b
            WHERE NOT EXISTS (SELECT 1 FROM main.commits c WHERE c.hash = b.hash))
         + (SELECT COUNT(*) FROM bundle.sync_blobs b
            WHERE NOT EXISTS (SELECT 1 FROM main.package_blobs c WHERE c.hash = b.hash))
         + (SELECT COUNT(*) FROM bundle.sync_branches b
            WHERE NOT EXISTS (SELECT 1 FROM main.branches c WHERE c.id = b.id))
         + (SELECT COUNT(*) FROM bundle.sync_bases b
            WHERE NOT EXISTS (
              SELECT 1 FROM main.branch_name_bases c
              WHERE c.branch_id = b.branch_id AND c.owner = b.owner AND c.modules = b.modules
                AND c.name = b.name))"
    if news = "0" then
      let _ = run conn "DETACH DATABASE bundle" []
      let upTo = scalar conn "SELECT COALESCE(MAX(seq), 0) FROM main.sync_journal"
      let blobsUpTo =
        scalar conn "SELECT COALESCE(MAX(rowid), 0) FROM main.package_blobs"
      Ok
        { noCounts with
            journalBefore = int64 upTo
            upTo = { Cursor.journal = int64 upTo; blobs = int64 blobsUpTo }
            sentUpTo = sentUpTo }
    else
      let localLabel = label conn
      let incomingLabel =
        scalar
          conn
          "SELECT COALESCE(MAX(value), '') FROM bundle.sync_meta WHERE key = 'from'"
      use tx = conn.BeginTransaction()
      let journalBefore =
        scalar conn "SELECT COALESCE(MAX(seq), 0) FROM main.sync_journal"
      let _ = run conn "INSERT INTO main.sync_replaying (held) VALUES (1)" []

      let journal =
        run
          conn
          "INSERT OR IGNORE INTO main.sync_journal
             (id, at, kind, key, branch, op, commit_hash, name, merged_at, archived_at,
              came_from)
           SELECT id, at, kind, key, branch, op, commit_hash, name, merged_at, archived_at,
                  (SELECT COALESCE(MAX(value), '?') FROM bundle.sync_meta WHERE key = 'id')
           FROM bundle.sync_journal ORDER BY seq"
          []
      // The latest entry for every row the bundle names, by an entry or by an op, and whether it is
      // one of the bundle's. The ops' rows are here so that a removal this store made keeps a copy
      // held elsewhere from coming back.
      let _ =
        run
          conn
          "CREATE TEMP TABLE sync_latest AS
           SELECT j.kind, j.key, j.branch, j.op, j.commit_hash, j.name, j.merged_at,
                  j.archived_at,
                  EXISTS (SELECT 1 FROM bundle.sync_journal b WHERE b.id = j.id) AS sent
           FROM main.sync_journal j
           WHERE j.key IN (
               SELECT key FROM bundle.sync_journal
               UNION SELECT 'op ' || branch_id || ' ' || id FROM bundle.sync_ops)
             AND NOT EXISTS (
               SELECT 1 FROM main.sync_journal k
               WHERE k.key = j.key AND (k.at > j.at OR (k.at = j.at AND k.id > j.id)))"
          []
      let _ =
        run conn "CREATE INDEX temp.sync_latest_op ON sync_latest (op, branch)" []

      // Parents before children is not needed: a branch row names its parent by id only.
      let branches =
        run
          conn
          "INSERT OR IGNORE INTO main.branches (id, name, parent_id, created_at)
           SELECT id, name, parent_id, created_at FROM bundle.sync_branches"
          []
      let _ =
        run
          conn
          "UPDATE main.branches
           SET name = l.name, merged_at = l.merged_at, archived_at = l.archived_at
           FROM temp.sync_latest l
           WHERE l.kind = 'branch' AND l.sent AND l.branch = branches.id"
          []
      let commits =
        run
          conn
          "INSERT OR IGNORE INTO main.commits
             (hash, branch_id, message, origin_ts, created_at, parent, housekeeping)
           SELECT hash, branch_id, message, origin_ts, created_at, parent, housekeeping
           FROM bundle.sync_commits"
          []
      let _ =
        run
          conn
          "UPDATE main.commits SET branch_id = l.branch
           FROM temp.sync_latest l
           WHERE l.kind = 'commit' AND l.sent AND l.commit_hash = commits.hash
             AND commits.branch_id <> l.branch"
          []
      let bases =
        run
          conn
          "INSERT OR IGNORE INTO main.branch_name_bases (branch_id, owner, modules, name, base_hash)
           SELECT branch_id, owner, modules, name, base_hash FROM bundle.sync_bases"
          []
      let blobs =
        run
          conn
          "INSERT OR IGNORE INTO main.package_blobs (hash, length, bytes)
           SELECT hash, length, bytes FROM bundle.sync_blobs"
          []

      // Gone over there, so gone here: what each op wrote on its branch, then the op, as a local
      // discard drops them.
      let gone =
        pairs
          conn
          "SELECT l.branch, json_group_array(l.op)
           FROM temp.sync_latest l
           WHERE l.kind = 'op-gone'
             AND EXISTS (
               SELECT 1 FROM main.package_ops p WHERE p.id = l.op AND p.branch_id = l.branch)
           GROUP BY l.branch"
      let dropped =
        scalar
          conn
          "SELECT COUNT(*) FROM temp.sync_latest l
           WHERE l.kind = 'op-gone'
             AND EXISTS (
               SELECT 1 FROM main.package_ops p WHERE p.id = l.op AND p.branch_id = l.branch)"
      let ctx = PreparedBatch.newCtx conn
      try
        for (branch, ids) in gone do
          Layers.dropRowsOf
            ctx
            (PT.BranchId.Id(System.Guid.Parse branch))
            "op_id IN (SELECT value FROM json_each($ids))"
            (fun cmd -> PreparedBatch.p cmd "$ids" ids)
          let _ =
            run
              conn
              "DELETE FROM main.package_ops
               WHERE branch_id = $branch AND id IN (SELECT value FROM json_each($ids))"
              [ "$branch", box branch; "$ids", box ids ]
          ()
      finally
        PreparedBatch.disposeCtx ctx

      // A commit made over there of ops this store holds in its draft: the same commit here. The
      // fold already put those ops in the draft layer, so their rows move to the committed one,
      // as a local commit moves them.
      let adopted =
        pairs
          conn
          "SELECT DISTINCT b.branch_id, b.commit_hash
           FROM bundle.sync_ops b
           JOIN main.package_ops p ON p.id = b.id AND p.branch_id = b.branch_id
           WHERE p.commit_hash IS NULL AND b.commit_hash IS NOT NULL"
      // Driven by the bundle's rows, not the store's: the store's draft can be large.
      let committed =
        run
          conn
          "UPDATE main.package_ops SET commit_hash = b.commit_hash
           FROM bundle.sync_ops b
           WHERE b.id = package_ops.id AND b.branch_id = package_ops.branch_id
             AND package_ops.commit_hash IS NULL AND b.commit_hash IS NOT NULL"
          []
      let ctx = PreparedBatch.newCtx conn
      try
        for (branch, hash) in adopted do
          Layers.commitRowsOf
            ctx
            (PT.BranchId.Id(System.Guid.Parse branch))
            "op_id IN (SELECT id FROM package_ops WHERE branch_id = $branch AND commit_hash = $hash)"
            (fun cmd -> PreparedBatch.p cmd "$hash" hash)
      finally
        PreparedBatch.disposeCtx ctx

      // Before the ops land, while this store's bindings are still its own.
      let conflicts = recordDivergences conn localLabel incomingLabel

      // Every op the receiver lacks, except one whose latest entry says it is gone from its branch.
      let ops =
        run
          conn
          "INSERT OR IGNORE INTO main.package_ops
             (id, branch_id, authored_on, op_blob, op_kind, commit_hash, source,
              origin_ts)
           SELECT b.id, b.branch_id, b.authored_on, b.op_blob, b.op_kind, b.commit_hash, b.source,
                  b.origin_ts
           FROM bundle.sync_ops b
           WHERE NOT EXISTS (
             SELECT 1 FROM temp.sync_latest l
             WHERE l.kind = 'op-gone' AND l.branch = b.branch_id AND l.op = b.id)
           ORDER BY b.seq"
          []
      let newest = scalar conn "SELECT MAX(origin_ts) FROM bundle.sync_ops"
      if newest <> "" then OriginTs.observe newest
      let folded = foldPending conn
      // What the sender evaluated, for the values that just folded with none.
      let values =
        run
          conn
          "UPDATE main.package_values
           SET rt_dval = v.rt_dval, value_type = v.value_type
           FROM bundle.sync_values v
           WHERE package_values.hash = v.hash AND package_values.rt_dval IS NULL
             AND EXISTS (
               SELECT 1 FROM bundle.sync_meta WHERE key = 'build' AND value = $build)"
          [ "$build", box LibConfig.Config.buildHash ]
      let _ = run conn "DROP TABLE temp.sync_latest" []
      let _ = run conn "DELETE FROM main.sync_replaying" []
      let journalAfter =
        scalar conn "SELECT COALESCE(MAX(seq), 0) FROM main.sync_journal"
      let blobsAfter =
        scalar conn "SELECT COALESCE(MAX(rowid), 0) FROM main.package_blobs"
      tx.Commit()
      let _ = run conn "DETACH DATABASE bundle" []

      Caching.invalidateAll ()
      Ok
        { ops = ops
          folded = folded
          values = values
          committed = committed
          dropped = int64 dropped
          commits = commits
          branches = branches
          bases = bases
          blobs = blobs
          journal = journal
          conflicts = conflicts
          journalBefore = int64 journalBefore
          upTo = { Cursor.journal = int64 journalAfter; blobs = int64 blobsAfter }
          sentUpTo = sentUpTo }


// ------------------
// The stores this one syncs with
// ------------------

/// How far into the store <param peer> (an `instance.id`) the store at <param storePath> holds
/// everything, when it has ever been told: where the next session with it starts.
let receivedFrom (storePath : string) (peer : string) : Option<Cursor> =
  use conn = openStore storePath
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "SELECT received_journal, received_blobs FROM main.sync_peers
     WHERE id = $peer AND received_journal IS NOT NULL AND received_blobs IS NOT NULL"
  cmd.Parameters.AddWithValue("$peer", peer) |> ignore<SqliteParameter>
  use reader = cmd.ExecuteReader()
  Sqlite.readRows
    (fun r -> { Cursor.journal = r.GetInt64 0; blobs = r.GetInt64 1 })
    reader
  |> List.tryHead

/// Remember that the store at <param storePath> holds everything of <param peer>'s up to
/// <param upTo>.
let recordReceived
  (storePath : string)
  (peer : string)
  (name : string)
  (upTo : Cursor)
  : unit =
  use conn = openStore storePath
  let _ =
    run
      conn
      "INSERT INTO main.sync_peers (id, name, received_journal, received_blobs, seen_at)
       VALUES ($peer, $name, $journal, $blobs, strftime('%Y-%m-%dT%H:%M:%fZ','now'))
       ON CONFLICT(id) DO UPDATE SET
         name = excluded.name,
         received_journal = excluded.received_journal,
         received_blobs = excluded.received_blobs,
         seen_at = excluded.seen_at"
      [ "$peer", box peer
        "$name", box name
        "$journal", box upTo.journal
        "$blobs", box upTo.blobs ]
  ()

/// Whether <param cursor> is a point the store at <param storePath> has reached. One it has not
/// was handed out by another store under this one's name, and a session cannot start from it. The
/// journal numbers its entries without ever reusing a number, through a purge too, so a point it
/// handed out stays a point in it.
let reaches (storePath : string) (cursor : Cursor) : bool =
  use conn = openStore storePath
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "SELECT COALESCE(
              (SELECT seq FROM main.sqlite_sequence WHERE name = 'sync_journal'), 0) >= $journal
        AND (SELECT COALESCE(MAX(rowid), 0) FROM main.package_blobs) >= $blobs"
  cmd.Parameters.AddWithValue("$journal", cursor.journal)
  |> ignore<SqliteParameter>
  cmd.Parameters.AddWithValue("$blobs", cursor.blobs) |> ignore<SqliteParameter>
  let v = cmd.ExecuteScalar()
  string v = "1"

/// Remember that <param peer> holds everything of the store at <param storePath>'s journal up to
/// <param journal>. With <param trim>, also drop each entry that a later entry for the same row
/// replaced, and that every store this one has heard from in the last `sync.keepDays` days (7 unless
/// set) holds; answers how many went. A store that comes back later still gets the entry that
/// replaced it, which is the one that counts.
///
/// The latest entry for a row always stays: it is what says a removed op is removed when a copy of
/// it comes back from a store that has not heard. So the journal shrinks to one entry a row, and the
/// churn above that (a draft refreshed a hundred times, an edit undone) goes.
/// The most entries one trim drops. A trim runs between a session's steps, and on a busy machine
/// tens of thousands at once held the session for seconds; the rest go at the next acknowledgement.
let trimBatch = 2000

let recordAcked
  (storePath : string)
  (peer : string)
  (name : string)
  (journal : int64)
  (trim : bool)
  : int64 =
  use conn = openStore storePath
  let _ =
    run
      conn
      "INSERT INTO main.sync_peers (id, name, acked_journal, seen_at)
       VALUES ($peer, $name, $journal, strftime('%Y-%m-%dT%H:%M:%fZ','now'))
       ON CONFLICT(id) DO UPDATE SET
         name = excluded.name,
         acked_journal = MAX(acked_journal, excluded.acked_journal),
         seen_at = excluded.seen_at"
      [ "$peer", box peer; "$name", box name; "$journal", box journal ]
  if trim then
    run
      conn
      ("DELETE FROM main.sync_journal WHERE seq IN (
       SELECT seq FROM main.sync_journal
       WHERE seq <= (SELECT MIN(acked_journal) FROM main.sync_peers
                     WHERE seen_at > strftime('%Y-%m-%dT%H:%M:%fZ', 'now',
                       '-' || COALESCE(
                         (SELECT value FROM main.config_v0 WHERE key = 'sync.keepDays'),
                         '7') || ' days'))
         AND EXISTS (
           SELECT 1 FROM main.sync_journal k
           WHERE k.key = sync_journal.key
             AND (k.at > sync_journal.at OR (k.at = sync_journal.at AND k.id > sync_journal.id)))
       ORDER BY seq LIMIT "
       + string trimBatch
       + ")")
      []
  else
    0L

/// How many changes made in the store at <param storePath> the store <param peer> has not said it
/// holds. An op that only adds content goes with the binding that names it, so it is not counted
/// apart: one saved function is one change.
let notYetAcked (storePath : string) (peer : string) : int64 =
  use conn = openStore storePath
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "SELECT COUNT(*) FROM main.sync_journal j
     WHERE j.came_from = ''
       AND j.seq > COALESCE((SELECT acked_journal FROM main.sync_peers WHERE id = $peer), 0)
       AND NOT (j.kind = 'op' AND EXISTS (
         SELECT 1 FROM main.package_ops p
         WHERE p.id = j.op AND p.branch_id = j.branch AND p.op_kind LIKE 'add-%'))"
  cmd.Parameters.AddWithValue("$peer", peer) |> ignore<SqliteParameter>
  let n = cmd.ExecuteScalar()
  unbox<int64> n

/// On a server that knows people by their tokens: record that the store <param peer> is
/// <param person>'s. A store belongs to the first person it syncs as, and is refused to anyone
/// else, because its drafts go only to that person.
let bindPerson
  (storePath : string)
  (peer : string)
  (name : string)
  (person : string)
  : Result<unit, string> =
  use conn = openStore storePath
  let _ =
    run
      conn
      "INSERT INTO main.sync_peers (id, name, seen_at, person)
       VALUES ($peer, $name, strftime('%Y-%m-%dT%H:%M:%fZ','now'), $person)
       ON CONFLICT(id) DO UPDATE SET
         person = CASE WHEN person = '' THEN excluded.person ELSE person END"
      [ "$peer", box peer; "$name", box name; "$person", box person ]
  use cmd = conn.CreateCommand()
  cmd.CommandText <- "SELECT person FROM main.sync_peers WHERE id = $peer"
  cmd.Parameters.AddWithValue("$peer", peer) |> ignore<SqliteParameter>
  match string (cmd.ExecuteScalar()) with
  | owner when owner = person -> Ok()
  | owner -> Error owner

/// The newest stamp an op or a commit in the bundle at <param bundlePath> carries, or "" when it
/// carries none.
let newestStampIn (bundlePath : string) : string =
  use conn = openStore bundlePath
  scalar
    conn
    "SELECT COALESCE(MAX(stamp), '') FROM (
       SELECT MAX(origin_ts) AS stamp FROM main.sync_ops
       UNION ALL SELECT MAX(origin_ts) AS stamp FROM main.sync_commits)"

let import (storePath : string) (bundlePath : string) : Result<Counts, string> =
  importWith false storePath bundlePath

/// `import`, for the first bundle two stores exchange: a branch it brings that has the name of one
/// of this store's live branches is joined to it, and stays joined (`sync_branch_aliases`).
let importFirst (storePath : string) (bundlePath : string) : Result<Counts, string> =
  importWith true storePath bundlePath
