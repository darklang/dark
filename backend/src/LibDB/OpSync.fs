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
/// Receiving is APPENDING. The ops land past the fold's mark, and the ordinary fold applies them, the
/// same one that applies ops another process wrote (`Seed.applyUnappliedOps`, or `Seed.growIfNeeded`
/// at the next start, which also evaluates their values). There is no second write path for ops
/// that came from somewhere else, and last-writer-wins by each op's own stamp decides a name the two
/// stores bound differently, exactly as it does on one machine.
///
/// Removals travel as entries of the sync journal (`sync_journal`, filled by triggers): a discarded
/// draft, a merge moving a branch's ops to its parent, an archive, a branch renamed. An anti-join
/// cannot see a row that is gone, so the journal is what says it went. Each entry says what one row
/// is now, and for each row the latest entry wins, by the entry's stamp and then its id, the same
/// way in every store. A bundle carries the entries the receiver lacks, and the import applies
/// those that are now the latest for their row. An op whose latest entry here says it is gone is
/// not taken back from a store that still holds it.
///
/// What does not travel: conflict records stay local (the decisions that settle them are ops, and
/// travel with the rest), and so do the name bases a merge or an archive removes. A replayed merge
/// moves rows; it does not run the merge's conflict detection again.
module LibDB.OpSync

open System.Threading.Tasks
open FSharp.Control.Tasks

open Microsoft.Data.Sqlite

open Prelude

module PT = LibExecution.ProgramTypes


/// The shape of the have and bundle files. Each side refuses a file of another version.
let formatVersion = "2"

/// How many rows a step moved, per kind. `committed` and `dropped` are the import's alone: ops it
/// moved out of the receiver's draft into a commit the sender made, and ops it removed because the
/// sender's journal says they went.
type Counts =
  { ops : int64
    committed : int64
    dropped : int64
    commits : int64
    branches : int64
    bases : int64
    blobs : int64
    journal : int64 }


/// A connection to the store at <param path>, which must exist: a mistyped path is an error, not a
/// new empty store.
///
/// `ReadWriteCreate` all the same, because a database ATTACHed to the connection opens with its
/// flags, and the have and bundle files are created that way.
let private openStore (path : string) : Task<SqliteConnection> =
  task {
    if not (System.IO.File.Exists path) then
      Exception.raiseInternal "no store at this path" [ "path", path ]
    let conn =
      new SqliteConnection($"Data Source={path};Mode=ReadWriteCreate;Pooling=false")
    do! conn.OpenAsync()
    use cmd = conn.CreateCommand()
    cmd.CommandText <- "PRAGMA busy_timeout=5000;"
    let! _ = cmd.ExecuteNonQueryAsync()
    return conn
  }

let private run
  (conn : SqliteConnection)
  (sql : string)
  (ps : List<string * obj>)
  : Task<int64> =
  task {
    use cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    for (name, value) in ps do
      cmd.Parameters.AddWithValue(name, value) |> ignore<SqliteParameter>
    let! n = cmd.ExecuteNonQueryAsync()
    return int64 n
  }

let private scalar (conn : SqliteConnection) (sql : string) : Task<string> =
  task {
    use cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    let! v = cmd.ExecuteScalarAsync()
    return
      match v with
      | null -> ""
      | :? System.DBNull -> ""
      | v -> string v
  }

let private pairs
  (conn : SqliteConnection)
  (sql : string)
  : Task<List<string * string>> =
  task {
    use cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    use! reader = cmd.ExecuteReaderAsync()
    return Sqlite.readRows (fun r -> (r.GetString 0, r.GetString 1)) reader
  }

let private count (conn : SqliteConnection) (table : string) : Task<int64> =
  task {
    let! n = scalar conn $"SELECT COUNT(*) FROM {table}"
    return int64 n
  }

/// None when the store on <param conn> keeps a sync journal; otherwise why not. A store that predates
/// it can say what it holds but not what it removed, and syncing with it would bring removed ops back.
let private checkJournal
  (conn : SqliteConnection)
  (path : string)
  : Task<Option<string>> =
  task {
    let! has =
      scalar
        conn
        "SELECT COUNT(*) FROM main.sqlite_master WHERE type = 'table' AND name = 'sync_journal'"
    if has = "1" then
      return None
    else
      return Some $"the store at {path} keeps no sync journal"
  }

/// A fresh file at <param path>: an earlier one there is replaced, never added to.
let private fresh (path : string) : unit =
  for suffix in [ ""; "-wal"; "-shm"; "-journal" ] do
    if System.IO.File.Exists(path + suffix) then
      System.IO.File.Delete(path + suffix)

let private attach (conn : SqliteConnection) (path : string) (alias : string) =
  run conn $"ATTACH DATABASE $path AS {alias}" [ "$path", box path ]

let private writeMeta (conn : SqliteConnection) (alias : string) : Task<unit> =
  task {
    let! _ =
      run
        conn
        $"CREATE TABLE {alias}.sync_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
        []
    let! _ =
      run
        conn
        $"INSERT INTO {alias}.sync_meta VALUES ('format', $v)"
        [ "$v", box formatVersion ]
    return ()
  }

/// None when the attached file is one this build reads; otherwise why not.
let private checkMeta
  (conn : SqliteConnection)
  (alias : string)
  (what : string)
  : Task<Option<string>> =
  task {
    let! hasMeta =
      scalar
        conn
        $"SELECT COUNT(*) FROM {alias}.sqlite_master WHERE name = 'sync_meta'"
    if hasMeta <> "1" then
      return Some $"not a sync {what} file"
    else
      let! version =
        scalar conn $"SELECT value FROM {alias}.sync_meta WHERE key = 'format'"
      if version = formatVersion then
        return None
      else
        return
          Some
            $"a sync {what} file in format {version}; this build reads format {formatVersion}"
  }


/// Write what the store at <param storePath> holds to <param havePath>, for the other side's
/// `export`: each op's (id, branch, commit), the commit and blob hashes, and the ids of the journal
/// entries it holds. Reads the store only.
let writeHave (storePath : string) (havePath : string) : Task<Result<unit, string>> =
  task {
    use! conn = openStore storePath
    match! checkJournal conn storePath with
    | Some reason -> return Error reason
    | None ->
      fresh havePath
      let! _ = attach conn havePath "have"
      do! writeMeta conn "have"
      let! _ =
        run
          conn
          "CREATE TABLE have.have_ops (
             id TEXT NOT NULL, branch_id TEXT NOT NULL, commit_hash TEXT,
             PRIMARY KEY (id, branch_id)) WITHOUT ROWID"
          []
      let! _ =
        run
          conn
          "INSERT INTO have.have_ops SELECT id, branch_id, commit_hash FROM main.package_ops"
          []
      let! _ =
        run
          conn
          "CREATE TABLE have.have_commits (hash TEXT PRIMARY KEY) WITHOUT ROWID"
          []
      let! _ =
        run conn "INSERT INTO have.have_commits SELECT hash FROM main.commits" []
      let! _ =
        run
          conn
          "CREATE TABLE have.have_blobs (hash TEXT PRIMARY KEY) WITHOUT ROWID"
          []
      let! _ =
        run conn "INSERT INTO have.have_blobs SELECT hash FROM main.package_blobs" []
      let! _ =
        run
          conn
          "CREATE TABLE have.have_journal (id TEXT PRIMARY KEY) WITHOUT ROWID"
          []
      let! _ =
        run conn "INSERT INTO have.have_journal SELECT id FROM main.sync_journal" []
      let! _ = run conn "DETACH DATABASE have" []
      return Ok()
  }


/// Write to <param bundlePath> every row of the store at <param storePath> that the store described
/// by <param havePath> lacks: ops (in the order this store logged them), commits, blobs and journal
/// entries (in the order this store learned them), plus the branch registry and the name bases of
/// every branch an exported op is on. Reads the store only.
let export
  (storePath : string)
  (havePath : string)
  (bundlePath : string)
  : Task<Result<Counts, string>> =
  task {
    use! conn = openStore storePath
    let! _ = attach conn havePath "have"
    let! refused =
      task {
        match! checkJournal conn storePath with
        | Some reason -> return Some reason
        | None -> return! checkMeta conn "have" "have"
      }
    match refused with
    | Some reason -> return Error reason
    | None ->
      fresh bundlePath
      let! _ = attach conn bundlePath "bundle"
      // One read transaction, so the bundle is one moment of the store.
      use tx = conn.BeginTransaction(deferred = true)
      do! writeMeta conn "bundle"
      let! _ =
        run
          conn
          "CREATE TABLE bundle.sync_ops AS
           SELECT p.rowid AS seq, p.id, p.branch_id, p.authored_on, p.op_blob, p.op_kind,
                  p.commit_hash, p.source, p.origin_ts
           FROM main.package_ops p
           WHERE NOT EXISTS (
             SELECT 1 FROM have.have_ops h
             WHERE h.id = p.id AND h.branch_id = p.branch_id AND h.commit_hash IS p.commit_hash)
           ORDER BY p.rowid"
          []
      let! _ =
        run
          conn
          "CREATE TABLE bundle.sync_commits AS
           SELECT c.hash, c.branch_id, c.message, c.origin_ts, c.created_at, c.parent,
                  c.housekeeping
           FROM main.commits c
           WHERE NOT EXISTS (SELECT 1 FROM have.have_commits h WHERE h.hash = c.hash)"
          []
      let! _ =
        run
          conn
          "CREATE TABLE bundle.sync_branches AS
           SELECT id, name, parent_id, created_at FROM main.branches"
          []
      let! _ =
        run
          conn
          "CREATE TABLE bundle.sync_bases AS
           SELECT branch_id, owner, modules, name, base_hash FROM main.branch_name_bases
           WHERE branch_id IN (SELECT DISTINCT branch_id FROM bundle.sync_ops)"
          []
      let! _ =
        run
          conn
          "CREATE TABLE bundle.sync_blobs AS
           SELECT b.hash, b.length, b.bytes FROM main.package_blobs b
           WHERE NOT EXISTS (SELECT 1 FROM have.have_blobs h WHERE h.hash = b.hash)"
          []
      let! _ =
        run
          conn
          "CREATE TABLE bundle.sync_journal AS
           SELECT j.seq, j.id, j.at, j.kind, j.key, j.branch, j.op, j.commit_hash, j.name,
                  j.merged_at, j.archived_at
           FROM main.sync_journal j
           WHERE NOT EXISTS (SELECT 1 FROM have.have_journal h WHERE h.id = j.id)
           ORDER BY j.seq"
          []
      // `CREATE TABLE AS` reports no row count, so the counts are read back.
      let! ops = count conn "bundle.sync_ops"
      let! commits = count conn "bundle.sync_commits"
      let! branches = count conn "bundle.sync_branches"
      let! bases = count conn "bundle.sync_bases"
      let! blobs = count conn "bundle.sync_blobs"
      let! journal = count conn "bundle.sync_journal"
      tx.Commit()
      let! _ = run conn "DETACH DATABASE bundle" []
      let! _ = run conn "DETACH DATABASE have" []
      return
        Ok
          { ops = ops
            committed = 0L
            dropped = 0L
            commits = commits
            branches = branches
            bases = bases
            blobs = blobs
            journal = journal }
  }


/// Append the bundle at <param bundlePath> to the store at <param storePath>, in one transaction,
/// and say what changed. Rows the store already holds are left as they are; ops arrive unfolded,
/// for the ordinary fold. A branch is registered by its id with the name and parent it was created
/// with. The authoring clock moves past the newest stamp received, so what is written here next
/// sorts after it.
///
/// The bundle's journal entries join this store's, keeping their ids and stamps, and every row whose
/// latest entry is now one of them is made to match it: an op gone from a branch is dropped with
/// what it wrote, a commit moves to the branch it names, a branch takes the name and marks it names.
/// The triggers are held off meanwhile, so none of this is journalled a second time.
let import
  (storePath : string)
  (bundlePath : string)
  : Task<Result<Counts, string>> =
  task {
    use! conn = openStore storePath
    let! _ = attach conn bundlePath "bundle"
    let! refused =
      task {
        match! checkJournal conn storePath with
        | Some reason -> return Some reason
        | None -> return! checkMeta conn "bundle" "bundle"
      }
    match refused with
    | Some reason -> return Error reason
    | None ->
      use tx = conn.BeginTransaction()
      let! _ = run conn "INSERT INTO main.sync_replaying (held) VALUES (1)" []

      let! journal =
        run
          conn
          "INSERT OR IGNORE INTO main.sync_journal
             (id, at, kind, key, branch, op, commit_hash, name, merged_at, archived_at)
           SELECT id, at, kind, key, branch, op, commit_hash, name, merged_at, archived_at
           FROM bundle.sync_journal ORDER BY seq"
          []
      // The latest entry for every row the bundle names, by an entry or by an op, and whether it is
      // one of the bundle's. The ops' rows are here so that a removal this store made keeps a copy
      // held elsewhere from coming back.
      let! _ =
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

      // Parents before children is not needed: a branch row names its parent by id only.
      let! branches =
        run
          conn
          "INSERT OR IGNORE INTO main.branches (id, name, parent_id, created_at)
           SELECT id, name, parent_id, created_at FROM bundle.sync_branches"
          []
      let! _ =
        run
          conn
          "UPDATE main.branches
           SET name = l.name, merged_at = l.merged_at, archived_at = l.archived_at
           FROM temp.sync_latest l
           WHERE l.kind = 'branch' AND l.sent AND l.branch = branches.id"
          []
      let! commits =
        run
          conn
          "INSERT OR IGNORE INTO main.commits
             (hash, branch_id, message, origin_ts, created_at, parent, housekeeping)
           SELECT hash, branch_id, message, origin_ts, created_at, parent, housekeeping
           FROM bundle.sync_commits"
          []
      let! _ =
        run
          conn
          "UPDATE main.commits SET branch_id = l.branch
           FROM temp.sync_latest l
           WHERE l.kind = 'commit' AND l.sent AND l.commit_hash = commits.hash
             AND commits.branch_id <> l.branch"
          []
      let! bases =
        run
          conn
          "INSERT OR IGNORE INTO main.branch_name_bases (branch_id, owner, modules, name, base_hash)
           SELECT branch_id, owner, modules, name, base_hash FROM bundle.sync_bases"
          []
      let! blobs =
        run
          conn
          "INSERT OR IGNORE INTO main.package_blobs (hash, length, bytes)
           SELECT hash, length, bytes FROM bundle.sync_blobs"
          []

      // Gone over there, so gone here: what each op wrote on its branch, then the op, as a local
      // discard drops them.
      let! gone =
        pairs
          conn
          "SELECT l.branch, json_group_array(l.op)
           FROM temp.sync_latest l
           WHERE l.kind = 'op-gone'
             AND EXISTS (
               SELECT 1 FROM main.package_ops p WHERE p.id = l.op AND p.branch_id = l.branch)
           GROUP BY l.branch"
      let! dropped =
        scalar
          conn
          "SELECT COUNT(*) FROM temp.sync_latest l
           WHERE l.kind = 'op-gone'
             AND EXISTS (
               SELECT 1 FROM main.package_ops p WHERE p.id = l.op AND p.branch_id = l.branch)"
      let ctx = PreparedBatch.newCtx conn
      try
        for (branch, ids) in gone do
          do!
            Layers.dropRowsOf
              ctx
              (PT.BranchId.Id(System.Guid.Parse branch))
              "op_id IN (SELECT value FROM json_each($ids))"
              (fun cmd -> PreparedBatch.p cmd "$ids" ids)
          let! _ =
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
      let! adopted =
        pairs
          conn
          "SELECT DISTINCT b.branch_id, b.commit_hash
           FROM bundle.sync_ops b
           JOIN main.package_ops p ON p.id = b.id AND p.branch_id = b.branch_id
           WHERE p.commit_hash IS NULL AND b.commit_hash IS NOT NULL"
      let! committed =
        run
          conn
          "UPDATE main.package_ops
           SET commit_hash = (
             SELECT b.commit_hash FROM bundle.sync_ops b
             WHERE b.id = package_ops.id AND b.branch_id = package_ops.branch_id)
           WHERE commit_hash IS NULL
             AND EXISTS (
               SELECT 1 FROM bundle.sync_ops b
               WHERE b.id = package_ops.id AND b.branch_id = package_ops.branch_id
                 AND b.commit_hash IS NOT NULL)"
          []
      let ctx = PreparedBatch.newCtx conn
      try
        for (branch, hash) in adopted do
          do!
            Layers.commitRowsOf
              ctx
              (PT.BranchId.Id(System.Guid.Parse branch))
              "op_id IN (SELECT id FROM package_ops WHERE branch_id = $branch AND commit_hash = $hash)"
              (fun cmd -> PreparedBatch.p cmd "$hash" hash)
      finally
        PreparedBatch.disposeCtx ctx

      // Every op the receiver lacks, except one whose latest entry says it is gone from its branch.
      let! ops =
        run
          conn
          "INSERT OR IGNORE INTO main.package_ops
             (id, branch_id, authored_on, op_blob, op_kind, commit_hash, source,
              origin_ts)
           SELECT b.id, b.branch_id, b.authored_on, b.op_blob, b.op_kind, 0, b.commit_hash, b.source,
                  b.origin_ts
           FROM bundle.sync_ops b
           WHERE NOT EXISTS (
             SELECT 1 FROM temp.sync_latest l
             WHERE l.kind = 'op-gone' AND l.branch = b.branch_id AND l.op = b.id)
           ORDER BY b.seq"
          []
      let! newest = scalar conn "SELECT MAX(origin_ts) FROM bundle.sync_ops"
      let! _ = run conn "DROP TABLE temp.sync_latest" []
      let! _ = run conn "DELETE FROM main.sync_replaying" []
      tx.Commit()
      let! _ = run conn "DETACH DATABASE bundle" []

      if newest <> "" then OriginTs.observe newest
      Caching.invalidateAll ()
      return
        Ok
          { ops = ops
            committed = committed
            dropped = int64 dropped
            commits = commits
            branches = branches
            bases = bases
            blobs = blobs
            journal = journal }
  }
