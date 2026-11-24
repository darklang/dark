/// What a purge has to empty, checked against the schema rather than against a list someone remembered to
/// update.
///
/// `purge` empties `package_ops` so the store can be refilled from disk. Anything whose rows describe rows
/// in `package_ops` has to go at the same time, or it is left referring to a log that no longer contains
/// what it names. Two of those tables are canonical -- nothing rebuilds them -- so a stale row survives
/// every subsequent reload and can only be removed by hand.
///
/// This is checked statically, by reading the schema, because the honest dynamic test would call `purge ()`
/// and the suite shares one store: it would empty the store out from under every other test in the run.
/// The static form still catches the thing that actually goes wrong, which is a table being ADDED and the
/// purge list not being told.
///
/// A leak here is quiet: every reader joins through to `package_ops`, so the dead rows simply never
/// appear.
module Tests.Purge

open Expecto

open System.Threading.Tasks

open Prelude

open Fumble
open LibDB.Sqlite
open Microsoft.Data.Sqlite

module Purge = LibDB.Purge

open TestUtils.TestUtils


/// Every table in this store carrying a column named `op_id`.
///
/// The column name IS the coupling. A table that stores an op id is making a claim about the contents of
/// `package_ops`, and emptying the log without emptying it leaves that claim false.
let private tablesWithOpId () : Task<List<string>> =
  Sql.query
    """
    SELECT m.name AS name
    FROM sqlite_master m
    JOIN pragma_table_info(m.name) p
    WHERE m.type = 'table' AND p.name = 'op_id'
    """
  |> Sql.executeRows (fun read -> read.string "name")
  |> Task.FromResult


let opIdTablesArePurged =
  testTask "every table holding an op_id is emptied by a purge" {
    let! withOpId = tablesWithOpId ()

    // If this is empty the query is wrong, not the schema, and the assertion below would pass vacuously.
    Expect.isNonEmpty withOpId "found tables carrying an op_id"

    let missed =
      withOpId |> List.filter (fun t -> not (List.contains t Purge.tables))
    let missedNames = String.concat ", " missed

    Expect.isEmpty
      missed
      $"every op_id table is in Purge.tables (missing: {missedNames}). \
        A table naming ops has to be emptied with them, or a purge leaves it pointing at a log that no \
        longer holds what it names. Add it to `Purge.tables`, or if it genuinely should outlive a reload, \
        say why in the NOT-here note there."
  }


let purgeTablesAllExist =
  testTask "every table a purge names exists in the schema" {
    // The other direction, and the reason it is worth asserting: `purge` filters its list by existence, so
    // a table renamed out from under it is silently skipped rather than failing. That is right for a store
    // mid-migration and wrong as a permanent state, because the rows it meant to delete stop being deleted
    // and nothing says so.
    let present =
      Sql.query "SELECT name FROM sqlite_master WHERE type = 'table'"
      |> Sql.executeRows (fun read -> read.string "name")

    let ghosts = Purge.tables |> List.filter (fun t -> not (List.contains t present))
    let ghostNames = String.concat ", " ghosts

    Expect.isEmpty
      ghosts
      $"no table in Purge.tables is missing from the schema (ghosts: {ghostNames}). \
        `purge` skips tables that don't exist, so a renamed one stops being emptied silently."
  }


/// The OTHER coupling, and the one the test above cannot see: a table can name the log by content
/// hash rather than by `op_id`.
///
/// Two kinds, and they need opposite answers, so this cannot be automatic. A cache keyed on content
/// (`type_checked`, `package_blobs`) stays TRUE across a reload, because a hash is its content. A claim
/// about the log's STATE (`conflicts`) does not: replace the log and it describes something that
/// never happened to it.
///
/// So the guard is an allowlist. A new table with a hash column fails this until someone writes down which
/// kind it is. That is the whole point: without the forced answer a table simply sits outside the
/// purge, and nothing says so.
let private hashCoupledSurvivors =
  [ "commits" // canonical; a commit whose ops are gone is empty, not wrong
    "package_blobs" // content-addressed cache: hash IS the content
    "package_caps" // ditto, keyed on the item's hash
    // The `dark test run` result cache. A pass keyed on the test's hash, the runtime, the branch
    // and the result type's binding stays true across a reload.
    "package_test_results"
    // The reload's trait-call cache: what the checker answered for a body, keyed on the body's hash
    // and on everything else the answer depends on, so it stays true across a reload.
    "trait_call_resolutions"
    "schema_state_v0" // describes the SCHEMA, not the log
    "trace_fn_calls" // observations of past runs; deliberately outlive their code
    // Same: a run is a trace, and `entry_hash` records which handler served a request as a
    // fact about that run. A run whose handler is gone is history, not a stale claim.
    "traces"
    // Same again: `fn_hash` is the version the run WENT THROUGH, which is what lets a resume
    // say which of its callees have been edited since. A hash the store no longer has is the
    // interesting case, not a broken one -- the resume reads it as "gone" and says nothing.
    "trace_fns"
    // A survivor because its hash names its OWN bytes, not anything in the log: no reload can
    // make a row describe something that did not happen. And it belongs to a trace, which
    // outlives its code like the rows above.
    "trace_blobs"
    "type_checked" // content-addressed cache: a clean result stays clean
    // A sync server's tokens: `hash` is a token's SHA-256, nothing in the log.
    "sync_tokens" ]


let hashCoupledTablesAreClassified =
  testTask
    "every table naming the log by hash is either purged or a declared survivor" {
    let withHash =
      Sql.query
        """
        SELECT DISTINCT m.name AS name
        FROM sqlite_master m
        JOIN pragma_table_info(m.name) p
        WHERE m.type = 'table' AND p.name LIKE '%hash%'
        """
      |> Sql.executeRows (fun read -> read.string "name")

    Expect.isNonEmpty withHash "found tables carrying a hash column"

    let unclassified =
      withHash
      |> List.filter (fun t ->
        not (List.contains t Purge.tables)
        && not (List.contains t hashCoupledSurvivors))

    let names = String.concat ", " unclassified

    Expect.isEmpty
      unclassified
      $"every hash-coupled table is classified (unclassified: {names}). \
        A table naming the log by content hash is either a cache that stays true across a reload, or a \
        claim about the log's state that does not. Put it in `Purge.tables` or in \
        `hashCoupledSurvivors` with a note saying which."
  }


let logStateProjectionsArePurged =
  testTask "conflicts are emptied by a purge" {
    // Named rather than derived, because what makes this different from `type_checked` is meaning,
    // not shape. Left behind, it describes a log that no longer exists: the store reports
    // "1 conflict to review" and then shows neither version, since the ops it names are gone.
    for t in [ "conflicts" ] do
      Expect.isTrue
        (List.contains t Purge.tables)
        $"`{t}` is in Purge.tables. It asserts something about the LOG (two versions competed), so \
          replacing the log makes it false, and it reads as actionable while being neither \
          reviewable nor resolvable."
  }


/// Every table the fold regenerates is purged. Derived from `Seed.projectionTables` rather than listed,
/// so it catches the next projection whatever its shape.
let foldProjectionsArePurged =
  testTask "every table the fold regenerates is in Purge.tables" {
    for t in LibDB.Seed.projectionTables do
      Expect.isTrue
        (List.contains t Purge.tables)
        $"`{t}` is a fold projection (Seed.projectionTables) and must be in Purge.tables: a projection \
          that outlives the log it was folded from is a stale claim."
  }


/// `Seed.exportFrom` on a scratch store, whose tables are older than the schema: `sync_peers`
/// predates `person`, as the builder's store did when the release seed shipped without it.
///
/// The seed must carry main's committed history and nothing of the builder's: not its draft, not
/// its branches, not its config, and must have every table the schema declares, in today's shape.
let seedExportBuildsFromTheSchema =
  testTask
    "Seed.exportFrom keeps main's commits, drops the builder's state, takes the schema's shape" {
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-seed-export-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    let sourcePath = System.IO.Path.Combine(dir, "source.db")
    let seedPath = System.IO.Path.Combine(dir, "seed.db")

    let exec (path : string) (sql : string) : unit =
      use conn = new SqliteConnection($"Data Source={path};Pooling=false")
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <- sql
      cmd.ExecuteNonQuery() |> ignore<int>

    let scalar (sql : string) : string =
      use conn =
        new SqliteConnection($"Data Source={seedPath};Mode=ReadOnly;Pooling=false")
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <- sql
      string (cmd.ExecuteScalar())

    let main = "00000000-0000-0000-0000-000000000001"
    let other = "11111111-1111-1111-1111-111111111111"

    try
      exec
        sourcePath
        $"""
        CREATE TABLE package_ops (seq INTEGER PRIMARY KEY, id TEXT, branch_id TEXT, authored_on TEXT,
          op_blob BLOB, op_kind TEXT, commit_hash TEXT, source TEXT, created_at TEXT, origin_ts TEXT);
        INSERT INTO package_ops VALUES
          (1, 'op-committed', '{main}', '{main}', x'00', 'unknown', 'c1', 'op', '2026-01-01', 't1'),
          (2, 'op-draft', '{main}', '{main}', x'00', 'unknown', NULL, 'op', '2026-01-01', 't2'),
          (3, 'op-branch', '{other}', '{other}', x'00', 'unknown', 'c2', 'op', '2026-01-01', 't3');
        CREATE TABLE commits (hash TEXT PRIMARY KEY, branch_id TEXT, message TEXT, origin_ts TEXT);
        INSERT INTO commits VALUES ('c1', '{main}', 'kept', 't1'), ('c2', '{other}', 'dropped', 't3');
        CREATE TABLE config_v0 (key TEXT PRIMARY KEY, value TEXT);
        INSERT INTO config_v0 VALUES ('instance.id', 'the-builders');
        CREATE TABLE sync_peers (id TEXT PRIMARY KEY, name TEXT, seen_at TEXT);
        INSERT INTO sync_peers VALUES ('peer', 'the builder''s laptop', 'now');
        """

      LibDB.Seed.exportFrom sourcePath seedPath

      Expect.equal
        (scalar "SELECT group_concat(id) FROM package_ops")
        "op-committed"
        "only main's committed op: not the draft, not another branch's"
      Expect.equal
        (scalar "SELECT group_concat(hash) FROM commits")
        "c1"
        "only main's commits"
      Expect.equal
        (scalar "SELECT housekeeping FROM commits")
        "0"
        "a column the source lacks takes the schema's default"
      for t in [ "config_v0"; "sync_peers"; "sync_journal"; "branches"; "conflicts" ] do
        Expect.equal
          (scalar $"SELECT COUNT(*) FROM {t}")
          "0"
          $"`{t}` is the builder's"
      Expect.equal
        (scalar
          "SELECT COUNT(*) FROM pragma_table_info('sync_peers') WHERE name = 'person'")
        "1"
        "`sync_peers` has the schema's columns, not the source's"
      Expect.equal
        (scalar "SELECT seq FROM package_ops_applied_through")
        "0"
        "no op counts as applied"
    finally
      System.IO.Directory.Delete(dir, true)
  }


/// Every table the schema DECLARES is a table the store HAS.
///
/// The schema is a directory now, read by concatenating `migrations/schema/*.sql` in filename
/// order. Two ways that can go wrong quietly: a file lands in the directory and the read path never
/// picks it up (a filter, a sort, a glob that missed it), or a file is read and its statements never
/// run. Either way the store is short a table and nothing says so until something queries it.
///
/// So: parse the CREATE TABLE names out of the files on disk and check the store has each one. The
/// store under test was built by this bootstrap, so agreement is the whole claim.
let everyDeclaredTableExists =
  testTask "every table the schema files declare exists in the store" {
    let dir = System.IO.Path.Combine(LibConfig.Dirs.migrationsDir, "schema")

    let declared =
      System.IO.Directory.GetFiles(dir, "*.sql")
      |> Array.toList
      |> List.sort
      |> List.collect (fun path ->
        System.Text.RegularExpressions.Regex.Matches(
          System.IO.File.ReadAllText path,
          @"CREATE TABLE IF NOT EXISTS\s+(\w+)"
        )
        |> Seq.map (fun m -> m.Groups[1].Value)
        |> List.ofSeq)

    Expect.isNonEmpty declared "the schema directory declares tables"

    let present =
      Sql.query "SELECT name FROM sqlite_master WHERE type = 'table'"
      |> Sql.executeRows (fun read -> read.string "name")

    let missing = declared |> List.filter (fun t -> not (List.contains t present))

    Expect.isEmpty
      missing
      $"""every declared table exists (missing: {String.concat ", " missing}). \
        A table declared in `migrations/schema/` and absent from the store means the file it is in \
        never reached the bootstrap -- check the read in `LocalExec.Migrations.schemaSql` and the \
        glob in `Cli.fsproj`."""
  }

/// `Seed.checkpointWal` against a store that has been WRITTEN to.
///
/// The write is the test. A checkpoint on an EMPTY log has nothing to do and succeeds on a
/// read-only connection as readily as on a writable one, so a version of this without the
/// INSERT would pass against the very bug it exists to catch.
let seedExportFoldsANonEmptyWal =
  testTask "Seed.checkpointWal folds a non-empty write-ahead log" {
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-seed-checkpoint-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    let dbPath = System.IO.Path.Combine(dir, "scratch.db")
    let walPath = dbPath + "-wal"

    let exec (connStr : string) (sql : string) : unit =
      use conn = new SqliteConnection(connStr)
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <- sql
      cmd.ExecuteNonQuery() |> ignore<int>

    let readBack () : string =
      use conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly")
      conn.Open()
      use cmd = conn.CreateCommand()
      cmd.CommandText <- "SELECT v FROM t"
      string (cmd.ExecuteScalar())

    try
      let rw = $"Data Source={dbPath};Mode=ReadWriteCreate;Cache=Private"
      exec rw "PRAGMA journal_mode=WAL"
      exec rw "CREATE TABLE t (v TEXT)"
      exec rw "INSERT INTO t (v) VALUES ('written')"

      Expect.isGreaterThan
        (System.IO.FileInfo walPath).Length
        0L
        "the log is non-empty before the checkpoint, which is the only case that can fail"

      // Raises SqliteException, error 10, `disk I/O error`, if this ever opens read-only again.
      LibDB.Seed.checkpointWal dbPath

      Expect.equal
        (System.IO.FileInfo walPath).Length
        0L
        "TRUNCATE leaves the log empty, so a plain file copy of the db carries the write"

      Expect.equal
        (readBack ())
        "written"
        "the folded write reads back from the main file, which is the point of folding it"
    finally
      System.IO.Directory.Delete(dir, true)
  }


let tests =
  testList
    "Purge"
    [ opIdTablesArePurged
      purgeTablesAllExist
      hashCoupledTablesAreClassified
      logStateProjectionsArePurged
      foldProjectionsArePurged
      seedExportFoldsANonEmptyWal
      seedExportBuildsFromTheSchema
      everyDeclaredTableExists ]
