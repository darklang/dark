/// Shape changes to CANONICAL tables, on stores that already exist.
///
/// `migrations/schema/*.sql` declares the from-scratch shape, but `CREATE TABLE IF NOT EXISTS` no-ops
/// against a table that already exists, so a new column never reaches an existing store from those files.
///
/// In principle a PROJECTION needs no step, since dropping and re-folding it rebuilds the new shape. In
/// practice nothing on the shipped path does that: `rebuildProjections` is reachable only from LocalExec,
/// which is not shipped. So a projection whose shape changed needs a step here too, and `locations` has
/// two. Fixing that properly means teaching startup to notice the drift and re-fold; until then, a step.
///
/// This is THE mechanism, and the only one. There used to be a second (`migrations/incremental/*.sql`,
/// now deleted) with no written rule for choosing between them, which is how the same migration got
/// written twice and failed in both directions: a raw `.sql` file runs on FRESH stores too, where the
/// schema has already created the table with the new shape, and `ALTER TABLE ... ADD COLUMN` then fails
/// with "duplicate column name" -- SQLite has no `ADD COLUMN IF NOT EXISTS`. A step has to LOOK at the
/// store before acting, which a raw SQL file cannot.
///
/// So steps are code: stable name, at most once per store, recorded in `system_migrations_v0` inside a
/// transaction. Every step must be safe against a store that already has the desired shape, since that is
/// what a fresh store is. `addColumnIfMissing` checks before it acts.
module LibDB.Releases

open Fumble
open LibDB.Sqlite

open Prelude


/// Does <param table> already have <param column>?
let private hasColumn (table : string) (column : string) : bool =
  // `pragma_table_info` is the queryable form of `PRAGMA table_info`, so this can be a normal SELECT.
  // Table name is interpolated because a pragma-table argument cannot be a bound parameter; it is a
  // literal in this file, never caller input.
  Sql.query $"SELECT 1 AS n FROM pragma_table_info('{table}') WHERE name = @c"
  |> Sql.parameters [ "c", Sql.string column ]
  |> Sql.executeExistsSync


let private tableExists (table : string) : bool =
  Sql.query "SELECT 1 AS n FROM sqlite_master WHERE type = 'table' AND name = @t"
  |> Sql.parameters [ "t", Sql.string table ]
  |> Sql.executeExistsSync


/// Add a column, or do nothing if it is already there (the FRESH store, where the schema just declared
/// it).
let addColumnIfMissing
  (table : string)
  (column : string)
  (declaration : string)
  : unit =
  if tableExists table && not (hasColumn table column) then
    print $"  release: adding {table}.{column}"
    Sql.query $"ALTER TABLE {table} ADD COLUMN {column} {declaration}"
    |> Sql.executeStatementSync


// ---------------------
// The steps
// ---------------------
//
// APPEND ONLY. A step's name is how a store remembers having run it, so renaming one re-runs it and
// reordering changes what "already applied" means.

type Step = { name : string; run : unit -> unit }


let steps : List<Step> =
  [
    // A conflict is recorded against a name; this scopes it to a BRANCH too. Note the default here is
    // '' while the schema declares main's uuid, so a conflict row that predates the column is
    // scoped to no branch at all and no listing shows it. Deliberate: a conflict is a finding about a
    // log this store has since replaced, and re-detection produces it again under a real branch id.
    { name = "20260731_000001_conflicts_branch_id"
      run =
        fun () ->
          addColumnIfMissing "conflicts" "branch_id" "TEXT NOT NULL DEFAULT ''" }

    // A commit names the commit it follows, so the graph is a chain. Existing rows get '', which reads
    // as "nothing before this one": true of the first, a lie about the rest, and unrecoverable.
    { name = "20260819_000001_commits_parent"
      run =
        fun () -> addColumnIfMissing "commits" "parent" "TEXT NOT NULL DEFAULT ''" }

    // Defaults are chosen so existing rows keep the meaning they already had:
    //   `effective = 1`  -- everything in an old store is main's, and main's ops take effect.
    //   `parent_id`      -- main's well-known id, which is what every pre-branch branch forked from.
    //   `op_id`, `previous`, `author`, `origin_ts` -- empty, meaning "not recorded", which is true.
    { name = "20260828_000001_package_ops_effective"
      run =
        fun () ->
          addColumnIfMissing "package_ops" "effective" "INTEGER NOT NULL DEFAULT 1" }

    { name = "20260828_000002_branches_parent_id"
      run =
        fun () ->
          addColumnIfMissing
            "branches"
            "parent_id"
            "TEXT NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001'" }

    { name = "20260828_000003_commits_author"
      run =
        fun () -> addColumnIfMissing "commits" "author" "TEXT NOT NULL DEFAULT ''" }

    { name = "20260828_000004_commits_origin_ts"
      run =
        fun () -> addColumnIfMissing "commits" "origin_ts" "TEXT NOT NULL DEFAULT ''" }

    { name = "20260828_000005_locations_op_id"
      run =
        fun () -> addColumnIfMissing "locations" "op_id" "TEXT NOT NULL DEFAULT ''" }

    { name = "20260828_000006_locations_previous"
      run = fun () -> addColumnIfMissing "locations" "previous" "TEXT NULL" }

    // The branch twin of `locations.source`; see `migrations/schema/`. Without it a branch records no
    // provenance.
    { name = "20260904_000001_op_branches_source"
      run =
        fun () ->
          addColumnIfMissing "op_branches" "source" "TEXT NOT NULL DEFAULT 'op'" }

    // A store made under the previous SCM has `branch_ops`, its separate op log for branch structure.
    // Nothing reads it now, and NOTHING in it carries over -- not the branches, and not main's ops
    // either, whatever an earlier version of this note claimed. Say so ONCE, at the first boot that
    // sees it, rather than let `dark branches` come up empty with no explanation. The table is left
    // where it is; wiping is the person's call.
    { name = "20260904_000002_previous_scm_store"
      run =
        fun () ->
          if tableExists "branch_ops" then
            System.Console.Error.WriteLine
              "note: this store was made by a previous version of the SCM (it has a `branch_ops` table). \
               Nothing in it carries over. Wipe the store (`rm ~/.darklang/data.db*`): the packages \
               re-grow from this binary, and `dark pull` brings the rest back from your relay." }

    // What a relay's stored bundle CONTAINS, so a push can be compared with it rather than replacing
    // it blind. `relay_branches` is hosted data, not a projection, so nothing else brings these to a
    // store that already exists -- and a relay that has them missing does not degrade quietly, it
    // fails every branch push with "no column named max_ts".
    { name = "20260908_000001_relay_branch_freshness"
      run =
        fun () ->
          addColumnIfMissing "relay_branches" "max_ts" "TEXT NOT NULL DEFAULT ''"
          addColumnIfMissing "relay_branches" "op_count" "INTEGER NOT NULL DEFAULT 0" }

    // The LWW register for doc comments. A whole table rather than a column, so `IF NOT EXISTS`
    // WOULD have reached an existing store from the schema -- but only because the bootstrap
    // replays it, which it does not promise to. Named here so the store records having got it, and
    // so the answer to "how does a shape change reach an existing store" stays one answer.
    { name = "20260908_000002_item_docs"
      run =
        fun () ->
          Sql.query
            "CREATE TABLE IF NOT EXISTS item_docs (
               item_hash TEXT NOT NULL,
               part TEXT NOT NULL,
               within TEXT NOT NULL,
               text TEXT NOT NULL,
               origin_ts TEXT NOT NULL,
               PRIMARY KEY (item_hash, part, within))"
          |> Sql.executeStatementSync }

    // ...and then keyed on the LOCATION instead, because content is shared and ten names holding one
    // declaration do not mean one thing. `item_docs` never reached a released build; it goes.
    { name = "20260909_000001_location_docs"
      run =
        fun () ->
          Sql.query "DROP TABLE IF EXISTS item_docs" |> Sql.executeStatementSync

          Sql.query
            "CREATE TABLE IF NOT EXISTS location_docs (
               owner TEXT NOT NULL,
               modules TEXT NOT NULL,
               name TEXT NOT NULL,
               kind TEXT NOT NULL,
               within TEXT NOT NULL,
               text TEXT NOT NULL,
               origin_ts TEXT NOT NULL,
               PRIMARY KEY (owner, modules, name, kind, within))"
          |> Sql.executeStatementSync }

    // A doc divergence is about one PART of a declaration, and settling it means writing that part.
    // Without this the resolution path could name the conflict but not what it was about.
    { name = "20260909_000002_conflicts_part"
      run =
        fun () -> addColumnIfMissing "conflicts" "part" "TEXT NOT NULL DEFAULT ''" }

    // NEW STEPS GO ABOVE THIS LINE -- `scripts/migrations/new` appends here, and edits nothing else.
    ]


let private alreadyRun () : Set<string> =
  if not (tableExists "system_migrations_v0") then
    Set.empty
  else
    Sql.query "SELECT name FROM system_migrations_v0"
    |> Sql.execute (fun read -> read.string "name")
    |> Result.unwrap
    |> Set.ofList


/// Replay the schema statements that `keep` selects, against an existing store.
///
/// Every statement in that file is `CREATE ... IF NOT EXISTS` or `INSERT OR IGNORE`, so this is safe on
/// every startup and does nothing once the store is current. Passed in rather than read from disk: the
/// shipped binary has no `backend/migrations` beside it.
///
/// Split in three on purpose, because ORDER matters against an existing store:
///
///   1. tables   -- `CREATE TABLE IF NOT EXISTS` brings across anything wholly new
///   2. columns  -- `steps` below, which is the only thing that can widen a table that already exists
///   3. indexes  -- `CREATE INDEX IF NOT EXISTS`, which FAILS if it names a column step 2 just added
///
/// Doing it in one pass fails exactly there: the schema indexes `package_ops(effective)`, and on a
/// store predating that column the index cannot be created.
let private runStatements (keep : string -> bool) (schemaSql : string) : unit =
  // Comments FIRST, then split. The schema's comments contain semicolons ("NULL = DRAFT; Gates
  // nothing"), so splitting first cuts statements in half and SQLite reports "incomplete input".
  // No `--` appears inside a string literal in that file, so truncating at one is safe here.
  let stripped =
    schemaSql.Split('\n')
    |> Array.map (fun line ->
      match line.IndexOf "--" with
      | -1 -> line
      | i -> line.Substring(0, i))
    |> String.concat "\n"

  let statements =
    stripped.Split(';')
    |> Array.map (fun st -> st.Trim())
    |> Array.filter (fun st -> st <> "" && keep (st.ToUpperInvariant()))

  if not (Array.isEmpty statements) then
    use conn = new Microsoft.Data.Sqlite.SqliteConnection(LibDB.Sqlite.connString)
    conn.Open()

    for st in statements do
      use cmd = conn.CreateCommand()
      cmd.CommandText <- st
      cmd.ExecuteNonQuery() |> ignore<int>

/// Step 1: tables (and the seeded account row), which carry a wholly new table to an existing store.
let applySchemaTables (schemaSql : string) : unit =
  runStatements
    (fun upper ->
      upper.StartsWith "CREATE TABLE" || upper.StartsWith "INSERT OR IGNORE")
    schemaSql

/// Step 3: indexes, once every column they name exists. `UNIQUE` has to be matched separately:
/// `CREATE UNIQUE INDEX` does not start with "CREATE INDEX", and `package_dependencies` has one.
let applySchemaIndexes (schemaSql : string) : unit =
  runStatements
    (fun upper ->
      upper.StartsWith "CREATE INDEX" || upper.StartsWith "CREATE UNIQUE INDEX")
    schemaSql


/// Step 2: run every step this store has not run, in order. `steps` carry new COLUMNS, which `IF NOT
/// EXISTS` cannot. Called after the schema bootstrap, so on a fresh store every step is a no-op that
/// records itself.
///
/// There is no test suite over this. Each step's guard (`addColumnIfMissing`, `tableExists`) is what
/// makes it safe to run against a store of any age, so a new step that skips those has nothing
/// checking it.
let runPending () : unit =
  // A step's name is its identity in `system_migrations_v0`, so two steps sharing one would run as
  // one and record as one, silently. Refused here, where every store passes on startup, because no
  // test constructs this list.
  let duplicates =
    steps
    |> List.countBy _.name
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map fst

  if not (List.isEmpty duplicates) then
    Exception.raiseInternal
      "duplicate release step name(s)"
      [ "names", String.concat ", " duplicates ]

  let done_ = alreadyRun ()

  for step in steps do
    if not (Set.contains step.name done_) then
      print $"Running release step: {step.name}"
      step.run ()

      Sql.query
        "INSERT INTO system_migrations_v0 (name, execution_date, sql)
         VALUES (@name, CURRENT_TIMESTAMP, @sql)
         ON CONFLICT(name) DO NOTHING"
      |> Sql.parameters
        [ "name", Sql.string step.name
          // The `sql` column wants the statement that ran; a code step has no single statement, so name
          // the step rather than leaving it empty.
          "sql", Sql.string $"(release step: {step.name})" ]
      |> Sql.executeStatementSync
