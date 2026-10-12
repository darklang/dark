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


/// Create a table, or do nothing if it is already there (the FRESH store, where the schema just
/// declared it). The statement carries its own `IF NOT EXISTS`, so this is about the print.
let createTableIfMissing (table : string) (ddl : string) : unit =
  if not (tableExists table) then
    System.Console.Error.WriteLine $"  release: adding {table}"
    Sql.query ddl |> Sql.executeStatementSync


/// Add a column, or do nothing if it is already there (the FRESH store, where the schema just declared
/// it).
let addColumnIfMissing
  (table : string)
  (column : string)
  (declaration : string)
  : unit =
  if tableExists table && not (hasColumn table column) then
    System.Console.Error.WriteLine $"  release: adding {table}.{column}"
    Sql.query $"ALTER TABLE {table} ADD COLUMN {column} {declaration}"
    |> Sql.executeStatementSync


// ---------------------
// The steps
// ---------------------
//
// APPEND ONLY. A step's name is how a store remembers having run it, so renaming one re-runs it and
// reordering changes what "already applied" means.

type Step = { name : string; run : unit -> unit }

/// A step that raised, by name. It is not recorded, so the next start runs it again.
exception StepFailed of step : string * inner : exn


let steps : List<Step> =
  // EMPTY, and that is the state this file was reset to rather than a mechanism nobody wired up.
  //
  // Every step this list used to hold brought a store up to a shape from BEFORE the schema was cut
  // into one file (`migrations/schema/00-initial-schema.sql`). A store that old cannot be opened by
  // this build whatever these did -- the molt dropped the sync tables, the user-space tables and
  // `commits.author` -- so carrying them would have been pretending to support an upgrade that
  // does not work. They are in git, at the commit that cut the schema, if one is ever wanted as a
  // model.
  //
  // The mechanism is untouched and is the one to use for the NEXT change. APPEND ONLY: a step's
  // name is how a store remembers having run it, so renaming one re-runs it and reordering changes
  // what "already applied" means. `scripts/migrations/new` scaffolds a step and its schema half
  // together, which is the point: `CREATE TABLE IF NOT EXISTS` never reaches a store that already
  // has the table, so the schema file alone changes nothing for anybody who has run this before.
  //
  // NEW STEPS GO HERE.
  []


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
      // Progress, not output. On a first run this is the first thing a language server
      // would send its editor, where stdout is the protocol channel.
      System.Console.Error.WriteLine $"Running release step: {step.name}"
      try
        step.run ()
      with e ->
        raise (StepFailed(step.name, e))

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
