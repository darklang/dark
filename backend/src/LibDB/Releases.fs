/// How an existing store reaches the shape this build expects, for every host that opens one (the CLI's
/// `extract`, LocalExec's migrations, the browser's `Boot`).
///
/// Three passes, in this order:
///
///   1. the base schema's tables (`migrations/schema/*.sql`, frozen once merged): `CREATE TABLE IF NOT
///      EXISTS`, which brings a wholly new table to an old store and leaves an existing table alone;
///   2. the upgrade units (`backend/migrations/upgrades/<name>/`, see `LibDB.Upgrades`), which carry
///      everything `IF NOT EXISTS` cannot: new columns, data fixups, refusals;
///   3. the base schema's indexes, once every column they name exists.
///
/// The units were a list of steps in this file until they moved to a directory each, under the same
/// names, so no store that had run a step runs its unit again.
module LibDB.Releases

open Fumble
open LibDB.Sqlite

open Prelude


/// Replay the schema statements that `keep` selects, against an existing store.
///
/// Every statement in that file is `CREATE ... IF NOT EXISTS` or `INSERT OR IGNORE`, so this is safe on
/// every startup and does nothing once the store is current. Passed in rather than read from disk: the
/// shipped binary has no `backend/migrations` beside it.
///
/// Split in three on purpose, because ORDER matters against an existing store:
///
///   1. tables   -- `CREATE TABLE IF NOT EXISTS` brings across anything wholly new
///   2. columns  -- the upgrade units, the only thing that can widen a table that already exists
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


/// Step 2: run every upgrade unit this store has not run, in order (`LibDB.Upgrades`). Units carry new
/// COLUMNS and data changes, which `IF NOT EXISTS` cannot. Called after the schema bootstrap, so on a
/// fresh store every unit is a no-op that records itself.
let runPending () : unit = Upgrades.runPending UpgradeRegistry.sources


/// The three passes, in order, for every host that opens a store. One function so the order cannot
/// drift between them: it once did, and LocalExec ran the whole schema file in one pass, which left a
/// schema file unable to index any column a unit adds. Safe to run on every start: each pass does
/// nothing once the store is current.
let upgrade (schemaSql : string) : unit =
  applySchemaTables schemaSql
  runPending ()
  applySchemaIndexes schemaSql
