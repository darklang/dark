/// Shape changes to CANONICAL tables, on stores that already exist.
///
/// `schema.sql` declares the from-scratch shape, but `CREATE TABLE IF NOT EXISTS` no-ops against a table
/// that already exists, so a new column never reaches an existing store from that file. Projections are
/// fine either way -- a schema change drops and re-folds them -- so this is only about the tables that
/// cannot be regenerated: `package_ops`, `branches`, `commits`, `conflicts`, `locations`.
///
/// Not an incremental `.sql` file, because those also run on FRESH stores, where `schema.sql` has already
/// created the table with the new shape. `ALTER TABLE ... ADD COLUMN` then fails with "duplicate column
/// name" and SQLite has no `ADD COLUMN IF NOT EXISTS`. A migration here has to LOOK at the store before
/// acting, which a raw SQL file cannot.
///
/// So steps are code: stable name, at most once per store, recorded in the same `system_migrations_v0`
/// table the incremental files use, inside a transaction. Adding one means appending to `steps`; never
/// rename or reorder an existing one, because the name IS the record of having run.
///
/// Every step must be safe against a store that already has the desired shape, since that is what a fresh
/// store is. `addColumnIfMissing` and `rebuildTable` both check first.
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


/// Add a column, or do nothing if it is already there.
///
/// The no-op case is the FRESH store, where `schema.sql` just declared it. Both paths have to end in the
/// same shape or the two ways of arriving at a store disagree.
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
    // A conflict is recorded against a name, and until now not against a BRANCH. That is why `ack` and
    // `override` refuse on a branch: closing a store-wide record while writing the fix into an overlay,
    // where it does nothing until merge, is incoherent enough that refusing was the better stopgap.
    //
    // Empty string means main, matching how a branch id is spelled everywhere else, so existing rows keep
    // meaning exactly what they meant.
    { name = "20260731_000001_conflicts_branch_id"
      run =
        fun () ->
          addColumnIfMissing "conflicts" "branch_id" "TEXT NOT NULL DEFAULT ''" } ]


let private alreadyRun () : Set<string> =
  if not (tableExists "system_migrations_v0") then
    Set.empty
  else
    Sql.query "SELECT name FROM system_migrations_v0"
    |> Sql.execute (fun read -> read.string "name")
    |> Result.unwrap
    |> Set.ofList


/// Run every step this store has not run, in order.
///
/// Called after the schema bootstrap, so the tables exist and a fresh store has already been given the
/// current shape. On a fresh store every step is therefore a no-op that records itself, which is the point:
/// the two ways of arriving at a store converge, and the record says so.
let runPending () : unit =
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
          // The `sql` column wants the statement that ran. A code step has no single statement, so record
          // what it was rather than leaving it empty and looking like a migration that did nothing.
          "sql", Sql.string $"(release step: {step.name})" ]
      |> Sql.executeStatementSync
