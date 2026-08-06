/// Shape changes to CANONICAL tables, on stores that already exist.
///
/// `schema.sql` declares the from-scratch shape, but `CREATE TABLE IF NOT EXISTS` no-ops against a table
/// that already exists, so a new column never reaches an existing store from that file. Projections are
/// fine either way -- a schema change drops and re-folds them -- so this is only about the tables that
/// cannot be regenerated: `package_ops`, `branches`, `commits`, `conflicts`, `locations`.
///
/// Not an incremental `.sql` file, because those run on FRESH stores too, where `schema.sql` has already
/// created the table with the new shape; `ALTER TABLE ... ADD COLUMN` then fails with "duplicate column
/// name" and SQLite has no `ADD COLUMN IF NOT EXISTS`. A step has to LOOK at the store before acting,
/// which a raw SQL file cannot.
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


/// Add a column, or do nothing if it is already there (the FRESH store, where `schema.sql` just declared
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
    // A conflict is recorded against a name; this scopes it to a BRANCH too. Empty string means main,
    // matching how a branch id is spelled everywhere else, so existing rows keep their meaning.
    { name = "20260731_000001_conflicts_branch_id"
      run =
        fun () ->
          addColumnIfMissing "conflicts" "branch_id" "TEXT NOT NULL DEFAULT ''" }

    // A commit now names the commit it follows, so the graph is a chain rather than a flat list. Existing
    // rows get '', which reads as "nothing before this one" -- true of the first commit and a lie about the
    // rest, but the parent of a commit made before the column existed was never recorded anywhere to
    // recover it from.
    { name = "20260819_000001_commits_parent"
      run =
        fun () -> addColumnIfMissing "commits" "parent" "TEXT NOT NULL DEFAULT ''" } ]


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
/// Called after the schema bootstrap, so the tables exist and a fresh store already has the current shape:
/// there, every step is a no-op that records itself.
/// Run whatever steps this store has not.
///
/// Reachable only from LocalExec (`darklang-local-exec migrations run`), so a shipped CLI never calls it:
/// a user's store keeps whatever schema the seed it was born from had. That is the gap to close before a
/// canonical table needs a new column in the field.
///
/// Untested. `Releases.Tests.fs` covered the release PLANNER, which this replaced, and went with it.
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
          // The `sql` column wants the statement that ran; a code step has no single statement, so name
          // the step rather than leaving it empty.
          "sql", Sql.string $"(release step: {step.name})" ]
      |> Sql.executeStatementSync
