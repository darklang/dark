/// The `dark test run` result cache (`package_test_results`) is part of the store schema.
///
/// The test runner used to create this table itself, from Dark, the first time `dark test run`
/// ran. Stores that did so still have that table and its rows, so the schema's declaration has to
/// be the same shape and has to leave the existing one, and the passes in it, alone.
module Tests.TestResultCache

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite

module TestResults = LibDB.TestResults


/// Verbatim what `Cli.Packages.Test.ensureCache` ran before the table moved into the schema.
let private oldRunnerDdl =
  "CREATE TABLE IF NOT EXISTS package_test_results (
     test_hash TEXT NOT NULL,
     runtime_hash TEXT NOT NULL,
     branch_id TEXT NOT NULL,
     result_type_hash TEXT NOT NULL,
     outcome_json TEXT NOT NULL,
     PRIMARY KEY (test_hash, runtime_hash, branch_id, result_type_hash)
   )"

let private schemaSql () : string =
  System.IO.Directory.GetFiles(
    System.IO.Path.Combine(LibConfig.Dirs.migrationsDir, "schema"),
    "*.sql"
  )
  |> Array.sort
  |> Array.map System.IO.File.ReadAllText
  |> String.concat "\n"

let private exec (dbPath : string) (sql : string) : unit =
  use conn = new SqliteConnection($"Data Source={dbPath}")
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  cmd.ExecuteNonQuery() |> ignore<int>

/// Each column of the table as (name, type, notnull, pk position).
let private shape (dbPath : string) : List<string * string * int64 * int64> =
  use conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly")
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "SELECT name, type, \"notnull\", pk FROM pragma_table_info('package_test_results') ORDER BY cid"
  use reader = cmd.ExecuteReader()
  [ while reader.Read() do
      yield
        reader.GetString 0, reader.GetString 1, reader.GetInt64 2, reader.GetInt64 3 ]

let private withScratchDir (f : string -> Task<unit>) : Task<unit> =
  task {
    let dir =
      System.IO.Path.Combine(
        System.IO.Path.GetTempPath(),
        $"dark-test-result-cache-{System.Guid.NewGuid()}"
      )
    System.IO.Directory.CreateDirectory dir |> ignore<System.IO.DirectoryInfo>
    try
      do! f dir
    finally
      SqliteConnection.ClearAllPools()
      System.IO.Directory.Delete(dir, true)
  }

let private key : TestResults.Key =
  { test = "test-hash"
    runtime = "runtime-hash:judge-hash"
    branch = "89282547-e4e6-4986-bcb6-db74bc6a8c0f"
    resultType = "result-type-hash" }


let schemaMatchesWhatTheRunnerCreated =
  testTask "the schema declares the table the test runner used to create" {
    do!
      withScratchDir (fun dir ->
        task {
          let old = System.IO.Path.Combine(dir, "old.db")
          let fresh = System.IO.Path.Combine(dir, "fresh.db")
          exec old oldRunnerDdl
          // Create the store in the same order as startup: tables, release steps,
          // then indexes. New indexes may need columns added by a release step.
          LibDB.Sqlite.Sql.useStoreForTesting fresh
          try
            let schema = schemaSql ()
            LibDB.Releases.applySchemaTables schema
            LibDB.Releases.applySchemaTables schema
            // The dated schema declares the column without relying on release steps.
            use conn =
              new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={fresh}")
            conn.Open()
            use command = conn.CreateCommand()
            command.CommandText <-
              "SELECT COUNT(*) FROM pragma_table_info('package_ops') WHERE name = 'op_kind'"
            Expect.equal
              (command.ExecuteScalar() |> unbox<int64>)
              1L
              "schema columns exist before migrations and tolerate replay"
            LibDB.Releases.runPending ()
            LibDB.Releases.applySchemaIndexes schema
          finally
            LibDB.Sqlite.Sql.resetStoreForTesting ()
          let freshShape = shape fresh
          Expect.isNonEmpty freshShape "a fresh store has the table"
          Expect.equal
            freshShape
            (shape old)
            "same columns, types, NOT NULLs and primary key as the runner's table"
        })
  }


/// A store from before the move: the runner made the table, and there is a pass in it. Bringing
/// the store forward runs the schema's tables against it, and the pass must still be there and
/// readable through the new path, and writable again.
let oldStoreKeepsItsResults =
  testTask "a store whose runner made the table keeps its results" {
    do!
      withScratchDir (fun dir ->
        task {
          let dbPath = System.IO.Path.Combine(dir, "data.db")
          exec dbPath oldRunnerDdl
          exec
            dbPath
            $"INSERT INTO package_test_results VALUES
                ('{key.test}', '{key.runtime}', '{key.branch}', '{key.resultType}', '\"Passed\"')"

          LibDB.Sqlite.Sql.useStoreForTesting dbPath
          try
            LibDB.Releases.applySchemaTables (schemaSql ())

            let! stored = TestResults.get key
            Expect.equal stored (Some "\"Passed\"") "the old pass is read back"

            let! other =
              TestResults.get { key with runtime = "another-build:judge-hash" }
            Expect.equal other None "another runtime is a different key"

            do! TestResults.put key "{\"Failed\":[[\"no\"]]}"
            let! replaced = TestResults.get key
            Expect.equal
              replaced
              (Some "{\"Failed\":[[\"no\"]]}")
              "storing under the same key replaces the outcome"
          finally
            LibDB.Sqlite.Sql.resetStoreForTesting ()
        })
  }


let tests =
  testList
    "TestResultCache"
    [ testSequenced schemaMatchesWhatTheRunnerCreated
      // Repoints the process-wide store, so nothing else may run beside it.
      testSequenced oldStoreKeepsItsResults ]
