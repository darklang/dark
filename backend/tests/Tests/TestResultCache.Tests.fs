/// The `dark test run` result cache (`package_test_results`).
module Tests.TestResultCache

open Expecto

open System.Threading.Tasks

open Prelude

open Microsoft.Data.Sqlite

module TestResults = LibDB.TestResults


let private schemaSql () : string =
  System.IO.Directory.GetFiles(
    System.IO.Path.Combine(LibConfig.Dirs.migrationsDir, "schema"),
    "*.sql"
  )
  |> Array.sort
  |> Array.map System.IO.File.ReadAllText
  |> String.concat "\n"

/// A fresh store at <param dbPath>, made from the schema, for <param f>.
let private withStore (dbPath : string) (f : unit -> unit) : unit =
  LibDB.Sqlite.Sql.useStoreForTesting dbPath
  try
    LibDB.StoreSchema.apply (schemaSql ())
    f ()
  finally
    LibDB.Sqlite.Sql.resetStoreForTesting ()

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


let storedResultsReadBack =
  testTask "a stored result reads back, and storing again replaces it" {
    do!
      withScratchDir (fun dir ->
        task {
          withStore (System.IO.Path.Combine(dir, "data.db")) (fun () ->
            TestResults.put key "\"Passed\""
            Expect.equal
              (TestResults.get key)
              (Some "\"Passed\"")
              "the pass is read back"

            let other =
              TestResults.get { key with runtime = "another-build:judge-hash" }
            Expect.equal other None "another runtime is a different key"

            TestResults.put key "{\"Failed\":[[\"no\"]]}"
            let replaced = TestResults.get key
            Expect.equal
              replaced
              (Some "{\"Failed\":[[\"no\"]]}")
              "storing under the same key replaces the outcome")
        })
  }


/// One read for a listing's tests answers each key in the order asked, with None where nothing
/// is stored, the same as asking for each one alone.
let manyMatchesOneAtATime =
  testTask "reading many results answers each key in order" {
    do!
      withScratchDir (fun dir ->
        task {
          withStore (System.IO.Path.Combine(dir, "data.db")) (fun () ->
            let other = { key with test = "other-test-hash" }
            TestResults.put key "\"Passed\""
            TestResults.put other "{\"Failed\":[[\"no\"]]}"
            let keys =
              [ other
                { key with test = "missing" }
                key
                { key with branch = "another-branch" }
                other ]
            let many = TestResults.getMany keys
            let single = keys |> List.map TestResults.get
            Expect.equal many single "same answers as one read per key"
            Expect.equal
              many
              [ Some "{\"Failed\":[[\"no\"]]}"
                None
                Some "\"Passed\""
                None
                Some "{\"Failed\":[[\"no\"]]}" ]
              "in the order asked"
            let none = TestResults.getMany []
            Expect.equal none [] "no keys, no results")
        })
  }


let tests =
  testList
    "TestResultCache"
    // Each repoints the process-wide store, so nothing else may run beside it.
    [ testSequenced storedResultsReadBack; testSequenced manyMatchesOneAtATime ]
