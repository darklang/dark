/// Tests for `LibDB.OpSync`: two copies of one store, an edit on one, and the other catching up.
module Tests.OpSync

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module OpSync = LibDB.OpSync

let private ok (r : Result<OpSync.Counts, string>) : OpSync.Counts =
  match r with
  | Ok counts -> counts
  | Error reason -> failtest reason

/// Two copies of the test store, `a` and `b`, and a directory for the files between them. Switching
/// LibDB's store is process-wide, so these tests run sequentially, and every one switches back.
let private withTwoStores (run : string -> string -> string -> Task<unit>) =
  task {
    let directory = System.IO.Directory.CreateTempSubdirectory("op-sync-")
    let original = LibDB.Sqlite.currentDbPath
    let a = System.IO.Path.Combine(directory.FullName, "a.db")
    let b = System.IO.Path.Combine(directory.FullName, "b.db")
    for path in [ a; b ] do
      match LibDB.Sqlite.Backup.toTestBaseline path with
      | Error message -> failtest message
      | Ok() -> ()
    try
      do! run directory.FullName a b
    finally
      Sql.useStoreForTesting original
      LibDB.Caching.invalidateAll ()
      try
        directory.Delete true
      with _ ->
        ()
  }

let private useStore (path : string) : unit =
  Sql.useStoreForTesting path
  LibDB.Caching.invalidateAll ()

/// Send what <param fromStore> has and <param toStore> lacks, the three steps in order.
let private sync
  (dir : string)
  (fromStore : string)
  (toStore : string)
  : Task<OpSync.Counts * OpSync.Counts> =
  task {
    let have = System.IO.Path.Combine(dir, "have.db")
    let bundle = System.IO.Path.Combine(dir, "bundle.db")
    do! OpSync.writeHave toStore have
    let! sent = OpSync.export fromStore have bundle
    let! received = OpSync.import toStore bundle
    return ok sent, ok received
  }

let private draftCount (store : string) (ids : List<string>) : Task<int64> =
  task {
    use conn =
      new Microsoft.Data.Sqlite.SqliteConnection(
        $"Data Source={store};Pooling=false"
      )
    do! conn.OpenAsync()
    use cmd = conn.CreateCommand()
    cmd.CommandText <-
      "SELECT COUNT(*) FROM package_ops
       WHERE id IN (SELECT value FROM json_each($ids)) AND commit_hash IS NULL"
    cmd.Parameters.AddWithValue("$ids", jsonStrings ids)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    let! n = cmd.ExecuteScalarAsync()
    return unbox<int64> n
  }

let nothingToSend =
  testTask "a store that holds everything is sent nothing" {
    do!
      withTwoStores (fun dir a b ->
        task {
          let! (sent, received) = sync dir a b
          Expect.equal sent.ops 0L "no ops to send"
          Expect.equal sent.commits 0L "no commits to send"
          Expect.equal sent.blobs 0L "no blobs to send"
          Expect.equal received.ops 0L "no ops arrive"
        })
  }

let draftThenCommit =
  testTask "an edit on one store arrives on the other once, and then its commit does" {
    do!
      withTwoStores (fun dir a b ->
        task {
          let location : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = "answer" }

          useStore a
          let! authored =
            authorIntoMain "module Tests.OpSync\nlet answer(): Int64 = 4217"
          let ids = authored |> List.map (LibDB.Inserts.computeOpHash >> string)

          let! (sent, received) = sync dir a b
          Expect.isGreaterThanOrEqual
            sent.ops
            (int64 (List.length ids))
            "the edit is sent"
          Expect.equal received.ops sent.ops "every op sent is new there"
          Expect.equal received.committed 0L "nothing was committed"

          let! (_, again) = sync dir a b
          Expect.equal again.ops 0L "a second sync sends nothing new"

          useStore b
          let! _ = LibDB.Seed.applyUnappliedOps ()
          let! found = pmPT.findFn location |> Ply.toTask
          Expect.isSome found "the other store binds the name once the fold has run"
          let! inDraft = draftCount b ids
          Expect.equal inDraft (int64 (List.length ids)) "it arrives in the draft"

          useStore a
          match!
            LibDB.ScmActions.commit
              PT.BranchId.Main
              "sync"
              LibDB.ScmActions.Whole
              false
          with
          | Error e -> failtest $"commit failed: {e}"
          | Ok _ -> ()

          let! (sent, received) = sync dir a b
          Expect.isGreaterThanOrEqual sent.commits 1L "the commit is sent"
          Expect.isGreaterThanOrEqual
            received.committed
            (int64 (List.length ids))
            "the other store's copies join it"
          Expect.equal received.ops 0L "no op arrives twice"
          let! inDraft = draftCount b ids
          Expect.equal inDraft 0L "none is left in the other store's draft"

          let! (sent, _) = sync dir a b
          Expect.equal sent.ops 0L "then the two agree"

          useStore b
          let! found = pmPT.findFn location |> Ply.toTask
          Expect.isSome found "the name is still bound once committed"
        })
  }

let wrongFile =
  testTask "a file that is not a bundle is refused" {
    do!
      withTwoStores (fun _ a b ->
        task {
          let! result = OpSync.import b a
          Expect.isError result "a store is not a bundle"
        })
  }

let tests =
  testSequenced <| testList "OpSync" [ nothingToSend; draftThenCommit; wrongFile ]
