/// The store a package-test run starts from (`LibDB.Sqlite.Backup.toTestBaseline`).
module Tests.TestBaseline

open Expecto
open LibDB.Sqlite

let private connect (path : string) =
  let connection =
    new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={path};Pooling=false")
  connection.Open()
  connection

let private scalar (path : string) (sql : string) : int64 =
  use connection = connect path
  use command = connection.CreateCommand()
  command.CommandText <- sql
  command.ExecuteScalar() :?> int64

let leavesTracesOut =
  testTask
    "a test run's store leaves out the traces and free pages of the store it copies" {
    let directory = System.IO.Directory.CreateTempSubdirectory("test-baseline-")
    let source = System.IO.Path.Combine(directory.FullName, "source.db")
    let baseline = System.IO.Path.Combine(directory.FullName, "baseline.db")
    let original = currentDbPath
    try
      match Backup.toTestBaseline source with
      | Error error -> failtest error
      | Ok() -> ()
      do
        use connection = connect source
        use insert = connection.CreateCommand()
        insert.CommandText <-
          "INSERT INTO trace_blobs(trace_id, hash, bytes) VALUES ('kept', 'h', zeroblob(4000000));
           INSERT INTO trace_blobs(trace_id, hash, bytes) VALUES ('freed', 'h', zeroblob(4000000));
           DELETE FROM trace_blobs WHERE trace_id = 'freed'"
        insert.ExecuteNonQuery() |> ignore<int>

      Sql.useStoreForTesting source
      let copied = Backup.toTestBaseline baseline
      Sql.useStoreForTesting original
      match copied with
      | Error error -> failtest error
      | Ok() -> ()

      let both sql = scalar source sql, scalar baseline sql
      Expect.equal
        (both "SELECT count(*) FROM trace_blobs")
        (1L, 0L)
        "the source's trace is left out"
      let sourceOps, baselineOps = both "SELECT count(*) FROM package_ops"
      Expect.isGreaterThan baselineOps 0L "there are packages"
      Expect.equal baselineOps sourceOps "the packages are all there"
      let sourceSeq, baselineSeq =
        both "SELECT seq FROM sqlite_sequence WHERE name = 'package_ops'"
      Expect.equal
        baselineSeq
        sourceSeq
        "new ops keep counting from where the store was"
      let sourceIndexes, baselineIndexes =
        both "SELECT count(*) FROM sqlite_master WHERE type = 'index'"
      Expect.equal baselineIndexes sourceIndexes "the indexes are all there"
      Expect.isLessThan
        (System.IO.FileInfo(baseline).Length)
        (System.IO.FileInfo(source).Length - 7000000L)
        "neither the traces nor their free pages are copied"
    finally
      Sql.useStoreForTesting original
      try
        directory.Delete true
      with _ ->
        ()
  }

let tests = testSequenced <| testList "TestBaseline" [ leavesTracesOut ]
