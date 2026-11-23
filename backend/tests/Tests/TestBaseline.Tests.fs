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
  testTask "a test run's store leaves out the traces of the store it copies" {
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
          "INSERT INTO trace_blobs(trace_id, hash, bytes) VALUES ('t', 'h', zeroblob(4000000))"
        insert.ExecuteNonQuery() |> ignore<int>

      Sql.useStoreForTesting source
      let copied = Backup.toTestBaseline baseline
      Sql.useStoreForTesting original
      match copied with
      | Error error -> failtest error
      | Ok() -> ()

      Expect.equal
        (scalar baseline "SELECT count(*) FROM trace_blobs")
        0L
        "no traces"
      Expect.isGreaterThan
        (scalar baseline "SELECT count(*) FROM package_ops")
        0L
        "the packages are all there"
      Expect.isLessThan
        (System.IO.FileInfo(baseline).Length)
        (System.IO.FileInfo(source).Length - 3000000L)
        "the traces' space is given back"
    finally
      Sql.useStoreForTesting original
      try
        directory.Delete true
      with _ ->
        ()
  }

let tests = testSequenced <| testList "TestBaseline" [ leavesTracesOut ]
