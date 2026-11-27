/// Test stores are layers over one shared file ("Test stores are layers", docs/internals.md).
/// These try to break that: a writer killed mid-write, two writers at once, a store made where a
/// crashed one was, and a process opening two layers over one file.
module Tests.LayerStores

open System.Threading.Tasks

open Expecto

open Prelude

let private sha (path : string) : string =
  use file = System.IO.File.OpenRead path
  System.Convert.ToHexString(System.Security.Cryptography.SHA256.HashData file)

let private count (i : CliInstance.T) (prefix : string) : int64 =
  use store = new CliInstance.Store(System.IO.Path.Combine(i.dir, "data.db"))
  use cmd = store.Connection.CreateCommand()
  cmd.CommandText <- "SELECT count(*) FROM config_v0 WHERE key LIKE @prefix || '%'"
  cmd.Parameters.AddWithValue("@prefix", prefix)
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  cmd.ExecuteScalar() :?> int64

/// Writes <param n> config keys, one transaction each.
let private writes (prefix : string) (n : int) : string list =
  [ "eval"
    $"Stdlib.List.range(1, {n}) |> Stdlib.List.map(fun n -> Darklang.LanguageTools.LocalStore.configSet(\"{prefix}\" + Stdlib.display(n), \"x\")) |> Stdlib.List.length" ]

let private allowWrites (i : CliInstance.T) : Task<unit> =
  task {
    let! (out, code) = CliInstance.runWithExit i [ "permissions"; "allow"; "native" ]
    Expect.equal code 0L $"permissions allow: {out}"
  }

let killedMidWrite =
  CliInstance.instanceTest
    "a writer killed mid-write leaves the shared file as it was, and its store still opens"
    (fun i ->
      task {
        let shared = sha (CliInstance.templatePath ())
        do! allowWrites i
        let wal = System.IO.Path.Combine(i.dir, "data.db-wal")
        let started = System.IO.FileInfo(wal).Length
        let writer = CliInstance.start i (writes "test.layer-kill-" 1000000)
        let clock = System.Diagnostics.Stopwatch.StartNew()
        while System.IO.FileInfo(wal).Length < started + 6_000_000L
              && not writer.HasExited
              && clock.Elapsed.TotalSeconds < 120.0 do
          do! Task.Delay 20
        // past SQLite's automatic checkpoint, about 4 MB of WAL
        Expect.isFalse
          writer.HasExited
          "the writer is still writing when it is killed"
        CliInstance.stop writer

        Expect.equal
          (sha (CliInstance.templatePath ()))
          shared
          "the shared file is unchanged"
        Expect.isTrue
          (System.IO.File.Exists(System.IO.Path.Combine(i.dir, "data.db-shm")))
          "the killed writer left its WAL index behind"
        Expect.isGreaterThan
          (count i "test.layer-kill-")
          0L
          "what it committed is kept"
        let! (out, code) = CliInstance.runWithExit i [ "eval"; "1 + 1" ]
        Expect.equal (code, out) (0L, "2") "the store opens after the kill"

        let other = CliInstance.create ()
        try
          Expect.equal
            (count other "test.layer-kill-")
            0L
            "a new store sees none of it"
        finally
          CliInstance.dispose other
      })

let twoAtOnce =
  CliInstance.instanceTest
    "two stores written at once each keep only their own writes"
    (fun a ->
      task {
        let shared = sha (CliInstance.templatePath ())
        let b = CliInstance.create ()
        try
          do! allowWrites a
          do! allowWrites b
          let! both =
            Task.WhenAll(
              CliInstance.runWithExit a (writes "test.layer-a-" 3000),
              CliInstance.runWithExit b (writes "test.layer-b-" 3000)
            )
          for (out, code) in both do
            Expect.equal (code, out) (0L, "3000") "each writer finishes"
          Expect.equal
            [ count a "test.layer-a-"
              count a "test.layer-b-"
              count b "test.layer-a-"
              count b "test.layer-b-" ]
            [ 3000L; 0L; 0L; 3000L ]
            "each store has its own writes and none of the other's"
          Expect.equal
            (sha (CliInstance.templatePath ()))
            shared
            "the shared file is unchanged"
          CliInstance.checkBindings b
        finally
          CliInstance.dispose b
      })

let madeWhereACrashedOneWas =
  CliInstance.instanceTest
    "a store made where a crashed one was does not read its leftover WAL"
    (fun i ->
      task {
        do! allowWrites i
        let! (out, code) =
          CliInstance.runWithExit i (writes "test.layer-crashed-" 5)
        Expect.equal (code, out) (0L, "5") "the first store writes"
        // a crash: the store and its marker gone, its WAL and WAL index left
        let db = System.IO.Path.Combine(i.dir, "data.db")
        System.IO.File.Delete db
        System.IO.File.Delete(db + "-layer")
        Expect.isTrue (System.IO.File.Exists(db + "-wal")) "the WAL is left"
        CliInstance.relayer i
        Expect.equal
          (count i "test.layer-crashed-")
          0L
          "the new store has none of the old one's writes"
      })

let oneLayerPerFile =
  testTask "a process cannot open two layers over one file" {
    let dir = System.IO.Directory.CreateTempSubdirectory("layer-rule-").FullName
    try
      let path name = System.IO.Path.Combine(dir, name)
      let connString name = "Data Source=" + path name + ";Pooling=False"
      do
        use base_ = new Microsoft.Data.Sqlite.SqliteConnection(connString "base.db")
        base_.Open()
        use cmd = base_.CreateCommand()
        cmd.CommandText <- "PRAGMA journal_mode=WAL; CREATE TABLE t (x INTEGER)"
        cmd.ExecuteNonQuery() |> ignore<int>
      System.IO.File.WriteAllText(path "base.db-layer", "")
      for name in [ "a.db"; "b.db" ] do
        CliInstance.layerStore (path "base.db") (path name)

      use _first = LibDB.Sqlite.openConnection (connString "a.db")
      Expect.throwsC
        (fun () ->
          LibDB.Sqlite.openConnection (connString "b.db")
          |> ignore<Microsoft.Data.Sqlite.SqliteConnection>)
        (fun e ->
          Expect.stringContains
            e.Message
            "two layer stores over one file"
            "the second is refused, saying why")
      use again = LibDB.Sqlite.openConnection (connString "a.db")
      Expect.equal
        again.State
        System.Data.ConnectionState.Open
        "the first opens again"
    finally
      Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools()
      System.IO.Directory.Delete(dir, true)
  }

let tests =
  testList
    "LayerStores"
    [ killedMidWrite; twoAtOnce; madeWhereACrashedOneWas; oneLayerPerFile ]
