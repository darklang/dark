/// Tests for `LibDB.OpSync`: two copies of one store, an edit or a removal on one, and the other
/// catching up.
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
    match! OpSync.writeHave toStore have with
    | Error reason -> failtest reason
    | Ok() -> ()
    let! sent = OpSync.export fromStore have bundle
    let! received = OpSync.import toStore bundle
    return ok sent, ok received
  }

/// A number from <param store>, with `$ids` bound to <param ids>.
let private countIn
  (store : string)
  (sql : string)
  (ids : List<string>)
  : Task<int64> =
  task {
    use conn =
      new Microsoft.Data.Sqlite.SqliteConnection(
        $"Data Source={store};Pooling=false"
      )
    do! conn.OpenAsync()
    use cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    cmd.Parameters.AddWithValue("$ids", jsonStrings ids)
    |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
    let! n = cmd.ExecuteScalarAsync()
    return unbox<int64> n
  }

let private draftCount (store : string) (ids : List<string>) : Task<int64> =
  countIn
    store
    "SELECT COUNT(*) FROM package_ops
     WHERE id IN (SELECT value FROM json_each($ids)) AND commit_hash IS NULL"
    ids

/// How many of <param ids> <param store> holds, on any branch, in a commit or not.
let private heldCount (store : string) (ids : List<string>) : Task<int64> =
  countIn
    store
    "SELECT COUNT(DISTINCT id) FROM package_ops WHERE id IN (SELECT value FROM json_each($ids))"
    ids

/// Make <param store> one from before the journal: no triggers, no table.
let private forgetJournal (store : string) : Task<unit> =
  task {
    use conn =
      new Microsoft.Data.Sqlite.SqliteConnection(
        $"Data Source={store};Pooling=false"
      )
    do! conn.OpenAsync()
    use read = conn.CreateCommand()
    read.CommandText <-
      "SELECT name FROM sqlite_master WHERE type = 'trigger' AND name LIKE 'sync_journal_%'"
    use! reader = read.ExecuteReaderAsync()
    let triggers = LibDB.Sqlite.readRows (fun r -> r.GetString 0) reader
    reader.Close()
    for statement in
      (triggers |> List.map (fun t -> $"DROP TRIGGER {t}"))
      @ [ "DROP TABLE sync_journal"; "DROP TABLE sync_replaying" ] do
      use cmd = conn.CreateCommand()
      cmd.CommandText <- statement
      let! _ = cmd.ExecuteNonQueryAsync()
      ()
  }

let private rowCount (store : string) (table : string) : Task<int64> =
  countIn store $"SELECT COUNT(*) FROM {table}" []

/// Author <param source> into main's draft on <param store>, and the ids of what it wrote.
let private authorOn (store : string) (source : string) : Task<List<string>> =
  task {
    useStore store
    let! authored = authorIntoMain source
    return authored |> List.map (LibDB.Inserts.computeOpHash >> string)
  }

let private discardOn (store : string) (ids : List<string>) : Task<unit> =
  task {
    useStore store
    match! LibDB.ScmActions.discard PT.BranchId.Main (LibDB.ScmActions.Ops ids) with
    | Error e -> failtest $"discard failed: {e}"
    | Ok _ -> ()
  }

let private commitOn (store : string) : Task<unit> =
  task {
    useStore store
    match!
      LibDB.ScmActions.commit PT.BranchId.Main "sync" LibDB.ScmActions.Whole false
    with
    | Error e -> failtest $"commit failed: {e}"
    | Ok _ -> ()
  }

let private findOn (store : string) (location : PT.PackageLocation) =
  task {
    useStore store
    let! _ = LibDB.Seed.applyUnappliedOps ()
    return! pmPT.findFn location |> Ply.toTask
  }

/// Sync both ways until neither side has anything to send, and say how many rounds that took.
let private settle (dir : string) (a : string) (b : string) : Task<int> =
  let rec go (round : int) =
    task {
      let! (toB, _) = sync dir a b
      let! (toA, _) = sync dir b a
      let quiet (c : OpSync.Counts) =
        c.ops = 0L && c.commits = 0L && c.blobs = 0L && c.journal = 0L
      if quiet toB && quiet toA then return round
      elif round >= 4 then return failtest "the two stores never agree"
      else return! go (round + 1)
    }
  go 1

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

let discardTravels =
  testTask
    "a discard on one store removes the op on the other, and it does not come back" {
    do!
      withTwoStores (fun dir a b ->
        task {
          let location : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = "discarded" }
          let! ids = authorOn a "module Tests.OpSync\nlet discarded(): Int64 = 5381"
          let! _ = sync dir a b
          let! found = findOn b location
          Expect.isSome found "the edit arrived"

          do! discardOn a ids

          // The other way first: that store still holds the op, and this one must not take it back.
          let! (_, back) = sync dir b a
          Expect.equal back.ops 0L "the discarded op is not taken back"
          let! held = heldCount a ids
          Expect.equal held 0L "it stays discarded where it was discarded"

          let! (sent, received) = sync dir a b
          Expect.isGreaterThanOrEqual sent.journal 1L "the discard is sent"
          Expect.equal
            received.dropped
            (int64 (List.length ids))
            "every discarded op goes over there"
          let! held = heldCount b ids
          Expect.equal held 0L "the other store holds none of them"
          let! found = findOn b location
          Expect.isNone found "and the name is unbound there too"

          let! _ = settle dir a b
          let! held = heldCount a ids
          Expect.equal held 0L "after syncing both ways it is still gone"
        })
  }

let commitThenDiscard =
  testTask "a commit and then a discard leave both stores the same" {
    do!
      withTwoStores (fun dir a b ->
        task {
          let kept : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = "kept" }
          let dropped : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = "dropped" }
          let! keptIds = authorOn a "module Tests.OpSync\nlet kept(): Int64 = 7919"
          do! commitOn a
          let! droppedIds =
            authorOn a "module Tests.OpSync\nlet dropped(): Int64 = 104729"
          let! _ = sync dir a b
          do! discardOn a droppedIds

          let! rounds = settle dir a b
          Expect.isLessThanOrEqual rounds 2 "they agree after one exchange each way"

          for store in [ a; b ] do
            let! held = heldCount store keptIds
            Expect.equal
              held
              (int64 (List.length keptIds))
              "the committed edit is there"
            let! inDraft = draftCount store keptIds
            Expect.equal inDraft 0L "and committed"
            let! gone = heldCount store droppedIds
            Expect.equal gone 0L "the discarded edit is not"
            let! found = findOn store kept
            Expect.isSome found "the committed name is bound"
            let! found = findOn store dropped
            Expect.isNone found "the discarded one is not"

          let! journalA = rowCount a "sync_journal"
          let! journalB = rowCount b "sync_journal"
          Expect.equal journalA journalB "both hold the same journal"
        })
  }

let importTwice =
  testTask "importing the same bundle twice changes nothing the second time" {
    do!
      withTwoStores (fun dir a b ->
        task {
          let! ids = authorOn a "module Tests.OpSync\nlet twice(): Int64 = 6007"
          let! _ = sync dir a b
          do! discardOn a ids
          let! _ = authorOn a "module Tests.OpSync\nlet twiceAgain(): Int64 = 6011"

          let have = System.IO.Path.Combine(dir, "have.db")
          let bundle = System.IO.Path.Combine(dir, "bundle.db")
          match! OpSync.writeHave b have with
          | Error reason -> failtest reason
          | Ok() -> ()
          let! _ = OpSync.export a have bundle
          let! first = OpSync.import b bundle
          let first = ok first
          Expect.isGreaterThanOrEqual first.dropped 1L "the first import drops"
          Expect.isGreaterThanOrEqual first.ops 1L "and adds"

          let! opsBefore = rowCount b "package_ops"
          let! journalBefore = rowCount b "sync_journal"
          let! second = OpSync.import b bundle
          let second = ok second
          Expect.equal
            second
            { first with
                ops = 0L
                committed = 0L
                dropped = 0L
                commits = 0L
                branches = 0L
                bases = 0L
                blobs = 0L
                journal = 0L
                conflicts = []
                journalBefore = first.upTo.journal }
            "the second import counts nothing"
          let! opsAfter = rowCount b "package_ops"
          let! journalAfter = rowCount b "sync_journal"
          Expect.equal opsAfter opsBefore "no op added or removed"
          Expect.equal journalAfter journalBefore "no entry added"
        })
  }

let storeWithoutJournal =
  testTask "a store that keeps no journal is refused, and nothing changes" {
    do!
      withTwoStores (fun dir a b ->
        task {
          let! _ = authorOn a "module Tests.OpSync\nlet unsent(): Int64 = 3571"
          let! opsBefore = rowCount b "package_ops"
          do! forgetJournal a

          let have = System.IO.Path.Combine(dir, "have.db")
          let bundle = System.IO.Path.Combine(dir, "bundle.db")
          match! OpSync.writeHave b have with
          | Error reason -> failtest reason
          | Ok() -> ()
          let! sent = OpSync.export a have bundle
          Expect.isError sent "it cannot say what it removed, so it sends nothing"
          Expect.isFalse (System.IO.File.Exists bundle) "and writes no bundle"

          let! asReceiver = OpSync.writeHave a have
          Expect.isError asReceiver "nor can it receive"

          let! opsAfter = rowCount b "package_ops"
          Expect.equal opsAfter opsBefore "the other store is unchanged"
        })
  }

let mergeTravels =
  testTask
    "a merge on one store moves the branch's ops on the other, and marks it merged" {
    do!
      withTwoStores (fun dir a b ->
        task {
          let location : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = "merged" }
          useStore a
          let branch = PT.BranchId.Id(System.Guid.NewGuid())
          do! LibDB.Branches.createBranch branch "op-sync-merge" PT.BranchId.Main
          let! ops =
            parsePackageOps "module Tests.OpSync\nlet merged(): Int64 = 2207"
          let! _ = LibDB.Inserts.authorOn branch "op" ops
          let! _ = sync dir a b

          useStore a
          match!
            LibDB.ScmActions.merge branch [] (fun _ -> Task.FromResult()) (fun () ->
              Task.FromResult())
          with
          | Error _ -> failtest "the merge failed"
          | Ok _ -> ()
          let! _ = settle dir a b

          let onBranch =
            $"SELECT COUNT(*) FROM package_ops WHERE branch_id = '{branch}'"
          let isMerged =
            $"SELECT COUNT(*) FROM branches WHERE id = '{branch}' AND merged_at IS NOT NULL"
          for store in [ a; b ] do
            let! left = countIn store onBranch []
            Expect.equal left 0L "nothing is left on the merged branch"
            let! merged = countIn store isMerged []
            Expect.equal merged 1L "the branch is marked merged"
            let! found = findOn store location
            Expect.isSome found "main binds the name"
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

// ------------------
// Two stores linked by a session (`LibDB.SyncLink`), as two machines are over ssh
// ------------------

module SyncLink = LibDB.SyncLink

/// A text from <param store>.
let private textIn (store : string) (sql : string) : Task<string> =
  task {
    use conn =
      new Microsoft.Data.Sqlite.SqliteConnection(
        $"Data Source={store};Pooling=false"
      )
    do! conn.OpenAsync()
    use cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    let! v = cmd.ExecuteScalarAsync()
    return
      match v with
      | null -> ""
      | :? System.DBNull -> ""
      | v -> string v
  }

/// Wait for <param condition> to hold, up to ten seconds, or fail saying <param what>.
let private eventually
  (what : string)
  (condition : unit -> Task<bool>)
  : Task<unit> =
  task {
    let deadline = System.DateTime.UtcNow.AddSeconds 10.0
    let rec go () =
      task {
        let! ok = condition ()
        if ok then
          return ()
        elif System.DateTime.UtcNow > deadline then
          return failtest $"never happened: {what}"
        else
          do! Task.Delay 50
          return! go ()
      }
    return! go ()
  }

/// One end of a test session: the store, quick polling, and the lines it would say kept in <param said>.
let private sideOf
  (dir : string)
  (name : string)
  (store : string)
  (keepGoing : unit -> bool)
  (said : System.Collections.Concurrent.ConcurrentQueue<string>)
  : SyncLink.Side =
  { store = store
    workDir = System.IO.Path.Combine(dir, name)
    afterImport = fun _ -> ()
    say = said.Enqueue
    connected = fun _ -> ()
    log =
      fun line ->
        said.Enqueue(
          "log " + System.DateTime.UtcNow.ToString("HH:mm:ss.fff") + " " + line
        )
    keepGoing = keepGoing
    pollMs = 20 }

/// Run a session between <param a> and <param b> over in-process pipes while <param body> runs,
/// then end it. <param stopA> turns A's side off, as `dark sync off` does. Answers how each end ended.
let private linked
  (dir : string)
  (a : string)
  (b : string)
  (stopA : System.Threading.ManualResetEventSlim)
  (body :
    System.Collections.Concurrent.ConcurrentQueue<string>
      -> System.Collections.Concurrent.ConcurrentQueue<string>
      -> Task<unit>)
  : Task<SyncLink.Ended * SyncLink.Ended> =
  task {
    let saidA = System.Collections.Concurrent.ConcurrentQueue<string>()
    let saidB = System.Collections.Concurrent.ConcurrentQueue<string>()
    use done_ = new System.Threading.ManualResetEventSlim(false)
    use aToB =
      new System.IO.Pipes.AnonymousPipeServerStream(
        System.IO.Pipes.PipeDirection.Out
      )
    use bFromA =
      new System.IO.Pipes.AnonymousPipeClientStream(
        System.IO.Pipes.PipeDirection.In,
        aToB.ClientSafePipeHandle
      )
    use bToA =
      new System.IO.Pipes.AnonymousPipeServerStream(
        System.IO.Pipes.PipeDirection.Out
      )
    use aFromB =
      new System.IO.Pipes.AnonymousPipeClientStream(
        System.IO.Pipes.PipeDirection.In,
        bToA.ClientSafePipeHandle
      )
    let endOf
      (side : SyncLink.Side)
      (input : System.IO.Stream)
      (output : System.IO.Stream)
      : Task<SyncLink.Ended> =
      Task.Factory.StartNew(
        (fun () ->
          let ended = SyncLink.run side input output false
          // As a process exiting does: the other end reads the end of its stream.
          output.Dispose()
          ended),
        TaskCreationOptions.LongRunning
      )
    let endA =
      endOf
        (sideOf dir "a" a (fun () -> not stopA.IsSet && not done_.IsSet) saidA)
        aFromB
        aToB
    let endB = endOf (sideOf dir "b" b (fun () -> not done_.IsSet) saidB) bFromA bToA
    let! failed =
      task {
        try
          do! body saidA saidB
          return None
        with e ->
          return Some e
      }
    done_.Set()
    let! endedA = endA
    let! endedB = endB
    match failed with
    | None -> return endedA, endedB
    | Some e ->
      // What each end did, so a failure here says where the exchange stopped.
      let lines (q : System.Collections.Concurrent.ConcurrentQueue<string>) =
        q |> Seq.filter (fun l -> l.StartsWith "log ") |> String.concat "\n  "
      return
        failtest
          $"{e.Message}\nA ended {endedA}:\n  {lines saidA}\nB ended {endedB}:\n  {lines saidB}"
  }

let private noStop () = new System.Threading.ManualResetEventSlim(false)

let private bindingOn (store : string) (name : string) : Task<string> =
  textIn
    store
    $"SELECT item_hash FROM locations
      WHERE owner = 'Tests' AND modules = 'OpSync' AND name = '{name}'
        AND branch_id = '{PT.BranchId.Main}' AND live = 1
      ORDER BY draft DESC LIMIT 1"

let linkedEditArrives =
  testTask "linked: an edit on one store arrives on the other while they are linked" {
    do!
      withTwoStores (fun dir a b ->
        task {
          use stop = noStop ()
          let location : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = "linkedEdit" }
          let! _ =
            linked dir a b stop (fun _ _ ->
              task {
                let! ids =
                  authorOn a "module Tests.OpSync\nlet linkedEdit(): Int64 = 8111"
                do!
                  eventually "the edit reaches the other store" (fun () ->
                    task {
                      let! held = heldCount b ids
                      return held = int64 (List.length ids)
                    })
                do! commitOn a
                do!
                  eventually "its commit follows" (fun () ->
                    task {
                      let! inDraft = draftCount b ids
                      return inDraft = 0L
                    })
              })
          let! found = findOn b location
          Expect.isSome found "the other store binds the name once folded"
        })
  }

let linkedDiscardArrives =
  testTask "linked: a discard on one store removes the edit on the other" {
    do!
      withTwoStores (fun dir a b ->
        task {
          use stop = noStop ()
          let location : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = "linkedDiscard" }
          let! _ =
            linked dir a b stop (fun _ _ ->
              task {
                let! ids =
                  authorOn
                    a
                    "module Tests.OpSync\nlet linkedDiscard(): Int64 = 8117"
                do!
                  eventually "the edit arrives" (fun () ->
                    task {
                      let! held = heldCount b ids
                      return held = int64 (List.length ids)
                    })
                do! discardOn a ids
                do!
                  eventually "the discard arrives" (fun () ->
                    task {
                      let! held = heldCount b ids
                      return held = 0L
                    })
                // A moment more, for anything that would carry it back.
                do! Task.Delay 300
                let! back = heldCount a ids
                Expect.equal
                  back
                  0L
                  "it does not come back to where it was discarded"
              })
          let! found = findOn b location
          Expect.isNone found "the name is unbound on the other store too"
        })
  }

let linkedConflict =
  testTask
    "linked: a name changed on both while apart is a conflict on both, and its resolution travels" {
    do!
      withTwoStores (fun dir a b ->
        task {
          // Unique to this run: the stores are copies of the shared test store, which other tests
          // and earlier runs write to, and ops are content-addressed.
          let run = System.Guid.NewGuid().ToString("N").Substring(0, 8)
          let name = $"both{run}"
          let seed = int64 (System.Random.Shared.Next(1_000_000, 900_000_000))
          let location : PT.PackageLocation =
            { owner = "Tests"; modules = [ "OpSync" ]; name = name }
          // Both start from the same version.
          let! _ = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = {seed}"
          let! _ = sync dir a b
          let! _ = findOn b location
          // Apart: each changes it.
          let! editA =
            authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = {seed + 1L}"
          let! hashA = bindingOn a name
          do! Task.Delay 5
          let! editB =
            authorOn b $"module Tests.OpSync\nlet {name}(): Int64 = {seed + 2L}"
          let! hashB = bindingOn b name
          Expect.notEqual hashA hashB "the two versions differ"

          let pending =
            $"SELECT id FROM conflicts WHERE name = '{name}' AND status = 'pending'"
          use stop = noStop ()
          let! _ =
            linked dir a b stop (fun saidA saidB ->
              task {
                do!
                  eventually "each store receives the other's edit" (fun () ->
                    task {
                      let! onA = heldCount a editB
                      let! onB = heldCount b editA
                      return
                        onA = int64 (List.length editB)
                        && onB = int64 (List.length editA)
                    })
                let! idA = textIn a pending
                let! idB = textIn b pending
                let everything =
                  $"SELECT COALESCE(group_concat(id || ':' || status), 'none') FROM conflicts
                   WHERE name = '{name}'"
                let! allA = textIn a everything
                let! allB = textIn b everything
                let! nowA = bindingOn a name
                let! nowB = bindingOn b name
                Expect.notEqual
                  idA
                  ""
                  $"the store that edited first records it (A: {allA}, B: {allB}; A binds {nowA}, B binds {nowB}; A made {hashA}, B made {hashB})"
                Expect.equal idB idA "the other records the same conflict"
                Expect.isTrue
                  (saidA |> Seq.exists (fun l -> l.Contains $"Tests.OpSync.{name}"))
                  "and says so, in one line"
                Expect.isTrue
                  (saidB |> Seq.exists (fun l -> l.Contains $"Tests.OpSync.{name}"))
                  "on both"

                // Both fold to the same winner: the later edit, B's.
                let! _ = findOn a location
                let! _ = findOn b location
                let! wonA = bindingOn a name
                let! wonB = bindingOn b name
                Expect.equal wonA hashB "A keeps the newer version"
                Expect.equal wonB hashB "and so does B"

                // Resolved on A, for A's version: a decision, which travels as an op.
                useStore a
                let decision =
                  PT.PackageOp.Decision(
                    idA,
                    location,
                    "",
                    PT.DecisionKind.Override(PT.Reference.PackageFn(PT.Hash hashA))
                  )
                let! _ =
                  LibDB.Inserts.authorOn PT.BranchId.Main "resolution" [ decision ]
                let decisionId = [ string (LibDB.Inserts.computeOpHash decision) ]
                do!
                  eventually "the resolution reaches B" (fun () ->
                    task {
                      let! held = heldCount b decisionId
                      return held = 1L
                    })
              })
          let! _ = findOn b location
          let settled = $"SELECT status FROM conflicts WHERE name = '{name}'"
          let! statusA = textIn a settled
          let! statusB = textIn b settled
          Expect.equal statusA "overridden" "settled where it was resolved"
          Expect.equal statusB "overridden" "and on the other store"
          let! nowB = bindingOn b name
          Expect.equal nowB hashA "B binds the version A chose"
        })
  }

let linkedOff =
  testTask "linked: turning sync off ends the session, and nothing travels after" {
    do!
      withTwoStores (fun dir a b ->
        task {
          use stop = noStop ()
          let! (endedA, endedB) =
            linked dir a b stop (fun _ _ ->
              task {
                let! first =
                  authorOn a "module Tests.OpSync\nlet beforeOff(): Int64 = 8123"
                do!
                  eventually "an edit travels while on" (fun () ->
                    task {
                      let! held = heldCount b first
                      return held = int64 (List.length first)
                    })
                stop.Set()
                // A's next poll sees it.
                do! Task.Delay 500
                let! fromA =
                  authorOn a "module Tests.OpSync\nlet afterOffA(): Int64 = 8129"
                let! fromB =
                  authorOn b "module Tests.OpSync\nlet afterOffB(): Int64 = 8147"
                do! Task.Delay 1000
                let! onB = heldCount b fromA
                let! onA = heldCount a fromB
                Expect.equal onB 0L "nothing goes from A after off"
                Expect.equal onA 0L "nothing comes to A after off"
              })
          Expect.equal
            endedA
            SyncLink.Stopped
            "A's end stopped because sync was turned off"
          match endedB with
          | SyncLink.PeerGone _ -> ()
          | other -> failtest $"B's end should see A go, not {other}"
        })
  }

let peerDown =
  testTask
    "a peer that is down or stalled never holds up local work, and is said once" {
    do!
      withTwoStores (fun dir a _ ->
        task {
          for (what, transport) in
            [ "down",
              { SyncLink.program = "sh"; SyncLink.arguments = [ "-c"; "exit 255" ] }
              "stalled",
              { SyncLink.program = "sh"; SyncLink.arguments = [ "-c"; "sleep 30" ] } ] do
            use stop = noStop ()
            let said = System.Collections.Concurrent.ConcurrentQueue<string>()
            let watcher =
              Task.Factory.StartNew(
                (fun () ->
                  SyncLink.watch
                    { side = sideOf dir what a (fun () -> not stop.IsSet) said
                      transport = transport
                      peer = "peer"
                      patience = System.TimeSpan.Zero
                      writeState = fun _ -> () }),
                TaskCreationOptions.LongRunning
              )
            do! Task.Delay 300
            let clock = System.Diagnostics.Stopwatch.StartNew()
            let! ids =
              authorOn a $"module Tests.OpSync\nlet whilePeer{what}(): Int64 = 8161"
            do! commitOn a
            let! held = heldCount a ids
            Expect.equal
              held
              (int64 (List.length ids))
              $"local work lands with the peer {what}"
            Expect.isLessThan
              clock.ElapsedMilliseconds
              10000L
              $"and is not held up ({what})"
            if what = "down" then
              // Several attempts by now; out of reach is said once, not per attempt.
              do! Task.Delay 2000
              let lines =
                said
                |> Seq.filter (fun l -> l.Contains "can't reach peer")
                |> Seq.length
              Expect.equal lines 1 "out of reach is said once"
            let stopped = System.Diagnostics.Stopwatch.StartNew()
            stop.Set()
            let! _ = watcher
            Expect.isLessThan
              stopped.ElapsedMilliseconds
              3000L
              $"turning off stops it at once ({what})"
        })
  }

let tests =
  testSequenced
  <| testList
    "OpSync"
    [ nothingToSend
      draftThenCommit
      discardTravels
      commitThenDiscard
      importTwice
      mergeTravels
      storeWithoutJournal
      wrongFile
      linkedEditArrives
      linkedDiscardArrives
      linkedConflict
      linkedOff
      peerDown ]
