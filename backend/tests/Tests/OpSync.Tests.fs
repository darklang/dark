/// Tests for `LibDB.OpSync`: two copies of one store, an edit or a removal on one, and the other
/// catching up.
module Tests.OpSync

open Expecto

open System.Threading.Tasks


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
let private withTwoStores (run : string -> string -> string -> unit) =
  let directory = System.IO.Directory.CreateTempSubdirectory("op-sync-")
  let original = LibDB.Sqlite.currentDbPath
  let a = System.IO.Path.Combine(directory.FullName, "a.db")
  let b = System.IO.Path.Combine(directory.FullName, "b.db")
  for path in [ a; b ] do
    match LibDB.Sqlite.Backup.toTestBaseline path with
    | Error message -> failtest message
    | Ok() -> ()
  try
    run directory.FullName a b
  finally
    Sql.useStoreForTesting original
    LibDB.Caching.invalidateAll ()
    try
      directory.Delete true
    with _ ->
      ()

let private useStore (path : string) : unit =
  Sql.useStoreForTesting path
  LibDB.Caching.invalidateAll ()

/// Send what <param fromStore> has and <param toStore> lacks, the three steps in order.
let private sync
  (dir : string)
  (fromStore : string)
  (toStore : string)
  : OpSync.Counts * OpSync.Counts =
  let have = System.IO.Path.Combine(dir, "have.db")
  let bundle = System.IO.Path.Combine(dir, "bundle.db")
  match OpSync.writeHave toStore have with
  | Error reason -> failtest reason
  | Ok() -> ()
  let sent = OpSync.export fromStore have bundle
  let received = OpSync.import toStore bundle
  ok sent, ok received

/// A number from <param store>, with `$ids` bound to <param ids>.
let private countIn
  (store : string)
  (sql : string)
  (ids : List<string>)
  : int64 =
  use conn =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={store};Pooling=false"
    )
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  cmd.Parameters.AddWithValue("$ids", jsonStrings ids)
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  let n = cmd.ExecuteScalar()
  unbox<int64> n

let private draftCount (store : string) (ids : List<string>) : int64 =
  countIn
    store
    "SELECT COUNT(*) FROM package_ops
     WHERE id IN (SELECT value FROM json_each($ids)) AND commit_hash IS NULL"
    ids

/// How many of <param ids> <param store> holds, on any branch, in a commit or not.
let private heldCount (store : string) (ids : List<string>) : int64 =
  countIn
    store
    "SELECT COUNT(DISTINCT id) FROM package_ops WHERE id IN (SELECT value FROM json_each($ids))"
    ids

/// Make <param store> one from before the journal: no triggers, no table.
let private forgetJournal (store : string) : unit =
  use conn =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={store};Pooling=false"
    )
  conn.Open()
  use read = conn.CreateCommand()
  read.CommandText <-
    "SELECT name FROM sqlite_master WHERE type = 'trigger' AND name LIKE 'sync_journal_%'"
  use reader = read.ExecuteReader()
  let triggers = LibDB.Sqlite.readRows (fun r -> r.GetString 0) reader
  reader.Close()
  for statement in
    (triggers |> List.map (fun t -> $"DROP TRIGGER {t}"))
    @ [ "DROP TABLE sync_journal"; "DROP TABLE sync_replaying" ] do
    use cmd = conn.CreateCommand()
    cmd.CommandText <- statement
    let _ = cmd.ExecuteNonQuery()
    ()

let private rowCount (store : string) (table : string) : int64 =
  countIn store $"SELECT COUNT(*) FROM {table}" []

/// A text from <param store>.
let private textIn (store : string) (sql : string) : string =
  use conn =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={store};Pooling=false"
    )
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- sql
  let v = cmd.ExecuteScalar()
  match v with
  | null -> ""
  | :? System.DBNull -> ""
  | v -> string v

/// Author <param source> into main's draft on <param store> as the CLI does, and the ids of what
/// it wrote. Each binding names the version it replaced (`SCM.Lineage`), which the parser alone
/// cannot know.
let private authorOn (store : string) (source : string) : List<string> =
  useStore store
  let parsed = (parsePackageOps source).Result
  let rec lineage (ops : List<PT.PackageOp>) : List<PT.PackageOp> =
    match ops with
    | [] -> []
    | PT.PackageOp.SetName(loc, target, None) :: rest ->
      let modules = String.concat "." loc.modules
      let held =
        textIn
          store
          $"SELECT item_hash FROM locations
            WHERE owner = '{loc.owner}' AND modules = '{modules}'
              AND name = '{loc.name}' AND item_type = '{target.kind.toString ()}'
              AND branch_id = '{PT.BranchId.Main}' AND live = 1
            ORDER BY draft DESC LIMIT 1"
      let previous = if held = "" then None else Some(PT.Hash held)
      let rest = lineage rest
      PT.PackageOp.SetName(loc, target, previous) :: rest
    | op :: rest ->
      let rest = lineage rest
      op :: rest
  let ops = lineage parsed
  LibDB.Queries.ensureAuthoringClockIsCurrent ()
  let names = LibDB.ReferenceNames.captureForStorage PT.BranchId.Main ops
  let _ = LibDB.Inserts.insertAndApplyOps (ops @ names)
  let _ = LibDB.WipRefresh.refresh pmPT
  ops |> List.map (LibDB.Inserts.computeOpHash >> string)

let private discardOn (store : string) (ids : List<string>) : unit =
  useStore store
  match LibDB.ScmActions.discard PT.BranchId.Main (LibDB.ScmActions.Ops ids) with
  | Error e -> failtest $"discard failed: {e}"
  | Ok _ -> ()

let private commitOn (store : string) : unit =
  useStore store
  match
    LibDB.ScmActions.commit PT.BranchId.Main "sync" LibDB.ScmActions.Whole false
  with
  | Error e -> failtest $"commit failed: {e}"
  | Ok _ -> ()

let private findOn (store : string) (location : PT.PackageLocation) =
  useStore store
  let _ = LibDB.Seed.applyUnappliedOps ()
  pmPT.findFn location

/// Sync both ways until neither side has anything to send, and say how many rounds that took.
let private settle (dir : string) (a : string) (b : string) : int =
  let rec go (round : int) =
    let (toB, _) = sync dir a b
    let (toA, _) = sync dir b a
    let quiet (c : OpSync.Counts) =
      c.ops = 0L && c.commits = 0L && c.blobs = 0L && c.journal = 0L
    if quiet toB && quiet toA then round
    elif round >= 4 then failtest "the two stores never agree"
    else go (round + 1)
  go 1

let nothingToSend =
  testTask "a store that holds everything is sent nothing" {
    withTwoStores (fun dir a b ->
      let (sent, received) = sync dir a b
      Expect.equal sent.ops 0L "no ops to send"
      Expect.equal sent.commits 0L "no commits to send"
      Expect.equal sent.blobs 0L "no blobs to send"
      Expect.equal received.ops 0L "no ops arrive")
  }

let draftThenCommit =
  testTask "an edit on one store arrives on the other once, and then its commit does" {
    withTwoStores (fun dir a b ->
      let location : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = "answer" }

      useStore a
      let authored =
        (authorIntoMain "module Tests.OpSync\nlet answer(): Int64 = 4217").Result
      let ids = authored |> List.map (LibDB.Inserts.computeOpHash >> string)

      let (sent, received) = sync dir a b
      Expect.isGreaterThanOrEqual
        sent.ops
        (int64 (List.length ids))
        "the edit is sent"
      Expect.equal received.ops sent.ops "every op sent is new there"
      Expect.equal received.committed 0L "nothing was committed"

      let (_, again) = sync dir a b
      Expect.equal again.ops 0L "a second sync sends nothing new"

      useStore b
      let _ = LibDB.Seed.applyUnappliedOps ()
      let found = pmPT.findFn location
      Expect.isSome found "the other store binds the name once the fold has run"
      let inDraft = draftCount b ids
      Expect.equal inDraft (int64 (List.length ids)) "it arrives in the draft"

      useStore a
      match
        LibDB.ScmActions.commit
          PT.BranchId.Main
          "sync"
          LibDB.ScmActions.Whole
          false
      with
      | Error e -> failtest $"commit failed: {e}"
      | Ok _ -> ()

      let (sent, received) = sync dir a b
      Expect.isGreaterThanOrEqual sent.commits 1L "the commit is sent"
      Expect.isGreaterThanOrEqual
        received.committed
        (int64 (List.length ids))
        "the other store's copies join it"
      Expect.equal received.ops 0L "no op arrives twice"
      let inDraft = draftCount b ids
      Expect.equal inDraft 0L "none is left in the other store's draft"

      let (sent, _) = sync dir a b
      Expect.equal sent.ops 0L "then the two agree"

      useStore b
      let found = pmPT.findFn location
      Expect.isSome found "the name is still bound once committed")
  }

let discardTravels =
  testTask
    "a discard on one store removes the op on the other, and it does not come back" {
    withTwoStores (fun dir a b ->
      let location : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = "discarded" }
      let ids = authorOn a "module Tests.OpSync\nlet discarded(): Int64 = 5381"
      let _ = sync dir a b
      let found = findOn b location
      Expect.isSome found "the edit arrived"

      discardOn a ids

      // The other way first: that store still holds the op, and this one must not take it back.
      let (_, back) = sync dir b a
      Expect.equal back.ops 0L "the discarded op is not taken back"
      let held = heldCount a ids
      Expect.equal held 0L "it stays discarded where it was discarded"

      let (sent, received) = sync dir a b
      Expect.isGreaterThanOrEqual sent.journal 1L "the discard is sent"
      Expect.equal
        received.dropped
        (int64 (List.length ids))
        "every discarded op goes over there"
      let held = heldCount b ids
      Expect.equal held 0L "the other store holds none of them"
      let found = findOn b location
      Expect.isNone found "and the name is unbound there too"

      let _ = settle dir a b
      let held = heldCount a ids
      Expect.equal held 0L "after syncing both ways it is still gone")
  }

let commitThenDiscard =
  testTask "a commit and then a discard leave both stores the same" {
    withTwoStores (fun dir a b ->
      let kept : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = "kept" }
      let dropped : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = "dropped" }
      let keptIds = authorOn a "module Tests.OpSync\nlet kept(): Int64 = 7919"
      commitOn a
      let droppedIds =
        authorOn a "module Tests.OpSync\nlet dropped(): Int64 = 104729"
      let _ = sync dir a b
      discardOn a droppedIds

      let rounds = settle dir a b
      Expect.isLessThanOrEqual rounds 2 "they agree after one exchange each way"

      for store in [ a; b ] do
        let held = heldCount store keptIds
        Expect.equal
          held
          (int64 (List.length keptIds))
          "the committed edit is there"
        let inDraft = draftCount store keptIds
        Expect.equal inDraft 0L "and committed"
        let gone = heldCount store droppedIds
        Expect.equal gone 0L "the discarded edit is not"
        let found = findOn store kept
        Expect.isSome found "the committed name is bound"
        let found = findOn store dropped
        Expect.isNone found "the discarded one is not"

      let journalA = rowCount a "sync_journal"
      let journalB = rowCount b "sync_journal"
      Expect.equal journalA journalB "both hold the same journal")
  }

let importTwice =
  testTask "importing the same bundle twice changes nothing the second time" {
    withTwoStores (fun dir a b ->
      let ids = authorOn a "module Tests.OpSync\nlet twice(): Int64 = 6007"
      let _ = sync dir a b
      discardOn a ids
      let _ = authorOn a "module Tests.OpSync\nlet twiceAgain(): Int64 = 6011"

      let have = System.IO.Path.Combine(dir, "have.db")
      let bundle = System.IO.Path.Combine(dir, "bundle.db")
      match OpSync.writeHave b have with
      | Error reason -> failtest reason
      | Ok() -> ()
      let _ = OpSync.export a have bundle
      let first = OpSync.import b bundle
      let first = ok first
      Expect.isGreaterThanOrEqual first.dropped 1L "the first import drops"
      Expect.isGreaterThanOrEqual first.ops 1L "and adds"

      let opsBefore = rowCount b "package_ops"
      let journalBefore = rowCount b "sync_journal"
      let second = OpSync.import b bundle
      let second = ok second
      Expect.equal
        second
        { first with
            ops = 0L
            folded = 0L
            values = 0L
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
      let opsAfter = rowCount b "package_ops"
      let journalAfter = rowCount b "sync_journal"
      Expect.equal opsAfter opsBefore "no op added or removed"
      Expect.equal journalAfter journalBefore "no entry added")
  }

let storeWithoutJournal =
  testTask "a store that keeps no journal is refused, and nothing changes" {
    withTwoStores (fun dir a b ->
      let _ = authorOn a "module Tests.OpSync\nlet unsent(): Int64 = 3571"
      let opsBefore = rowCount b "package_ops"
      forgetJournal a

      let have = System.IO.Path.Combine(dir, "have.db")
      let bundle = System.IO.Path.Combine(dir, "bundle.db")
      match OpSync.writeHave b have with
      | Error reason -> failtest reason
      | Ok() -> ()
      let sent = OpSync.export a have bundle
      Expect.isError sent "it cannot say what it removed, so it sends nothing"
      Expect.isFalse (System.IO.File.Exists bundle) "and writes no bundle"

      let asReceiver = OpSync.writeHave a have
      Expect.isError asReceiver "nor can it receive"

      let opsAfter = rowCount b "package_ops"
      Expect.equal opsAfter opsBefore "the other store is unchanged")
  }

let mergeTravels =
  testTask
    "a merge on one store moves the branch's ops on the other, and marks it merged" {
    withTwoStores (fun dir a b ->
      let location : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = "merged" }
      useStore a
      let branch = PT.BranchId.Id(System.Guid.NewGuid())
      LibDB.Branches.createBranch branch "op-sync-merge" PT.BranchId.Main
      let ops =
        (parsePackageOps "module Tests.OpSync\nlet merged(): Int64 = 2207").Result
      let _ = LibDB.Inserts.authorOn branch "op" ops
      let _ = sync dir a b

      useStore a
      match
        LibDB.ScmActions.merge branch [] (fun _ -> ()) (fun () ->
          ())
      with
      | Error _ -> failtest "the merge failed"
      | Ok _ -> ()
      let _ = settle dir a b

      let onBranch =
        $"SELECT COUNT(*) FROM package_ops WHERE branch_id = '{branch}'"
      let isMerged =
        $"SELECT COUNT(*) FROM branches WHERE id = '{branch}' AND merged_at IS NOT NULL"
      for store in [ a; b ] do
        let left = countIn store onBranch []
        Expect.equal left 0L "nothing is left on the merged branch"
        let merged = countIn store isMerged []
        Expect.equal merged 1L "the branch is marked merged"
        let found = findOn store location
        Expect.isSome found "main binds the name")
  }

let wrongFile =
  testTask "a file that is not a bundle is refused" {
    withTwoStores (fun _ a b ->
      let result = OpSync.import b a
      Expect.isError result "a store is not a bundle")
  }

// ------------------
// Two stores linked by a session (`LibDB.SyncLink`), as two machines are over ssh
// ------------------

module SyncLink = LibDB.SyncLink

/// Wait for <param condition> to hold, up to ten seconds, or fail saying <param what>.
let private eventually
  (what : string)
  (condition : unit -> bool)
  : unit =
  let deadline = System.DateTime.UtcNow.AddSeconds 10.0
  let rec go () =
    let ok = condition ()
    if ok then
      ()
    elif System.DateTime.UtcNow > deadline then
      failtest $"never happened: {what}"
    else
      System.Threading.Thread.Sleep 50
      go ()
  go ()

/// Where the end called <param name> records how its link stands, as the watcher does.
let private standingOf (dir : string) (name : string) : string =
  System.IO.Path.Combine(dir, name, "state")

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
    note = SyncLink.Standing.update (standingOf dir name)
    log =
      fun line ->
        said.Enqueue(
          "log " + System.DateTime.UtcNow.ToString("HH:mm:ss.fff") + " " + line
        )
    keepGoing = keepGoing
    pollMs = 20
    quietMs = 5000 }

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
      -> unit)
  : SyncLink.Ended * SyncLink.Ended =
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
  let failed =
    try
      body saidA saidB
      None
    with e ->
      Some e
  done_.Set()
  let endedA = endA.Result
  let endedB = endB.Result
  match failed with
  | None -> endedA, endedB
  | Some e ->
    // What each end did, so a failure here says where the exchange stopped.
    let lines (q : System.Collections.Concurrent.ConcurrentQueue<string>) =
      q |> Seq.filter (fun l -> l.StartsWith "log ") |> String.concat "\n  "
    failtest
      $"{e.Message}\nA ended {endedA}:\n  {lines saidA}\nB ended {endedB}:\n  {lines saidB}"

let private noStop () = new System.Threading.ManualResetEventSlim(false)

let private bindingOn (store : string) (name : string) : string =
  textIn
    store
    $"SELECT item_hash FROM locations
      WHERE owner = 'Tests' AND modules = 'OpSync' AND name = '{name}'
        AND branch_id = '{PT.BranchId.Main}' AND live = 1
      ORDER BY draft DESC LIMIT 1"

let linkedEditArrives =
  testTask "linked: an edit on one store arrives on the other while they are linked" {
    withTwoStores (fun dir a b ->
      use stop = noStop ()
      let location : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = "linkedEdit" }
      let _ =
        linked dir a b stop (fun _ _ ->
          let ids =
            authorOn a "module Tests.OpSync\nlet linkedEdit(): Int64 = 8111"
          eventually "the edit reaches the other store" (fun () ->
            let held = heldCount b ids
            held = int64 (List.length ids))
          commitOn a
          eventually "its commit follows" (fun () ->
            let inDraft = draftCount b ids
            inDraft = 0L))
      let found = findOn b location
      Expect.isSome found "the other store binds the name once folded")
  }

let linkedDiscardArrives =
  testTask "linked: a discard on one store removes the edit on the other" {
    withTwoStores (fun dir a b ->
      use stop = noStop ()
      let location : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = "linkedDiscard" }
      let _ =
        linked dir a b stop (fun _ _ ->
          let ids =
            authorOn
              a
              "module Tests.OpSync\nlet linkedDiscard(): Int64 = 8117"
          eventually "the edit arrives" (fun () ->
            let held = heldCount b ids
            held = int64 (List.length ids))
          discardOn a ids
          eventually "the discard arrives" (fun () ->
            let held = heldCount b ids
            held = 0L)
          // A moment more, for anything that would carry it back.
          System.Threading.Thread.Sleep 300
          let back = heldCount a ids
          Expect.equal
            back
            0L
            "it does not come back to where it was discarded")
      let found = findOn b location
      Expect.isNone found "the name is unbound on the other store too")
  }

let linkedConflict =
  testTask
    "linked: a name changed on both while apart is a conflict on both, and its resolution travels" {
    withTwoStores (fun dir a b ->
      // Unique to this run: the stores are copies of the shared test store, which other tests
      // and earlier runs write to, and ops are content-addressed.
      let run = System.Guid.NewGuid().ToString("N").Substring(0, 8)
      let name = $"both{run}"
      let seed = int64 (System.Random.Shared.Next(1_000_000, 900_000_000))
      let location : PT.PackageLocation =
        { owner = "Tests"; modules = [ "OpSync" ]; name = name }
      // Both start from the same version.
      let _ = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = {seed}"
      let _ = sync dir a b
      let _ = findOn b location
      // Apart: each changes it.
      let editA =
        authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = {seed + 1L}"
      let hashA = bindingOn a name
      System.Threading.Thread.Sleep 5
      let editB =
        authorOn b $"module Tests.OpSync\nlet {name}(): Int64 = {seed + 2L}"
      let hashB = bindingOn b name
      Expect.notEqual hashA hashB "the two versions differ"

      let pending =
        $"SELECT id FROM conflicts WHERE name = '{name}' AND status = 'pending'"
      use stop = noStop ()
      let _ =
        linked dir a b stop (fun saidA saidB ->
          eventually "each store receives the other's edit" (fun () ->
            let onA = heldCount a editB
            let onB = heldCount b editA
            onA = int64 (List.length editB)
            && onB = int64 (List.length editA))
          let idA = textIn a pending
          let idB = textIn b pending
          let everything =
            $"SELECT COALESCE(group_concat(id || ':' || status), 'none') FROM conflicts
             WHERE name = '{name}'"
          let allA = textIn a everything
          let allB = textIn b everything
          let nowA = bindingOn a name
          let nowB = bindingOn b name
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
          let _ = findOn a location
          let _ = findOn b location
          let wonA = bindingOn a name
          let wonB = bindingOn b name
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
          let _ =
            LibDB.Inserts.authorOn PT.BranchId.Main "resolution" [ decision ]
          let decisionId = [ string (LibDB.Inserts.computeOpHash decision) ]
          eventually "the resolution reaches B" (fun () ->
            let held = heldCount b decisionId
            held = 1L))
      let _ = findOn b location
      let settled = $"SELECT status FROM conflicts WHERE name = '{name}'"
      let statusA = textIn a settled
      let statusB = textIn b settled
      Expect.equal statusA "overridden" "settled where it was resolved"
      Expect.equal statusB "overridden" "and on the other store"
      let nowB = bindingOn b name
      Expect.equal nowB hashA "B binds the version A chose")
  }

let linkedOff =
  testTask "linked: turning sync off ends the session, and nothing travels after" {
    withTwoStores (fun dir a b ->
      use stop = noStop ()
      let (endedA, endedB) =
        linked dir a b stop (fun _ _ ->
          let first =
            authorOn a "module Tests.OpSync\nlet beforeOff(): Int64 = 8123"
          eventually "an edit travels while on" (fun () ->
            let held = heldCount b first
            held = int64 (List.length first))
          stop.Set()
          // A's next poll sees it.
          System.Threading.Thread.Sleep 500
          let fromA =
            authorOn a "module Tests.OpSync\nlet afterOffA(): Int64 = 8129"
          let fromB =
            authorOn b "module Tests.OpSync\nlet afterOffB(): Int64 = 8147"
          System.Threading.Thread.Sleep 1000
          let onB = heldCount b fromA
          let onA = heldCount a fromB
          Expect.equal onB 0L "nothing goes from A after off"
          Expect.equal onA 0L "nothing comes to A after off")
      Expect.equal
        endedA
        SyncLink.Stopped
        "A's end stopped because sync was turned off"
      match endedB with
      | SyncLink.PeerGone _ -> ()
      | other -> failtest $"B's end should see A go, not {other}")
  }

let peerDown =
  testTask
    "a peer that is down or stalled never holds up local work, and is said once" {
    withTwoStores (fun dir a _ ->
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
                  mention = said.Enqueue }),
            TaskCreationOptions.LongRunning
          )
        System.Threading.Thread.Sleep 300
        let clock = System.Diagnostics.Stopwatch.StartNew()
        let ids =
          authorOn a $"module Tests.OpSync\nlet whilePeer{what}(): Int64 = 8161"
        commitOn a
        let held = heldCount a ids
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
          System.Threading.Thread.Sleep 2000
          let lines =
            said
            |> Seq.filter (fun l -> l.Contains "can't reach peer")
            |> Seq.length
          Expect.equal lines 1 "out of reach is said once"
        let stopped = System.Diagnostics.Stopwatch.StartNew()
        stop.Set()
        watcher.Wait()
        Expect.isLessThan
          stopped.ElapsedMilliseconds
          3000L
          $"turning off stops it at once ({what})")
  }

// ------------------
// Conflicts over a removal or a rename, and a resolution that comes first
// ------------------

/// <param prefix> and something no other run uses: the stores are copies of the shared test store,
/// which earlier runs wrote to, and ops are content-addressed.
let private unique (prefix : string) : string =
  prefix + System.Guid.NewGuid().ToString("N").Substring(0, 8)

/// A name <param name> bound on both stores to the same version: its location, and the version.
let private shared
  (dir : string)
  (a : string)
  (b : string)
  (name : string)
  : PT.PackageLocation * string =
  let seed = int64 (System.Random.Shared.Next(1_000_000, 900_000_000))
  let location : PT.PackageLocation =
    { owner = "Tests"; modules = [ "OpSync" ]; name = name }
  let _ = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = {seed}"
  let _ = sync dir a b
  let _ = findOn b location
  let h0 =
    textIn
      a
      $"SELECT item_hash FROM locations
        WHERE owner = 'Tests' AND modules = 'OpSync' AND name = '{name}'
          AND branch_id = '{PT.BranchId.Main}' AND live = 1
        ORDER BY draft DESC LIMIT 1"
  location, h0

let private conflictsOn (store : string) (name : string) : string =
  textIn
    store
    $"SELECT COALESCE(group_concat(id || ':' || kind || ':' || status, ','), '') FROM conflicts
      WHERE name = '{name}'"

let removedVsEdited =
  testTask
    "a name changed on one store and removed on the other is a conflict on both, with one id" {
    withTwoStores (fun dir a b ->
      let name = unique "gone"
      let (location, h0) = shared dir a b name
      let _ = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = 71"
      System.Threading.Thread.Sleep 5
      useStore b
      let _ =
        LibDB.Inserts.authorOn
          PT.BranchId.Main
          "op"
          [ PT.PackageOp.Unbind(location, Some(PT.Hash h0)) ]
      let (_, onB) = sync dir a b
      let (_, onA) = sync dir b a
      let recordedA = conflictsOn a name
      let recordedB = conflictsOn b name
      Expect.stringContains recordedA "removed-vs-edited:pending" "A records it"
      Expect.equal recordedB recordedA "B records the same one"
      Expect.equal (List.length onA.conflicts) 1 "A is told once"
      Expect.equal (List.length onB.conflicts) 1 "and so is B"
      let line = SyncLink.conflictLine onB.conflicts.Head
      Expect.stringContains
        line
        "removed on this machine"
        "B's line says it removed it"
      // The removal is newer, so both lose the edit, and both say so.
      for store in [ a; b ] do
        let found = findOn store location
        Expect.isNone found "the newer removal stands on both")
  }

let renamedVsEdited =
  testTask
    "a name changed on one store and renamed on the other is a conflict on both, naming where it went" {
    withTwoStores (fun dir a b ->
      let name = unique "moved"
      let (location, h0) = shared dir a b name
      let _ = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = 73"
      System.Threading.Thread.Sleep 5
      useStore b
      let newLocation = { location with name = name + "New" }
      let _ =
        LibDB.Inserts.authorOn
          PT.BranchId.Main
          "op"
          [ PT.PackageOp.Rename(
              location,
              newLocation,
              PT.Reference.PackageFn(PT.Hash h0),
              [],
              "test"
            ) ]
      let (_, onB) = sync dir a b
      let (_, onA) = sync dir b a
      let recordedA = conflictsOn a name
      let recordedB = conflictsOn b name
      Expect.stringContains recordedA "removed-vs-edited:pending" "A records it"
      Expect.equal recordedB recordedA "B records the same one"
      let line = SyncLink.conflictLine onA.conflicts.Head
      Expect.stringContains
        line
        $"renamed to Tests.OpSync.{name}New"
        "A's line says where the rename took it"
      Expect.stringContains line "the rename stands" "the newer rename stands"
      Expect.equal (List.length onB.conflicts) 1 "B is told too"
      for store in [ a; b ] do
        let found = findOn store location
        Expect.isNone found "the old name is gone on both"
        let moved = findOn store newLocation
        Expect.isSome moved "and the new one is bound")
  }

let editAfterRemoval =
  testTask
    "an edit made after the other store removed the name, on the version it removed, is a conflict on both" {
    withTwoStores (fun dir a b ->
      let name = unique "later"
      let (location, h0) = shared dir a b name
      useStore b
      let _ =
        LibDB.Inserts.authorOn
          PT.BranchId.Main
          "op"
          [ PT.PackageOp.Unbind(location, Some(PT.Hash h0)) ]
      // Newer than the removal, so only the version it replaced says it never saw it.
      System.Threading.Thread.Sleep 5
      let _ = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = 75"
      let (_, onB) = sync dir a b
      let (_, onA) = sync dir b a
      let recordedA = conflictsOn a name
      let recordedB = conflictsOn b name
      Expect.stringContains recordedB "removed-vs-edited:pending" "B records it"
      Expect.equal recordedA recordedB "A records the same one"
      Expect.equal (List.length onA.conflicts) 1 "A is told once"
      Expect.equal (List.length onB.conflicts) 1 "and so is B"
      for store in [ a; b ] do
        let found = findOn store location
        Expect.isSome found "the newer edit stands on both")
  }

let recreatedAfterRemoval =
  testTask
    "a name made again after its removal arrived is no conflict on either store" {
    withTwoStores (fun dir a b ->
      let name = unique "again"
      let (location, h0) = shared dir a b name
      useStore b
      let _ =
        LibDB.Inserts.authorOn
          PT.BranchId.Main
          "op"
          [ PT.PackageOp.Unbind(location, Some(PT.Hash h0)) ]
      let _ = sync dir b a
      let _ = findOn a location
      let _ = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = 77"
      let (_, onB) = sync dir a b
      let recordedA = conflictsOn a name
      let recordedB = conflictsOn b name
      Expect.equal recordedA "" "A records nothing"
      Expect.equal recordedB "" "nor does B"
      Expect.isEmpty onB.conflicts "and B is told nothing"
      let found = findOn b location
      Expect.isSome found "the name is bound again on B")
  }

let resolutionFirst =
  testTask
    "a resolution that arrives before its conflict keeps it settled once it appears" {
    withTwoStores (fun dir a b ->
      let name = unique "first"
      let (location, _) = shared dir a b name
      let editA = authorOn a $"module Tests.OpSync\nlet {name}(): Int64 = 81"
      let hashA = bindingOn a name
      System.Threading.Thread.Sleep 5
      let _ = authorOn b $"module Tests.OpSync\nlet {name}(): Int64 = 82"
      let hashB = bindingOn b name
      // Decided elsewhere, for B's version, and sent to B before A's edit is.
      let id = LibDB.ScmDetect.conflictId "Tests" "OpSync" name [ hashA; hashB ]
      let before = OpSync.cursorOf a
      useStore a
      let _ =
        LibDB.Inserts.authorOn
          PT.BranchId.Main
          "resolution"
          [ PT.PackageOp.Decision(
              id,
              location,
              "",
              PT.DecisionKind.Override(PT.Reference.PackageFn(PT.Hash hashB))
            ) ]
      let bundle = System.IO.Path.Combine(dir, "decision.db")
      let _ = OpSync.exportSince a before "the other store" bundle
      let _ = OpSync.import b bundle
      let early = heldCount b editA
      Expect.equal early 0L "the decision arrives without A's edit"
      let settledFirst =
        textIn
          b
          $"SELECT COALESCE(MAX(status), '') FROM conflicts WHERE id = '{id}'"
      Expect.equal
        settledFirst
        "overridden"
        "B keeps the decision with nothing to settle yet"

      let (_, onB) = sync dir a b
      let held = heldCount b editA
      Expect.equal held (int64 (List.length editA)) "A's edit arrives after it"
      Expect.isEmpty onB.conflicts "and is not announced as a new conflict"
      let status = textIn b $"SELECT status FROM conflicts WHERE id = '{id}'"
      Expect.equal status "overridden" "it is settled on B"
      let candidates =
        textIn b $"SELECT candidates FROM conflicts WHERE id = '{id}'"
      Expect.stringContains candidates hashA "and now names both versions"
      let _ = findOn b location
      let bound = bindingOn b name
      Expect.equal bound hashB "B binds the version the decision chose")
  }

// ------------------
// Sessions after the first, and the first command after a restart
// ------------------

let resumeFromAgreedPoint =
  testTask
    "linked: a second session starts from where the first ended, sends no have file, and trims" {
    withTwoStores (fun dir a b ->
      use stop = noStop ()
      let _ =
        linked dir a b stop (fun _ _ ->
          let ids =
            authorOn a "module Tests.OpSync\nlet resumed(): Int64 = 9001"
          eventually "the edit reaches B" (fun () ->
            let held = heldCount b ids
            held = int64 (List.length ids)))
      // Apart: A commits, which replaces each op's journal entry with one naming the commit.
      let later =
        authorOn a "module Tests.OpSync\nlet resumedLater(): Int64 = 9002"
      commitOn a
      let replaced =
        "SELECT COUNT(*) FROM (SELECT key FROM sync_journal GROUP BY key HAVING COUNT(*) > 1)"
      let replacedBefore = countIn a replaced []
      Expect.isGreaterThan
        replacedBefore
        0L
        "A's journal holds replaced entries"
      use stop2 = noStop ()
      let _ =
        linked dir a b stop2 (fun saidA saidB ->
          eventually "the later edit and the commit reach B" (fun () ->
            let inDraft = draftCount b later
            let held = heldCount b later
            held = int64 (List.length later) && inDraft = 0L)
          eventually "A trims what B holds" (fun () ->
            let left = countIn a replaced []
            left = 0L)
          let lines
            (q : System.Collections.Concurrent.ConcurrentQueue<string>)
            =
            q |> Seq.filter (fun l -> l.StartsWith "log ") |> List.ofSeq
          for (who, said) in [ "A", saidA; "B", saidB ] do
            Expect.isTrue
              (lines said
               |> List.exists (fun l -> l.Contains "resuming from"))
              $"{who} resumes"
            Expect.isFalse
              (lines said |> List.exists (fun l -> l.Contains "have of"))
              $"{who} sends no have file"
          // B made nothing, and what it holds of A's does not go back to A.
          Expect.isFalse
            (lines saidB |> List.exists (fun l -> l.Contains "sent"))
            "B sends A nothing")
      ())
  }

let firstCommandWaits =
  testTask
    "linked: the first command after a restart waits for the other store's changes, and finds them" {
    withTwoStores (fun dir a b ->
      let name = unique "waited"
      let _ = authorOn b $"module Tests.OpSync\nlet {name}(): Int64 = 9101"
      let madeOnB = bindingOn b name
      use stop = noStop ()
      let started = System.DateTime.UtcNow
      let _ =
        linked dir a b stop (fun _ _ ->
          let waited =
            SyncLink.awaitCatchUp
              (fun () -> SyncLink.Standing.read (standingOf dir "a"))
              started
              (System.TimeSpan.FromSeconds 5.0)
          Expect.equal
            waited
            SyncLink.CaughtUp
            "the catch-up lands within the wait"
          let onA = bindingOn a name
          Expect.equal
            onA
            madeOnB
            "and the command finds B's edit already folded")
      ())
  }

/// Give <param store> its own `instance.id`: the test stores are copies of one, and a hub tells its
/// clients apart by it.
let private nameStore (store : string) (id : string) : unit =
  use conn =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={store};Pooling=false"
    )
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <-
    "INSERT INTO config_v0 (key, value) VALUES ('instance.id', $id)
     ON CONFLICT(key) DO UPDATE SET value = excluded.value"
  cmd.Parameters.AddWithValue("$id", id)
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  let _ = cmd.ExecuteNonQuery()
  ()

let hubWithTwoClients =
  testTask
    "linked: two clients of one hub, never linked to each other, each get the other's edit through it" {
    withTwoStores (fun dir a hub ->
      let c = System.IO.Path.Combine(dir, "c.db")
      match LibDB.Sqlite.Backup.toTestBaseline c with
      | Error message -> failtest message
      | Ok() -> ()
      nameStore a "client-a"
      nameStore hub "hub"
      nameStore c "client-c"
      let second = System.IO.Path.Combine(dir, "second")
      System.IO.Directory.CreateDirectory second
      |> ignore<System.IO.DirectoryInfo>
      use stopA = noStop ()
      use stopC = noStop ()
      let _ =
        linked dir a hub stopA (fun _ _ ->
          let _ =
            linked second c hub stopC (fun _ _ ->
              let fromA =
                authorOn
                  a
                  "module Tests.OpSync\nlet viaHubA(): Int64 = 9201"
              let fromC =
                authorOn
                  c
                  "module Tests.OpSync\nlet viaHubC(): Int64 = 9202"
              eventually "A's edit reaches C through the hub" (fun () ->
                let held = heldCount c fromA
                held = int64 (List.length fromA))
              eventually "and C's reaches A" (fun () ->
                let held = heldCount a fromC
                held = int64 (List.length fromC)))
          ())
      let peers =
        "SELECT group_concat(id, ',') FROM (SELECT id FROM sync_peers ORDER BY id)"
      let onHub = textIn hub peers
      Expect.equal
        onHub
        "client-a,client-c"
        "the hub keeps a point for each client")
  }

let firstCommandOutOfReach =
  testTask
    "the first command after a restart waits only a moment when the other store is out of reach" {
    withTwoStores (fun dir a _ ->
      for (what, transport, expected) in
        [ "down",
          { SyncLink.program = "sh"; SyncLink.arguments = [ "-c"; "exit 255" ] },
          "out of reach"
          "stalled",
          { SyncLink.program = "sh"; SyncLink.arguments = [ "-c"; "sleep 30" ] },
          "still going" ] do
        use stop = noStop ()
        let said = System.Collections.Concurrent.ConcurrentQueue<string>()
        let started = System.DateTime.UtcNow
        let watcher =
          Task.Factory.StartNew(
            (fun () ->
              SyncLink.watch
                { side = sideOf dir what a (fun () -> not stop.IsSet) said
                  transport = transport
                  peer = "peer"
                  patience = System.TimeSpan.FromMinutes 10.0
                  mention = said.Enqueue }),
            TaskCreationOptions.LongRunning
          )
        let clock = System.Diagnostics.Stopwatch.StartNew()
        let waited =
          SyncLink.awaitCatchUp
            (fun () -> SyncLink.Standing.read (standingOf dir what))
            started
            (System.TimeSpan.FromMilliseconds 1500.0)
        let answer =
          match waited with
          | SyncLink.CaughtUp -> "caught up"
          | SyncLink.OutOfReach _ -> "out of reach"
          | SyncLink.StillGoing -> "still going"
        Expect.equal answer expected $"the wait says why ({what})"
        Expect.isLessThan
          clock.ElapsedMilliseconds
          2500L
          $"and is short ({what})"
        stop.Set()
        watcher.Wait()
        ())
  }

/// The processes whose command line holds <param marker>.
let private processesWith (marker : string) : int =
  System.IO.Directory.GetDirectories "/proc"
  |> Array.filter (fun d -> System.Char.IsDigit((System.IO.Path.GetFileName d)[0]))
  |> Array.filter (fun d ->
    try
      (System.IO.File.ReadAllText(System.IO.Path.Combine(d, "cmdline"))).Contains
        marker
    with _ ->
      false)
  |> Array.length

let quietPeer =
  testTask
    "linked: an end that stops answering is given up on, and the watcher connects again" {
    withTwoStores (fun dir a _ ->
      // As a stopped process is: both pipes stay open, nothing comes out of one, and nothing
      // reads the other.
      use silent =
        new System.IO.Pipes.AnonymousPipeServerStream(
          System.IO.Pipes.PipeDirection.Out
        )
      use fromPeer =
        new System.IO.Pipes.AnonymousPipeClientStream(
          System.IO.Pipes.PipeDirection.In,
          silent.ClientSafePipeHandle
        )
      use toPeer =
        new System.IO.Pipes.AnonymousPipeServerStream(
          System.IO.Pipes.PipeDirection.Out
        )
      use _unread =
        new System.IO.Pipes.AnonymousPipeClientStream(
          System.IO.Pipes.PipeDirection.In,
          toPeer.ClientSafePipeHandle
        )
      let said = System.Collections.Concurrent.ConcurrentQueue<string>()
      let side =
        { sideOf dir "quiet" a (fun () -> true) said with quietMs = 400 }
      let clock = System.Diagnostics.Stopwatch.StartNew()
      let ended = SyncLink.run side fromPeer toPeer false
      // The pipe's write end first, so the session's reader sees it end.
      silent.Dispose()
      Expect.equal ended SyncLink.Unanswered "the session ends"
      Expect.isLessThan
        clock.ElapsedMilliseconds
        3000L
        "soon after it goes quiet"

      // The watcher, against a peer that never answers: each attempt is given up on and its
      // process ended, so there is never more than one, and none once it stops.
      let marker = "31.4159" + string (System.Random.Shared.Next(100, 999))
      use stop = noStop ()
      let attempts = System.Collections.Concurrent.ConcurrentQueue<string>()
      let counts = System.Collections.Concurrent.ConcurrentQueue<int>()
      let side =
        { sideOf dir "rewatch" a (fun () -> not stop.IsSet) attempts with
            quietMs = 300 }
      let watcher =
        Task.Factory.StartNew(
          (fun () ->
            SyncLink.watch
              { side = side
                transport =
                  { SyncLink.program = "sh"
                    SyncLink.arguments = [ "-c"; $"exec sleep {marker}" ] }
                peer = "peer"
                patience = System.TimeSpan.Zero
                mention = attempts.Enqueue }),
          TaskCreationOptions.LongRunning
        )
      for _ in 1..40 do
        System.Threading.Thread.Sleep 100
        counts.Enqueue(processesWith $"sleep {marker}")
      stop.Set()
      watcher.Wait()
      let tries =
        attempts
        |> Seq.filter (fun l -> l.Contains "connection failed")
        |> Seq.length
      Expect.isGreaterThanOrEqual tries 3 "it tries again, and again"
      Expect.isLessThanOrEqual (Seq.max counts) 1 "one attempt at a time"
      Expect.equal (processesWith $"sleep {marker}") 0 "and none is left behind"
      let mentions =
        attempts
        |> Seq.filter (fun l -> l.Contains "can't reach peer")
        |> Seq.length
      Expect.equal mentions 1 "out of reach is mentioned once")
  }

/// A copy of <param store> at <param path>, whole, as copying a home directory makes.
let private copyStore (store : string) (path : string) : unit =
  use conn =
    new Microsoft.Data.Sqlite.SqliteConnection(
      $"Data Source={store};Pooling=false"
    )
  conn.Open()
  use cmd = conn.CreateCommand()
  cmd.CommandText <- "VACUUM INTO $path"
  cmd.Parameters.AddWithValue("$path", path)
  |> ignore<Microsoft.Data.Sqlite.SqliteParameter>
  let _ = cmd.ExecuteNonQuery()
  ()

let private instanceIdOf (store : string) : string =
  textIn store "SELECT value FROM config_v0 WHERE key = 'instance.id'"

let copiedStore =
  testTask
    "linked: a copied store takes its own id before it syncs, and a hub keeps the two apart" {
    withTwoStores (fun dir a hub ->
      nameStore hub "hub"
      useStore a
      LibDB.Config.ensureInstance ()
      let original = instanceIdOf a
      let c = System.IO.Path.Combine(dir, "copy-of-a.db")
      copyStore a c
      let copied = instanceIdOf c
      Expect.equal copied original "the copy starts with the same id"

      useStore c
      LibDB.Config.ensureInstance ()
      let own = instanceIdOf c
      Expect.notEqual own original "the copy takes its own"
      useStore a
      LibDB.Config.ensureInstance ()
      let kept = instanceIdOf a
      Expect.equal kept original "the original keeps its id"
      useStore c
      LibDB.Config.ensureInstance ()
      let still = instanceIdOf c
      Expect.equal still own "and the copy keeps its new one"

      let second = System.IO.Path.Combine(dir, "second")
      System.IO.Directory.CreateDirectory second
      |> ignore<System.IO.DirectoryInfo>
      use stopA = noStop ()
      use stopC = noStop ()
      let _ =
        linked dir a hub stopA (fun _ _ ->
          let _ =
            linked second c hub stopC (fun _ _ ->
              let fromA =
                authorOn
                  a
                  "module Tests.OpSync\nlet fromOriginal(): Int64 = 9301"
              let fromC =
                authorOn
                  c
                  "module Tests.OpSync\nlet fromCopy(): Int64 = 9302"
              eventually
                "the original's edit reaches the copy"
                (fun () ->
                  let held = heldCount c fromA
                  held = int64 (List.length fromA))
              eventually
                "and the copy's reaches the original"
                (fun () ->
                  let held = heldCount a fromC
                  held = int64 (List.length fromC)))
          ())
      let peers =
        textIn
          hub
          "SELECT group_concat(id, ',') FROM (SELECT id FROM sync_peers ORDER BY id)"
      Expect.equal
        peers
        (String.concat "," (List.sort [ original; own ]))
        "the hub keeps a point for each")
  }

let keepDays =
  testTask "a store that stopped syncing holds back trimming only for sync.keepDays" {
    withTwoStores (fun _ a _ ->
      let _ = authorOn a "module Tests.OpSync\nlet trimmed(): Int64 = 9401"
      commitOn a
      let newest = countIn a "SELECT MAX(seq) FROM sync_journal" []
      // Heard from three days ago, holding nothing.
      let _ =
        countIn
          a
          "INSERT INTO sync_peers (id, name, acked_journal, seen_at)
           VALUES ('away', 'away', 0, strftime('%Y-%m-%dT%H:%M:%fZ','now','-3 days'))
           RETURNING 0"
          []
      let held = OpSync.recordAcked a "here" "here" newest true
      Expect.equal held 0L "within the default week, it holds trimming back"
      useStore a
      LibDB.Config.set "sync.keepDays" "2"
      let trimmed = OpSync.recordAcked a "here" "here" newest true
      Expect.isGreaterThan trimmed 0L "past the setting, it does not")
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
      peerDown
      removedVsEdited
      renamedVsEdited
      resolutionFirst
      resumeFromAgreedPoint
      firstCommandWaits
      firstCommandOutOfReach
      hubWithTwoClients
      editAfterRemoval
      recreatedAfterRemoval
      quietPeer
      copiedStore
      keepDays ]
