/// Two stores, one wire: does the op log actually converge?
///
/// This is the claim the whole design rests on and the one thing a single-store test cannot reach. An op's
/// id is its content, `origin_ts` is portable, and the fold picks a winner deterministically -- so two
/// instances that have seen the same ops must agree on what every name means, whatever order the ops
/// arrived in.
///
/// Each "instance" is its own SQLite file, and `Sql.useStoreForTesting` repoints all of LibDB at it, so
/// these are real separate stores rather than two branches pretending. That swap is process-global, hence
/// `testSequenced` and a teardown that always hands the default store back.
///
/// WHAT THIS IS, exactly: two STORES, one process, one clock, one set of caches. Not two machines. It
/// cannot catch anything that needs process isolation (config state, boot-time growth, a real race), and
/// stamps are passed in explicitly rather than read from a clock, so clock SKEW is simulable but a clock
/// that has genuinely run ahead is not. Say "two stores" in a test name here, not "two machines"; the
/// bash gates and a second laptop are what cover the rest.
///
/// The swap reaches DARK, not only F#: `Builtin.localDbPath` answers with the swapped path, so the
/// `Stdlib.Sqlite` calls in `SCM.*` -- push, pull, conflict detection and recording, resolve, every
/// branch-aware read -- run against the same store the F# side is on. Without that they would read
/// the default store while a test believed it was on instance B, which is most of the SCM.
///
/// The wire is driven directly (`importOpsBulk` + fold, or the Dark fns under `SCM.*`) rather than over
/// HTTP. HTTP has its own tests in `UnguardedOrigins`; mixing the two would mean a convergence failure
/// and a networking failure look alike.
module Tests.MultiInstance

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module Seed = LibDB.Seed
module Inserts = LibDB.Inserts
module Branches = LibDB.Branches
module Queries = LibDB.Queries
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing

open TestUtils.TestUtils


// --- instances -----------------------------------------------------------------------------------

type Instance = { name : string; path : string }

let private deleteStore (path : string) : unit =
  for suffix in [ ""; "-wal"; "-shm" ] do
    try
      System.IO.File.Delete(path + suffix)
    with _ ->
      ()

/// Copy a store AND its WAL/SHM sidecars, so the copy opens as a consistent snapshot.
let private copyStore (src : string) (dst : string) : unit =
  deleteStore dst
  for suffix in [ ""; "-wal"; "-shm" ] do
    if System.IO.File.Exists(src + suffix) then
      System.IO.File.Copy(src + suffix, dst + suffix, overwrite = true)

let private tmpPath (name : string) : string =
  $"/tmp/dark-multiinstance-{name}-{System.Guid.NewGuid()}.db"

/// Snapshot of the default seeded store, taken once. Every instance is a file copy of it, which is far
/// cheaper than seeding each one, and means instances start out agreeing -- which is the interesting
/// starting point for a convergence test.
let private baseline : Lazy<string> =
  lazy
    (let template = "/tmp/dark-multiinstance-baseline.db"
     copyStore LibConfig.Config.dbPath template
     template)

let private instance (name : string) : Instance =
  let path = tmpPath name
  copyStore (baseline.Force()) path
  Sql.useStoreForTesting path
  { name = name; path = path }

/// Point this process at <param inst>'s store: F#, and Dark's `Stdlib.Sqlite` with it.
///
/// The cache drop is not optional. The package manager and the branch overlay memoize by content
/// hash and by branch id, and two copies of one store share both, so without it instance B answers
/// a name lookup with the row instance A had cached.
let private activate (inst : Instance) : unit =
  Sql.useStoreForTesting inst.path
  LibDB.Caching.invalidateAll ()

/// Back to the shared store, caches dropped. Every test here must end with this or the next test in
/// the sequenced list reads an instance store that is about to be deleted.
let private teardown (insts : List<Instance>) : unit =
  Sql.resetStoreForTesting ()
  LibDB.Caching.invalidateAll ()
  insts |> List.iter (fun i -> deleteStore i.path)


/// A testTask over one fresh instance named <param store>: makes it, runs the body,
/// and always hands the default store back, whatever the body did.
let private oneStoreTest
  (name : string)
  (store : string)
  (body : Instance -> Task<unit>)
  : Test =
  testTask name {
    let inst = instance store
    try
      do! body inst
    finally
      teardown [ inst ]
  }

/// The two-instance frame: stores "a" and "b", torn down together.
let private twoStoreTest
  (name : string)
  (body : Instance -> Instance -> Task<unit>)
  : Test =
  testTask name {
    let a = instance "a"
    let b = instance "b"
    try
      do! body a b
    finally
      teardown [ a; b ]
  }


// --- the wire ------------------------------------------------------------------------------------

/// One op as it crosses the wire: (id, hex blob, origin stamp).
///
/// The stamp is passed in rather than minted, because it is the entire input to the last-write-wins rule
/// and a test that let it default would be asserting on wall-clock ordering.
let private wireOp
  (op : PT.PackageOp)
  (originTs : string)
  : string * string * string =
  let opId = Inserts.computeOpHash op
  let blob = BS.PT.PackageOp.serialize opId op
  (string opId, System.Convert.ToHexString blob, originTs)

/// Receive ops the way a pull does: bulk insert, then fold.
let private receive (records : List<string * string * string>) : Task<int64> =
  task {
    let! n = Inserts.importOpsBulk "" records
    let! _ = Seed.applyUnappliedOps ()
    return n
  }

let private loc (name : string) : PT.PackageLocation =
  { owner = "MultiInstance"; modules = [ "Converge" ]; name = name }

let private hashOf (s : string) : PT.Hash =
  // A stable stand-in for real content. These tests are about which BINDING wins, so the hashes only need
  // to be distinct and reproducible, not to name anything that exists.
  PT.Hash(
    System.Convert.ToHexString(
      System.Security.Cryptography.SHA256.HashData(UTF8.toBytes s)
    )
    |> String.toLowercase
  )

let private setName (name : string) (content : string) : PT.PackageOp =
  // None: these fixtures stand for two instances each authoring a name from nothing, which is exactly
  // the case `previous` exists to make visible.
  PT.PackageOp.SetName(loc name, PT.Reference.PackageFn(hashOf content), None)

/// What this store currently thinks the name means.
let private boundHash (name : string) : Task<Option<string>> =
  liveBoundHash (loc name)


/// The fold's mark on one op: 0 pending, 1 applied, 2 deferred (folded, did nothing, waiting for the
/// branch it names). Deferred is the state that separates "not folded yet" from "folded and had no
/// work to do", and only the flag can tell them apart from outside.
let private appliedFlag (opId : string) : Task<int64> =
  Sql.query "SELECT applied AS a FROM package_ops WHERE id = @id"
  |> Sql.parameters [ "id", Sql.string opId ]
  |> Sql.executeRowAsync (fun read -> read.int64 "a")


/// A propagation decision as an op: `pin` this location, with the author's words attached.
///
/// `decidedAt` is what makes each decision a DISTINCT op. Passed in for the same reason `originTs` is:
/// letting it default would make the test depend on the wall clock.
let private pin
  (name : string)
  (reason : string)
  (decidedAt : string)
  : PT.PackageOp =
  PT.PackageOp.Decision(
    $"pin:{name}:{decidedAt}",
    loc name,
    reason,
    PT.DecisionKind.Propagation PT.PropagationPolicy.Pin
  )

/// The policy this store holds for a name on main, as (policy, note).
let private policyFor (name : string) : Task<Option<string * string>> =
  let l = loc name
  Sql.query
    // Main's id comes from the product's own constant, never a literal: a hand-spelled one asks for
    // a branch nothing writes, so the query answers None and the test passes while asserting nothing.
    "SELECT policy, COALESCE(note, '') AS note FROM propagation_policy
     WHERE branch_id = @main AND owner = @o AND modules = @m AND name = @n"
  |> Sql.parameters
    [ "main", Sql.string (string PT.BranchId.Main)
      "o", Sql.string l.owner
      "m", Sql.string (String.concat "." l.modules)
      "n", Sql.string l.name ]
  |> Sql.executeRowOptionAsync (fun read ->
    (read.string "policy", read.string "note"))


// --- tests ---------------------------------------------------------------------------------------

/// The base case, and the one everything else assumes: an op authored on one instance means the same thing
/// on the other once it has crossed.
let opsCrossAndMeanTheSameThing =
  twoStoreTest "an op authored on A means the same thing on B" (fun a b ->
    task {
      activate a
      let op = setName "alpha" "v1"
      let! _ = receive [ wireOp op "2026-01-01T00:00:00.000Z" ]
      let! onA = boundHash "alpha"
      Expect.isSome onA "A bound the name"

      activate b
      let! before = boundHash "alpha"
      Expect.isNone before "B has never heard of it"

      let! _ = receive [ wireOp op "2026-01-01T00:00:00.000Z" ]
      let! onB = boundHash "alpha"
      Expect.equal onB onA "B resolves the name to exactly what A does"
    })

/// Order must not matter. The whole point of a content-addressed log with a portable stamp is that two
/// instances receiving the same ops in opposite orders end up identical -- otherwise "who synced first"
/// silently decides what your code means.
let convergenceIsOrderIndependent =
  twoStoreTest
    "two instances converge on the same winner whatever order ops arrive in"
    (fun a b ->
      task {
        let earlier = wireOp (setName "beta" "from-a") "2026-01-01T00:00:00.000Z"
        let later = wireOp (setName "beta" "from-b") "2026-01-02T00:00:00.000Z"

        activate a
        let! _ = receive [ earlier ]
        let! _ = receive [ later ]
        let! onA = boundHash "beta"

        // B sees the same two ops, newest first.
        activate b
        let! _ = receive [ later ]
        let! _ = receive [ earlier ]
        let! onB = boundHash "beta"

        Expect.equal onB onA "both instances agree"
        Expect.isSome onA "and they agree on something, not on nothing"

        // And they agree on the LATER one specifically, which is the rule rather than an accident. Asserted
        // against the later edit's hash, not against A's own answer read a second time.
        let winner = hashOf "from-b" |> fun (PT.Hash h) -> h
        let (winnerId, _, _) = later
        let! winnerRow =
          Sql.query "SELECT 1 AS n FROM package_ops WHERE id = @id"
          |> Sql.parameters [ "id", Sql.string winnerId ]
          |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
        Expect.isSome winnerRow "the later op is in the log"
        Expect.equal onA (Some winner) "A resolves the name to the later edit"
        Expect.equal onB (Some winner) "and so does B"
      })

/// Re-receiving is free. Sync is not transactional across a network, so the same op arrives twice all the
/// time; if that were not idempotent, every retry would grow the log and re-fold work already done.
let receivingTwiceChangesNothing =
  oneStoreTest "receiving the same op twice is idempotent" "a" (fun a ->
    task {
      activate a
      let op = wireOp (setName "gamma" "only") "2026-01-01T00:00:00.000Z"

      let! first = receive [ op ]
      let! afterFirst = boundHash "gamma"
      let! second = receive [ op ]
      let! afterSecond = boundHash "gamma"

      Expect.equal first 1L "the first receive inserted it"
      Expect.equal second 0L "the second inserted nothing: same content, same id"
      // isSome before the comparison, or this passes by comparing two Nones and proves nothing about
      // idempotence at all.
      Expect.isSome afterFirst "the first receive actually bound the name"
      Expect.equal afterSecond afterFirst "and the binding is untouched"
    })

/// A divergence has to be RECORDED, not just settled. Converging quietly is the easy half; the promise
/// this design makes is that the version that lost is still there and you get told about it.
/// A `SetName` that arrives alone must bind ITS OWN name and touch no other.
let siblingNamesSurviveALoneSetName =
  oneStoreTest
    "a SetName arriving without its Add leaves other names on that hash alone"
    "lonesetname"
    (fun a ->
      task {
        activate a

        // Two names, one hash: `setName` builds both from the same content.
        let! _ =
          receive
            [ wireOp (setName "twinA" "shared-body") "2026-01-01T00:00:00.000Z" ]
        let! _ =
          receive
            [ wireOp (setName "twinB" "shared-body") "2026-01-01T00:00:01.000Z" ]

        let! aBefore = boundHash "twinA"
        let! bBefore = boundHash "twinB"
        Expect.isSome aBefore "twinA is bound"
        Expect.equal
          bBefore
          aBefore
          "and twinB is bound to the SAME hash, which is the whole setup"

        // A third name for that same content, arriving on its own.
        let! _ =
          receive
            [ wireOp (setName "twinC" "shared-body") "2026-01-02T00:00:00.000Z" ]

        let! aAfter = boundHash "twinA"
        let! bAfter = boundHash "twinB"
        let! cAfter = boundHash "twinC"
        Expect.equal cAfter aBefore "the arriving name binds the shared hash"
        Expect.equal aAfter aBefore "and twinA is untouched"
        Expect.equal bAfter bBefore "and so is twinB"
      })


/// A bundle carrying a name's `SetName` but not the `AddX` that carries its content binds the name to
/// something this store has never held.
///
/// It is reachable, not theoretical: a push goes in chunks, and a peer that stops between them sends
/// exactly this. Confirmed end to end by pushing a SetName-only bundle at a relay and pulling it.
///
/// The fold applies it rather than refusing, which is the right call -- the content may arrive in the
/// next page, and a fold that rejected forward references could not accept an out-of-order log at all.
/// What matters is that the gap is VISIBLE afterwards, and that it is reported as a missing op rather
/// than a damaged projection: re-folding replays the same SetName and rebinds it identically, so telling
/// someone to reload spends a full rebuild to watch the same line come back.
let bindingWithoutItsContentIsVisible =
  oneStoreTest
    "a name whose content never arrived is bound, and the gap is findable"
    "dangling"
    (fun a ->
      task {
        activate a

        // No AddFn: this is the whole point. `setName` references a hash nothing ever added.
        let! _ =
          receive
            [ wireOp (setName "orphan" "never-added") "2026-01-01T00:00:00.000Z" ]

        let! bound = boundHash "orphan"
        Expect.isSome bound "the fold applied the SetName rather than dropping it"

        // The same condition `SCM.StoreHealth.problems` reports, asked directly: a LIVE name whose hash is
        // in none of the three content tables.
        let! dangling =
          Sql.query
            """
            SELECT COUNT(*) AS n FROM locations l
            WHERE l.unlisted_at IS NULL
              AND l.owner = @o AND l.modules = @m AND l.name = @n
              AND NOT EXISTS (SELECT 1 FROM package_functions f WHERE f.hash = l.item_hash)
              AND NOT EXISTS (SELECT 1 FROM package_types t WHERE t.hash = l.item_hash)
              AND NOT EXISTS (SELECT 1 FROM package_values v WHERE v.hash = l.item_hash)
            """
          |> Sql.parameters
            [ "o", Sql.string (loc "orphan").owner
              "m", Sql.string (String.concat "." (loc "orphan").modules)
              "n", Sql.string "orphan" ]
          |> Sql.executeRowAsync (fun read -> read.int64 "n")

        Expect.equal
          dangling
          1L
          "store health can see the name has no content behind it"
      })


let divergenceIsRecordedNotJustResolved =
  oneStoreTest
    "when both sides moved a name, the loser is kept and recorded"
    "a"
    (fun a ->
      task {
        activate a
        let mine = setName "delta" "mine"
        let theirs = setName "delta" "theirs"

        let! _ = receive [ wireOp mine "2026-01-01T00:00:00.000Z" ]
        let! _ = receive [ wireOp theirs "2026-01-02T00:00:00.000Z" ]

        let! now = boundHash "delta"
        Expect.isSome now "the name still means something"

        // Both versions remain in the log. That is what "the loser is kept" means concretely: the op that
        // lost is not deleted, so anything pointing at it still resolves.
        let (mineId, _, _) = wireOp mine "2026-01-01T00:00:00.000Z"
        let (theirsId, _, _) = wireOp theirs "2026-01-02T00:00:00.000Z"

        let stillThere (id : string) =
          Sql.query "SELECT 1 AS n FROM package_ops WHERE id = @id"
          |> Sql.parameters [ "id", Sql.string id ]
          |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")

        let! loserRow = stillThere mineId
        let! winnerRow = stillThere theirsId
        Expect.isSome loserRow "the losing op is still in the log"
        Expect.isSome winnerRow "so is the winning one"
      })

/// An instance that has been away must catch up in one go, not one op at a time. This is the shape a real
/// pull has: a batch, arriving at a store that is several edits behind.
let aBatchCatchesAnInstanceUp =
  twoStoreTest "a batch of ops brings a behind instance level" (fun a b ->
    task {
      activate a
      // Distinct CONTENT per name, and that matters. Identical content is the same item, so a standalone
      // SetName binding a hash that is already live elsewhere is a RENAME: the fold unlists the old
      // location. Give three names one hash and you end up with one live name, correctly.
      let ops =
        [ wireOp (setName "e1" "e1-v1") "2026-01-01T00:00:00.000Z"
          wireOp (setName "e2" "e2-v1") "2026-01-01T00:00:01.000Z"
          wireOp (setName "e3" "e3-v1") "2026-01-01T00:00:02.000Z" ]
      let! nA = receive ops
      Expect.equal nA 3L "A inserted all three"
      let! a1 = boundHash "e1"
      let! a2 = boundHash "e2"
      let! a3 = boundHash "e3"

      activate b
      let! n = receive ops
      Expect.equal n 3L "all three arrived in one batch"
      let! b1 = boundHash "e1"
      let! b2 = boundHash "e2"
      let! b3 = boundHash "e3"

      // Same trap as above: assert the values exist before asserting they match, or three Nones equalling
      // three Nones reads as a pass.
      Expect.isSome a1 "A bound e1"
      Expect.isSome a2 "A bound e2"
      Expect.isSome a3 "A bound e3"
      Expect.equal b1 a1 "e1 agrees"
      Expect.equal b2 a2 "e2 agrees"
      Expect.equal b3 a3 "e3 agrees"
    })


/// DECISIONS travel, not just code.
///
/// This is the claim that separates this sync from one that ships source. A pin is a `Decision` op, so it
/// crosses the wire like any other op and lands in the receiving store's `propagation_policy` projection
/// with the author's reason intact. If it did not, the same edit would cascade differently on each machine
/// and the two would drift apart while both looked healthy.
///
/// The reason is asserted, not just the policy. A pin whose "why" is dropped in transit is a pin nobody on
/// the far machine can evaluate, and it ages into an unexplained rule.
let decisionsCrossTheWire =
  twoStoreTest "a pin authored on A holds on B, with its reason" (fun a b ->
    task {
      activate a
      let op =
        pin
          "policyTravels"
          "the signature moved; callers stay on the old one"
          "2026-02-01T00:00:00.000Z"
      let! _ = receive [ wireOp op "2026-02-01T00:00:00.000Z" ]
      let! onA = policyFor "policyTravels"
      Expect.equal
        onA
        (Some("pin", "the signature moved; callers stay on the old one"))
        "A recorded the decision"

      activate b
      let! before = policyFor "policyTravels"
      Expect.isNone before "B has no opinion about this name yet"

      let! _ = receive [ wireOp op "2026-02-01T00:00:00.000Z" ]
      let! onB = policyFor "policyTravels"
      Expect.equal onB onA "B holds the same decision, for the same stated reason"
    })


/// A peer whose clock is ahead must not win your name forever.
///
/// The stamp is a LOGICAL clock, not a wall clock: it advances past anything it observes. Without that
/// step, an op stamped in the future is unbeatable on this instance -- every later edit you make to that
/// name is stamped "now", loses the comparison, and vanishes. Not once: every time, until their clock is
/// overtaken in real time.
///
/// This does not claim to order concurrent edits. It claims the narrower and more important thing: an edit
/// you make AFTER seeing theirs sorts after theirs, because it did happen after.
let localEditsBeatAFastPeer =
  oneStoreTest
    "an edit made after seeing a future-stamped op still wins"
    "a"
    (fun a ->
      task {
        activate a

        // Their clock is years fast. Nothing here can produce a wall-clock stamp that beats it.
        let theirs = setName "contested" "theirs"
        let! _ = receive [ wireOp theirs "2035-06-01T00:00:00.000Z" ]
        let! afterTheirs = boundHash "contested"
        Expect.isSome afterTheirs "their version landed"

        // Now edit the same name locally, having seen theirs. The stamp comes from the real authoring clock,
        // which is the whole point: it has to have moved past 2035 by observing their op.
        let mine = setName "contested" "mine"
        let! _ = receive [ wireOp mine (Inserts.nextOriginTs ()) ]

        let! afterMine = boundHash "contested"
        let (PT.Hash expected) = hashOf "mine"
        Expect.equal
          afterMine
          (Some expected)
          "the edit made after seeing theirs wins, rather than losing to their clock"
      })



/// The fold reads the pending set on one connection and marks applied inside a transaction on another.
/// Marking by ID rather than by predicate is what keeps that safe: a `serve` committing an op between
/// the two halves matches the predicate but was never folded, so a predicate sweep would record it as
/// applied and nothing would ever re-read it. By id, it is simply left for the next pass.
/// Deterministic: the "concurrent" write is simply placed between the two halves.
let aWriteBetweenReadAndMarkIsNotLost =
  oneStoreTest
    "an op committed between the fold's read and its mark stays pending, then folds"
    "a"
    (fun a ->
      task {
        activate a
        let first = setName "early" "v1"
        let! _ =
          Inserts.importOpsBulk "" [ wireOp first "2026-01-01T00:00:00.000Z" ]
        let! pending = Seed.readPending ()
        Expect.equal (List.length pending) 1 "one op pending when the fold read"

        // The writer that lands between the read and the mark.
        let late = setName "late" "v1"
        let lateId = string (Inserts.computeOpHash late)
        let! _ = Inserts.importOpsBulk "" [ wireOp late "2026-01-01T00:00:01.000Z" ]

        let! folded = Seed.foldRead pending
        Expect.equal folded 1L "the fold took what it read"

        let! lateApplied = appliedFlag lateId
        Expect.equal
          lateApplied
          0L
          "the late op is still pending, since nothing folded it"
        let! lateBound = boundHash "late"
        Expect.isNone lateBound "and it is not live yet"

        let! next = Seed.applyUnappliedOps ()
        Expect.equal next 1L "the next pass takes exactly it"
        let! lateBoundNow = boundHash "late"
        Expect.isSome lateBoundNow "and then it is live"
      })


/// A merge event names what it moved. A peer holding the branch may hold MORE of it than the merger saw
/// (its own unpushed edits); the event must not take those into main, and the branch must stay live here
/// holding exactly them. Before the event carried ids, it flipped everything tagged to the branch.
let aMergeEventLeavesUnpushedWorkOnTheBranch =
  oneStoreTest
    "a peer's merge event folds only what the merger moved, and the branch keeps the rest"
    "b"
    (fun b ->
      task {
        let x = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        do! Branches.createBranch x "shared-x" PT.BranchId.Main
        let shared = [ setName "shared1" "s1"; setName "shared2" "s2" ]
        let extra = setName "mine-only" "m1"
        let! _ = Branches.storeDeltaOps x (shared @ [ extra ])

        let sharedIds = shared |> List.map Inserts.computeOpHash
        let event =
          PT.PackageOp.BranchEvent(
            x,
            PT.Merged sharedIds,
            "2026-01-02T00:00:00.000Z"
          )
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]

        let! s1 = boundHash "shared1"
        Expect.isSome s1 "what the merger moved is live on main here"
        let! mine = boundHash "mine-only"
        Expect.isNone mine "the unpushed op is not"

        let extraId = string (Inserts.computeOpHash extra)
        let! (effective, tagged) =
          Sql.query
            "SELECT p.effective AS e,
                    (SELECT count(*) FROM op_branches WHERE op_id = p.id AND branch_id = @b) AS t
             FROM package_ops p WHERE p.id = @id"
          |> Sql.parameters [ "id", Sql.string extraId; "b", Sql.string (string x) ]
          |> Sql.executeRowAsync (fun read -> (read.int64 "e", read.int64 "t"))
        Expect.equal
          (effective, tagged)
          (0L, 1L)
          "it is still inert and still on the branch"

        let! merged =
          Sql.query "SELECT merged_at IS NOT NULL AS m FROM branches WHERE id = @b"
          |> Sql.parameters [ "b", Sql.string (string x) ]
          |> Sql.executeRowAsync (fun read -> read.int64 "m")
        Expect.equal merged 0L "and the branch stays live, holding exactly that"
      })


/// A merge into a NON-main parent, arriving from a peer, retags onto the parent as the local merge does.
/// The parent is carried in the event for exactly this reason: flipping into main regardless would land
/// a child branch's work in the other machine's main, which nobody there had merged.
let aMergeEventHonoursTheParent =
  oneStoreTest
    "a peer's merge of a branch off a branch retags onto the parent, not into main"
    "b"
    (fun b ->
      task {
        let pA = PT.BranchId.Id(System.Guid.NewGuid())
        let pB = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        do! Branches.createBranch pA "chain-a" PT.BranchId.Main
        do! Branches.createBranch pB "chain-b" pA
        let op = setName "chain-name" "cb1"
        let! _ = Branches.storeDeltaOps pB [ op ]
        do!
          execSqlP
            "INSERT OR IGNORE INTO branch_name_bases (branch_id, owner, modules, name, base_hash)
             VALUES (@b, 'MultiInstance', 'Converge', 'chain-name', 'the-fork-hash')"
            [ "b", Sql.string (string pB) ]

        let event =
          PT.PackageOp.BranchEvent(
            pB,
            PT.Merged [ Inserts.computeOpHash op ],
            "2026-01-02T00:00:00.000Z"
          )
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]

        let! onMain = boundHash "chain-name"
        Expect.isNone onMain "nothing reached main"
        let opId = string (Inserts.computeOpHash op)
        let! taggedTo =
          Sql.query "SELECT branch_id FROM op_branches WHERE op_id = @id"
          |> Sql.parameters [ "id", Sql.string opId ]
          |> Sql.executeAsync (fun read -> read.string "branch_id")
        Expect.equal taggedTo [ string pA ] "the op now belongs to the parent"
        let! basesOnA =
          Sql.query "SELECT name FROM branch_name_bases WHERE branch_id = @b"
          |> Sql.parameters [ "b", Sql.string (string pA) ]
          |> Sql.executeAsync (fun read -> read.string "name")
        Expect.equal basesOnA [ "chain-name" ] "and so does its base"
        let! merged =
          Sql.query "SELECT merged_at IS NOT NULL AS m FROM branches WHERE id = @b"
          |> Sql.parameters [ "b", Sql.string (string pB) ]
          |> Sql.executeRowAsync (fun read -> read.int64 "m")
        Expect.equal merged 1L "the child is merged"
      })


/// What an arriving merge event lands is committed the way the event was. A pull commits the event on the
/// way in, but the ops it flips came by a branch bundle, uncommitted; left that way they sit in main's
/// draft as "1 item changed" nobody on this machine made.
let aMergeEventCommitsWhatItFlips =
  oneStoreTest
    "a peer's merge event stamps the ops it flips with its own commit"
    "b"
    (fun b ->
      task {
        let x = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        do! Branches.createBranch x "stamped-x" PT.BranchId.Main
        let op = setName "stamped-name" "s1"
        let! _ = Branches.storeDeltaOps x [ op ]
        let event =
          PT.PackageOp.BranchEvent(
            x,
            PT.Merged [ Inserts.computeOpHash op ],
            "2026-01-02T00:00:00.000Z"
          )
        // The way a pull delivers it: committed on arrival, then folded.
        let! _ =
          Inserts.importOpsBulk
            "sync-commit-1"
            [ wireOp event "2026-01-02T00:00:00.000Z" ]
        let! _ = Seed.applyUnappliedOps ()

        let! commit =
          Sql.query
            "SELECT COALESCE(commit_hash, '') AS c FROM package_ops WHERE id = @id"
          |> Sql.parameters [ "id", Sql.string (string (Inserts.computeOpHash op)) ]
          |> Sql.executeRowAsync (fun read -> read.string "c")
        Expect.equal
          commit
          "sync-commit-1"
          "the flipped op carries the event's commit"
      })

let private unbind (name : string) (previous : string) : PT.PackageOp =
  PT.PackageOp.Unbind(loc name, Some(hashOf previous))

/// An `Unbind` takes a name out and leaves its content alone, and a later binding brings the name back.
let anUnbindRemovesTheNameAndNothingElse =
  oneStoreTest
    "an unbind removes the name; the content stays, and a later bind revives the name"
    "a"
    (fun a ->
      task {
        activate a
        let! _ = receive [ wireOp (setName "gone" "g1") "2026-01-01T00:00:00.000Z" ]
        let! before = boundHash "gone"
        Expect.isSome before "bound before"

        let! _ = receive [ wireOp (unbind "gone" "g1") "2026-01-02T00:00:00.000Z" ]
        let! after = boundHash "gone"
        Expect.isNone after "the name resolves to nothing after the unbind"

        // The unbound row is history, not gone: it is what `dark log` and a merge read.
        let! rows =
          Sql.query
            "SELECT count(*) AS n FROM locations
             WHERE owner = 'MultiInstance' AND modules = 'Converge' AND name = 'gone'"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        Expect.equal
          rows
          2L
          "the binding it unlisted and the unbind's own tombstone"

        let! _ = receive [ wireOp (setName "gone" "g2") "2026-01-03T00:00:00.000Z" ]
        let! revived = boundHash "gone"
        let (PT.Hash g2) = hashOf "g2"
        Expect.equal
          revived
          (Some g2)
          "a binding authored after the unbind takes the name again"
      })

/// The order the two ops ARRIVE in must not decide whether the name exists. Without the tombstone, a
/// binding authored before an unbind but arriving after it found nothing live and bound the name; the
/// peer that saw them in authoring order had no name. Two stores, one log, two answers.
let unbindConvergesWhateverOrderOpsArrive =
  twoStoreTest
    "a bind authored before an unbind loses to it whichever arrives first"
    (fun a b ->
      task {
        let bound = wireOp (setName "order" "o1") "2026-01-01T00:00:00.000Z"
        let removed = wireOp (unbind "order" "o1") "2026-01-02T00:00:00.000Z"
        let rebound = wireOp (setName "order" "o2") "2026-01-03T00:00:00.000Z"

        activate a
        let! _ = receive [ bound ]
        let! _ = receive [ removed ]
        let! onA = boundHash "order"
        Expect.isNone onA "in authoring order: the name is gone"

        activate b
        let! _ = receive [ removed ]
        let! _ = receive [ bound ]
        let! onB = boundHash "order"
        Expect.isNone onB "unbind first, then the older bind: still gone"

        // And an unbind arriving late does not take out a name bound after it.
        let! _ = receive [ rebound ]
        let! _ = receive [ removed ]
        let! onB2 = boundHash "order"
        let (PT.Hash o2) = hashOf "o2"
        Expect.equal
          onB2
          (Some o2)
          "the later bind holds against a re-received older unbind"
      })

/// A branch's unbind is inert on main until the branch merges, and then it is main's.
let aMergedUnbindTakesTheNameOffMain =
  oneStoreTest
    "an unbind authored on a branch removes the name from main when the branch merges"
    "b"
    (fun b ->
      task {
        let x = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        let! _ =
          receive [ wireOp (setName "landed" "l1") "2026-01-01T00:00:00.000Z" ]
        do! Branches.createBranch x "unbind-x" PT.BranchId.Main
        let op = unbind "landed" "l1"
        let! _ = Branches.storeDeltaOps x [ op ]
        let! still = boundHash "landed"
        Expect.isSome
          still
          "the branch's unbind changes nothing on main while it is a branch op"

        let event =
          PT.PackageOp.BranchEvent(
            x,
            PT.Merged [ Inserts.computeOpHash op ],
            "2026-01-02T00:00:00.000Z"
          )
        let! _ =
          Inserts.importOpsBulk
            "sync-commit-2"
            [ wireOp event "2026-01-02T00:00:00.000Z" ]
        let! _ = Seed.applyUnappliedOps ()
        let! after = boundHash "landed"
        Expect.isNone after "merged, the name is gone from main"
      })

/// A relay is also somebody's authoring instance. Ops a client pushed sit `effective = 0`, untagged and
/// uncommitted, which is the shape of main's own draft, so every "main's ops" read has to say
/// `effective = 1` as well as "not tagged". Without it, authoring here swept a peer's ops into main's
/// draft and a rewrite folded them into the code this store runs, which is exactly what storing them
/// inert is for; and `discard` deleted them outright.
let hostedOpsAreNotThisStoresDraft =
  oneStoreTest
    "ops a client pushed here are not this store's draft, and a discard does not eat them"
    "a"
    (fun a ->
      task {
        activate a
        let hosted = setName "hosted-by-a-peer" "theirs"
        let hostedId = Inserts.computeOpHash hosted
        let! _ =
          Inserts.storeOpsWithOwner
            "some-peer"
            [ wireOp hosted "2026-01-01T00:00:00.000Z" ]

        let! wip = Queries.getWipOps ()
        let wipIds = wip |> List.map Inserts.computeOpHash |> Set.ofList
        Expect.isFalse
          (Set.contains hostedId wipIds)
          "a hosted op is not main's WIP, so authoring cannot re-insert it effective"

        let! draft = Queries.getDraftOps ()
        let draftIds = draft |> List.map Inserts.computeOpHash |> Set.ofList
        Expect.isFalse (Set.contains hostedId draftIds) "nor main's draft"

        // The delete `discard` runs, against the same store.
        for sql in Inserts.draftDeletes do
          do! Sql.query sql |> Sql.executeStatementAsync

        let! stillThere =
          countSql
            "SELECT count(*) AS n FROM package_ops WHERE id = @id"
            [ "id", Sql.string (string hostedId) ]
        Expect.equal stillThere 1L "and a discard leaves it where it is"

        let! effective =
          Sql.query "SELECT effective AS e FROM package_ops WHERE id = @id"
          |> Sql.parameters [ "id", Sql.string (string hostedId) ]
          |> Sql.executeRowAsync (fun read -> read.int64 "e")
        Expect.equal
          effective
          0L
          "still inert: a relay serves what it is handed, it does not run it"
      })

// --- the Dark half -------------------------------------------------------------------------------
//
// Everything below runs DARK against whichever instance is active, which the F#-only tests above
// cannot do. It works because `Builtin.localDbPath` follows the store swap, so `Stdlib.Sqlite` and
// the F# connection are on the same file.

/// Run <param code> against the ACTIVE instance and return its answer as a string.
let private darkOn (code : string) : Task<string> =
  task {
    match! evalDarkExpr code with
    | Ok dv -> return string dv
    | Error(rte, _) -> return failtest $"the Dark call failed: {rte}\n  code: {code}"
  }

/// Dark source for a pending same-name-different-hash conflict on TwoStore.Cascade,
/// between a local and an incoming candidate, auto-resolved to the incoming one.
let private conflictLiteral
  (id : string)
  (name : string)
  (mine : string)
  (theirs : string)
  : string =
  "Darklang.SCM.Conflicts.Conflict { id = \""
  + id
  + "\"; owner = \"TwoStore\"; "
  + "modules = \"Cascade\"; name = \""
  + name
  + "\"; itemType = \"fn\"; kind = \"same-name-different-hash\"; "
  + "candidates = [ Darklang.SCM.Conflicts.Candidate { side = \"local\"; hash = \""
  + mine
  + "\"; originTs = \"\"; author = \"\" }; "
  + "Darklang.SCM.Conflicts.Candidate { side = \"incoming\"; hash = \""
  + theirs
  + "\"; originTs = \"\"; author = \"\" } ]; "
  + "autoResolvedTo = \""
  + theirs
  + "\"; reason = \"\"; status = \"pending\"; resolvedBy = \"\" }"

/// The Dark conflict detector, run on the receiving store, over two edits to one name made
/// independently on two stores.
///
/// `SCM.Conflicts` decides which side wins and whether the divergence is even a conflict, and it is
/// Dark. If the Dark side did not follow the store swap it would answer about the DEFAULT store
/// whatever a test had activated, plausibly and wrongly, so this asserts the swap itself before any
/// test leans on it.
let private theDarkDetectorSeesTheStoreItIsOn =
  twoStoreTest
    "Dark reads the instance it was pointed at, not the store the process started on"
    (fun a b ->
      task {
        // Two stores, one name, two different bodies, B's authored later.
        activate a
        let! _ =
          receive [ wireOp (setName "seen" "from-a") "2026-01-01T00:00:00.000Z" ]

        activate b
        let! _ =
          receive [ wireOp (setName "seen" "from-b") "2026-01-02T00:00:00.000Z" ]

        // A branch-aware read, in Dark, answers about the store it is on. Deliberately NOT asserted via
        // `identity ()`: a chosen identity survives being copied, by design, so two copies of a store
        // whose name was set by hand legitimately share one, and the assertion would depend on whether
        // anything else in the run had named the dev store.
        let liveHere =
          "Darklang.SCM.PackageOps.liveBindingFor Darklang.SCM.Branch.mainBranchId "
          + "(Darklang.LanguageTools.ProgramTypes.PackageLocation "
          + "{ owner = \"MultiInstance\"; modules = [\"Converge\"]; name = \"seen\" })"
        let! bSees = darkOn liveHere
        let (PT.Hash fromB) = hashOf "from-b"
        Expect.stringContains bSees fromB "B's Dark read sees B's binding"

        activate a
        let! aSees = darkOn liveHere
        let (PT.Hash fromA) = hashOf "from-a"
        Expect.stringContains aSees fromA "and A's sees A's, in the same process"
      })

/// A merge event that arrives BEFORE the branch it merged still lands when the branch shows up.
///
/// `dark pull` (main, which carries the event) and `dark branch pull` (the bundle) are separate
/// commands, and that order is the natural one. Folded against a store with none of the branch's ops
/// tagged, the event has nothing to flip; marking itself applied there would retire it for good, so
/// the merger's main would hold the work, this store would not, and `dark branches` would go on
/// showing the branch as live with nothing saying otherwise on either side.
let private aMergeEventWaitsForItsBranch =
  oneStoreTest
    "a merge event that arrives before its branch applies when the branch lands"
    "b"
    (fun b ->
      task {
        let x = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        // The branch is REGISTERED here (a peer told us it exists) but holds no ops yet.
        do! Branches.createBranch x "early-event" PT.BranchId.Main
        let op = setName "early" "e1"
        let opId = Inserts.computeOpHash op
        let event =
          PT.PackageOp.BranchEvent(
            x,
            PT.Merged [ opId ],
            "2026-01-02T00:00:00.000Z"
          )

        // Main first: the event folds with nothing to flip.
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]
        let! notYet = boundHash "early"
        Expect.isNone notYet "nothing is live yet: the op it names has not arrived"

        let! stillPending = appliedFlag (string (Inserts.computeOpHash event))
        // 2 is DEFERRED: folded, did nothing, waiting. Not 0, which would make the fold loop chase it
        // forever and raise "did not settle".
        Expect.equal
          stillPending
          2L
          "and the event is deferred, waiting for its branch"

        // Now the bundle. Storing it, re-arming and folding is what `scmImportBranchOps` does.
        let! _ = Branches.storeDeltaOps x [ op ]
        do! Branches.undeferBranchEvents x
        let! _ = Seed.applyUnappliedOps ()

        let! landed = boundHash "early"
        let (PT.Hash e1) = hashOf "e1"
        Expect.equal
          landed
          (Some e1)
          "the merge lands the moment its branch's ops do"

        let! nowApplied = appliedFlag (string (Inserts.computeOpHash event))
        Expect.equal
          nowApplied
          1L
          "and the event is applied once it has done its work"
      })

/// A store that reconnects from nothing meets every merge event before every branch. The event for
/// a branch it has never heard of folds to nothing and marks itself done, correctly; when the bundle
/// then registers the branch, the event has to run again or the branch reads as live and empty for
/// good. Seen on a real machine: two merged branches listed as live with 0 ops after a wipe and a
/// reconnect.
let aMergeEventForALaterBranchStillApplies =
  oneStoreTest
    "a merge event that predates any knowledge of its branch applies once the bundle lands"
    "b"
    (fun b ->
      task {
        let x = PT.BranchId.Id(System.Guid.NewGuid())
        activate b
        // No `createBranch`: this store has never heard of x when the event arrives.
        let op = setName "from-nowhere" "n1"
        let event =
          PT.PackageOp.BranchEvent(
            x,
            PT.Merged [ Inserts.computeOpHash op ],
            "2026-01-02T00:00:00.000Z"
          )
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]
        let! applied = appliedFlag (string (Inserts.computeOpHash event))
        Expect.equal
          applied
          1L
          "an event for an unknown branch is applied and done, not parked"

        // The bundle: the branch registers, its ops land inert, and the import re-arms the event.
        do! Branches.createBranch x "late-branch" PT.BranchId.Main
        let! _ = Branches.storeDeltaOps x [ op ]
        do! Branches.undeferBranchEvents x
        let! _ = Seed.applyUnappliedOps ()

        let! landed = boundHash "from-nowhere"
        let (PT.Hash n1) = hashOf "n1"
        Expect.equal
          landed
          (Some n1)
          "the merged work is live on main once its branch arrives"
      })

/// The other half of the rule: an event for a branch this store has never heard of folds to nothing
/// and STAYS applied. Waiting for ops that will never come would re-decode it at every startup.
let private anEventForAnUnknownBranchDoesNotWait =
  oneStoreTest
    "a merge event for a branch this store never had is applied and done"
    "b"
    (fun b ->
      task {
        activate b
        let stranger = PT.BranchId.Id(System.Guid.NewGuid())
        let op = setName "not-ours" "n1"
        let event =
          PT.PackageOp.BranchEvent(
            stranger,
            PT.Merged [ Inserts.computeOpHash op ],
            "2026-01-02T00:00:00.000Z"
          )
        let! _ = receive [ wireOp event "2026-01-02T00:00:00.000Z" ]

        let! applied = appliedFlag (string (Inserts.computeOpHash event))
        Expect.equal
          applied
          1L
          "a colleague's private branch is none of this store's business"
      })

/// An override cascades the way an edit does.
///
/// Two stores edit one fn; the later stamp wins on both, and the loser overrides back to its own
/// version. Overriding rebinds the NAME. A caller of that fn had followed the winner's version by
/// hash when the pull landed; without a cascade it goes on calling the winner's version while the
/// name says otherwise, on every machine the override reaches. Seen on two real machines: `render`
/// bound the override, `summary` printed the other side's output.
let anOverrideRepointsCallers =
  oneStoreTest
    "overriding a conflict repoints the callers that had followed the loser"
    "a"
    (fun a ->
      task {
        activate a
        // One fn, one caller of it. The test helper authors without the cascade the `fn` verb runs,
        // so each step that would have cascaded in real use does so here by hand, through the same
        // wrapper the verb uses.
        let! shared =
          authorIntoMain (
            "module TwoStore.Cascade\n\n"
            + "let base (x: Int64) : Int64 = x + 1L\n"
            + "let caller (x: Int64) : Int64 = TwoStore.Cascade.base x\n"
          )
        let! _ = LibDB.Inserts.commitAllAsBaseline "shared"
        let (PT.Hash origHash) = hashBoundTo shared "base"
        let repoint (fromHash : string) (toHash : string) =
          darkOn (
            "Darklang.SCM.Propagation.repointDependents Darklang.SCM.Branch.mainBranchId "
            + "(Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Cascade\"]; name = \"base\" }) "
            + "Darklang.LanguageTools.ProgramTypes.ItemKind.Fn "
            + "[ Darklang.LanguageTools.ProgramTypes.Hash.Hash \""
            + fromHash
            + "\" ] "
            + "(Darklang.LanguageTools.ProgramTypes.Hash.Hash \""
            + toHash
            + "\")"
          )

        // "Mine": this store's edit, cascaded. "Theirs": a later edit that arrives by sync and wins
        // by stamp; a real pull would also bring the sender's cascaded caller, so cascade here too.
        let! mine =
          authorIntoMain
            "module TwoStore.Cascade\n\nlet base (x: Int64) : Int64 = x + 10L\n"
        let (PT.Hash mineHash) = hashBoundTo mine "base"
        let! _ = repoint origHash mineHash
        // Their version has to be real content or the caller cannot be repointed at it.
        let! theirsOps =
          authorIntoMain
            "module TwoStore.Cascade\n\nlet base (x: Int64) : Int64 = x + 100L\n"
        let (PT.Hash theirsHash) = hashBoundTo theirsOps "base"
        let! _ = repoint mineHash theirsHash
        let! before = darkOn "TwoStore.Cascade.caller 0L"
        Expect.stringContains
          before
          "100"
          "before the override, the caller follows the winning version"

        // The override, through the real resolve path, back to mine.
        let! _ =
          darkOn (
            "let c = "
            + conflictLiteral "cascade01" "base" mineHash theirsHash
            + "\nDarklang.SCM.PackageOps.settleConflict Darklang.SCM.Branch.mainBranchId c \""
            + mineHash
            + "\""
          )

        let! baseNow = darkOn "TwoStore.Cascade.base 0L"
        Expect.stringContains
          baseNow
          "10L"
          "the name binds my version after the override"
        let! callerNow = darkOn "TwoStore.Cascade.caller 0L"
        Expect.stringContains
          callerNow
          "10L"
          "and the caller follows it, rather than still calling the other version by hash"
        Expect.isFalse
          (callerNow.Contains "100")
          "which means it no longer calls the loser"

        // The cascade also settles the conflicts it makes moot. On two machines the caller's own
        // conflict (each side's cascade had made a different caller) stayed pending after the override
        // rebound the caller to a third version, asking for a choice between two versions nobody ran.
        // A conflict whose live binding is neither candidate is superseded; one whose live binding is a
        // candidate is still a real question.
        let! _ =
          darkOn (
            "Darklang.SCM.Conflicts.record Darklang.SCM.Branch.mainBranchId [ "
            + conflictLiteral "moot01" "caller" "aaaa" "bbbb"
            + "; "
            + conflictLiteral "live01" "base" mineHash theirsHash
            + " ]"
          )
        let! (stillPending : string) =
          darkOn
            "Darklang.SCM.Conflicts.pending () |> Stdlib.List.map (fun c -> c.id)"
        Expect.isFalse
          (stillPending.Contains "moot01")
          $"the caller's conflict, rebound past both candidates, is settled: {stillPending}"
        Expect.stringContains
          stillPending
          "live01"
          $"the one whose live binding is a candidate stays: {stillPending}"
        let! status =
          Sql.query "SELECT status FROM conflicts WHERE id = 'moot01'"
          |> Sql.executeRowAsync (fun read -> read.string "status")
        Expect.equal
          status
          "superseded"
          "and it is recorded as superseded, not deleted"
      })

/// Main sync carries the author's commit, so the same work reads the same on every store.
///
/// A's ops arrive on B by the main channel and are filed under A's commit id and message, not under
/// an import commit of B's; the import commit that landed them is deleted once nothing is left under
/// it. Before, `dark commits` on B read "synced from A" for everything A ever did, under ids B minted.
let mainSyncCarriesTheAuthorsCommit =
  twoStoreTest
    "ops synced on main are filed under the author's commit on the other store"
    (fun a b ->
      task {
        activate a
        let! _ =
          authorIntoMain
            "module TwoStore.Attrib\n\nlet f (x: Int64) : Int64 = x + 7L\n"
        let! aCommit = LibDB.Inserts.commitAllAsBaseline "f, by a"
        // What A would put on the wire for this commit: the ops, and the commit row they name. Through a
        // file rather than a string literal, as `sync export` does; the bundle is JSON with quotes in it.
        let path =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            "dark-attrib-bundle.json"
          )
        let! (written : string) =
          darkOn (
            "let json = Darklang.SCM.Wire.exportOps () |> Stdlib.List.filter (fun o -> o.commit == \""
            + aCommit
            + "\") |> Darklang.SCM.Wire.wireEncode\n"
            + "match Stdlib.Cli.File.writeText \""
            + path
            + "\" json with | Ok _ -> \"written\" | Error e -> e.message"
          )
        Expect.stringContains written "written" $"a wrote its bundle: {written}"

        activate b
        let! (imported : string) =
          darkOn (
            "match Darklang.SCM.Wire.wireDecode (Stdlib.String.toBlob (Builtin.unwrap (Stdlib.Cli.File.readText \""
            + path
            + "\"))) with\n"
            + "| Ok bundle -> (match Darklang.SCM.Wire.importFrom \"peer:a\" bundle.ops bundle.commits with | Ok o -> Stdlib.Int.toString o.imported | Error e -> e)\n"
            + "| Error e -> e"
          )
        Expect.isFalse
          (imported.Contains "not")
          $"the import went through: {imported}"

        let! filedUnder =
          Sql.query
            "SELECT COALESCE(commit_hash, '') AS c FROM package_ops p
             WHERE p.id IN (SELECT op_id FROM locations WHERE owner = 'TwoStore' AND modules = 'Attrib')"
          |> Sql.executeAsync (fun read -> read.string "c")
        Expect.isNonEmpty filedUnder "b has the ops"
        Expect.allEqual
          filedUnder
          aCommit
          "and files them under a's commit id, not its own"

        let! message =
          Sql.query "SELECT message FROM commits WHERE hash = @h"
          |> Sql.parameters [ "h", Sql.string aCommit ]
          |> Sql.executeRowAsync (fun read -> read.string "message")
        Expect.equal message "f, by a" "with a's message"

        let! leftover =
          Sql.query
            "SELECT count(*) AS n FROM commits WHERE message LIKE 'synced from%' OR message LIKE 'imported from%'"
          |> Sql.executeRowAsync (fun read -> read.int64 "n")
        Expect.equal
          leftover
          0L
          "and the import commit, left with nothing under it, is gone"
      })

/// The two "your decision was superseded" reports check WHO acted, not only what happened.
///
/// A peer's policy with no local one before it supersedes nothing of yours; your own later edit is a
/// change of mind, which closes the record rather than reporting you to yourself. Without either rule
/// the store greets a fresh puller with findings about choices they never made -- which is exactly how
/// it presented, on a store whose whole history had arrived by sync.
let supersededReportsCheckAuthorship =
  oneStoreTest
    "a peer's decision alone, or your own later edit, is not a superseded-decision finding"
    "a"
    (fun a ->
      task {
        activate a
        // A peer's pin arrives with an author; no local choice preceded it.
        let! _ =
          darkOn (
            "let op = Darklang.LanguageTools.ProgramTypes.PackageOp.Decision(\"peerpin01\", "
            + "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Sup\"]; name = \"f\" }, \"\", "
            + "Darklang.LanguageTools.ProgramTypes.DecisionKind.Propagation Darklang.LanguageTools.ProgramTypes.PropagationPolicy.Pin) in "
            + "Darklang.SCM.PackageOps.add Darklang.SCM.Branch.mainBranchId [ op ]"
          )
        do!
          execSql
            "INSERT INTO op_owners (op_id, owner)
             SELECT id, 'peer-1' FROM package_ops
             WHERE id NOT IN (SELECT op_id FROM op_owners)
               AND substr(op_blob, 9, 1) = X'0B'"
        // A second peer replaces the first: still nobody's surprise but theirs. This is the arm the
        // authorship check exists for -- without it the first peer's choice is recorded as "yours".
        let! _ =
          darkOn (
            "let op = Darklang.LanguageTools.ProgramTypes.PackageOp.Decision(\"peerpin02\", "
            + "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Sup\"]; name = \"f\" }, \"\", "
            + "Darklang.LanguageTools.ProgramTypes.DecisionKind.Propagation Darklang.LanguageTools.ProgramTypes.PropagationPolicy.Follow) in "
            + "Darklang.SCM.PackageOps.add Darklang.SCM.Branch.mainBranchId [ op ]"
          )
        do!
          execSql
            "INSERT INTO op_owners (op_id, owner)
             SELECT id, 'peer-2' FROM package_ops
             WHERE id NOT IN (SELECT op_id FROM op_owners)
               AND substr(op_blob, 9, 1) = X'0B'"
        let! (policies : string) =
          darkOn
            "Darklang.SCM.PackageOps.supersededPolicies () |> Stdlib.List.length |> Stdlib.Int.toString"
        Expect.equal
          policies
          "DString \"0\""
          $"a peer's first-and-only choice supersedes nothing of yours: {policies}"

        // Your own override, then your own edit: a change of mind, not a finding.
        let! ops =
          authorIntoMain
            "module TwoStore.Sup\n\nlet g (x: Int64) : Int64 = x + 1L\n"
        let (PT.Hash gHash) = hashBoundTo ops "g"
        let! _ =
          darkOn (
            "let op = Darklang.LanguageTools.ProgramTypes.PackageOp.Decision(\"selfov01\", "
            + "Darklang.LanguageTools.ProgramTypes.PackageLocation { owner = \"TwoStore\"; modules = [\"Sup\"]; name = \"g\" }, \"\", "
            + "Darklang.LanguageTools.ProgramTypes.DecisionKind.Override (Darklang.LanguageTools.ProgramTypes.Reference.PackageFn (Darklang.LanguageTools.ProgramTypes.Hash.Hash \""
            + gHash
            + "\"))) in "
            + "Darklang.SCM.PackageOps.add Darklang.SCM.Branch.mainBranchId [ op ]"
          )
        // The conflicts-table guard is what makes the fold run at all.
        do!
          execSql
            "INSERT INTO conflicts (id, owner, modules, name, item_type, kind, candidates, auto_resolved_to, reason, status, origin_ts, branch_id)
             VALUES ('selfov01', 'TwoStore', 'Sup', 'g', 'fn', 'same-name-different-hash', '[]', '', '', 'overridden',
                     strftime('%Y-%m-%dT%H:%M:%fZ','now'), '00000000-0000-0000-0000-000000000001')"
        let! _ =
          authorIntoMain
            "module TwoStore.Sup\n\nlet g (x: Int64) : Int64 = x + 2L\n"
        let! (overrides : string) =
          darkOn
            "Darklang.SCM.PackageOps.supersededOverrides () |> Stdlib.List.length |> Stdlib.Int.toString"
        Expect.equal
          overrides
          "DString \"0\""
          "editing past your own override is a change of mind, not a finding"
      })

/// An import whose insert fails leaves no commit row behind: the row is minted before the insert
/// (the insert stamps against it), so the failure arm has to take it back out.
let aFailedImportLeavesNoCommit =
  oneStoreTest
    "an import that fails leaves no synced-from commit naming ops that never arrived"
    "a"
    (fun a ->
      task {
        activate a
        let! (before : string) =
          darkOn
            "Stdlib.Sqlite.scalarInt (Stdlib.LocalStore.path ()) \"SELECT count(*) AS n FROM commits\" \"n\" |> Stdlib.Option.withDefault 0L |> Stdlib.Int64.toString"
        // blobHex that is not hex: `scmImportOps` throws inside, and importFrom's error arm runs.
        let! (result : string) =
          darkOn (
            "let op = Darklang.SCM.Wire.SyncOp { id = \"7c9e6679-7425-40de-944b-e07fc1f90ae9\"; blobHex = \"zznothex\"; ts = \"2026-01-01T00:00:00.000Z\"; author = \"peer-1\"; commit = \"\" } in "
            + "match Darklang.SCM.Wire.importFrom \"peer:x\" [ op ] [] with | Ok _ -> \"ok\" | Error e -> \"error\""
          )
        let! (after : string) =
          darkOn
            "Stdlib.Sqlite.scalarInt (Stdlib.LocalStore.path ()) \"SELECT count(*) AS n FROM commits\" \"n\" |> Stdlib.Option.withDefault 0L |> Stdlib.Int64.toString"
        Expect.equal
          after
          before
          $"no commit row survives a failed import (import said: {result})"
      })

let tests =
  testSequenced
  <| testList
    "MultiInstance"
    [ opsCrossAndMeanTheSameThing
      convergenceIsOrderIndependent
      receivingTwiceChangesNothing
      bindingWithoutItsContentIsVisible
      siblingNamesSurviveALoneSetName
      divergenceIsRecordedNotJustResolved
      aBatchCatchesAnInstanceUp
      decisionsCrossTheWire
      localEditsBeatAFastPeer
      aWriteBetweenReadAndMarkIsNotLost
      aMergeEventLeavesUnpushedWorkOnTheBranch
      aMergeEventHonoursTheParent
      aMergeEventCommitsWhatItFlips
      anUnbindRemovesTheNameAndNothingElse
      unbindConvergesWhateverOrderOpsArrive
      aMergedUnbindTakesTheNameOffMain
      hostedOpsAreNotThisStoresDraft
      theDarkDetectorSeesTheStoreItIsOn
      aMergeEventWaitsForItsBranch
      anEventForAnUnknownBranchDoesNotWait
      aMergeEventForALaterBranchStillApplies
      mainSyncCarriesTheAuthorsCommit
      supersededReportsCheckAuthorship
      aFailedImportLeavesNoCommit
      anOverrideRepointsCallers ]
