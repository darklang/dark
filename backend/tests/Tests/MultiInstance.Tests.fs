/// Two stores, one wire: does the op log actually converge?
///
/// This is the claim the whole design rests on and the one thing a single-store test cannot reach. An op's
/// id is its content, `origin_ts` is portable, and the fold picks a winner deterministically -- so two
/// instances that have seen the same ops must agree on what every name means, whatever order the ops
/// arrived in. Nothing checked that.
///
/// Each "instance" is its own SQLite file, and `Sql.useStoreForTesting` repoints all of LibDB at it, so
/// these are real separate stores rather than two branches pretending. That swap is process-global, hence
/// `testSequenced` and a teardown that always hands the default store back.
///
/// The wire is driven directly (`importOpsBulk` + fold) rather than over HTTP. HTTP has its own tests in
/// `SyncTransport`; mixing the two would mean a convergence failure and a networking failure look alike.
module Tests.MultiInstance

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Fumble
open LibDB.Sqlite

module Seed = LibDB.Seed
module Inserts = LibDB.Inserts
module PT = LibExecution.ProgramTypes
module BS = LibSerialization.Binary.Serialization
module Hashing = LibSerialization.Hashing.Hashing

open TestUtils.TestUtils


// ── instances ────────────────────────────────────────────────────────────────────────────────────

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

let private activate (inst : Instance) : unit = Sql.useStoreForTesting inst.path

let private teardown (insts : List<Instance>) : unit =
  Sql.resetStoreForTesting ()
  insts |> List.iter (fun i -> deleteStore i.path)


// ── the wire ─────────────────────────────────────────────────────────────────────────────────────

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
  let l = loc name
  Sql.query
    "SELECT item_hash FROM locations
     WHERE owner = @o AND modules = @m AND name = @n AND unlisted_at IS NULL"
  |> Sql.parameters
    [ "o", Sql.string l.owner
      "m", Sql.string (String.concat "." l.modules)
      "n", Sql.string l.name ]
  |> Sql.executeRowOptionAsync (fun read -> read.string "item_hash")


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
    // Main's id comes from the product's own constant, never a literal. Spelled by hand this asked for ''
    // and kept passing while `dark propagate list` showed nothing; spelled by hand again it would have
    // gone stale the moment main's id became a uuid.
    "SELECT policy, COALESCE(note, '') AS note FROM propagation_policy
     WHERE branch_id = @main AND owner = @o AND modules = @m AND name = @n"
  |> Sql.parameters
    [ "main", Sql.string (string PT.BranchId.Main)
      "o", Sql.string l.owner
      "m", Sql.string (String.concat "." l.modules)
      "n", Sql.string l.name ]
  |> Sql.executeRowOptionAsync (fun read ->
    (read.string "policy", read.string "note"))


// ── tests ────────────────────────────────────────────────────────────────────────────────────────

/// The base case, and the one everything else assumes: an op authored on one instance means the same thing
/// on the other once it has crossed.
let opsCrossAndMeanTheSameThing =
  testTask "an op authored on A means the same thing on B" {
    let a = instance "a"
    let b = instance "b"

    try
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
    finally
      teardown [ a; b ]
  }

/// Order must not matter. The whole point of a content-addressed log with a portable stamp is that two
/// instances receiving the same ops in opposite orders end up identical -- otherwise "who synced first"
/// silently decides what your code means.
let convergenceIsOrderIndependent =
  testTask "two instances converge on the same winner whatever order ops arrive in" {
    let a = instance "a"
    let b = instance "b"

    try
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

      // And they agree on the LATER one specifically, which is the rule rather than an accident.
      activate a
      let! expected = boundHash "beta"
      let (winnerId, _, _) = later
      let! winnerRow =
        Sql.query "SELECT 1 AS n FROM package_ops WHERE id = @id"
        |> Sql.parameters [ "id", Sql.string winnerId ]
        |> Sql.executeRowOptionAsync (fun read -> read.int64 "n")
      Expect.isSome winnerRow "the later op is in the log"
      Expect.equal expected onB "and it is what the name resolves to"
    finally
      teardown [ a; b ]
  }

/// Re-receiving is free. Sync is not transactional across a network, so the same op arrives twice all the
/// time; if that were not idempotent, every retry would grow the log and re-fold work already done.
let receivingTwiceChangesNothing =
  testTask "receiving the same op twice is idempotent" {
    let a = instance "a"

    try
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
    finally
      teardown [ a ]
  }

/// A divergence has to be RECORDED, not just settled. Converging quietly is the easy half; the promise
/// this design makes is that the version that lost is still there and you get told about it.
/// A `SetName` that arrives alone must bind ITS OWN name and touch no other.
///
/// The fold used to read a standalone SetName -- one with no `Add*` in the same batch -- as a RENAME, and
/// deprecate every other location on that hash. Two names with identical bodies are one item here, which
/// the codebase calls routine, so that unlisted a colleague's names on arrival. Batch framing is not
/// intent: a page boundary or a truncated bundle splits an Add from its SetName and produces exactly this
/// op, and nothing in the model ever says "this is a rename".
let siblingNamesSurviveALoneSetName =
  testTask "a SetName arriving without its Add leaves other names on that hash alone" {
    let a = instance "lonesetname"

    try
      activate a

      // Two names, one hash: `setName` builds both from the same content.
      let! _ =
        receive [ wireOp (setName "twinA" "shared-body") "2026-01-01T00:00:00.000Z" ]
      let! _ =
        receive [ wireOp (setName "twinB" "shared-body") "2026-01-01T00:00:01.000Z" ]

      let! aBefore = boundHash "twinA"
      let! bBefore = boundHash "twinB"
      Expect.isSome aBefore "twinA is bound"
      Expect.equal
        bBefore
        aBefore
        "and twinB is bound to the SAME hash, which is the whole setup"

      // A third name for that same content, arriving on its own.
      let! _ =
        receive [ wireOp (setName "twinC" "shared-body") "2026-01-02T00:00:00.000Z" ]

      let! aAfter = boundHash "twinA"
      let! bAfter = boundHash "twinB"
      let! cAfter = boundHash "twinC"
      Expect.equal cAfter aBefore "the arriving name binds the shared hash"
      Expect.equal aAfter aBefore "and twinA is untouched"
      Expect.equal bAfter bBefore "and so is twinB"
    finally
      teardown [ a ]
  }


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
  testTask "a name whose content never arrived is bound, and the gap is findable" {
    let a = instance "dangling"

    try
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
    finally
      teardown [ a ]
  }


let divergenceIsRecordedNotJustResolved =
  testTask "when both sides moved a name, the loser is kept and recorded" {
    let a = instance "a"

    try
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
    finally
      teardown [ a ]
  }

/// An instance that has been away must catch up in one go, not one op at a time. This is the shape a real
/// pull has: a batch, arriving at a store that is several edits behind.
let aBatchCatchesAnInstanceUp =
  testTask "a batch of ops brings a behind instance level" {
    let a = instance "a"
    let b = instance "b"

    try
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
    finally
      teardown [ a; b ]
  }


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
  testTask "a pin authored on A holds on B, with its reason" {
    let a = instance "a"
    let b = instance "b"

    try
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
    finally
      teardown [ a; b ]
  }


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
  testTask "an edit made after seeing a future-stamped op still wins" {
    let a = instance "a"

    try
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
    finally
      teardown [ a ]
  }


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
      localEditsBeatAFastPeer ]
