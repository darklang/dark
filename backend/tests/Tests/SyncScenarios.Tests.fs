/// Scenario coverage for the sync conflict layer (`Sync.routeDivergences`) — the wire that turns a
/// surfaced `name → two hashes` divergence into a first-class `PT.SyncConflict.Divergence` a
/// `Sync.SyncPolicy` decides. Complements `SyncIdempotency.Tests` (the transport's idempotence +
/// LWW); here we exercise the POLICY layer.
///
/// Most scenarios are DATA: one `Scenario` record describes a divergent pull (local vs incoming hash +
/// authoring times) and a policy, and `runScenario` runs it and checks the live binding, the number of
/// reconciling ops, and the recorded conflict. Adding a case is a few fields. The handful that don't
/// fit the single-divergence shape (ties, multi-binding batches, re-pulls) stay as explicit tests.
module Tests.SyncScenarios

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module Inserts = LibDB.Inserts
module Conflicts = LibDB.Conflicts
module Resolutions = LibDB.Resolutions
module Sync = LibDB.Sync
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

// ── helpers ──────────────────────────────────────────────────────────────────────────────────

/// An authoring stamp relative to now (positive = future/newer, negative = past/older) — same
/// format the schema/sync use; no baked-in year so it runs in any calendar year.
let private relTs (minutesFromNow : float) : string =
  System.DateTime.UtcNow
    .AddMinutes(minutesFromNow)
    .ToString("yyyy-MM-ddTHH:mm:ss.fffZ")

let private callCtx : RT.CallContext =
  { branchId = PT.mainBranchId; threadID = System.Guid.NewGuid() }

/// The shipped default (mirrors `Sync.defaultSyncPolicy`): accept the last-writer-wins outcome the
/// fold already applied — pick no override, so `routeDivergences` leaves the LWW outcome standing.
let private defaultPolicy : Sync.SyncPolicy = fun _conflict _ctx -> Sync.AcceptLww

/// A keep-local policy: override to the LOCAL candidate (candidates[0]) — "my version wins".
let private keepLocalPolicy : Sync.SyncPolicy =
  fun conflict _ctx ->
    match conflict with
    | PT.SyncConflict.Divergence(_loc, local :: _) -> Sync.OverrideTo local
    | PT.SyncConflict.Divergence(_loc, []) -> Sync.AcceptLww

/// A keep-incoming policy: override to the INCOMING candidate (candidates[1]) — what already applied,
/// so a no-op rebind.
let private keepIncomingPolicy : Sync.SyncPolicy =
  fun conflict _ctx ->
    match conflict with
    | PT.SyncConflict.Divergence(_loc, _local :: incoming :: _) ->
      Sync.OverrideTo incoming
    | PT.SyncConflict.Divergence _ -> Sync.AcceptLww

let private liveHash (loc : PT.PackageLocation) : Task<Option<string>> =
  Sql.query
    "SELECT item_hash FROM locations WHERE owner=@o AND modules=@m AND name=@n AND unlisted_at IS NULL LIMIT 1"
  |> Sql.parameters
    [ "o", Sql.string loc.owner
      "m", Sql.string (String.concat "." loc.modules)
      "n", Sql.string loc.name ]
  |> Sql.executeAsync (fun read -> read.string "item_hash")
  |> fun t ->
      task {
        let! rows = t
        return List.tryHead rows
      }

let private fqOf (loc : PT.PackageLocation) : string =
  let mods = String.concat "." loc.modules
  if mods = "" then
    $"{loc.owner}.{loc.name}"
  else
    $"{loc.owner}.{mods}.{loc.name}"

let private uniqueName (prefix : string) : string =
  prefix + System.Guid.NewGuid().ToString().Replace("-", "")

let private hashChar (c : char) = System.String(c, 64)

/// Establish a local binding (name → hash) with an authoring stamp, then PULL a divergent incoming
/// (name → other hash) through the real receiver. Returns (loc, divergences) so the scenario can
/// route them through a chosen dispatch policy. `remote` keys the conflict record.
let private setupDivergentPull
  (loc : PT.PackageLocation)
  (kind : PT.ItemKind)
  (localHash : string)
  (localTs : float)
  (incomingHash : string)
  (incomingTs : float)
  (remote : string)
  : Task<List<string * string * string>> =
  task {
    let localRef = PT.Reference.fromHashAndKind (PT.Hash localHash, kind)
    let incomingRef = PT.Reference.fromHashAndKind (PT.Hash incomingHash, kind)
    let localOp = PT.PackageOp.SetName(loc, localRef)
    let! _ =
      Inserts.insertAndApplyOpsWithOrigin
        PT.mainBranchId
        None
        [ localOp ]
        (Map.ofList [ (Inserts.computeOpHash localOp, relTs localTs) ])
    let! (_cursor, divs) =
      Sync.applyRemoteOps
        remote
        PT.mainBranchId
        None
        [ (1L, relTs incomingTs, PT.PackageOp.SetName(loc, incomingRef)) ]
    return divs
  }

// ── the dense scenario form: a divergent pull as DATA ──────────────────────────────────────────

type private Policy =
  | Default // the runtime default — surface as data, LWW stands
  | KeepLocal // override to our hash
  | KeepIncoming // affirm the applied incoming hash (no-op)
  | SubstituteUnrelated // a hash bound to neither side — must be ignored

/// Which binding the location should hold after routing.
type private Winner =
  | Local
  | Incoming

/// One divergent-pull scenario. A local binding (hash `local`, authored `localAge` minutes ago) meets
/// an incoming binding (hash `incoming`, `incomingAge` minutes ago) for the same name; `policy`
/// resolves the pull. Expect the live binding on `winner`, the policy to emit `reconciled` ops, and the
/// recorded conflict's `overridden` flag to be `overridden`. (`'b' > 'a'`, so the higher hash is `'b'`.)
type private Scenario =
  { desc : string
    kind : PT.ItemKind
    local : char
    localAge : float
    incoming : char
    incomingAge : float
    policy : Policy
    winner : Winner
    reconciled : int
    overridden : bool }

let private policyFor (policy : Policy) : Sync.SyncPolicy =
  match policy with
  | Default -> defaultPolicy
  | KeepLocal -> keepLocalPolicy
  | KeepIncoming -> keepIncomingPolicy
  | SubstituteUnrelated ->
    fun _ _ ->
      Sync.OverrideTo(
        PT.Reference.fromHashAndKind (PT.Hash(hashChar 'z'), PT.ItemKind.Fn)
      )

let private runScenario (s : Scenario) : Test =
  testTask s.desc {
    let loc : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "S" ]; name = uniqueName "s" }
    let localH, incomingH = hashChar s.local, hashChar s.incoming
    let remote = uniqueName "r"
    let! divs =
      setupDivergentPull loc s.kind localH s.localAge incomingH s.incomingAge remote
    Expect.equal (List.length divs) 1 $"{s.desc}: exactly one divergence surfaced"
    let! reconciled =
      Sync.routeDivergences (policyFor s.policy) callCtx remote PT.mainBranchId divs
    Expect.equal reconciled s.reconciled $"{s.desc}: reconciling-op count"
    let! winner = liveHash loc
    let expected =
      match s.winner with
      | Local -> localH
      | Incoming -> incomingH
    Expect.equal winner (Some expected) $"{s.desc}: live binding"
    let! all = Conflicts.list ()
    match
      all |> List.filter (fun (c : Conflicts.Conflict) -> c.location = fqOf loc)
    with
    | [ c ] ->
      Expect.equal c.overridden s.overridden $"{s.desc}: conflict.overridden"
    | other ->
      failtest
        $"{s.desc}: expected exactly one recorded conflict, got {List.length other}"
  }

/// The scenario table. Each row is one convergence/policy case; the runner does the rest.
let private scenarios : List<Scenario> =
  [ { desc = "default policy: incoming is newer → LWW keeps it, nothing reconciled"
      kind = PT.ItemKind.Fn
      local = 'a'
      localAge = -120.0
      incoming = 'b'
      incomingAge = -60.0
      policy = Default
      winner = Incoming
      reconciled = 0
      overridden = false }
    { desc =
        "keep-local: override the LWW loss → our hash wins, conflict marked overridden"
      kind = PT.ItemKind.Fn
      local = 'a'
      localAge = -120.0
      incoming = 'b'
      incomingAge = -60.0
      policy = KeepLocal
      winner = Local
      reconciled = 1
      overridden = true }
    { desc = "keep-incoming: affirm the already-applied bind → a safe no-op"
      kind = PT.ItemKind.Fn
      local = 'a'
      localAge = -120.0
      incoming = 'b'
      incomingAge = -60.0
      policy = KeepIncoming
      winner = Incoming
      reconciled = 0
      overridden = false }
    { desc = "unknown substitute (bound to neither side) is ignored → LWW stands"
      kind = PT.ItemKind.Fn
      local = 'a'
      localAge = -120.0
      incoming = 'b'
      incomingAge = -60.0
      policy = SubstituteUnrelated
      winner = Incoming
      reconciled = 0
      overridden = false }
    { desc = "incoming is OLDER → local stays (the divergence is still recorded)"
      kind = PT.ItemKind.Fn
      local = 'a'
      localAge = -30.0
      incoming = 'b'
      incomingAge = -90.0
      policy = Default
      winner = Local
      reconciled = 0
      overridden = false }
    { desc = "a TYPE binding diverges too — default keeps the newer incoming"
      kind = PT.ItemKind.Type
      local = 'c'
      localAge = -120.0
      incoming = 'd'
      incomingAge = -60.0
      policy = Default
      winner = Incoming
      reconciled = 0
      overridden = false }
    { desc = "keep-local works for a TYPE binding (kind isn't fn-only)"
      kind = PT.ItemKind.Type
      local = 'c'
      localAge = -120.0
      incoming = 'd'
      incomingAge = -60.0
      policy = KeepLocal
      winner = Local
      reconciled = 1
      overridden = true }
    { desc = "keep-incoming on a TYPE binding is also a no-op"
      kind = PT.ItemKind.Type
      local = 'c'
      localAge = -120.0
      incoming = 'd'
      incomingAge = -60.0
      policy = KeepIncoming
      winner = Incoming
      reconciled = 0
      overridden = false }
    { desc = "a VALUE binding diverges too — default keeps the newer incoming"
      kind = PT.ItemKind.Value
      local = 'e'
      localAge = -120.0
      incoming = 'f'
      incomingAge = -60.0
      policy = Default
      winner = Incoming
      reconciled = 0
      overridden = false }
    { desc =
        "keep-local works for a VALUE binding (every item kind reconciles the same way)"
      kind = PT.ItemKind.Value
      local = 'e'
      localAge = -120.0
      incoming = 'f'
      incomingAge = -60.0
      policy = KeepLocal
      winner = Local
      reconciled = 1
      overridden = true } ]

// ── the scenarios that don't fit the single-divergence table ───────────────────────────────────

let private emptyConverged =
  testTask
    "empty divergence list routes to a clean zero (the converged steady state)" {
    let remote = uniqueName "rempty"
    let! reconciled =
      Sync.routeDivergences keepLocalPolicy callCtx remote PT.mainBranchId []
    Expect.equal reconciled 0 "no divergences → nothing reconciled, no ops"
  }

// The same-millisecond cross-instance tie: two DIFFERENT ops bind one name with the EXACT same
// origin_ts (two machines authored in the same ms). Resolution must be DETERMINISTIC — the higher
// item hash wins — so both machines converge regardless of which side they hold. Run it both ways.
let private sameMsTie =
  testTask
    "same-millisecond tie resolves deterministically by hash (higher wins, both ways)" {
    let mk suffix : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "Tie" ]; name = uniqueName suffix }
    let lowH, highH = hashChar 'a', hashChar 'b' // 'b' > 'a' → highH wins
    let tie = "2025-01-01T00:00:00.000Z" // one exact stamp both sides share
    let refOf h = PT.Reference.fromHashAndKind (PT.Hash h, PT.ItemKind.Fn)

    let runTie (localH : string) (incomingH : string) : Task<Option<string>> =
      task {
        let loc = mk "tie"
        let localOp = PT.PackageOp.SetName(loc, refOf localH)
        let! _ =
          Inserts.insertAndApplyOpsWithOrigin
            PT.mainBranchId
            None
            [ localOp ]
            (Map.ofList [ (Inserts.computeOpHash localOp, tie) ])
        let! _ =
          Sync.applyRemoteOps
            (uniqueName "rtie")
            PT.mainBranchId
            None
            [ (1L, tie, PT.PackageOp.SetName(loc, refOf incomingH)) ]
        return! liveHash loc
      }

    // incoming holds the higher hash → incoming wins
    let! a = runTie lowH highH
    Expect.equal a (Some highH) "higher hash wins (incoming was higher)"
    // local holds the higher hash → incoming is stale, local stays — SAME winner
    let! b = runTie highH lowH
    Expect.equal b (Some highH) "higher hash wins (incoming was lower → stale)"
  }

let private multiDivergenceBatch =
  testTask
    "multi-divergence batch (default policy): every location surfaces + LWW converges" {
    // The SHIPPED path: a single pull carrying TWO divergent bindings. The default policy
    // reconciles nothing (surface-as-data); each location converges to its own LWW winner.
    let mk suffix : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "Multi" ]; name = uniqueName suffix }
    let loc1, loc2 = mk "m1", mk "m2"
    let local1, incoming1 = hashChar 'a', hashChar 'b'
    let local2, incoming2 = hashChar 'c', hashChar 'd'
    let remote = uniqueName "rmulti"
    let! divs1 =
      setupDivergentPull loc1 PT.ItemKind.Fn local1 -120.0 incoming1 -60.0 remote
    let! divs2 =
      setupDivergentPull loc2 PT.ItemKind.Fn local2 -120.0 incoming2 -60.0 remote
    let divs = divs1 @ divs2
    Expect.equal (List.length divs) 2 "two divergences collected from the batch"
    let! reconciled =
      Sync.routeDivergences defaultPolicy callCtx remote PT.mainBranchId divs
    Expect.equal reconciled 0 "default policy reconciles nothing (surface-as-data)"
    let! w1 = liveHash loc1
    let! w2 = liveHash loc2
    Expect.equal w1 (Some incoming1) "first location converged to its LWW winner"
    Expect.equal w2 (Some incoming2) "second location converged to its LWW winner"
  }

// Regression: a keep-local override must APPEND a distinct, newer op (a fresh `OverrideName` with its
// own rowid), not re-stamp the existing op in place. Incremental sync is by commit-rowid, so a re-stamp
// (same rowid) never reaches a peer that already pulled the op — the binding stays diverged. A fresh op
// above the peer's cursor DOES ride the next pull, and its newest `origin_ts` wins timestamp-LWW. This
// asserts both the local effect (our hash wins) AND the propagation property (a new op, above the prior
// max rowid, carrying the newest stamp).
let private keepLocalAppendsPropagableOverride =
  testTask "keep-local override appends a distinct, newer op so it can propagate" {
    let loc : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "Prop" ]; name = uniqueName "p" }
    let local, incoming = hashChar 'a', hashChar 'b'
    let remote = uniqueName "rprop"
    let! divs =
      setupDivergentPull loc PT.ItemKind.Fn local -120.0 incoming -60.0 remote
    let maxRowid () : Task<int64> =
      Sql.query "SELECT COALESCE(MAX(rowid), 0) AS m FROM package_ops"
      |> Sql.executeRowAsync (fun read -> read.int64 "m")
    let stampOf sql ps : Task<string> =
      Sql.query sql
      |> Sql.parameters ps
      |> Sql.executeRowAsync (fun read -> read.string "origin_ts")
    let incomingRef = PT.Reference.fromHashAndKind (PT.Hash incoming, PT.ItemKind.Fn)
    let incomingOpId = Inserts.computeOpHash (PT.PackageOp.SetName(loc, incomingRef))
    let! cursorBefore = maxRowid ()
    let! incomingStamp =
      stampOf
        "SELECT origin_ts FROM package_ops WHERE id = @id LIMIT 1"
        [ "id", Sql.uuid incomingOpId ]
    // keep-local override (the same path the human 'mine' override uses)
    let! _ =
      Sync.routeDivergences keepLocalPolicy callCtx remote PT.mainBranchId divs
    // 1. our hash is the live binding
    let! winner = liveHash loc
    Expect.equal winner (Some local) "keep-local: our hash is the live binding"
    // 2. a NEW op was appended above any synced peer's cursor (an in-place re-stamp would add no row)
    let! cursorAfter = maxRowid ()
    Expect.isGreaterThan
      cursorAfter
      cursorBefore
      "the override appended a new op above the peer's cursor — so it rides the next incremental pull"
    // 3. that newest op carries the newest origin_ts → a re-pulling peer adopts our hash by timestamp-LWW
    let! overrideStamp =
      stampOf "SELECT origin_ts FROM package_ops ORDER BY rowid DESC LIMIT 1" []
    Expect.isGreaterThan
      overrideStamp
      incomingStamp
      "the override op's origin_ts is the newest — a re-pulling peer re-adopts our hash"
  }

// An override only propagates if it survives the wire: a peer DESERIALIZES the op_blob (read tag 8) and
// folds it. The keep-local path only ever serializes + folds the in-memory op, so exercise the read path
// directly — `OverrideName` (with its `resolvedAt`) must round-trip byte-for-byte through the op codec.
let private overrideOpRoundTrips =
  test "OverrideName round-trips through the op serializer (rides the wire — tag 8)" {
    let loc : PT.PackageLocation = { owner = "RT"; modules = [ "M" ]; name = "x" }
    let target = PT.Reference.fromHashAndKind (PT.Hash(hashChar 'a'), PT.ItemKind.Fn)
    let op = PT.PackageOp.OverrideName(loc, target, "2026-06-11T12:34:56.789Z")
    let id = Inserts.computeOpHash op
    let blob = LibSerialization.Binary.Serialization.PT.PackageOp.serialize id op
    let decoded =
      LibSerialization.Binary.Serialization.PT.PackageOp.deserialize id blob
    Expect.equal
      decoded
      op
      "OverrideName survives binary serialize → deserialize unchanged"
  }

// A `SyncConflict` and its `DivergenceResolution` must survive the binary codec — they're persisted
// (the recorded conflict's blob) and may travel, so a tag-byte round-trip is the contract. Covers both
// `ResolvedBy` cases (`Auto policy` and `Human`) since they have distinct tags.
let private syncConflictRoundTrips =
  test "SyncConflict + DivergenceResolution round-trip through the binary serializer" {
    let loc : PT.PackageLocation = { owner = "RT"; modules = [ "M" ]; name = "x" }
    let refA = PT.Reference.fromHashAndKind (PT.Hash(hashChar 'a'), PT.ItemKind.Fn)
    let refB = PT.Reference.fromHashAndKind (PT.Hash(hashChar 'b'), PT.ItemKind.Fn)

    let conflict = PT.SyncConflict.Divergence(loc, [ refA; refB ])
    let cBlob =
      LibSerialization.Binary.Serialization.PT.SyncConflict.serialize "c" conflict
    let cDecoded =
      LibSerialization.Binary.Serialization.PT.SyncConflict.deserialize "c" cBlob
    Expect.equal cDecoded conflict "SyncConflict survives serialize → deserialize"

    let resAuto : PT.DivergenceResolution =
      { chosen = refB; by = PT.ResolvedBy.Auto "last-writer-wins" }
    let aBlob =
      LibSerialization.Binary.Serialization.PT.DivergenceResolution.serialize
        "a"
        resAuto
    let aDecoded =
      LibSerialization.Binary.Serialization.PT.DivergenceResolution.deserialize
        "a"
        aBlob
    Expect.equal aDecoded resAuto "DivergenceResolution(Auto) survives round-trip"

    let resHuman : PT.DivergenceResolution =
      { chosen = refA; by = PT.ResolvedBy.Human }
    let hBlob =
      LibSerialization.Binary.Serialization.PT.DivergenceResolution.serialize
        "h"
        resHuman
    let hDecoded =
      LibSerialization.Binary.Serialization.PT.DivergenceResolution.deserialize
        "h"
        hBlob
    Expect.equal hDecoded resHuman "DivergenceResolution(Human) survives round-trip"
  }

// End-to-end, the RECEIVER half: a peer currently bound to the incoming hash (it already pulled the
// race) receives the OTHER machine's committed override op over the normal receive path and must ADOPT
// our hash. This is the actual cross-machine propagation — the headline override-propagation claim — exercised through
// `applyRemoteOps` (the same path an HTTP/file pull uses). An `OverrideName` is NOT re-flagged as a new
// divergence (it isn't a SetName), so it just folds, and its newer stamp wins timestamp-LWW.
let private overridePropagatesToPeer =
  testTask
    "a peer receiving a committed override op adopts our hash (end-to-end, receiver side)" {
    let loc : PT.PackageLocation =
      { owner = "Recv"; modules = [ "O" ]; name = uniqueName "r" }
    let ours, theirs = hashChar 'a', hashChar 'b'
    let remote = uniqueName "rrecv"
    // the peer is currently bound to the incoming hash `theirs` (older), having pulled the race already
    let theirsOp =
      PT.PackageOp.SetName(
        loc,
        PT.Reference.fromHashAndKind (PT.Hash theirs, PT.ItemKind.Fn)
      )
    let! _ =
      Inserts.insertAndApplyOpsWithOrigin
        PT.mainBranchId
        None
        [ theirsOp ]
        (Map.ofList [ (Inserts.computeOpHash theirsOp, relTs -60.0) ])
    let! before = liveHash loc
    Expect.equal
      before
      (Some theirs)
      "precondition: the peer holds the incoming hash"
    // now it pulls the other machine's override op (re-bind to `ours`, newest stamp) over the wire path
    let overrideOp =
      PT.PackageOp.OverrideName(
        loc,
        PT.Reference.fromHashAndKind (PT.Hash ours, PT.ItemKind.Fn),
        relTs 0.0
      )
    let! _ =
      Sync.applyRemoteOps remote PT.mainBranchId None [ (1L, relTs 0.0, overrideOp) ]
    let! after = liveHash loc
    Expect.equal
      after
      (Some ours)
      "the peer adopted our override — the resolution propagated cross-machine"
  }

let private orderIndependent =
  testTask
    "order-independent: both machines converge to the newer op regardless of arrival side" {
    let mk suffix : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "Order" ]; name = uniqueName suffix }
    let a, b = hashChar 'a', hashChar 'b'
    // machine-1: local=a (older), incoming=b (newer) → b wins as the incoming
    let loc1 = mk "ord1"
    let! _ =
      setupDivergentPull loc1 PT.ItemKind.Fn a -120.0 b -60.0 (uniqueName "ro1")
    let! w1 = liveHash loc1
    // machine-2: local=b (newer), incoming=a (older) → b stays as the local
    let loc2 = mk "ord2"
    let! _ =
      setupDivergentPull loc2 PT.ItemKind.Fn b -60.0 a -120.0 (uniqueName "ro2")
    let! w2 = liveHash loc2
    Expect.equal w1 (Some b) "machine where b arrived incoming: b (newer) won"
    Expect.equal
      w2
      (Some b)
      "machine where b was local: b (newer) stayed — same winner"
  }

let private idempotentRePull =
  testTask
    "re-pulling an already-applied divergent op is a no-op (no new conflict, no flip)" {
    let loc : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "NoOp" ]; name = uniqueName "np" }
    let localH, incomingH = hashChar 'a', hashChar 'b'
    let remote = uniqueName "rnoop"
    let! _ =
      setupDivergentPull loc PT.ItemKind.Fn localH -120.0 incomingH -60.0 remote
    let! winner1 = liveHash loc
    Expect.equal winner1 (Some incomingH) "incoming (newer) won the first pull"
    // re-deliver the same incoming op
    let incomingRef =
      PT.Reference.fromHashAndKind (PT.Hash incomingH, PT.ItemKind.Fn)
    let! (_cursor, divs2) =
      Sync.applyRemoteOps
        remote
        PT.mainBranchId
        None
        [ (1L, relTs -60.0, PT.PackageOp.SetName(loc, incomingRef)) ]
    Expect.isEmpty divs2 "re-pulling the same op surfaces no new divergence"
    let! winner2 = liveHash loc
    Expect.equal
      winner2
      (Some incomingH)
      "binding unchanged after the idempotent re-pull"
  }

// A resolution STICKS and propagates: after keep-local re-binds + re-stamps our hash to now, the
// superseded incoming op (its old, now-stale stamp) re-arriving does NOT flip the binding back. And
// because our op is now the newest-by-creation, a peer pulling it re-adopts our hash — the resolution
// rides sync to the other instances (it doesn't live only on the machine that made it).
let private resolutionSticks =
  testTask
    "a keep-local resolution holds: the superseded incoming op re-arriving doesn't undo it" {
    let loc : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "Stick" ]; name = uniqueName "st" }
    let local, incoming = hashChar 'a', hashChar 'b'
    let remote = uniqueName "rstick"
    // incoming newer → it won the pull; keep-local overrides, re-binding our hash with a now-stamp
    let! divs =
      setupDivergentPull loc PT.ItemKind.Fn local -120.0 incoming -60.0 remote
    let! _ =
      Sync.routeDivergences keepLocalPolicy callCtx remote PT.mainBranchId divs
    let! afterResolve = liveHash loc
    Expect.equal
      afterResolve
      (Some local)
      "keep-local re-bound the location to our hash"
    // the same incoming op re-arrives with its original (now-stale) stamp — must not win
    let incomingRef = PT.Reference.fromHashAndKind (PT.Hash incoming, PT.ItemKind.Fn)
    let! (_c, _divs2) =
      Sync.applyRemoteOps
        remote
        PT.mainBranchId
        None
        [ (1L, relTs -60.0, PT.PackageOp.SetName(loc, incomingRef)) ]
    let! afterRePull = liveHash loc
    Expect.equal
      afterRePull
      (Some local)
      "the resolution holds — the superseded incoming op doesn't flip it back"
  }

// Weird timing: ops don't arrive in creation order. A stale op (older authoring time) delivered AFTER
// a fresher one must NOT overwrite it — convergence is by authoring time, not arrival order.
let private lateStaleArrival =
  testTask
    "out-of-order timing: a stale op arriving after a fresher one does not overwrite it" {
    let loc : PT.PackageLocation =
      { owner = "Scenario"; modules = [ "Late" ]; name = uniqueName "lt" }
    let a, b, c = hashChar 'a', hashChar 'b', hashChar 'c'
    let remote = uniqueName "rlate"
    let refOf h = PT.Reference.fromHashAndKind (PT.Hash h, PT.ItemKind.Fn)
    // local 'a' @ -60
    let aOp = PT.PackageOp.SetName(loc, refOf a)
    let! _ =
      Inserts.insertAndApplyOpsWithOrigin
        PT.mainBranchId
        None
        [ aOp ]
        (Map.ofList [ (Inserts.computeOpHash aOp, relTs -60.0) ])
    // a fresher 'c' @ -30 arrives → wins
    let! _ =
      Sync.applyRemoteOps
        remote
        PT.mainBranchId
        None
        [ (1L, relTs -30.0, PT.PackageOp.SetName(loc, refOf c)) ]
    let! afterFresh = liveHash loc
    Expect.equal
      afterFresh
      (Some c)
      "the freshest op ('c' @ -30) is the live binding"
    // a STALE 'b' @ -120 arrives LATE → older than 'c' → must not overwrite
    let! _ =
      Sync.applyRemoteOps
        remote
        PT.mainBranchId
        None
        [ (1L, relTs -120.0, PT.PackageOp.SetName(loc, refOf b)) ]
    let! afterStale = liveHash loc
    Expect.equal
      afterStale
      (Some c)
      "the stale late arrival ('b' @ -120) did not overwrite the fresher 'c'"
  }

// Three competing instances edit one name at different times; whatever order their ops arrive, all
// converge to the newest-by-creation. Proven by applying the same three ops in two different orders.
let private threeWayConverge =
  testTask
    "three competing instances converge to the newest op, regardless of arrival order" {
    let a, b, c = hashChar 'a', hashChar 'b', hashChar 'c'
    let refOf h = PT.Reference.fromHashAndKind (PT.Hash h, PT.ItemKind.Fn)
    // ages: a = -90 (oldest), c = -60, b = -30 (newest) → 'b' should win, both orders
    let converge
      (suffix : string)
      (order : List<string * float>)
      : Task<Option<string>> =
      task {
        let loc : PT.PackageLocation =
          { owner = "Scenario"; modules = [ "Three" ]; name = uniqueName suffix }
        match order with
        | (h0, t0) :: rest ->
          let op0 = PT.PackageOp.SetName(loc, refOf h0)
          let! _ =
            Inserts.insertAndApplyOpsWithOrigin
              PT.mainBranchId
              None
              [ op0 ]
              (Map.ofList [ (Inserts.computeOpHash op0, relTs t0) ])
          for (h, t) in rest do
            let! _ =
              Sync.applyRemoteOps
                (uniqueName "r3")
                PT.mainBranchId
                None
                [ (1L, relTs t, PT.PackageOp.SetName(loc, refOf h)) ]
            ()
          return! liveHash loc
        | [] -> return None
      }
    let! w1 = converge "o1" [ (a, -90.0); (c, -60.0); (b, -30.0) ]
    let! w2 = converge "o2" [ (b, -30.0); (a, -90.0); (c, -60.0) ]
    Expect.equal
      w1
      (Some b)
      "converges to the newest ('b' @ -30) — arrival order a, c, b"
    Expect.equal
      w2
      (Some b)
      "converges to the newest ('b' @ -30) — arrival order b, a, c (same winner)"
  }

// The resolution OVERLAY: a resolution overrides the op-fold binding when its `at` is newer, and is
// skipped when stale — the same timestamp-LWW that orders ops, so it converges. This is the mechanism
// that replaces `OverrideName` (a synced decision over the fold, not a new op).
let private resolutionOverlayApplies =
  testTask
    "a resolution overrides the op-fold binding by its newer `at` (a stale one is skipped)" {
    let loc : PT.PackageLocation =
      { owner = "Resln"; modules = [ "R" ]; name = uniqueName "r" }
    let a, b, c = hashChar 'a', hashChar 'b', hashChar 'c'
    let refOf h = PT.Reference.fromHashAndKind (PT.Hash h, PT.ItemKind.Fn)
    // op-fold binds loc -> a (authored -60)
    let aOp = PT.PackageOp.SetName(loc, refOf a)
    let! _ =
      Inserts.insertAndApplyOpsWithOrigin
        PT.mainBranchId
        None
        [ aOp ]
        (Map.ofList [ (Inserts.computeOpHash aOp, relTs -60.0) ])
    let! before = liveHash loc
    Expect.equal before (Some a) "precondition: op-fold bound loc -> a"
    // a resolution choosing b, stamped NEWER (now) -> overrides to b
    do!
      Resolutions.recordAndApply (
        Resolutions.mk loc (refOf b) "human" PT.mainBranchId (relTs 0.0)
      )
    let! afterB = liveHash loc
    Expect.equal
      afterB
      (Some b)
      "the resolution (newer `at`) overrode the binding to b"
    // a STALE resolution choosing c, stamped OLDER (-120) -> skipped, b stays
    do!
      Resolutions.recordAndApply (
        Resolutions.mk loc (refOf c) "human" PT.mainBranchId (relTs -120.0)
      )
    let! afterC = liveHash loc
    Expect.equal
      afterC
      (Some b)
      "the stale resolution (older `at`) was skipped — b stays"
  }

// ── all scenarios ──────────────────────────────────────────────────────────────────────────────

let tests =
  testSequenced
  <| testList
    "SyncScenarios"
    ((scenarios |> List.map runScenario)
     @ [ emptyConverged
         sameMsTie
         multiDivergenceBatch
         keepLocalAppendsPropagableOverride
         overrideOpRoundTrips
         syncConflictRoundTrips
         overridePropagatesToPeer
         orderIndependent
         idempotentRePull
         resolutionSticks
         lateStaleArrival
         threeWayConverge
         resolutionOverlayApplies ])
