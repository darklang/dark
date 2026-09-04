/// Tests for the Release migrator's planning + guard logic (LibDB.Releases).
///
/// The DB-mutating half (storedRelease/writeRelease/applyPending) is exercised by every suite startup
/// — the migrator runs in `LocalExec.Migrations.run` before the Tests binary boots, so if it threw the
/// suite wouldn't start. These tests pin the PURE half — `pendingReleases` (which steps to apply) and
/// `registryIsWellFormed` (the gap/dup/over-code guard) — over injected registries, plus a guard that
/// the REAL registry is well-formed against the code's current Release.
module Tests.Releases

open Expecto
open Prelude

module Releases = LibDB.Releases

/// a DURABLE step that only declares its Release number (no migration body) — enough for the planning tests
let private step (n : int) : Releases.Release =
  { n = n; sql = ""; reserialize = None; clearForRebuild = false }

/// a CLEAN-BREAK step (invalidates on-disk content, e.g. a hashing change)
let private cleanBreakStep (n : int) : Releases.Release =
  { n = n; sql = ""; reserialize = None; clearForRebuild = true }


let pendingReleasesEmpty =
  test "pendingReleases: an empty registry never has anything to apply" {
    Expect.isEmpty (Releases.pendingReleases [] 1 5) "empty registry → no steps"
  }

let pendingReleasesRange =
  test "pendingReleases: returns exactly the steps in (storeN, codeN], ascending" {
    let registry = [ step 4; step 2; step 3; step 5 ] // deliberately unsorted
    Expect.equal
      (Releases.pendingReleases registry 2 5 |> List.map (fun r -> r.n))
      [ 3; 4; 5 ]
      "store 2 → code 5 applies 3,4,5 in order"
    Expect.equal
      (Releases.pendingReleases registry 3 4 |> List.map (fun r -> r.n))
      [ 4 ]
      "store 3 → code 4 applies only 4"
  }

let pendingReleasesNoneWhenCurrent =
  test "pendingReleases: nothing to do when the store is already at the code Release" {
    let registry = [ step 2; step 3 ]
    Expect.isEmpty
      (Releases.pendingReleases registry 3 3)
      "store == code → no steps (the common steady state)"
  }

let pendingReleasesSkipsApplied =
  test "pendingReleases: a step at or below the store Release is never re-applied" {
    let registry = [ step 2; step 3; step 4 ]
    Expect.equal
      (Releases.pendingReleases registry 3 4 |> List.map (fun r -> r.n))
      [ 4 ]
      "already-applied steps (<= storeN) are excluded"
  }

let registryWellFormed =
  test
    "registryIsWellFormed: a contiguous, distinct, in-range registry is well-formed" {
    Expect.isTrue
      (Releases.registryIsWellFormed [ step 2; step 3; step 4 ] 4)
      "2,3,4 with code 4 is well-formed"
    Expect.isTrue
      (Releases.registryIsWellFormed [] 2)
      "an empty registry is well-formed"
  }

let registryRejectsGap =
  test "registryIsWellFormed: a GAP is rejected (it would silently skip a migration)" {
    Expect.isFalse
      (Releases.registryIsWellFormed [ step 2; step 4 ] 4)
      "2 then 4 (missing 3) is not well-formed"
  }

let registryRejectsDuplicate =
  test "registryIsWellFormed: a DUPLICATE is rejected (it would double-apply)" {
    Expect.isFalse
      (Releases.registryIsWellFormed [ step 2; step 3; step 3 ] 3)
      "a repeated Release number is not well-formed"
  }

let registryRejectsAboveCode =
  test
    "registryIsWellFormed: a step ABOVE the code Release is rejected (unreachable)" {
    Expect.isFalse
      (Releases.registryIsWellFormed [ step 2; step 3 ] 2)
      "a step at Release 3 when the code speaks only 2 is not well-formed"
  }

let shippedRegistryWellFormed =
  // The guard that protects the shipped registry: whatever steps exist must be well-formed against the
  // Release this binary speaks. `applyPending` asserts this at boot; pin it here too so a malformed registry
  // fails in CI, not on someone's machine at startup.
  test
    "the shipped Release registry is well-formed against the code's current Release" {
    Expect.isTrue
      (Releases.registryIsWellFormed Releases.releases Releases.currentRelease)
      "LibDB.Releases.releases is contiguous/distinct and none above the current Release"
  }

let release3IsCleanBreak =
  test "Release 3 is the shipped meaning-stable-hashing clean-break step" {
    match Releases.releases |> List.tryFind (fun r -> r.n = 3) with
    | Some r ->
      Expect.isTrue
        r.clearForRebuild
        "Release 3 is a clean-break (clearForRebuild) — pre-v3 data is disposable, rebuilt from source"
    | None -> failtest "expected a Release 3 entry in the shipped registry"
  }

let v2UpgradesToCurrent =
  test "a v2 store upgrades to the current Release via the shipped steps" {
    let steps =
      Releases.pendingReleases Releases.releases 2 Releases.currentRelease
      |> List.map (fun r -> r.n)
    Expect.equal
      steps
      [ 3; 4; 5 ]
      "store 2 → current applies Release 3 (clean-break), 4 (committed_seq), 5 (effect-system clean-break)"
  }

// planRelease — the boot-guard DECISION, pure so the refuse-newer safety property is unit-tested without
// mutating the store. ReleaseAction carries a Release (which holds a function), so it has no structural
// equality — we match instead of Expect.equal.

let planReleaseStampFresh =
  test "planRelease: no stored Release → StampFresh (fresh or pre-tracking store)" {
    match Releases.planRelease [ step 3 ] None 3 with
    | Releases.StampFresh -> ()
    | other -> failtest $"expected StampFresh, got %A{other}"
  }

let planReleaseUpToDate =
  test "planRelease: store == code → UpToDate (the steady state)" {
    match Releases.planRelease [ step 3 ] (Some 3) 3 with
    | Releases.UpToDate -> ()
    | other -> failtest $"expected UpToDate, got %A{other}"
  }

let planReleaseRefuseNewer =
  test
    "planRelease: store > code → RefuseNewer (never open a newer store with older code)" {
    match Releases.planRelease [ step 3 ] (Some 5) 3 with
    | Releases.RefuseNewer s ->
      Expect.equal s 5 "refuses, reporting the store's Release"
    | other -> failtest $"expected RefuseNewer 5, got %A{other}"
  }

let planReleaseMigrate =
  test "planRelease: store < code → Migrate the pending steps, in order" {
    match Releases.planRelease [ step 2; step 3; step 4 ] (Some 2) 4 with
    | Releases.Migrate steps ->
      Expect.equal
        (steps |> List.map (fun r -> r.n))
        [ 3; 4 ]
        "store 2 → code 4 migrates exactly 3,4 ascending"
    | other -> failtest $"expected Migrate [3;4], got %A{other}"
  }

// planCliUpgrade — the CLI's data-preserving vs re-seed decision, layered on planRelease. Pure, so the
// "durable migrates in place / clean-break re-seeds / newer refuses" policy is pinned without a store.

let planCliUpgradeProceed =
  test "planCliUpgrade: store == code → Proceed" {
    Expect.equal
      (Releases.planCliUpgrade [ step 3 ] (Some 3) 3)
      Releases.CliUpgrade.Proceed
      "already current → nothing to do"
  }

let planCliUpgradeRefuseNewer =
  test "planCliUpgrade: store > code → RefuseNewer (older code must not open it)" {
    Expect.equal
      (Releases.planCliUpgrade [ step 3 ] (Some 5) 3)
      (Releases.CliUpgrade.RefuseNewer 5)
      "a newer store is refused, reporting its Release"
  }

let planCliUpgradeMigrateInPlace =
  test "planCliUpgrade: all-durable pending steps → MigrateInPlace (data preserved)" {
    Expect.equal
      (Releases.planCliUpgrade [ step 2; step 3; step 4 ] (Some 2) 4)
      Releases.CliUpgrade.MigrateInPlace
      "durable-only migration runs in place and keeps the store"
  }

let planCliUpgradeReseedOnCleanBreak =
  test
    "planCliUpgrade: any pending clean-break step → Reseed (content can't migrate in place)" {
    Expect.equal
      (Releases.planCliUpgrade [ step 2; cleanBreakStep 3; step 4 ] (Some 2) 4)
      Releases.CliUpgrade.Reseed
      "a clean break anywhere in the pending range forces a re-seed"
  }

let planCliUpgradeReseedPreTracking =
  test "planCliUpgrade: pre-tracking store (no stamp) → Reseed (unknown format)" {
    Expect.equal
      (Releases.planCliUpgrade [ step 3 ] None 3)
      Releases.CliUpgrade.Reseed
      "the CLI can't trust an unstamped store's on-disk format → re-seed"
  }

let applyPendingRefusesNewer =
  // The boot guard end-to-end over the real store (the pure decision is tested above; this exercises
  // writeRelease → storedRelease → applyPending against the DB). We stamp the store NEWER than the code and
  // confirm applyPending refuses. We only exercise the REFUSE path — it raises before any migration runs, so
  // it's non-destructive; a real forward step would execute the Release-3 clean-break and wipe the shared
  // test DB. try/finally restores the stamp (the store was born at currentRelease, so it's a no-op).
  test
    "applyPending refuses a store stamped at a NEWER Release (boot guard, over the DB)" {
    let cur = Releases.currentRelease
    try
      Releases.writeRelease (cur + 1)
      Expect.equal
        (Releases.storedRelease ())
        (Some(cur + 1))
        "store stamped one Release newer than the code"
      Expect.throws
        (fun () -> Releases.applyPending cur)
        "older code must refuse to open a store from a newer Release"
    finally
      Releases.writeRelease cur
  }


let tests =
  testList
    "Releases"
    [ pendingReleasesEmpty
      pendingReleasesRange
      pendingReleasesNoneWhenCurrent
      pendingReleasesSkipsApplied
      registryWellFormed
      registryRejectsGap
      registryRejectsDuplicate
      registryRejectsAboveCode
      shippedRegistryWellFormed
      release3IsCleanBreak
      v2UpgradesToCurrent
      planReleaseStampFresh
      planReleaseUpToDate
      planReleaseRefuseNewer
      planReleaseMigrate
      planCliUpgradeProceed
      planCliUpgradeRefuseNewer
      planCliUpgradeMigrateInPlace
      planCliUpgradeReseedOnCleanBreak
      planCliUpgradeReseedPreTracking
      applyPendingRefusesNewer ]
