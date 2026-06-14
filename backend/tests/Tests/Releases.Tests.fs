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
module Sync = LibDB.Sync

/// a step that only declares its Release number (no migration body) — enough for the planning tests
let private step (n : int) : Releases.Release =
  { n = n; sql = ""; reserialize = None; clearForRebuild = false }

let tests =
  testList
    "Releases"
    [ test "pendingReleases: an empty registry never has anything to apply" {
        Expect.isEmpty (Releases.pendingReleases [] 1 5) "empty registry → no steps"
      }

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

      test
        "pendingReleases: nothing to do when the store is already at the code Release" {
        let registry = [ step 2; step 3 ]
        Expect.isEmpty
          (Releases.pendingReleases registry 3 3)
          "store == code → no steps (the common steady state)"
      }

      test
        "pendingReleases: a step at or below the store Release is never re-applied" {
        let registry = [ step 2; step 3; step 4 ]
        Expect.equal
          (Releases.pendingReleases registry 3 4 |> List.map (fun r -> r.n))
          [ 4 ]
          "already-applied steps (<= storeN) are excluded"
      }

      test
        "registryIsWellFormed: a contiguous, distinct, in-range registry is well-formed" {
        Expect.isTrue
          (Releases.registryIsWellFormed [ step 2; step 3; step 4 ] 4)
          "2,3,4 with code 4 is well-formed"
        Expect.isTrue
          (Releases.registryIsWellFormed [] 2)
          "an empty registry is well-formed"
      }

      test
        "registryIsWellFormed: a GAP is rejected (it would silently skip a migration)" {
        Expect.isFalse
          (Releases.registryIsWellFormed [ step 2; step 4 ] 4)
          "2 then 4 (missing 3) is not well-formed"
      }

      test "registryIsWellFormed: a DUPLICATE is rejected (it would double-apply)" {
        Expect.isFalse
          (Releases.registryIsWellFormed [ step 2; step 3; step 3 ] 3)
          "a repeated Release number is not well-formed"
      }

      test
        "registryIsWellFormed: a step ABOVE the code Release is rejected (unreachable)" {
        Expect.isFalse
          (Releases.registryIsWellFormed [ step 2; step 3 ] 2)
          "a step at Release 3 when the code speaks only 2 is not well-formed"
      }

      // The guard that protects the shipped registry: whatever steps exist must be well-formed against
      // the Release this binary actually speaks. `applyPending` asserts this at boot; pin it here too so
      // a malformed registry fails in CI, not on someone's machine at startup.
      test
        "the shipped Release registry is well-formed against the code's current Release" {
        Expect.isTrue
          (Releases.registryIsWellFormed Releases.releases Sync.wireFormatVersion)
          "LibDB.Releases.releases is contiguous/distinct and none above wireFormatVersion"
      }

      // Release 3 (meaning-stable hashing) is the first real step, and it's a clean-break boundary.
      test "Release 3 is the shipped meaning-stable-hashing clean-break step" {
        match Releases.releases |> List.tryFind (fun r -> r.n = 3) with
        | Some r ->
          Expect.isTrue
            r.clearForRebuild
            "Release 3 is a clean-break (clearForRebuild) — pre-v3 data is disposable, rebuilt from source"
        | None -> failtest "expected a Release 3 entry in the shipped registry"
      }

      // a v2 store upgrades to the current code Release via exactly the shipped steps (here: just 3).
      test "a v2 store upgrades to the current Release via the shipped steps" {
        let steps =
          Releases.pendingReleases Releases.releases 2 Sync.wireFormatVersion
          |> List.map (fun r -> r.n)
        Expect.equal
          steps
          [ 3 ]
          "store 2 → code 3 applies exactly Release 3 (the clean-break)"
      } ]
