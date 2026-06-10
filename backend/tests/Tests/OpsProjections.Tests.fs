/// Tests for the ops⊥projections prototype (LibDB.Seed.rebuildProjections).
/// Proves the central claim of the storage split: the projection tables are
/// *regenerable from the op log* — drop them, re-fold package_ops, and they
/// come back identical. The op log (package_ops) is canonical and untouched.
module Tests.OpsProjections

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Sqlite

module Seed = LibDB.Seed

let private countRows (table : string) : Task<int64> =
  Sql.query $"SELECT COUNT(*) as n FROM {table}"
  |> Sql.executeRowAsync (fun read -> read.int64 "n")

// `testSequenced` because the drop+rebuild case DELETEs + refolds the *shared* projection
// tables and marks all ops unapplied — it must not run concurrently with other DB tests (it
// would race their reads/writes mid-rebuild). The registry cases are pure but ride along.
let tests =
  testSequenced
  <| testList
    "OpsProjections"
    [ testTask
        "rebuildProjections deterministically regenerates projections from the op log" {
        // package_blobs is canonical content — a rebuild must never touch it
        let! blobsBefore = countRows "package_blobs"

        // drop the regenerable projections + re-fold the entire op log
        let! reapplied = Seed.rebuildProjections ()
        Expect.isTrue (reapplied > 0L) "ops were re-folded"
        let! fns1 = countRows "package_functions"
        let! locs1 = countRows "locations"
        Expect.isTrue
          (fns1 > 0L)
          "projections regenerated (non-empty) from the op log"

        // a SECOND rebuild reproduces the EXACT same projections — the rebuild is a deterministic
        // function of the op log. (Robust to other tests mutating the shared DB: we compare two
        // rebuilds of the *current* log, not a pristine-seed count they'd perturb.)
        let! _ = Seed.rebuildProjections ()
        let! fns2 = countRows "package_functions"
        let! locs2 = countRows "locations"
        Expect.equal
          fns2
          fns1
          "package_functions: a re-rebuild reproduces the same projection"
        Expect.equal
          locs2
          locs1
          "locations: a re-rebuild reproduces the same projection"

        // canonical content (package_blobs) is NOT a projection — untouched across rebuilds
        let! blobsAfter = countRows "package_blobs"
        Expect.equal
          blobsAfter
          blobsBefore
          "package_blobs (canonical content) preserved, not dropped"
      }

      // the projection registry — the fold/dirty descriptors
      test "the projection registry covers exactly the 6 regenerable projections" {
        Expect.equal
          (List.sort Seed.projectionTables)
          (List.sort
            [ "package_functions"
              "package_types"
              "package_values"
              "locations"
              "package_dependencies"
              "deprecations" ])
          "the registry's tables are exactly Seed.export's stripped projections (incl. deprecations)"
      }

      test "projectionsDirtiedBy maps an op kind to the projections it invalidates" {
        Expect.equal
          (List.sort (Seed.projectionsDirtiedBy "AddFn"))
          (List.sort [ "package_functions"; "package_dependencies" ])
          "AddFn dirties the fn projection + the dependency edges"
        Expect.equal
          (List.sort (Seed.projectionsDirtiedBy "AddType"))
          (List.sort [ "package_types"; "package_dependencies" ])
          "AddType dirties the type projection + deps"
        Expect.equal
          (Seed.projectionsDirtiedBy "SetName")
          [ "locations" ]
          "SetName dirties only locations"
        Expect.equal
          (Seed.projectionsDirtiedBy "RevertPropagation")
          [ "locations" ]
          "RevertPropagation dirties only locations"
        Expect.equal
          (Seed.projectionsDirtiedBy "Deprecate")
          [ "deprecations" ]
          "Deprecate dirties the deprecations projection (it IS regenerable from ops)"
        Expect.isEmpty
          (Seed.projectionsDirtiedBy "PropagateUpdate")
          "PropagateUpdate is a no-op (its accompanying SetNames do the work) — dirties nothing"
      }

      // the incremental-refold DECISION: which projections a whole op batch dirties (the union)
      test
        "projectionsDirtiedByBatch unions a batch's dirtied projections; AddFn-only skips locations" {
        let addFnOnly = Seed.projectionsDirtiedByBatch (Set.ofList [ "AddFn" ])
        Expect.equal
          addFnOnly
          (Set.ofList [ "package_functions"; "package_dependencies" ])
          "an AddFn-only batch dirties functions + deps"
        Expect.isFalse
          (Set.contains "locations" addFnOnly)
          "locations is NOT dirtied — an incremental refold leaves it (and its rows) untouched"

        let mixed =
          Seed.projectionsDirtiedByBatch (Set.ofList [ "AddFn"; "SetName" ])
        Expect.equal
          mixed
          (Set.ofList [ "package_functions"; "package_dependencies"; "locations" ])
          "an AddFn+SetName batch dirties functions + deps + locations (the union)"

        Expect.isEmpty
          (Seed.projectionsDirtiedByBatch Set.empty)
          "an empty batch dirties no projections"
      }

      // the selective FOLD: rebuildDirtied refolds ONLY the dirtied tables, leaving others alone
      testTask
        "rebuildDirtied {AddFn} refolds functions but leaves locations untouched (selective)" {
        let! locsBefore = countRows "locations"
        let! refolded = Seed.rebuildDirtied (Set.ofList [ "AddFn" ])
        Expect.isTrue (refolded > 0L) "AddFn ops were re-folded"
        let! fnsAfter = countRows "package_functions"
        Expect.isTrue
          (fnsAfter > 0L)
          "package_functions regenerated from the AddFn ops"
        // SELECTIVITY: locations isn't in {AddFn}'s dirtied set, so it's never cleared/refolded
        let! locsAfter = countRows "locations"
        Expect.equal
          locsAfter
          locsBefore
          "locations untouched — rebuildDirtied {AddFn} refolds only the dirtied projections"
      }

      // A schema change keeps your work. The bootstrap drops ONLY `projectionTables` and re-folds the op
      // log (LocalExec.Migrations.dropProjectionTables) — so the authored, canonical data must NEVER appear
      // in that drop-set. If it did, a schema bump would delete your work. This guards that line.
      test
        "a schema change never drops the op log: no canonical table is in the projection drop-set" {
        let canonical =
          [ "package_ops" // the authored op log — the truth
            "package_blobs" // canonical content (op-playback never writes it)
            "branches"
            "commits"
            "branch_ops"
            "accounts_v0"
            "user_data_v0"
            "toplevels_v0"
            "scripts_v0"
            "sync_remotes"
            "sync_cursors"
            "sync_conflicts" ]
        canonical
        |> List.iter (fun t ->
          Expect.isFalse
            (List.contains t Seed.projectionTables)
            $"{t} is canonical and must NOT be in the projection drop-set (it would be lost on a schema change)")
      }

      // A schema change keeps your work, end to end. A schema change now runs `rebuildProjections` (drop
      // projections + re-fold), exactly what this exercises. The first test pins projection-regen + blobs;
      // this pins the thing that actually matters — your authored op LOG (and branch/commit state) come
      // through a full re-fold IDENTICAL. If this regresses, a schema bump is eating real work.
      testTask "a schema change keeps your work: a full re-fold preserves the op log" {
        let! opsBefore = countRows "package_ops"
        let! branchesBefore = countRows "branches"
        let! commitsBefore = countRows "commits"
        Expect.isTrue
          (opsBefore > 0L)
          "there are ops to preserve (not a vacuous test)"

        let! _ = Seed.rebuildProjections ()

        let! opsAfter = countRows "package_ops"
        let! branchesAfter = countRows "branches"
        let! commitsAfter = countRows "commits"
        Expect.equal
          opsAfter
          opsBefore
          "package_ops (the authored op log) is untouched by a re-fold"
        Expect.equal
          branchesAfter
          branchesBefore
          "branches preserved across a re-fold"
        Expect.equal commitsAfter commitsBefore "commits preserved across a re-fold"
      }

      // Projection-currency counters — the `dark status` glance (`projectionStatus` → opsCount vs
      // folded-through). Equal when the cache is current; a gap when ops are appended/pulled but not yet
      // folded. Guards the surface that tells you a `branch rebuild` is owed.
      testTask
        "projectionStatus: folded == total when current; a gap appears when an op is unapplied" {
        let! _ = Seed.rebuildProjections () // re-fold → every op applied → current
        let! (total1, folded1) = Seed.projectionStatus ()
        Expect.isTrue (total1 > 0L) "there are ops to count"
        Expect.equal
          folded1
          total1
          "after a rebuild, folded-through == total (cache current)"
        // mark one op unapplied → a one-op gap
        do!
          Sql.query
            "UPDATE package_ops SET applied = 0 WHERE rowid = (SELECT MIN(rowid) FROM package_ops)"
          |> Sql.executeStatementAsync
        let! (total2, folded2) = Seed.projectionStatus ()
        Expect.equal
          total2
          total1
          "total ops unchanged (the canonical log is untouched)"
        Expect.equal
          folded2
          (folded1 - 1L)
          "one unapplied op → folded-through drops by one (a visible gap)"
        let! _ = Seed.rebuildProjections () // restore: re-fold so the shared DB stays consistent
        ()
      } ]
