/// Refreshes WIP items on a branch by re-resolving unresolved NameResolution
/// nodes and recomputing content-addressed hashes.
///
/// When items are added incrementally, earlier items may have unresolved
/// references to items added later. This module walks all WIP items,
/// re-resolves what's now resolvable, and recomputes SCC-aware hashes.
module LibDB.WipRefresh

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module HS = LibDB.HashStabilization
module DR = LibDB.DeferredResolver
module PackageLocation = LibDB.PackageLocation


/// Drop superseded Add+SetName pairs so each (FQN, kind) appears at most
/// once. Necessary, not just cosmetic: WIP ops are ordered by `created_at`
/// (1s resolution), so successive refreshes can read duplicates in
/// non-deterministic order. Replaying out-of-order overwrites the latest
/// body with an older one — manifesting as e.g. `z` reading back as `1L`
/// after edits 1L → 2L → 3L. Compacting here keeps WIP one pair per FQN
/// and sidesteps the ordering issue entirely.
let private compactWipOps (ops : List<PT.PackageOp>) : List<PT.PackageOp> =
  let opsArr = List.toArray ops
  let n = opsArr.Length

  // Walk in reverse, recording the LAST Add+SetName pair index per (FQN, kind).
  let lastPairIndices = System.Collections.Generic.HashSet<int>()
  let seenFqnKinds = System.Collections.Generic.HashSet<string * string>()

  let mutable i = n - 1
  while i > 0 do
    let key =
      match opsArr[i - 1], opsArr[i] with
      | PT.PackageOp.AddType _, PT.PackageOp.SetName(loc, PT.PackageType _, _) ->
        Some(PackageLocation.toFQN loc, "type")
      | PT.PackageOp.AddFn _, PT.PackageOp.SetName(loc, PT.PackageFn _, _) ->
        Some(PackageLocation.toFQN loc, "fn")
      | PT.PackageOp.AddValue _, PT.PackageOp.SetName(loc, PT.PackageValue _, _) ->
        Some(PackageLocation.toFQN loc, "value")
      | _ -> None

    match key with
    | Some k ->
      if not (seenFqnKinds.Contains k) then
        seenFqnKinds.Add k |> ignore<bool>
        lastPairIndices.Add(i - 1) |> ignore<bool>
        lastPairIndices.Add i |> ignore<bool>
      i <- i - 2
    | None -> i <- i - 1

  // Forward pass: keep last-pair indices and all non-pair ops.
  let result = ResizeArray<PT.PackageOp>()
  let mutable j = 0
  while j < n do
    let isPair =
      j + 1 < n
      && match opsArr[j], opsArr[j + 1] with
         | PT.PackageOp.AddType _, PT.PackageOp.SetName(_, PT.PackageType _, _)
         | PT.PackageOp.AddFn _, PT.PackageOp.SetName(_, PT.PackageFn _, _)
         | PT.PackageOp.AddValue _, PT.PackageOp.SetName(_, PT.PackageValue _, _) ->
           true
         | _ -> false

    if isPair then
      if lastPairIndices.Contains j then
        result.Add opsArr[j]
        result.Add opsArr[j + 1]
      j <- j + 2
    else
      result.Add opsArr[j]
      j <- j + 1

  result |> Seq.toList


/// Re-resolve all items in Add+SetName pairs, using the location for context.
///
/// ITERATIVE, not recursive. This walked the op list with `do! processOps rest`, one
/// nested task per op. F#'s task builder doesn't turn that into a tail call, so
/// stack depth grew with the SIZE OF THE OP LOG -- and since this runs on every
/// author (WipRefresh reads all WIP ops, which on main is the whole log), the
/// authoring path stack-overflowed once the store got big enough. It died silently
/// mid-`dark fn`: the item is already inserted, so the edit looks like it worked, but
/// the process dies before propagation runs and dependents quietly stop repointing.
/// Reached at ~14.7k ops.
///
/// Walking an array with a `while` keeps depth O(1) whatever the log size. Behavior
/// is unchanged: the same two-op-lookahead over Add+SetName pairs, in the same
/// order.
let private reResolveAllItems
  (pm : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let result = ResizeArray<PT.PackageOp>()
    let arr = List.toArray ops
    let mutable i = 0

    while i < arr.Length do
      let pair = if i + 1 < arr.Length then Some(arr[i], arr[i + 1]) else None

      match pair with
      | Some(PT.PackageOp.AddType t,
             PT.PackageOp.SetName(loc, (PT.PackageType _ as target), prev)) ->
        let! reResolved = DR.reResolveType pm loc.owner loc.modules t |> Ply.toTask
        result.Add(PT.PackageOp.AddType reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | Some(PT.PackageOp.AddFn f,
             PT.PackageOp.SetName(loc, (PT.PackageFn _ as target), prev)) ->
        let! reResolved = DR.reResolveFn pm loc.owner loc.modules f |> Ply.toTask
        result.Add(PT.PackageOp.AddFn reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | Some(PT.PackageOp.AddValue v,
             PT.PackageOp.SetName(loc, (PT.PackageValue _ as target), prev)) ->
        let! reResolved = DR.reResolveValue pm loc.owner loc.modules v |> Ply.toTask
        result.Add(PT.PackageOp.AddValue reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | _ ->
        result.Add arr[i]
        i <- i + 1

    return result |> Seq.toList
  }


/// Refresh all WIP items on a branch:
/// 1. Get all WIP ops
/// 2. Compact superseded edits to one pair per FQN (required for correctness;
///    runs before re-resolution so we don't re-resolve N copies of the same
///    item's NRs only to throw the older copies away).
/// 3. Re-resolve unresolved NameResolutions using current PM
/// 4. Run HashStabilization.computeRealHashes
/// 5. If any hashes changed, discard old WIP and re-insert
/// 6. Return count of changed items
let refresh (pm : PT.PackageManager) : Task<int64> =
  task {
    // 1. Get WIP ops
    let! wipOps = Queries.getWipOps ()

    if List.isEmpty wipOps then
      return 0L
    else
      // 2. Compact superseded edits — required for correctness, see
      // compactWipOps' header comment. Doing this before re-resolution
      // also avoids redundant `findInPM` calls on duplicate items
      // that compaction would have dropped anyway.
      let compactedOps = compactWipOps wipOps

      // 3. Re-resolve unresolved names
      let! reResolvedOps = reResolveAllItems pm compactedOps

      // If re-resolution changed nothing, computeRealHashes below is provably a
      // no-op: the ops were already stabilized before insert (scmAddOps), so
      // re-hashing the same fully-resolved ops returns the same hashes and the
      // `oldHashes = newHashes` guard would return 0L anyway -- after paying for an
      // SCC rehash over the whole ~10k-op store. Skip straight past it. This is the
      // authoring-perf win (~3.3s -> ~2.45s/author here): a new item or an update
      // with NO dependents does no re-resolution, so it no longer rehashes the
      // entire log. When a dependent exists, ITS reference re-resolves to the
      // updated hash (reResolvedOps != compactedOps) -- that IS the repointing, so
      // the full path below runs and does it. Same for forward-ref authoring. See
      // notes/fresh-arch/30e-authoring-perf-wiprefresh.md.
      if reResolvedOps = compactedOps then
        return 0L
      else

        // 4. Stabilize hashes (SCC-aware)
        let stabilizedOps = HS.computeRealHashes reResolvedOps

        // 5. Compare old and new hashes
        let oldHashes = HS.extractAllHashes wipOps |> Set.ofList
        let newHashes = HS.extractAllHashes stabilizedOps |> Set.ofList

        if oldHashes = newHashes then
          return 0L
        else
          // Count changed items (items that got a new hash)
          let changedCount = Set.difference newHashes oldHashes |> Set.count |> int64

          // 6. Discard old WIP and re-insert updated ops. Capture existing origin_ts
          //    FIRST so unchanged ops keep their authoring stamp on reinsert (ops
          //    whose hash didn't change keep the same content id); only the
          //    genuinely-changed ops mint a fresh stamp. Without this the
          //    whole-store reinsert would push origin_ts ~10s into the future and
          //    make the next update look stale to the fold's LWW.
          let! preserveTs = Queries.getWipOpOriginTs ()
          // Same reason, for the committing commit: the whole main log is deleted
          // and re-inserted here, so without carrying commit_hash forward a single
          // authoring refresh would un-commit all of history.
          let! preserveCommit = Queries.getWipOpCommits ()
          let! discardResult = Inserts.discardWipOps ()

          match discardResult with
          | Error msg ->
            System.Console.Error.WriteLine($"WipRefresh: discard failed: {msg}")
            return 0L
          | Ok _ ->
            let! _ =
              Inserts.insertAndApplyOpsPreservingTs
                preserveTs
                preserveCommit
                stabilizedOps
            return changedCount
  }
