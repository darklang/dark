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
      | PT.PackageOp.AddType _, PT.PackageOp.SetName(loc, PT.PackageType _) ->
        Some(PackageLocation.toFQN loc, "type")
      | PT.PackageOp.AddFn _, PT.PackageOp.SetName(loc, PT.PackageFn _) ->
        Some(PackageLocation.toFQN loc, "fn")
      | PT.PackageOp.AddValue _, PT.PackageOp.SetName(loc, PT.PackageValue _) ->
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
         | PT.PackageOp.AddType _, PT.PackageOp.SetName(_, PT.PackageType _)
         | PT.PackageOp.AddFn _, PT.PackageOp.SetName(_, PT.PackageFn _)
         | PT.PackageOp.AddValue _, PT.PackageOp.SetName(_, PT.PackageValue _) ->
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


/// Re-resolve all items in Add+SetName pairs, using the location for context
let private reResolveAllItems
  (pm : PT.PackageManager)
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let result = ResizeArray<PT.PackageOp>()

    let rec processOps (remaining : List<PT.PackageOp>) =
      task {
        match remaining with
        | PT.PackageOp.AddType t :: PT.PackageOp.SetName(loc,
                                                         (PT.PackageType _ as target)) :: rest ->
          let! reResolved =
            DR.reResolveType pm branchId loc.owner loc.modules t |> Ply.toTask

          result.Add(PT.PackageOp.AddType reResolved)
          result.Add(PT.PackageOp.SetName(loc, target))
          do! processOps rest

        | PT.PackageOp.AddFn f :: PT.PackageOp.SetName(loc,
                                                       (PT.PackageFn _ as target)) :: rest ->
          let! reResolved =
            DR.reResolveFn pm branchId loc.owner loc.modules f |> Ply.toTask

          result.Add(PT.PackageOp.AddFn reResolved)
          result.Add(PT.PackageOp.SetName(loc, target))
          do! processOps rest

        | PT.PackageOp.AddValue v :: PT.PackageOp.SetName(loc,
                                                          (PT.PackageValue _ as target)) :: rest ->
          let! reResolved =
            DR.reResolveValue pm branchId loc.owner loc.modules v |> Ply.toTask

          result.Add(PT.PackageOp.AddValue reResolved)
          result.Add(PT.PackageOp.SetName(loc, target))
          do! processOps rest

        | op :: rest ->
          result.Add(op)
          do! processOps rest

        | [] -> ()
      }

    do! processOps ops
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
let refresh (pm : PT.PackageManager) (branchId : System.Guid) : Task<int64> =
  task {
    // 1. Get WIP ops
    let! wipOps = Queries.getWipOps branchId

    if List.isEmpty wipOps then
      return 0L
    else
      // 2. Compact superseded edits — required for correctness, see
      // compactWipOps' header comment. Doing this before re-resolution
      // also avoids redundant `findInPM` calls on duplicate items
      // that compaction would have dropped anyway.
      let compactedOps = compactWipOps wipOps

      // 3. Re-resolve unresolved names
      let! reResolvedOps = reResolveAllItems pm branchId compactedOps

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

        // 6. Discard old WIP and re-insert updated ops
        let! discardResult = Inserts.discardWipOps branchId

        match discardResult with
        | Error msg ->
          System.Console.Error.WriteLine($"WipRefresh: discard failed: {msg}")
          return 0L
        | Ok _ ->
          let! _ = Inserts.insertAndApplyOpsAsWip branchId stabilizedOps
          return changedCount
  }
