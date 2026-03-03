/// Refreshes WIP items on a branch by re-resolving unresolved NameResolution
/// nodes and recomputing content-addressed hashes.
///
/// When items are added incrementally, earlier items may have unresolved
/// references to items added later. This module walks all WIP items,
/// re-resolves what's now resolvable, and recomputes SCC-aware hashes.
module LibPackageManager.WipRefresh

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module HS = LibPackageManager.HashStabilization
module DR = LibPackageManager.DeferredResolver


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
        | PT.PackageOp.AddType t :: PT.PackageOp.SetTypeName(hash, loc) :: rest ->
          let! reResolved =
            DR.reResolveType pm branchId loc.owner loc.modules t |> Ply.toTask

          result.Add(PT.PackageOp.AddType reResolved)
          result.Add(PT.PackageOp.SetTypeName(hash, loc))
          do! processOps rest

        | PT.PackageOp.AddFn f :: PT.PackageOp.SetFnName(hash, loc) :: rest ->
          let! reResolved =
            DR.reResolveFn pm branchId loc.owner loc.modules f |> Ply.toTask

          result.Add(PT.PackageOp.AddFn reResolved)
          result.Add(PT.PackageOp.SetFnName(hash, loc))
          do! processOps rest

        | PT.PackageOp.AddValue v :: PT.PackageOp.SetValueName(hash, loc) :: rest ->
          let! reResolved =
            DR.reResolveValue pm branchId loc.owner loc.modules v |> Ply.toTask

          result.Add(PT.PackageOp.AddValue reResolved)
          result.Add(PT.PackageOp.SetValueName(hash, loc))
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
/// 2. Re-resolve unresolved NameResolutions using current PM
/// 3. Run HashStabilization.computeRealHashes
/// 4. If any hashes changed, discard old WIP and re-insert
/// 5. Return count of changed items
let refresh (pm : PT.PackageManager) (branchId : System.Guid) : Task<int64> =
  task {
    // 1. Get WIP ops
    let! wipOps = Queries.getWipOps branchId

    if List.isEmpty wipOps then
      return 0L
    else
      // 2. Re-resolve unresolved names
      let! reResolvedOps = reResolveAllItems pm branchId wipOps

      // 3. Stabilize hashes (SCC-aware)
      let stabilizedOps = HS.computeRealHashes reResolvedOps

      // 4. Compare old and new hashes
      let oldHashes = HS.extractAllHashes wipOps |> Set.ofList
      let newHashes = HS.extractAllHashes stabilizedOps |> Set.ofList

      if oldHashes = newHashes then
        return 0L
      else
        // Count changed items (items that got a new hash)
        let changedCount = Set.difference newHashes oldHashes |> Set.count |> int64

        // 5. Discard old WIP and re-insert updated ops
        let! discardResult = Inserts.discardWipOps branchId

        match discardResult with
        | Error msg ->
          System.Console.Error.WriteLine($"WipRefresh: discard failed: {msg}")
          return 0L
        | Ok _ ->
          let! _ = Inserts.insertAndApplyOpsAsWip branchId stabilizedOps
          return changedCount
  }
