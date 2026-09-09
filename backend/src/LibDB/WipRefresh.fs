/// Refreshes MAIN's DRAFT by re-resolving unresolved NameResolution nodes and recomputing
/// content-addressed hashes. Branch ops are branch-pending rather than main WIP, and committed ops
/// are history; `Queries.getDraftOps` excludes both and this never sees them.
///
/// The draft and only the draft, because `compactWipOps` and `HashStabilization.computeRealHashes`
/// both keep ONE version per name. That is right for a draft, whose newest edit is the one that
/// counts, and it destroys history the moment committed ops are fed through it: an ordinary
/// authoring session (a forward reference, then the thing it referred to) takes every earlier
/// committed version of every name out of the canonical log. `Draft.rebuild` is the path that
/// handles the whole log, and it does not stabilize.
module LibDB.WipRefresh

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module HS = LibDB.HashStabilization
module DR = LibDB.DeferredResolver
module PackageLocation = LibDB.PackageLocation


/// Drop superseded Add+SetName pairs so each (FQN, kind) appears at most once. Necessary,
/// not cosmetic: WIP ops are ordered by `created_at` (1s resolution), so successive
/// refreshes can read duplicates in non-deterministic order, and replaying out of order
/// overwrites the latest body with an older one. One pair per FQN sidesteps that entirely.
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
/// ITERATIVE, not recursive. A recursive walk (`do! processOps rest`) is one nested task
/// per op, and F#'s task builder doesn't make that a tail call, so stack depth would grow
/// with the size of the op log -- which on main is the whole log, on every author. Walking
/// an array with a `while` keeps depth O(1) whatever the log size.
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


/// Refresh all of MAIN's WIP items:
/// 1. Get all WIP ops
/// 2. Compact superseded edits to one pair per FQN (required for correctness;
///    runs before re-resolution so we don't re-resolve N copies of the same
///    item's NRs only to throw the older copies away).
/// 3. Re-resolve unresolved NameResolutions using current PM
/// 4. Run HashStabilization.computeRealHashes
/// 5. If the ops changed at all, discard old WIP and re-insert
/// 6. Return count of changed items
let refresh (pm : PT.PackageManager) : Task<int64> =
  task {
    // 1. Get the draft. Not `getWipOps`, which returns committed ops too; see the module doc.
    let! wipOps = Queries.getDraftOps ()

    if List.isEmpty wipOps then
      return 0L
    else
      // 2. Compact superseded edits. Required for correctness (see compactWipOps), and
      // doing it before re-resolution avoids re-resolving copies about to be dropped.
      let compactedOps = compactWipOps wipOps

      // 3. Re-resolve unresolved names
      let! reResolvedOps = reResolveAllItems pm compactedOps

      // If re-resolution changed nothing, stabilization is a no-op: the ops were
      // stabilized before insert (scmAddOps), so re-hashing gives the same hashes --
      // after an SCC rehash of the whole log. Skip it; any dependent that exists
      // re-resolves to a new hash and takes the full path.
      if reResolvedOps = compactedOps then
        return 0L
      else

        // 4. Stabilize hashes (SCC-aware)
        let stabilizedOps = HS.computeRealHashes reResolvedOps

        // 5. Compare old and new hashes
        let oldHashes = HS.extractAllHashes wipOps |> Set.ofList
        let newHashes = HS.extractAllHashes stabilizedOps |> Set.ofList

        // Hashes PLUS op count: compaction is detectable without comparing transient Add-item
        // hashes, which stabilization may fill differently on each load.
        if
          oldHashes = newHashes && List.length stabilizedOps = List.length wipOps
        then
          return 0L
        else
          // Count changed items (items that got a new hash)
          let changedCount = Set.difference newHashes oldHashes |> Set.count |> int64

          // 6. Discard the old draft and re-insert the updated one, as ONE transaction: split in
          //    two, a crash between them deletes the draft with nothing to put back. Capture the
          //    existing origin_ts FIRST, so an op whose hash didn't change comes back with its own
          //    stamp rather than a fresh one. No commit: nothing here is committed, by construction.
          //    A failure raises into the author's `try/with` rather than reporting 0 changed.
          let! preserveTs = Queries.getWipOpOriginTs ()
          let! _ =
            Inserts.rewriteOpsAtomically
              Inserts.draftDeletes
              (fun opId ->
                match Map.tryFind opId preserveTs with
                | Some ts -> ts
                | None -> Inserts.nextOriginTs ())
              (fun _ -> None)
              "op"
              stabilizedOps
          return changedCount
  }
