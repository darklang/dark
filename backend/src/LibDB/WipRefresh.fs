/// Resolve references in main's draft when their declarations become available,
/// then update the affected hashes and saved reference names.
/// Keep older versions needed by renames, reference snapshots, or undo.
///
/// Only main's uncommitted operations are read here. Other branches' drafts and
/// committed operations stay unchanged. `Draft.rebuild` replays committed history
/// without recalculating its hashes.
module LibDB.WipRefresh

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module HS = LibDB.HashStabilization
module DR = LibDB.DeferredResolver
module PackageLocation = LibDB.PackageLocation


/// Resolve missing references using each declaration's owner and module.
/// Use a loop so a large draft doesn't build up nested tasks.
let private reResolveAllItems
  (pm : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let! referenceNames = ReferenceNames.load PT.BranchId.Main
    let referenceNames =
      ops
      // For a -> b -> c, copy reference names back from c to b, then b to a.
      // That gives the original declaration the names used after both renames.
      |> List.rev
      |> List.fold
        (fun state op ->
          match op with
          | PT.PackageOp.Rename(oldLocation, newLocation, target, _, _) ->
            match Map.tryFind (newLocation, target) state with
            | Some names -> Map.add (oldLocation, target) names state
            | None -> state
          | _ -> state)
        referenceNames
    let result = ResizeArray<PT.PackageOp>()
    let arr = List.toArray ops
    let mutable i = 0

    while i < arr.Length do
      let pair = if i + 1 < arr.Length then Some(arr[i], arr[i + 1]) else None

      match pair with
      | Some(PT.PackageOp.AddType t,
             PT.PackageOp.SetName(loc, (PT.PackageType _ as target), prev)) ->
        let t = ReferenceNames.patchType referenceNames loc t
        let! reResolved = DR.reResolveType pm loc.owner loc.modules t |> Ply.toTask
        result.Add(PT.PackageOp.AddType reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | Some(PT.PackageOp.AddFn f,
             PT.PackageOp.SetName(loc, (PT.PackageFn _ as target), prev)) ->
        let f = ReferenceNames.patchFn referenceNames loc f
        let! reResolved = DR.reResolveFn pm loc.owner loc.modules f |> Ply.toTask
        result.Add(PT.PackageOp.AddFn reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | Some(PT.PackageOp.AddValue v,
             PT.PackageOp.SetName(loc, (PT.PackageValue _ as target), prev)) ->
        let v = ReferenceNames.patchValue referenceNames loc v
        let! reResolved = DR.reResolveValue pm loc.owner loc.modules v |> Ply.toTask
        result.Add(PT.PackageOp.AddValue reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | Some(PT.PackageOp.AddTrait t,
             PT.PackageOp.SetName(loc, (PT.PackageTrait _ as target), prev)) ->
        let t = ReferenceNames.patchTrait referenceNames loc t
        let! reResolved = DR.reResolveTrait pm loc.owner loc.modules t |> Ply.toTask
        result.Add(PT.PackageOp.AddTrait reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | Some(PT.PackageOp.AddTraitImpl impl,
             PT.PackageOp.SetName(loc, (PT.PackageTraitImpl _ as target), prev)) ->
        let impl = ReferenceNames.patchImpl referenceNames loc impl
        let! reResolved =
          DR.reResolveImpl pm loc.owner loc.modules loc.name impl |> Ply.toTask
        result.Add(PT.PackageOp.AddTraitImpl reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | Some(PT.PackageOp.AddTest test,
             PT.PackageOp.SetName(loc, (PT.PackageTest _ as target), prev)) ->
        let test = ReferenceNames.patchTest referenceNames loc test
        let! reResolved =
          DR.reResolveTest pm loc.owner loc.modules test |> Ply.toTask
        result.Add(PT.PackageOp.AddTest reResolved)
        result.Add(PT.PackageOp.SetName(loc, target, prev))
        i <- i + 2

      | _ ->
        result.Add arr[i]
        i <- i + 1

    return result |> Seq.toList
  }


/// Find each declaration's name after the renames that follow it.
let private currentDeclarationLocations ops =
  let indexed = List.indexed ops
  let renames =
    indexed
    |> List.choose (fun (i, op) ->
      match op with
      | PT.PackageOp.Rename(oldLocation, newLocation, target, _, _) ->
        Some(i, oldLocation, newLocation, target)
      | _ -> None)
  indexed
  |> List.pairwise
  |> List.choose (fun ((_, declaration), (i, binding)) ->
    PT.tryDeclarationBinding (declaration, binding)
    |> Option.map (fun (location, target) ->
      let current =
        renames
        |> List.fold
          (fun current (j, oldLocation, newLocation, renamed) ->
            if j > i && current = oldLocation && target = renamed then
              newLocation
            else
              current)
          location
      i, current))
  |> Map.ofList

/// Select the declaration each name currently binds, including restored versions.
/// Return a function that puts the refreshed declarations back into the original
/// operation list, keeping older versions unchanged.
let private refreshableOps ops =
  let indexed = List.indexed ops
  let locations = currentDeclarationLocations ops
  let live =
    ops
    |> List.collect PT.packageOpBindingEffects
    |> List.fold
      (fun bindings op ->
        match op with
        | PT.PackageOp.SetName(location, target, _)
        | PT.PackageOp.Decision(_, location, _, PT.DecisionKind.Override target) ->
          Map.add location target bindings
        | PT.PackageOp.Unbind(location, _) -> Map.remove location bindings
        | _ -> bindings)
      Map.empty
  let latest =
    indexed
    |> List.choose (fun (i, op) ->
      match op, Map.tryFind i locations with
      | PT.PackageOp.SetName(_, target, _), Some location when
        Map.tryFind location live = Some target
        ->
        Some(location, i)
      | _ -> None)
    |> Map.ofList
  let frozen =
    locations
    |> Map.toList
    |> List.collect (fun (i, location) ->
      if Map.tryFind location latest = Some i then [] else [ i - 1; i ])
    |> Set.ofList
  indexed
  |> List.filter (fun (i, _) -> not (Set.contains i frozen))
  |> List.map snd,
  fun (refreshed : List<PT.PackageOp>) ->
    let active = indexed |> List.filter (fun (i, _) -> not (Set.contains i frozen))
    let replacements =
      List.zip active refreshed
      |> List.map (fun ((i, _), replacement) -> i, replacement)
      |> Map.ofList
    indexed
    |> List.map (fun (i, op) -> Map.tryFind i replacements |> Option.defaultValue op)

/// Calculate hashes using the declarations' current names so recursive calls
/// still connect to the declarations they refer to after a rename.
/// Then put the original SetName locations back so the saved renames replay in order.
let private stabilizeAtCurrentNames ops =
  let indexed = List.indexed ops
  let locations = currentDeclarationLocations ops
  let forHashing =
    indexed
    |> List.map (fun (i, op) ->
      match op, Map.tryFind i locations with
      | PT.PackageOp.SetName(_, target, previous), Some location ->
        PT.PackageOp.SetName(location, target, previous)
      | _ -> op)
  HS.computeRealHashes forHashing
  |> List.map2
    (fun original refreshed ->
      match original, refreshed with
      | PT.PackageOp.SetName(location, _, _),
        PT.PackageOp.SetName(_, target, previous) ->
        PT.PackageOp.SetName(location, target, previous)
      | _ -> refreshed)
    ops

/// Update bindings and saved reference names to match the refreshed declarations.
/// Update undo's revision IDs too, since changing an operation changes its ID.
/// The caller keeps the original timestamps when saving these operations.
let private refreshSnapshots before after =
  let replacements =
    List.zip (List.pairwise before) (List.pairwise after)
    |> List.choose
      (fun ((oldDeclaration, oldBinding), (newDeclaration, newBinding)) ->
        match
          PT.tryDeclarationBinding (oldDeclaration, oldBinding),
          PT.tryDeclarationBinding (newDeclaration, newBinding)
        with
        | Some(location, oldTarget), Some(_, newTarget) when oldTarget <> newTarget ->
          Some(
            (location, oldTarget),
            (newTarget,
             ReferenceNamesContent.refreshNames oldDeclaration newDeclaration)
          )
        | _ -> None)
    |> Map.ofList
  // A declaration can move more than once before its forward references resolve.
  // Carry its refreshed version through each rename, so a -> b -> c updates c too.
  let replacements =
    after
    |> List.fold
      (fun replacements op ->
        match op with
        | PT.PackageOp.Rename(oldLocation, newLocation, target, _, _) ->
          match Map.tryFind (oldLocation, target) replacements with
          | Some replacement ->
            Map.add (newLocation, target) replacement replacements
          | None -> replacements
        | _ -> replacements)
      replacements
  let snapshot (snapshot : PT.ReferenceNamesSnapshot) =
    match Map.tryFind (snapshot.location, snapshot.target) replacements with
    | Some(target, refreshNames) ->
      { snapshot with target = target; names = refreshNames snapshot.names }
    | None -> snapshot
  let updated =
    after
    |> List.map (function
      | PT.PackageOp.SetName(location, target, previous) ->
        let target =
          Map.tryFind (location, target) replacements
          |> Option.map fst
          |> Option.defaultValue target
        PT.PackageOp.SetName(location, target, previous)
      | PT.PackageOp.Decision(id, location, reason, PT.DecisionKind.Override target) ->
        let target =
          Map.tryFind (location, target) replacements
          |> Option.map fst
          |> Option.defaultValue target
        PT.PackageOp.Decision(id, location, reason, PT.DecisionKind.Override target)
      | PT.PackageOp.Rename(oldLocation, newLocation, target, snapshots, stamp) ->
        let newTarget =
          Map.tryFind (oldLocation, target) replacements
          |> Option.map fst
          |> Option.defaultValue target
        let snapshots =
          snapshots
          |> List.map (fun original ->
            let updated = snapshot original
            if original.location = newLocation && original.target = target then
              match Map.tryFind (oldLocation, target) replacements with
              | Some(target, refreshNames) ->
                { updated with
                    target = target
                    names = refreshNames original.names }
              | None -> updated
            else
              updated)
        PT.PackageOp.Rename(oldLocation, newLocation, newTarget, snapshots, stamp)
      | PT.PackageOp.SetReferenceNames(location, target, names, restating) ->
        let updated =
          snapshot
            { location = location
              target = target
              names = names
              restating = restating }
        PT.PackageOp.SetReferenceNames(
          updated.location,
          updated.target,
          updated.names,
          updated.restating
        )
      | PT.PackageOp.Decision(id,
                              location,
                              reason,
                              PT.DecisionKind.Restore(revision, target, names)) ->
        let saved =
          snapshot
            { location = location; target = target; names = names; restating = None }
        PT.PackageOp.Decision(
          id,
          location,
          reason,
          PT.DecisionKind.Restore(revision, saved.target, saved.names)
        )
      | op -> op)
  let revisions =
    List.zip before updated
    |> List.map (fun (oldOp, newOp) ->
      Inserts.computeOpHash oldOp, Inserts.computeOpHash newOp)
    |> Map.ofList
  updated
  |> List.map (function
    | PT.PackageOp.Decision(id,
                            location,
                            reason,
                            PT.DecisionKind.Restore(revision, target, names)) ->
      let revision = Map.tryFind revision revisions |> Option.defaultValue revision
      PT.PackageOp.Decision(
        id,
        location,
        reason,
        PT.DecisionKind.Restore(revision, target, names)
      )
    | op -> op)


/// Read main's draft, resolve missing references, and update hashes and saved names.
/// Save the refreshed draft only if nobody has changed it since we read it.
/// Return the number of new hashes, or None if another edit means we need to retry.
let private refreshOnce
  (afterRead : unit -> Task<unit>)
  (pm : PT.PackageManager)
  : Task<Option<int64>> =
  task {
    // Read only the draft; getWipOps would include committed operations too.
    let! (draftIds, wipOps) = Queries.getDraftOpsWithIds ()
    do! afterRead ()

    if List.isEmpty wipOps then
      return Some 0L
    else
      // Refresh live declarations, retaining inactive revisions for undo.
      let activeOps, restore = refreshableOps wipOps
      let! reResolvedOps = reResolveAllItems pm activeOps

      // These declarations were hashed when saved. If resolving references changed
      // nothing, we can skip recalculating their hashes and rewriting the draft.
      if reResolvedOps = activeOps then
        return Some 0L
      else

        // Recalculate hashes together so mutually recursive declarations stay connected.
        let stabilizedOps =
          stabilizeAtCurrentNames reResolvedOps |> restore |> refreshSnapshots wipOps

        // Compare the hashes referenced by SetName operations before and after refresh.
        let oldHashes = HS.extractAllHashes wipOps |> Set.ofList
        let newHashes = HS.extractAllHashes stabilizedOps |> Set.ofList

        // Check the operation count too, so removing older declarations counts as a change.
        // Use the binding hashes rather than temporary hashes on Add operations.
        if
          oldHashes = newHashes && List.length stabilizedOps = List.length wipOps
        then
          return Some 0L
        else
          // Count the new hashes introduced by refresh.
          let changedCount = Set.difference newHashes oldHashes |> Set.count |> int64

          // Replace the draft in one transaction so a failure cannot leave it half-written.
          // Preserve timestamps even when an operation's ID changes, so earlier edits
          // keep their place in history. The operations remain uncommitted.
          // A write failure reaches the caller instead of reporting that nothing changed.
          let! preserveTs = Queries.getWipOpOriginTs ()
          let preserveTs =
            List.zip wipOps stabilizedOps
            |> List.fold
              (fun stamps (oldOp, newOp) ->
                match Map.tryFind (Inserts.computeOpHash oldOp) preserveTs with
                | Some ts -> Map.add (Inserts.computeOpHash newOp) ts stamps
                | None -> stamps)
              preserveTs
          // Resolving forward references can make two bodies identical.
          // Save their reference names before those bodies are combined.
          let! referenceOps =
            ReferenceNames.captureForStorage PT.BranchId.Main stabilizedOps
          let stabilizedOps = stabilizedOps @ referenceOps
          match!
            Inserts.rewriteDraftIfUnchanged
              draftIds
              (fun opId ->
                match Map.tryFind opId preserveTs with
                | Some ts -> ts
                | None -> Inserts.nextOriginTs ())
              stabilizedOps
          with
          | Some _ -> return Some changedCount
          | None -> return None
  }

/// Run afterRead between reading and rewriting the draft, so tests can save
/// another edit at that point and check that refresh retries safely.
let refreshWith
  (afterRead : unit -> Task<unit>)
  (pm : PT.PackageManager)
  : Task<int64> =
  Telemetry.timeTask "author.wipRefresh" [] (fun () ->
    task {
      // Retry if another edit changed the draft while we were refreshing it.
      // Stop after ten attempts rather than retrying indefinitely.
      let mutable result = None
      let mutable attempts = 0
      while Option.isNone result && attempts < 10 do
        attempts <- attempts + 1
        let! r = refreshOnce afterRead pm
        result <- r
      match result with
      | Some n -> return n
      | None ->
        // The edit is already saved; refreshing the surrounding draft is what failed.
        // This message follows "Could not save <item>:", so explain that distinction.
        return
          Exception.raiseInternal
            ("it is stored, but the draft kept changing while names across it were "
             + $"being re-resolved ({attempts} tries), usually another save or your own "
             + "editor, so references waiting on it may not be linked yet. Run the same "
             + "save again to finish.")
            []
    })

let refresh (pm : PT.PackageManager) : Task<int64> =
  refreshWith (fun () -> Task.FromResult()) pm
