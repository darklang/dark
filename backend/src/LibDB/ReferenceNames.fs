/// Remember which names each caller uses, for each location and content version.
/// Two callers can share the same code while following different names.
/// Each branch applies its own changes on top of committed main.
module LibDB.ReferenceNames

open System.Threading.Tasks
open FSharp.Control.Tasks
open Fumble
open Prelude
open LibExecution.ProgramTypes
open LibDB.Sqlite

module PT = LibExecution.ProgramTypes
module AT = AstTransformer
module BS = LibSerialization.Binary.Serialization

type Names = List<PT.Reference * Option<PT.PackageLocation>>
type State = Map<PT.PackageLocation * PT.Reference, Names>

/// Apply reference-name changes here, using the original operations.
/// If two renames affect different calls in the same caller, keep both changes.
let private apply (state : State) (op : PT.PackageOp) : State =
  match op with
  | PT.PackageOp.Rename(oldLocation, newLocation, renamed, references, _) ->
    references
    |> List.fold
      (fun state snapshot ->
        // Change only references to the old name. Another rename may already
        // have updated other references in this caller; keep those changes.
        let moving = snapshot.location = newLocation && snapshot.target = renamed
        let previous =
          // Bring the declaration's reference names with it. The destination
          // may still have reference names from an item that used to live there.
          if moving then
            Map.tryFind (oldLocation, snapshot.target) state
          else
            Map.tryFind (snapshot.location, snapshot.target) state
        let names =
          match previous with
          | None -> snapshot.names
          | Some names ->
            if List.map fst names <> List.map fst snapshot.names then
              Exception.raiseInternal "Rename snapshot reference mismatch" []
            List.map2
              (fun (reference, current) (_, authored) ->
                reference,
                if
                  reference.kind = renamed.kind
                  && current = Some oldLocation
                  && authored = Some newLocation
                then
                  Some newLocation
                else
                  current)
              names
              snapshot.names
        Map.add (snapshot.location, snapshot.target) names state)
      state
  | PT.PackageOp.Decision(_, location, _, PT.DecisionKind.Restore(_, target, names))
  | PT.PackageOp.SetReferenceNames(location, target, names, _) ->
    Map.add (location, target) names state
  | _ -> state

/// Apply an operation to saved reference names using the same fold as branch reads.
/// Dark's undo history uses this to keep independent renames together.
let applySnapshots (snapshots : List<PT.ReferenceNamesSnapshot>) op =
  let state =
    snapshots
    |> List.map (fun snapshot ->
      (snapshot.location, snapshot.target), snapshot.names)
    |> Map.ofList
  apply state op
  |> Map.toList
  |> List.map (fun ((location, target), names) ->
    { PT.ReferenceNamesSnapshot.location = location
      target = target
      names = names
      restating = None })

// Reuse the result while the ordered operation IDs stay the same.
// Reading the IDs also catches SQL writes and commits made by another process.
let private mainStates =
  System.Collections.Concurrent.ConcurrentDictionary<bool, System.Guid list * State>()
do Caching.register (fun () -> mainStates.Clear())

let private loadAt (branch : PT.BranchId) (before : Option<string>) : Task<State> =
  task {
    let visibility =
      OpHistory.visibility (if branch.IsMain then "1 = 1" else "0 = 1") before
    let parameters =
      before |> Option.toList |> List.map (fun stamp -> "before", Sql.string stamp)
    let predicate =
      $"p.op_kind IN ('rename', 'reference-names', 'decision') AND p.effective = 1 AND p.id NOT IN (SELECT op_id FROM op_branches) AND {visibility}"
    let! ids =
      Sql.query
        $"SELECT p.id FROM package_ops p WHERE {predicate} ORDER BY p.origin_ts, p.id"
      |> Sql.parameters parameters
      |> Sql.executeAsync (fun r -> r.uuid "id")
    let! mainState =
      match mainStates.TryGetValue branch.IsMain with
      | true, (previous, state) when Option.isNone before && previous = ids ->
        Task.FromResult state
      | _ ->
        task {
          let! rows =
            Sql.query
              $"SELECT p.id, p.op_blob FROM package_ops p WHERE {predicate} ORDER BY p.origin_ts, p.id"
            |> Sql.parameters parameters
            |> Sql.executeAsync (fun r ->
              r.uuid "id",
              BS.PT.PackageOp.tryDeserialize (r.uuid "id") (r.bytes "op_blob"))
          let state = rows |> List.choose snd |> List.fold apply Map.empty
          if Option.isNone before then
            mainStates[branch.IsMain] <- (List.map fst rows, state)
          return state
        }
    let! overlay =
      if branch.IsMain then
        Task.FromResult []
      else
        Branches.chainOverlayOperations branch before
    return List.fold apply mainState overlay
  }

let load branch = loadAt branch None

let private patch visit (state : State) location target item =
  match Map.tryFind (location, target) state with
  | None -> item
  | Some names ->
    let mutable remaining = names
    let rename reference _ =
      match reference, remaining with
      | None, _ -> None
      | Some reference, (expected, location) :: rest when expected = reference ->
        remaining <- rest
        location
      | _ ->
        Exception.raiseInternal
          "Reference names do not match declaration content"
          [ "location", location; "target", target ]
    let result = visit { AT.emptyMapping with referenceName = Some rename } item
    if not (List.isEmpty remaining) then
      Exception.raiseInternal
        "Reference names outlast declaration content"
        [ "location", location; "target", target ]
    result

let patchType state loc (t : PT.PackageType.PackageType) =
  patch AT.transformType state loc (PT.PackageType t.hash) t
let patchFn state loc (f : PT.PackageFn.PackageFn) =
  patch AT.transformFn state loc (PT.PackageFn f.hash) f
let patchValue state loc (v : PT.PackageValue.PackageValue) =
  patch AT.transformValue state loc (PT.PackageValue v.hash) v
let patchTrait state loc (t : PT.Trait.Trait) =
  patch AT.transformTrait state loc (PT.PackageTrait t.hash) t
let patchImpl state loc (i : PT.TraitImpl.TraitImpl) =
  patch AT.transformImpl state loc (PT.PackageTraitImpl i.hash) i
let patchTest state loc (t : PT.PackageTest.PackageTest) =
  patch AT.transformTest state loc (PT.PackageTest t.hash) t

let patchResults
  (state : State)
  (r : PT.Search.SearchResults)
  : PT.Search.SearchResults =
  let patch f (i : PT.LocatedItem<'a>) =
    { i with entity = f state i.location i.entity }
  if Map.isEmpty state then
    r
  else
    { r with
        types = List.map (patch patchType) r.types
        fns = List.map (patch patchFn) r.fns
        values = List.map (patch patchValue) r.values
        traits = List.map (patch patchTrait) r.traits
        impls = List.map (patch patchImpl) r.impls
        tests = List.map (patch patchTest) r.tests }

let private storedNames = ReferenceNamesContent.storedNames

/// Use the caller's saved reference names when it has them.
/// Check that they still describe the same references, in the same order.
let internal effectiveNames (state : State) location target original =
  match Map.tryFind (location, target) state with
  | None -> original
  | Some names ->
    if List.map fst names <> List.map fst original then
      Exception.raiseInternal
        "Reference names do not match declaration content"
        [ "location", location; "target", target ]
    names

/// Find the branch's binding changes and hide main's uncommitted changes.
let branchBindings (branch : PT.BranchId) =
  task {
    if branch.IsMain then
      return Map.empty
    else
      let! mask = Queries.mainDraftMaskOps ()
      let! overlay = Branches.chainOverlayOps branch
      return
        (mask @ overlay)
        |> List.fold
          (fun state op ->
            match Branches.nameEffect op with
            | Some(Branches.Binds(location, kind, hash)) ->
              Map.add
                location
                (Some(PT.Reference.fromHashAndKind (Hash hash, kind)))
                state
            | Some(Branches.Ends location) -> Map.add location None state
            | None -> state)
          Map.empty
  }

/// Save each declaration's reference names before the store keeps one body per hash.
/// Authoring, propagation and draft refresh all need this before writing content.
let captureForStorage
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Task<List<PT.PackageOp>> =
  task {
    let captureDeclaration = ReferenceNamesContent.declaration
    let captured = ops |> List.map (fun op -> op, captureDeclaration op)
    let declarations =
      captured
      |> List.pairwise
      |> List.choose (fun ((declaration, names), (binding, _)) ->
        match PT.tryDeclarationBinding (declaration, binding), names with
        | Some(location, target), Some(_, names) -> Some(location, target, names)
        | _ -> None)
    if List.isEmpty declarations then
      return []
    else
      let! state = load branch
      let mutable canonical = Map.empty
      // The first Add is the blob that insertion keeps, even when several names share it.
      for (_, names) in captured do
        match names with
        | Some(target, names) when not (Map.containsKey target canonical) ->
          let! stored = storedNames target
          canonical <- Map.add target (Option.defaultValue names stored) canonical
        | _ -> ()
      let! bindings = branchBindings branch
      let result = ResizeArray<PT.PackageOp>()
      for (location, target, names) in declarations do
        match Map.tryFind (location, target) state with
        | Some existing when existing = names -> ()
        | None when Map.tryFind target canonical = Some names -> ()
        | existing ->
          // Keep the original names before the first reference-only edit.
          // A new alias has no earlier version at this location.
          if Option.isNone existing then
            let! current =
              match Map.tryFind location bindings with
              | Some current -> Task.FromResult current
              | None ->
                task {
                  let! rows =
                    Queries.getLiveLocationsForHashes
                      [ (let (Hash h) = target.hash in h) ]
                  let present =
                    rows
                    |> Map.toList
                    |> List.collect snd
                    |> List.contains (target.kind, location)
                  return if present then Some target else None
                }
            if current = Some target then
              match Map.tryFind target canonical with
              | Some original ->
                result.Add(
                  PT.PackageOp.SetReferenceNames(
                    location,
                    target,
                    original,
                    Some(OriginTs.next ())
                  )
                )
              | None -> ()
          result.Add(
            PT.PackageOp.SetReferenceNames(
              location,
              target,
              names,
              Some(OriginTs.next ())
            )
          )
      return List.ofSeq result
  }


/// Read the saved reference-name changes for these content versions.
/// Dark combines them with the original names when building snapshots.
let overrides branch (targets : List<PT.Reference>) before =
  task {
    let! state = loadAt branch before
    let wanted = Set.ofList targets
    return
      state
      |> Map.toList
      |> List.choose (fun ((location, target), names) ->
        if Set.contains target wanted then
          Some
            { PT.ReferenceNamesSnapshot.location = location
              target = target
              names = names
              restating = None }
        else
          None)
  }
