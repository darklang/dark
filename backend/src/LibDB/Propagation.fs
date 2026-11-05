/// Propagates package item updates to all dependents by creating new versions
/// with updated Hash references.
module LibDB.Propagation

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
open LibSerialization.Hashing

module PMQueries = Queries
module PMTypes = ProgramTypes
module PackageLocation = LibDB.PackageLocation
module AT = LibDB.AstTransformer
module HS = LibDB.HashStabilization


/// What a propagation moved. Reported back so the caller can say so; the state
/// change itself is the accompanying Add + SetName ops, which is all it ever was.
type PropagationResult = { repoints : List<PT.PropagateRepoint> }


/// Every item that transitively depends on the targets, filtered by FQN at each level so same-hash content
/// at other locations does not enter the cascade.
///
/// Dependents are found in <param branch>'s view: a name the branch rebound to a body of its own that does
/// not refer to the target is not one, whatever main's body at that name says.
///
/// Propagate the updated version to every caller on this branch, regardless of owner.
let private discoverDependents
  (referenceNames : ReferenceNames.State)
  (branch : PT.BranchId)
  (sourceLocations : List<PT.PackageLocation>)
  (sourceItemKind : PT.ItemKind)
  (fromSourceHashes : List<Hash>)
  : Task<List<PMQueries.LocationDependent>> =
  task {
    let key (target : PMQueries.LocationTarget) =
      (target.itemKind.toString (), PackageLocation.toFQN target.location)

    let dependentTarget
      (d : PMQueries.LocationDependent)
      : PMQueries.LocationTarget =
      { itemKind = d.itemKind; location = d.itemLocation; hashes = [ d.itemHash ] }

    let rec loop
      (pending : List<PMQueries.LocationTarget>)
      (processed : Set<string * string>)
      (dependentsByLevelRev : List<List<PMQueries.LocationDependent>>)
      =
      task {
        let toProcess =
          pending
          |> List.filter (fun target -> not (Set.contains (key target) processed))

        match toProcess with
        | [] -> return List.concat (List.rev dependentsByLevelRev)
        | _ ->
          let newProcessed =
            toProcess
            |> List.fold (fun acc target -> Set.add (key target) acc) processed

          let! batchDependents =
            ReferenceDependencies.dependents referenceNames branch toProcess

          let unseen =
            batchDependents
            |> List.filter (fun d ->
              not (Set.contains (key (dependentTarget d)) newProcessed))
            |> List.distinctBy (fun d -> key (dependentTarget d))

          let newPending = unseen |> List.map dependentTarget

          return! loop newPending newProcessed (unseen :: dependentsByLevelRev)
      }

    return!
      loop
        (sourceLocations
         |> List.map (fun sourceLocation ->
           { itemKind = sourceItemKind
             location = sourceLocation
             hashes = fromSourceHashes }))
        Set.empty
        []
  }


/// One affected item, fetched from the PM, ready to feed into SCC.
type private Affected =
  | AffectedType of
    fqn : string *
    item : PT.PackageType.PackageType *
    currentHash : Hash *
    location : PT.PackageLocation
  | AffectedFn of
    fqn : string *
    item : PT.PackageFn.PackageFn *
    currentHash : Hash *
    location : PT.PackageLocation
  | AffectedValue of
    fqn : string *
    item : PT.PackageValue.PackageValue *
    currentHash : Hash *
    location : PT.PackageLocation
  | AffectedTrait of
    fqn : string *
    item : PT.Trait.Trait *
    currentHash : Hash *
    location : PT.PackageLocation
  | AffectedTraitImpl of
    fqn : string *
    item : PT.TraitImpl.TraitImpl *
    currentHash : Hash *
    location : PT.PackageLocation
  | AffectedTest of
    fqn : string *
    item : PT.PackageTest.PackageTest *
    currentHash : Hash *
    location : PT.PackageLocation

let private affectedFqn =
  function
  | AffectedType(fqn, _, _, _) -> fqn
  | AffectedFn(fqn, _, _, _) -> fqn
  | AffectedValue(fqn, _, _, _) -> fqn
  | AffectedTrait(fqn, _, _, _) -> fqn
  | AffectedTraitImpl(fqn, _, _, _) -> fqn
  | AffectedTest(fqn, _, _, _) -> fqn

let private affectedCurrentHash =
  function
  | AffectedType(_, _, currentHash, _) -> currentHash
  | AffectedFn(_, _, currentHash, _) -> currentHash
  | AffectedValue(_, _, currentHash, _) -> currentHash
  | AffectedTrait(_, _, currentHash, _) -> currentHash
  | AffectedTraitImpl(_, _, currentHash, _) -> currentHash
  | AffectedTest(_, _, currentHash, _) -> currentHash


/// Resolve an item's authoritative hash from its location. The caller's
/// `toSourceHash` is sometimes a parser-time placeholder (e.g. empty on a
/// fresh `val ... = ...` from the CLI); the location row is the source of
/// truth post-WipRefresh.
let private resolveCurrentHash
  (branch : PT.BranchId)
  (loc : PT.PackageLocation)
  (kind : PT.ItemKind)
  (fallback : Hash)
  : Task<Hash> =
  task {
    let find =
      match kind with
      | PT.ItemKind.Type -> PMTypes.Type.find branch loc
      | PT.ItemKind.Fn -> PMTypes.Fn.find branch loc
      | PT.ItemKind.Value -> PMTypes.Value.find branch loc
      | PT.ItemKind.Trait -> PMTypes.Trait.find branch loc
      | PT.ItemKind.TraitImpl -> PMTypes.TraitImpl.find branch loc
      | PT.ItemKind.Test -> PMTypes.Test.find branch loc
    let! resolved = Ply.toTask find
    return resolved |> Option.defaultValue fallback
  }


/// Fetch one item from the PM by hash + kind, tagged with FQN/location.
let private fetchAffected
  (referenceNames : ReferenceNames.State)
  (fqn : string)
  (loc : PT.PackageLocation)
  (kind : PT.ItemKind)
  (hash : Hash)
  : Task<Result<Affected, string>> =
  task {
    match kind with
    | PT.ItemKind.Type ->
      let! item = Ply.toTask (PMTypes.Type.get hash)
      match item with
      | Some t ->
        return
          Ok(
            AffectedType(
              fqn,
              ReferenceNames.patchType referenceNames loc t,
              hash,
              loc
            )
          )
      | None -> return Error $"Type at {hash} not found"
    | PT.ItemKind.Fn ->
      let! item = Ply.toTask (PMTypes.Fn.get hash)
      match item with
      | Some f ->
        return
          Ok(AffectedFn(fqn, ReferenceNames.patchFn referenceNames loc f, hash, loc))
      | None -> return Error $"Fn at {hash} not found"
    | PT.ItemKind.Value ->
      let! item = Ply.toTask (PMTypes.Value.get hash)
      match item with
      | Some v ->
        return
          Ok(
            AffectedValue(
              fqn,
              ReferenceNames.patchValue referenceNames loc v,
              hash,
              loc
            )
          )
      | None -> return Error $"Value at {hash} not found"
    | PT.ItemKind.Trait ->
      let! item = Ply.toTask (PMTypes.Trait.get hash)
      match item with
      | Some t ->
        return
          Ok(
            AffectedTrait(
              fqn,
              ReferenceNames.patchTrait referenceNames loc t,
              hash,
              loc
            )
          )
      | None -> return Error $"Trait at {hash} not found"
    | PT.ItemKind.TraitImpl ->
      let! item = Ply.toTask (PMTypes.TraitImpl.get hash)
      match item with
      | Some i ->
        return
          Ok(
            AffectedTraitImpl(
              fqn,
              ReferenceNames.patchImpl referenceNames loc i,
              hash,
              loc
            )
          )
      | None -> return Error $"Impl at {hash} not found"
    | PT.ItemKind.Test ->
      let! item = Ply.toTask (PMTypes.Test.get hash)
      match item with
      | Some test ->
        return
          Ok(
            AffectedTest(
              fqn,
              ReferenceNames.patchTest referenceNames loc test,
              hash,
              loc
            )
          )
      | None -> return Error $"Test at {hash} not found"
  }


let private fetchAffectedDependents
  (referenceNames : ReferenceNames.State)
  (dependents : List<PMQueries.LocationDependent>)
  : Task<Result<List<Affected>, string>> =
  let rec loop remaining acc =
    task {
      match remaining with
      | [] -> return Ok(List.rev acc)
      | (dep : PMQueries.LocationDependent) :: rest ->
        let depFqn = PackageLocation.toFQN dep.itemLocation
        match!
          fetchAffected
            referenceNames
            depFqn
            dep.itemLocation
            dep.itemKind
            dep.itemHash
        with
        | Ok a -> return! loop rest (a :: acc)
        | Error e -> return Error e
    }
  loop dependents []


/// Build the SCC input from affected items. `seedMapping` covers
/// out-of-batch substitutions such as source refs when source is not
/// included in the SCC batch.
let private stabilizationFromAffected
  (seedMapping : AT.HashMapping)
  (affected : List<Affected>)
  : HS.Stabilization =
  affected
  |> List.fold
    (fun (input : HS.Input) a ->
      match a with
      | AffectedType(fqn, t, h, loc) ->
        { input with types = Map.add fqn (t, h, loc) input.types }
      | AffectedFn(fqn, f, h, loc) ->
        { input with fns = Map.add fqn (f, h, loc) input.fns }
      | AffectedValue(fqn, v, h, loc) ->
        { input with values = Map.add fqn (v, h, loc) input.values }
      | AffectedTrait(fqn, t, h, loc) ->
        { input with traits = Map.add fqn (t, h, loc) input.traits }
      | AffectedTraitImpl(fqn, i, h, loc) ->
        { input with impls = Map.add fqn (i, h, loc) input.impls }
      // Left out: nothing refers to a test, so it is never part of a cycle. Each
      // is hashed on its own below, once the others' hashes are final.
      | AffectedTest _ -> input)
    HS.emptyInput
  |> HS.stabilize seedMapping


/// Apply the SCC stabilization to one affected item: transform body, stamp
/// final hash, emit Add+SetName ops, plus a repoint when the hash actually
/// changed.
///
/// An item that stabilizes to the hash it already has emits NOTHING. That case is reached whenever
/// propagation runs a second time over an edit that already propagated -- `commit` does exactly
/// that -- and the op it would otherwise author is `SetName(loc, X, previous = X)`: a name rebound
/// to what it already holds. That op is not merely noise. It is a second naming of the same name in
/// one draft, so `Draft.collapse` keeps it and drops the FIRST one, which is the binding the fold
/// actually recorded, and dropping that relists the pre-edit version. Emitting it means committing
/// a propagated edit reverts its callers.
let private applyStabilization
  (s : HS.Stabilization)
  (a : Affected)
  : List<PT.PackageOp> * Option<PT.PropagateRepoint> =
  let fqn = affectedFqn a
  let transformedTest =
    match a with
    | AffectedTest(_, test, _, _) -> Some(AT.transformTest s.mapping test)
    | _ -> None
  let newHash =
    match transformedTest with
    | Some test -> Hashing.computeTestHash Hashing.Normal test
    | None -> Map.findUnsafe fqn s.fqnHashes
  // Past the guard below, `newHash` differs from `currentHash` by construction, so every
  // item that gets here repoints.
  let mkRepoint loc currentHash newRef =
    Some { location = loc; fromRef = newRef currentHash; toRef = newRef newHash }
  // Each `SetName` names what it replaced: `currentHash`, the same hash the repoint records as
  // `fromRef`. A caller that followed its dependency descends from the version it was on, and that
  // recorded lineage is what stops it looking like an independent creation to the other machine.
  if newHash = affectedCurrentHash a then
    [], None
  else
    match a with
    | AffectedType(_, t, currentHash, loc) ->
      let transformed = { AT.transformType s.mapping t with hash = newHash }
      let ops =
        [ PT.PackageOp.AddType transformed
          PT.PackageOp.SetName(loc, PT.PackageType newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageType
    | AffectedFn(_, f, currentHash, loc) ->
      let transformed = { AT.transformFn s.mapping f with hash = newHash }
      let ops =
        [ PT.PackageOp.AddFn transformed
          PT.PackageOp.SetName(loc, PT.PackageFn newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageFn
    | AffectedValue(_, v, currentHash, loc) ->
      let transformed = { AT.transformValue s.mapping v with hash = newHash }
      let ops =
        [ PT.PackageOp.AddValue transformed
          PT.PackageOp.SetName(loc, PT.PackageValue newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageValue
    | AffectedTrait(_, t, currentHash, loc) ->
      let transformed = { AT.transformTrait s.mapping t with hash = newHash }
      let ops =
        [ PT.PackageOp.AddTrait transformed
          PT.PackageOp.SetName(loc, PT.PackageTrait newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageTrait
    | AffectedTraitImpl(_, i, currentHash, loc) ->
      let transformed = { AT.transformImpl s.mapping i with hash = newHash }
      let ops =
        [ PT.PackageOp.AddTraitImpl transformed
          PT.PackageOp.SetName(loc, PT.PackageTraitImpl newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageTraitImpl
    | AffectedTest(_, _, currentHash, loc) ->
      let transformed =
        transformedTest
        |> Option.defaultWith (fun () ->
          Exception.raiseInternal "missing transformed package test" [ "fqn", fqn ])
        |> fun test -> { test with hash = newHash }
      let ops =
        [ PT.PackageOp.AddTest transformed
          PT.PackageOp.SetName(loc, PT.PackageTest newHash, Some currentHash) ]
      ops, mkRepoint loc currentHash PT.PackageTest


let private buildSeedMapping
  (sourceLocations : List<PT.PackageLocation>)
  (fromSourceHashes : List<Hash>)
  (resolvedSourceHash : Hash)
  : AT.HashMapping =
  { byLocation =
      sourceLocations
      |> List.fold (fun m loc -> Map.add loc resolvedSourceHash m) Map.empty
    byHash =
      fromSourceHashes
      |> List.fold (fun m h -> Map.add h resolvedSourceHash m) Map.empty
    // Propagation moves hashes; it never re-resolves a trait call. A dependent that follows a
    // newer implementation does it through the ordinary fn edge, which `byHash` above covers.
    referenceName = None
    resolvedImpls = Map.empty
    boundImpls = Map.empty }


/// Compute new hashes for source + all transitive dependents using SCC-aware
/// hashing, then emit Add+SetName ops and PropagateRepoints.
///
/// SCC hashing is required for mutually-recursive package items; location
/// data lets stale refs be matched without relying only on old hashes.
let private createAllItems
  (referenceNames : ReferenceNames.State)
  (branch : PT.BranchId)
  (fromSourceHashes : List<Hash>)
  (toSourceHash : Hash)
  (sourceLocation : PT.PackageLocation)
  (sourceLocations : List<PT.PackageLocation>)
  (sourceItemKind : PT.ItemKind)
  (dependents : List<PMQueries.LocationDependent>)
  : Task<Result<List<PT.PropagateRepoint> * List<PT.PackageOp> * Hash, string>> =
  task {
    if List.isEmpty dependents then
      return Ok([], [], toSourceHash)
    else
      let sourceFqn = PackageLocation.toFQN sourceLocation

      // The source's CURRENT hash beats whatever the caller passed: the CLI
      // sometimes passes a parser-time placeholder, and an EMPTY one would seed the
      // substitution map with "rewrite every reference to nothing", leaving
      // dependents pointing at no hash. Resolving through the branch's own PM gives
      // the branch's binding where it has one and main's where it doesn't.
      let! resolvedSourceHash =
        resolveCurrentHash branch sourceLocation sourceItemKind toSourceHash

      match! fetchAffectedDependents referenceNames dependents with
      | Error e -> return Error e
      | Ok dependentsAffected ->
        // Only include source when mutual recursion makes it part of the
        // affected SCC; otherwise keep the user's already-computed source hash.
        let dependentHashes =
          dependents |> List.map (fun d -> d.itemHash) |> Set.ofList
        let! sourceDeps = PMQueries.getDependencies resolvedSourceHash
        // CLEANUP: this cycle check is still hash-based. It can
        // conservatively include the source when a dependency hash collides
        // with an affected item at another FQN. Make forward dependency
        // lookup location-aware too, then compare by (location, kind).
        let sourceInCycle =
          sourceDeps
          |> List.exists (fun dep -> Set.contains dep.itemHash dependentHashes)

        let! affectedAndSource =
          if sourceInCycle then
            task {
              match!
                fetchAffected
                  referenceNames
                  sourceFqn
                  sourceLocation
                  sourceItemKind
                  resolvedSourceHash
              with
              | Error e -> return Error e
              | Ok s -> return Ok(dependentsAffected @ [ s ], true)
            }
          else
            task { return Ok(dependentsAffected, false) }

        match affectedAndSource with
        | Error e -> return Error e
        | Ok(affected, sourceIncluded) ->
          // Seed both lookup paths: location for resolver-tagged refs, hash
          // for legacy/null-location refs. Hashing and AST rewriting must see
          // the same substitutions.
          let seedMapping =
            buildSeedMapping sourceLocations fromSourceHashes resolvedSourceHash

          let stabilization = stabilizationFromAffected seedMapping affected

          let finalSourceHash =
            if sourceIncluded then
              Map.findUnsafe sourceFqn stabilization.fqnHashes
            else
              resolvedSourceHash

          // Source emits ops only if the SCC changed its hash.
          let toEmit =
            if sourceIncluded && finalSourceHash = resolvedSourceHash then
              affected |> List.filter (fun a -> affectedFqn a <> sourceFqn)
            else
              affected

          let perItem = toEmit |> List.map (applyStabilization stabilization)
          let ops = perItem |> List.collect fst
          let repoints = perItem |> List.choose snd

          return Ok(repoints, ops, finalSourceHash)
  }


/// The entry point for propagation: called after a package item is updated, it repoints every
/// dependent, transitive ones included. `None` when there are no dependents; otherwise what moved
/// and the ops that move it.
///
/// <param branch> is the branch this runs on; main is an ordinary id here.
let propagate
  (branch : PT.BranchId)
  (sourceLocation : PT.PackageLocation)
  (sourceItemKind : PT.ItemKind)
  (fromSourceHashes : List<Hash>)
  (toSourceHash : Hash)
  : Task<Result<Option<PropagationResult * List<PT.PackageOp>>, string>> =
  task {
    let! referenceNames = ReferenceNames.load branch
    let sourceLocations = [ sourceLocation ]

    let! dependents =
      discoverDependents
        referenceNames
        branch
        sourceLocations
        sourceItemKind
        fromSourceHashes

    match dependents with
    | [] -> return Ok None
    | _ ->
      let! result =
        createAllItems
          referenceNames
          branch
          fromSourceHashes
          toSourceHash
          sourceLocation
          sourceLocations
          sourceItemKind
          dependents

      match result with
      | Error err -> return Error err
      | Ok(repoints, ops, _finalToSourceHash) ->
        // No marker op: the Add + SetName ops ARE the propagation. Grouping comes from
        // the commit, and "this version lost" from a recorded conflict.
        return Ok(Some({ repoints = repoints }, ops))
  }
