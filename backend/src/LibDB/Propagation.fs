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


type PropagationResult =
  { propagationId : uuid; repoints : List<PT.PropagateRepoint> }


/// Discover all items (transitive dependents) that need updating.
///
/// Filters by FQN at every cascade level so same-hash content at other
/// locations does not enter the cascade.
let private discoverDependents
  (branchChain : List<PT.BranchId>)
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
            PMQueries.getDependentsByTargets branchChain toProcess

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

let private affectedFqn =
  function
  | AffectedType(fqn, _, _, _) -> fqn
  | AffectedFn(fqn, _, _, _) -> fqn
  | AffectedValue(fqn, _, _, _) -> fqn


/// Resolve an item's authoritative hash from its location. The caller's
/// `toSourceHash` is sometimes a parser-time placeholder (e.g. empty on a
/// fresh `val ... = ...` from the CLI); the location row is the source of
/// truth post-WipRefresh.
let private resolveCurrentHash
  (branchChain : List<PT.BranchId>)
  (loc : PT.PackageLocation)
  (kind : PT.ItemKind)
  (fallback : Hash)
  : Task<Hash> =
  task {
    let find =
      match kind with
      | PT.ItemKind.Type -> PMTypes.Type.find branchChain loc
      | PT.ItemKind.Fn -> PMTypes.Fn.find branchChain loc
      | PT.ItemKind.Value -> PMTypes.Value.find branchChain loc
    let! resolved = Ply.toTask find
    return resolved |> Option.defaultValue fallback
  }


/// Fetch one item from the PM by hash + kind, tagged with FQN/location.
let private fetchAffected
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
      | Some t -> return Ok(AffectedType(fqn, t, hash, loc))
      | None -> return Error $"Type at {hash} not found"
    | PT.ItemKind.Fn ->
      let! item = Ply.toTask (PMTypes.Fn.get hash)
      match item with
      | Some f -> return Ok(AffectedFn(fqn, f, hash, loc))
      | None -> return Error $"Fn at {hash} not found"
    | PT.ItemKind.Value ->
      let! item = Ply.toTask (PMTypes.Value.get hash)
      match item with
      | Some v -> return Ok(AffectedValue(fqn, v, hash, loc))
      | None -> return Error $"Value at {hash} not found"
  }


let private fetchAffectedDependents
  (dependents : List<PMQueries.LocationDependent>)
  : Task<Result<List<Affected>, string>> =
  let rec loop remaining acc =
    task {
      match remaining with
      | [] -> return Ok(List.rev acc)
      | (dep : PMQueries.LocationDependent) :: rest ->
        let depFqn = PackageLocation.toFQN dep.itemLocation
        match! fetchAffected depFqn dep.itemLocation dep.itemKind dep.itemHash with
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
  let mutable types = Map.empty
  let mutable fns = Map.empty
  let mutable values = Map.empty
  for a in affected do
    match a with
    | AffectedType(fqn, t, h, loc) -> types <- Map.add fqn (t, h, loc) types
    | AffectedFn(fqn, f, h, loc) -> fns <- Map.add fqn (f, h, loc) fns
    | AffectedValue(fqn, v, h, loc) -> values <- Map.add fqn (v, h, loc) values
  HS.stabilize seedMapping { types = types; fns = fns; values = values }


/// Apply the SCC stabilization to one affected item: transform body, stamp
/// final hash, emit Add+SetName ops, plus a repoint when the hash actually
/// changed.
let private applyStabilization
  (s : HS.Stabilization)
  (a : Affected)
  : List<PT.PackageOp> * Option<PT.PropagateRepoint> =
  let fqn = affectedFqn a
  let newHash = Map.findUnsafe fqn s.fqnHashes
  let mkRepoint loc currentHash newRef =
    if newHash = currentHash then
      None
    else
      Some { location = loc; fromRef = newRef currentHash; toRef = newRef newHash }
  match a with
  | AffectedType(_, t, currentHash, loc) ->
    let transformed = { AT.transformType s.mapping t with hash = newHash }
    let ops =
      [ PT.PackageOp.AddType transformed
        PT.PackageOp.SetName(loc, PT.PackageType newHash) ]
    ops, mkRepoint loc currentHash PT.PackageType
  | AffectedFn(_, f, currentHash, loc) ->
    let transformed = { AT.transformFn s.mapping f with hash = newHash }
    let ops =
      [ PT.PackageOp.AddFn transformed
        PT.PackageOp.SetName(loc, PT.PackageFn newHash) ]
    ops, mkRepoint loc currentHash PT.PackageFn
  | AffectedValue(_, v, currentHash, loc) ->
    let transformed = { AT.transformValue s.mapping v with hash = newHash }
    let ops =
      [ PT.PackageOp.AddValue transformed
        PT.PackageOp.SetName(loc, PT.PackageValue newHash) ]
    ops, mkRepoint loc currentHash PT.PackageValue


let private buildSeedMapping
  (sourceLocations : List<PT.PackageLocation>)
  (sourceLocation : PT.PackageLocation)
  (fromSourceHashes : List<Hash>)
  (resolvedSourceHash : Hash)
  : AT.HashMapping =
  { byLocation =
      sourceLocations
      |> List.fold (fun m loc -> Map.add loc resolvedSourceHash m) Map.empty
    byHash =
      fromSourceHashes
      |> List.fold (fun m h -> Map.add h resolvedSourceHash m) Map.empty
    byLocationRename =
      sourceLocations
      |> List.filter (fun loc -> loc <> sourceLocation)
      |> List.fold (fun m loc -> Map.add loc sourceLocation m) Map.empty }


/// Compute new hashes for source + all transitive dependents using SCC-aware
/// hashing, then emit Add+SetName ops and PropagateRepoints.
///
/// SCC hashing is required for mutually-recursive package items; location
/// data lets stale refs be matched without relying only on old hashes.
let private createAllItems
  (branchChain : List<PT.BranchId>)
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

      // Source's hash from its location row beats whatever the caller passed
      // (the CLI sometimes passes a parser-time placeholder).
      let! resolvedSourceHash =
        resolveCurrentHash branchChain sourceLocation sourceItemKind toSourceHash

      match! fetchAffectedDependents dependents with
      | Error e -> return Error e
      | Ok dependentsAffected ->
        // Only include source when mutual recursion makes it part of the
        // affected SCC; otherwise keep the user's already-computed source hash.
        let dependentHashes =
          dependents |> List.map (fun d -> d.itemHash) |> Set.ofList
        let! sourceDeps = PMQueries.getDependencies branchChain resolvedSourceHash
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
            buildSeedMapping
              sourceLocations
              sourceLocation
              fromSourceHashes
              resolvedSourceHash

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


/// Propagates an update to all dependents (including transitive).
/// Returns None if no dependents, or Some(result, ops) if propagation occurred.
/// Entry point for the entire propagation process. Called after a package item is updated.
let propagate
  (branchId : PT.BranchId)
  (sourceLocation : PT.PackageLocation)
  (sourceItemKind : PT.ItemKind)
  (fromSourceHashes : List<Hash>)
  (toSourceHash : Hash)
  : Task<Result<Option<PropagationResult * List<PT.PackageOp>>, string>> =
  task {
    // Fetch branch chain once for all queries
    let! branchChain = Branches.getBranchChain branchId

    let! previousSourceLocations =
      PMQueries.getUnlistedLocationsForRefs
        branchChain
        sourceItemKind
        fromSourceHashes
    let sourceLocations =
      (sourceLocation :: previousSourceLocations) |> List.distinct

    let! dependents =
      discoverDependents branchChain sourceLocations sourceItemKind fromSourceHashes

    match dependents with
    | [] -> return Ok None
    | _ ->
      let! result =
        createAllItems
          branchChain
          fromSourceHashes
          toSourceHash
          sourceLocation
          sourceLocations
          sourceItemKind
          dependents

      match result with
      | Error err -> return Error err
      | Ok(repoints, ops, finalToSourceHash) ->
        let propagationId = System.Guid.NewGuid()

        let mkRef h = PT.Reference.fromHashAndKind (h, sourceItemKind)

        let propagateOp =
          PT.PackageOp.PropagateUpdate(
            propagationId,
            sourceLocation,
            fromSourceHashes |> List.map mkRef,
            mkRef finalToSourceHash,
            repoints
          )

        let allOps = ops @ [ propagateOp ]
        let result = { propagationId = propagationId; repoints = repoints }
        return Ok(Some(result, allOps))
  }
