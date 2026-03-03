/// Propagates package item updates to all dependents by creating new versions
/// with updated ContentHash references.
module LibPackageManager.Propagation

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
open LibSerialization.Hashing

module PMQueries = Queries
module PMTypes = ProgramTypes


type PropagationResult =
  { propagationId : uuid; repoints : List<PT.PropagateRepoint> }


/// Item-specific operations for retrieving, transforming, and creating ops
type private ItemProcessingContext<'T> =
  { itemKind : PT.ItemKind
    getItem : ContentHash -> Ply<Option<'T>> // Given a ContentHash, fetch the full item definition
    getLocations : ContentHash -> Ply<List<PT.PackageLocation>> // Given a ContentHash, look up the item's PackageLocations
    transform : Map<ContentHash, ContentHash> -> 'T -> 'T // Transforms the item by replacing old hashes with new hashes based on the mapping
    computeHash : 'T -> ContentHash // Compute content hash for the transformed item
    withNewId : ContentHash -> 'T -> 'T // Assigns a new ContentHash to the item
    makeAddOp : 'T -> PT.PackageOp // Creates an Add op for the item
    makeSetNameOp : ContentHash * PT.PackageLocation -> PT.PackageOp } // Creates a SetName op for the item


let private fnContext
  (branchChain : List<PT.BranchId>)
  : ItemProcessingContext<PT.PackageFn.PackageFn> =
  { itemKind = PT.ItemKind.Fn
    getItem = PMTypes.Fn.get
    getLocations = PMTypes.Fn.getLocations branchChain
    transform = AstTransformer.transformFn
    computeHash = Hashing.computeFnHash Hashing.Normal
    withNewId = fun hash fn -> { fn with hash = hash }
    makeAddOp = PT.PackageOp.AddFn
    makeSetNameOp = PT.PackageOp.SetFnName }


let private typeContext
  (branchChain : List<PT.BranchId>)
  : ItemProcessingContext<PT.PackageType.PackageType> =
  { itemKind = PT.ItemKind.Type
    getItem = PMTypes.Type.get
    getLocations = PMTypes.Type.getLocations branchChain
    transform = AstTransformer.transformType
    computeHash = Hashing.computeTypeHash Hashing.Normal
    withNewId = fun hash typ -> { typ with hash = hash }
    makeAddOp = PT.PackageOp.AddType
    makeSetNameOp = PT.PackageOp.SetTypeName }


let private valueContext
  (branchChain : List<PT.BranchId>)
  : ItemProcessingContext<PT.PackageValue.PackageValue> =
  { itemKind = PT.ItemKind.Value
    getItem = PMTypes.Value.get
    getLocations = PMTypes.Value.getLocations branchChain
    transform = AstTransformer.transformValue
    computeHash = Hashing.computeValueHash Hashing.Normal
    withNewId = fun hash value -> { value with hash = hash }
    makeAddOp = PT.PackageOp.AddValue
    makeSetNameOp = PT.PackageOp.SetValueName }


/// Creates a new version of an item with transformed ContentHash references.
/// Computes the real content hash from the transformed content.
/// Returns (repoint, ops, newHash) so the caller can build the mapping incrementally.
let private processItem<'T>
  (ctx : ItemProcessingContext<'T>)
  (itemHash : ContentHash)
  (mapping : Map<ContentHash, ContentHash>)
  : Task<Result<PT.PropagateRepoint * List<PT.PackageOp> * ContentHash, string>> =
  task {
    let kindName = ctx.itemKind.toString ()
    let! itemOpt = ctx.getItem itemHash
    match itemOpt with
    | Some item ->
      // Transform the item: rewrite all ContentHash references in its body using the mapping
      let transformed = ctx.transform mapping item
      // Compute the real content hash from the transformed content
      let newHash = ctx.computeHash transformed
      let newItem = ctx.withNewId newHash transformed

      let! locs = ctx.getLocations itemHash
      match locs with
      | loc :: _ ->
        let addOp = ctx.makeAddOp newItem
        let setNameOp = ctx.makeSetNameOp (newHash, loc)
        return
          Ok(
            { location = loc
              itemKind = ctx.itemKind
              fromHash = itemHash
              toHash = newHash },
            [ addOp; setNameOp ],
            newHash
          )
      | [] -> return Error $"Location for {kindName} {itemHash} not found"
    | None -> return Error $"{kindName} {itemHash} not found"
  }


/// Phase 1: Discover all items that need to be updated (transitive dependents)
let private discoverDependents
  (branchChain : List<PT.BranchId>)
  (fromSourceHashes : List<ContentHash>)
  (toSourceHash : ContentHash)
  : Task<List<PMQueries.PackageDep>> =
  task {
    let rec loop
      (pending : List<ContentHash>)
      (processed : Set<ContentHash>)
      (accumulated : List<PMQueries.PackageDep>)
      =
      task {
        // filter out any pending hashes that are already processed
        let toProcess =
          pending |> List.filter (fun id -> not (Set.contains id processed))

        match toProcess with
        | [] -> return accumulated
        | _ ->
          // Add all toProcess hashes to the processed set so we won't visit them again
          let newProcessed =
            toProcess |> List.fold (fun acc id -> Set.add id acc) processed

          // for all these hashes, find every item that depends on any of them
          let! batchDependents = PMQueries.getDependentsBatch branchChain toProcess

          // Filter the batch results:
          //  - Remove any that are already in the processed set (no cycles)
          //  - Deduplicate by itemHash
          //  - Convert to PackageDep { itemHash, itemKind }
          let newDeps =
            batchDependents
            |> List.filter (fun d -> not (Set.contains d.itemHash newProcessed))
            |> List.distinctBy (fun d -> d.itemHash)
            |> List.map (fun (d : PMQueries.BatchDependent) ->
              { PMQueries.itemHash = d.itemHash; PMQueries.itemKind = d.itemKind })

          let newPending = newDeps |> List.map (fun d -> d.itemHash)
          let newAccumulated = accumulated @ newDeps

          return! loop newPending newProcessed newAccumulated
      }

    // Start with fromSourceHashes as pending, toSourceHash already processed
    // (we don't want the source to be included as a dependent)
    return! loop fromSourceHashes (Set.singleton toSourceHash) []
  }


/// Check if source item needs to be updated (for mutual recursion)
let private sourceNeedsUpdate
  (branchChain : List<PT.BranchId>)
  (toSourceHash : ContentHash)
  (oldHashesBeingReplaced : Set<ContentHash>)
  : Task<bool> =
  task {
    // "what does the source item reference"
    let! deps = PMQueries.getDependencies branchChain toSourceHash
    return
      // If any of those references are in oldHashesBeingReplaced (i.e., they are dependents that are getting new hashes),
      // the source needs an update too.
      deps
      |> List.exists (fun dep -> Set.contains dep.itemHash oldHashesBeingReplaced)
  }


/// Helper to dispatch processItem based on item kind
let private processItemByKind
  (branchChain : List<PT.BranchId>)
  (itemHash : ContentHash)
  (itemKind : PT.ItemKind)
  (mapping : Map<ContentHash, ContentHash>)
  : Task<Result<PT.PropagateRepoint * List<PT.PackageOp> * ContentHash, string>> =
  task {
    match itemKind with
    | PT.ItemKind.Fn -> return! processItem (fnContext branchChain) itemHash mapping
    | PT.ItemKind.Type ->
      return! processItem (typeContext branchChain) itemHash mapping
    | PT.ItemKind.Value ->
      return! processItem (valueContext branchChain) itemHash mapping
  }


/// Phase 2 & 3: Process dependents sequentially, computing real content hashes
/// incrementally. Each item is transformed using the mapping-so-far (which grows
/// as each item's real hash is computed), ensuring that references to already-
/// processed dependents use their correct content hashes.
let private createAllItems
  (branchChain : List<PT.BranchId>)
  (fromSourceHashes : List<ContentHash>)
  (toSourceHash : ContentHash)
  (dependents : List<PMQueries.PackageDep>)
  (sourceItemKind : PT.ItemKind)
  : Task<Result<List<PT.PropagateRepoint> * List<PT.PackageOp> * ContentHash, string>> =
  task {
    match dependents with
    | [] -> return Ok([], [], toSourceHash)
    | _ ->
      // Start with source mapping
      let mutable mapping =
        fromSourceHashes |> List.map (fun id -> (id, toSourceHash)) |> Map.ofList

      let mutable repoints : List<PT.PropagateRepoint> = []
      let mutable allOpsRev : List<List<PT.PackageOp>> = []
      let mutable error : string option = None

      // Dependents are processed in BFS order from discoverDependents, which
      // guarantees that all transitive dependencies of item N are processed
      // before N itself, so the hash mapping is complete when each item is visited.
      for dep in dependents do
        if error.IsNone then
          let! result =
            processItemByKind branchChain dep.itemHash dep.itemKind mapping

          match result with
          | Error e -> error <- Some e
          | Ok(repoint, ops, newHash) ->
            mapping <- Map.add dep.itemHash newHash mapping
            repoints <- repoint :: repoints
            allOpsRev <- ops :: allOpsRev

      // Reverse to restore BFS processing order
      let repoints = List.rev repoints
      let allOps = allOpsRev |> List.rev |> List.concat

      match error with
      | Some e -> return Error e
      | None ->
        // Check if source needs update (mutual recursion: source references a dependent)
        let oldHashesBeingReplaced =
          dependents |> List.map (fun d -> d.itemHash) |> Set.ofList

        let! needsSourceUpdate =
          sourceNeedsUpdate branchChain toSourceHash oldHashesBeingReplaced

        if needsSourceUpdate then
          let! sourceResult =
            processItemByKind branchChain toSourceHash sourceItemKind mapping

          match sourceResult with
          | Error e -> return Error e
          | Ok(sourceRepoint, sourceOps, newSourceHash) ->
            return Ok(sourceRepoint :: repoints, allOps @ sourceOps, newSourceHash)
        else
          return Ok(repoints, allOps, toSourceHash)
  }


/// Propagates an update to all dependents (including transitive).
/// Returns None if no dependents, or Some(result, ops) if propagation occurred.
/// Entry point for the entire propagation process. Called after a package item is updated.
let propagate
  (branchId : PT.BranchId)
  (sourceLocation : PT.PackageLocation)
  (sourceItemKind : PT.ItemKind)
  (fromSourceHashes : List<ContentHash>)
  (toSourceHash : ContentHash)
  : Task<Result<Option<PropagationResult * List<PT.PackageOp>>, string>> =
  task {
    // Fetch branch chain once for all queries
    let! branchChain = Branches.getBranchChain branchId

    // Phase 1: Discover all items that need updating
    let! dependents = discoverDependents branchChain fromSourceHashes toSourceHash

    match dependents with
    | [] -> return Ok None
    | _ ->
      // Phase 2 & 3: Build mapping and create all items
      let! result =
        createAllItems
          branchChain
          fromSourceHashes
          toSourceHash
          dependents
          sourceItemKind

      match result with
      | Error err -> return Error err
      | Ok(repoints, ops, finalToSourceHash) ->
        let propagationId = System.Guid.NewGuid()

        let propagateOp =
          PT.PackageOp.PropagateUpdate(
            propagationId,
            sourceLocation,
            sourceItemKind,
            fromSourceHashes,
            finalToSourceHash,
            repoints
          )

        let allOps = ops @ [ propagateOp ]
        let result = { propagationId = propagationId; repoints = repoints }
        return Ok(Some(result, allOps))
  }
