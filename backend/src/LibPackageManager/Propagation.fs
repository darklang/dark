/// Propagates package item updates to all dependents by creating new versions
/// with updated UUID references.
module LibPackageManager.Propagation

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes

module PMQueries = Queries
module PMTypes = ProgramTypes


type PropagationResult =
  { propagationId : uuid; repoints : List<PT.PropagateRepoint> }


/// Item-specific operations for retrieving, transforming, and creating ops
type private ItemProcessingContext<'T> =
  { itemKind : PT.ItemKind
    getItem : uuid -> Ply<Option<'T>> // Given a UUID, fetch the full item definition
    getLocation : uuid -> Ply<Option<PT.PackageLocation>> // Given a UUID, look up the item's PackageLocation
    transform : Map<uuid, uuid> -> 'T -> 'T // Transforms the item by replacing old UUIDs with new UUIDs based on the mapping
    withNewId : uuid -> 'T -> 'T // Assigns a new UUID to the item
    makeAddOp : 'T -> PT.PackageOp // Creates an Add op for the item
    makeSetNameOp : uuid * PT.PackageLocation -> PT.PackageOp } // Creates a SetName op for the item


let private fnContext
  (branchChain : List<PT.BranchId>)
  : ItemProcessingContext<PT.PackageFn.PackageFn> =
  { itemKind = PT.ItemKind.Fn
    getItem = PMTypes.Fn.get
    getLocation = PMTypes.Fn.getLocation branchChain
    transform = AstTransformer.transformFn
    withNewId = fun id fn -> { fn with id = id }
    makeAddOp = PT.PackageOp.AddFn
    makeSetNameOp = PT.PackageOp.SetFnName }


let private typeContext
  (branchChain : List<PT.BranchId>)
  : ItemProcessingContext<PT.PackageType.PackageType> =
  { itemKind = PT.ItemKind.Type
    getItem = PMTypes.Type.get
    getLocation = PMTypes.Type.getLocation branchChain
    transform = AstTransformer.transformType
    withNewId = fun id typ -> { typ with id = id }
    makeAddOp = PT.PackageOp.AddType
    makeSetNameOp = PT.PackageOp.SetTypeName }


let private valueContext
  (branchChain : List<PT.BranchId>)
  : ItemProcessingContext<PT.PackageValue.PackageValue> =
  { itemKind = PT.ItemKind.Value
    getItem = PMTypes.Value.get
    getLocation = PMTypes.Value.getLocation branchChain
    transform = AstTransformer.transformValue
    withNewId = fun id value -> { value with id = id }
    makeAddOp = PT.PackageOp.AddValue
    makeSetNameOp = PT.PackageOp.SetValueName }


/// Item info collected during discovery phase
type private ItemToUpdate = { itemId : uuid; itemKind : PT.ItemKind; newUuid : uuid }


/// Creates a new version of an item with transformed UUID references
// Takes a single item and produces the ops needed to create its new version.
let private processItem<'T>
  (ctx : ItemProcessingContext<'T>)
  (itemId : uuid)
  (newUuid : uuid)
  (mapping : Map<uuid, uuid>)
  : Task<Result<PT.PropagateRepoint * List<PT.PackageOp>, string>> =
  task {
    let kindName = ctx.itemKind.toString ()
    let! itemOpt = ctx.getItem itemId
    match itemOpt with
    | Some item ->
      // Transform the item: rewrite all UUID references in its body using the mapping, then assign the new UUID
      let transformed = ctx.transform mapping item
      let newItem = ctx.withNewId newUuid transformed

      let! locOpt = ctx.getLocation itemId
      match locOpt with
      | Some loc ->
        // create Add and SetName ops
        let addOp = ctx.makeAddOp newItem
        let setNameOp = ctx.makeSetNameOp (newUuid, loc)
        return
          Ok(
            { location = loc
              itemKind = ctx.itemKind
              fromUUID = itemId
              toUUID = newUuid },
            [ addOp; setNameOp ]
          )
      | None -> return Error $"Location for {kindName} {itemId} not found"
    | None -> return Error $"{kindName} {itemId} not found"
  }


/// Phase 1: Discover all items that need to be updated (transitive dependents)
let private discoverDependents
  (branchChain : List<PT.BranchId>)
  (fromSourceUUIDs : List<uuid>)
  (toSourceUUID : uuid)
  : Task<List<PMQueries.PackageDep>> =
  task {
    let rec loop
      (pending : List<uuid>)
      (processed : Set<uuid>)
      (accumulated : List<PMQueries.PackageDep>)
      =
      task {
        // filter out any pending UUIDs that are already processed
        let toProcess =
          pending |> List.filter (fun id -> not (Set.contains id processed))

        match toProcess with
        | [] -> return accumulated
        | _ ->
          // Add all toProcess UUIDs to the processed set so we won't visit them again
          let newProcessed =
            toProcess |> List.fold (fun acc id -> Set.add id acc) processed

          // for all these UUIDs, find every item that depends on any of them
          let! batchDependents = PMQueries.getDependentsBatch branchChain toProcess

          // Filter the batch results:
          //  - Remove any that are already in the processed set (no cycles)
          //  - Deduplicate by itemId
          //  - Convert to PackageDep { itemId, itemKind }
          let newDeps =
            batchDependents
            |> List.filter (fun d -> not (Set.contains d.itemId newProcessed))
            |> List.distinctBy (fun d -> d.itemId)
            |> List.map (fun (d : PMQueries.BatchDependent) ->
              { PMQueries.itemId = d.itemId; PMQueries.itemKind = d.itemKind })

          let newPending = newDeps |> List.map (fun d -> d.itemId)
          let newAccumulated = accumulated @ newDeps

          return! loop newPending newProcessed newAccumulated
      }

    // Start with fromSourceUUIDs as pending, toSourceUUID already processed
    // (we don't want the source to be included as a dependent)
    return! loop fromSourceUUIDs (Set.singleton toSourceUUID) []
  }


/// Check if source item needs to be updated (for mutual recursion)
let private sourceNeedsUpdate
  (branchChain : List<PT.BranchId>)
  (toSourceUUID : uuid)
  (oldUuidsBeingReplaced : Set<uuid>)
  : Task<bool> =
  task {
    // "what does the source item reference"
    let! deps = PMQueries.getDependencies branchChain toSourceUUID
    return
      // If any of those references are in oldUuidsBeingReplaced (i.e., they are dependents that are getting new UUIDs),
      // the source needs an update too.
      deps |> List.exists (fun dep -> Set.contains dep.itemId oldUuidsBeingReplaced)
  }


/// Phase 2 & 3: Build mapping and create all items
let private createAllItems
  (branchChain : List<PT.BranchId>)
  (fromSourceUUIDs : List<uuid>)
  (toSourceUUID : uuid)
  (dependents : List<PMQueries.PackageDep>)
  (sourceItemKind : PT.ItemKind)
  : Task<Result<List<PT.PropagateRepoint> * List<PT.PackageOp> * uuid, string>> =
  task {
    match dependents with
    | [] -> return Ok([], [], toSourceUUID)
    | _ ->
      // Generate new UUIDs for all dependents
      let dependentsWithNewIds =
        dependents
        |> List.map (fun dep ->
          { itemId = dep.itemId
            itemKind = dep.itemKind
            newUuid = System.Guid.NewGuid() })

      // Collect all old UUIDs that are being replaced (for cycle detection)
      let oldUuidsBeingReplaced =
        dependentsWithNewIds |> List.map (fun d -> d.itemId) |> Set.ofList

      // Check if source needs update (for mutual recursion)
      let! needsSourceUpdate =
        sourceNeedsUpdate branchChain toSourceUUID oldUuidsBeingReplaced

      // Generate UUID for source if needed
      let sourceNewUuidOpt =
        if needsSourceUpdate then Some(System.Guid.NewGuid()) else None

      // Build complete mapping
      let baseMapping =
        fromSourceUUIDs |> List.map (fun id -> (id, toSourceUUID)) |> Map.ofList

      let dependentMapping =
        dependentsWithNewIds
        |> List.map (fun d -> (d.itemId, d.newUuid))
        |> Map.ofList

      // If source is being updated, add mapping from old source to new source
      let fullMapping =
        match sourceNewUuidOpt with
        | Some newSourceUuid ->
          baseMapping
          |> Map.fold (fun acc k _ -> Map.add k newSourceUuid acc) dependentMapping
          |> Map.add toSourceUUID newSourceUuid
        | None ->
          baseMapping |> Map.fold (fun acc k v -> Map.add k v acc) dependentMapping

      // Create all dependent items using the complete mapping
      let! dependentResults =
        dependentsWithNewIds
        |> List.map (fun item ->
          task {
            match item.itemKind with
            | PT.ItemKind.Fn ->
              return!
                processItem
                  (fnContext branchChain)
                  item.itemId
                  item.newUuid
                  fullMapping
            | PT.ItemKind.Type ->
              return!
                processItem
                  (typeContext branchChain)
                  item.itemId
                  item.newUuid
                  fullMapping
            | PT.ItemKind.Value ->
              return!
                processItem
                  (valueContext branchChain)
                  item.itemId
                  item.newUuid
                  fullMapping
          })
        |> Task.flatten

      // Check for errors
      let errors =
        dependentResults
        |> List.choose (fun r ->
          match r with
          | Error e -> Some e
          | Ok _ -> None)

      match errors with
      | err :: _ -> return Error err
      | [] ->
        let dependentRepoints, dependentOps =
          dependentResults
          |> List.choose (fun r ->
            match r with
            | Ok(rp, ops) -> Some(rp, ops)
            | Error _ -> None)
          |> List.unzip

        let flatDependentOps = dependentOps |> List.concat

        // Create source item if needed
        match sourceNewUuidOpt with
        | Some newSourceUuid ->
          let! sourceResult =
            match sourceItemKind with
            | PT.ItemKind.Fn ->
              processItem
                (fnContext branchChain)
                toSourceUUID
                newSourceUuid
                fullMapping
            | PT.ItemKind.Type ->
              processItem
                (typeContext branchChain)
                toSourceUUID
                newSourceUuid
                fullMapping
            | PT.ItemKind.Value ->
              processItem
                (valueContext branchChain)
                toSourceUUID
                newSourceUuid
                fullMapping

          match sourceResult with
          | Error e -> return Error e
          | Ok(sourceRepoint, sourceOps) ->
            return
              Ok(
                sourceRepoint :: dependentRepoints,
                flatDependentOps @ sourceOps,
                newSourceUuid
              )
        | None -> return Ok(dependentRepoints, flatDependentOps, toSourceUUID)
  }


/// Propagates an update to all dependents (including transitive).
/// Returns None if no dependents, or Some(result, ops) if propagation occurred.
/// Entry point for the entire propagation process. Called after a package item is updated.
let propagate
  (branchId : PT.BranchId)
  (sourceLocation : PT.PackageLocation)
  (sourceItemKind : PT.ItemKind)
  (fromSourceUUIDs : List<uuid>)
  (toSourceUUID : uuid)
  : Task<Result<Option<PropagationResult * List<PT.PackageOp>>, string>> =
  task {
    // Fetch branch chain once for all queries
    let! branchChain = Branches.getBranchChain branchId

    // Phase 1: Discover all items that need updating
    let! dependents = discoverDependents branchChain fromSourceUUIDs toSourceUUID

    match dependents with
    | [] -> return Ok None
    | _ ->
      // Phase 2 & 3: Build mapping and create all items
      let! result =
        createAllItems
          branchChain
          fromSourceUUIDs
          toSourceUUID
          dependents
          sourceItemKind

      match result with
      | Error err -> return Error err
      | Ok(repoints, ops, finalToSourceUUID) ->
        let propagationId = System.Guid.NewGuid()

        let propagateOp =
          PT.PackageOp.PropagateUpdate(
            propagationId,
            sourceLocation,
            sourceItemKind,
            fromSourceUUIDs,
            finalToSourceUUID,
            repoints
          )

        let allOps = ops @ [ propagateOp ]
        let result = { propagationId = propagationId; repoints = repoints }
        return Ok(Some(result, allOps))
  }
