module LibPackageManager.PackageManager

open Prelude
open LibExecution.ProgramTypes

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibPackageManager.Caching

module PMPT = ProgramTypes
module PMRT = RuntimeTypes


// TODO: bring back eager loading
let rt : RT.PackageManager =
  { getType = withCache PMRT.Type.get
    getFn = withCache PMRT.Fn.get
    getValue = withCache PMRT.Value.get

    init =
      uply {
        //eagerLoad
        return ()
      } }


/// Create a PT PackageManager.
/// Branch is passed per-lookup, not at construction time.
let pt : PT.PackageManager =
  let getBranchChain branchId =
    Branches.getBranchChain branchId |> Async.AwaitTask |> Async.RunSynchronously

  { findType =
      fun (branchId, location) ->
        let chain = getBranchChain branchId
        withCache (PMPT.Type.find chain) location
    findValue =
      fun (branchId, location) ->
        let chain = getBranchChain branchId
        withCache (PMPT.Value.find chain) location
    findFn =
      fun (branchId, location) ->
        let chain = getBranchChain branchId
        withCache (PMPT.Fn.find chain) location

    getType = withCache PMPT.Type.get
    getFn = withCache PMPT.Fn.get
    getValue = withCache PMPT.Value.get

    getTypeLocations =
      fun branchId id ->
        let chain = getBranchChain branchId
        PMPT.Type.getLocations chain id
    getValueLocations =
      fun branchId id ->
        let chain = getBranchChain branchId
        PMPT.Value.getLocations chain id
    getFnLocations =
      fun branchId id ->
        let chain = getBranchChain branchId
        PMPT.Fn.getLocations chain id

    search =
      fun (branchId, query) ->
        let chain = getBranchChain branchId
        PMPT.search chain query

    init = uply { return () } }


/// Create an in-memory PackageManager from a list of PackageOps.
/// This builds internal maps by applying each op sequentially.
/// Used for transient state during parsing, testing, etc.
let createInMemory (ops : List<PT.PackageOp>) : PT.PackageManager =
  // Build location maps by applying each op
  let typeLocations = ResizeArray<PT.PackageLocation * ContentHash>()
  let valueLocations = ResizeArray<PT.PackageLocation * ContentHash>()
  let fnLocations = ResizeArray<PT.PackageLocation * ContentHash>()

  for op in ops do
    match op with
    | PT.PackageOp.SetTypeName(hash, loc) -> typeLocations.Add(loc, hash)
    | PT.PackageOp.SetValueName(hash, loc) -> valueLocations.Add(loc, hash)
    | PT.PackageOp.SetFnName(hash, loc) -> fnLocations.Add(loc, hash)
    | PT.PackageOp.AddType _
    | PT.PackageOp.AddValue _
    | PT.PackageOp.AddFn _ -> ()

    // After propagation, dependents have new hashes.
    // For each repoint, update the location to point to toHash (the new version)
    | PT.PackageOp.PropagateUpdate(_, _, _, _, _, repoints) ->
      for repoint in repoints do
        match repoint.itemKind with
        | PT.ItemKind.Type -> typeLocations.Add(repoint.location, repoint.toHash)
        | PT.ItemKind.Value -> valueLocations.Add(repoint.location, repoint.toHash)
        | PT.ItemKind.Fn -> fnLocations.Add(repoint.location, repoint.toHash)

    // For each repoint, point the location back to fromHash (the old version).
    // Then also restore the source item's location to its pre-propagation hash
    | PT.PackageOp.RevertPropagation(_,
                                     _,
                                     sourceLocation,
                                     sourceItemKind,
                                     restoredSourceHash,
                                     revertedRepoints) ->
      // Reverse the repoints: locations go back to fromHash
      for repoint in revertedRepoints do
        match repoint.itemKind with
        | PT.ItemKind.Type -> typeLocations.Add(repoint.location, repoint.fromHash)
        | PT.ItemKind.Value -> valueLocations.Add(repoint.location, repoint.fromHash)
        | PT.ItemKind.Fn -> fnLocations.Add(repoint.location, repoint.fromHash)
      // Restore source location to committed hash
      match sourceItemKind with
      | PT.ItemKind.Type -> typeLocations.Add(sourceLocation, restoredSourceHash)
      | PT.ItemKind.Value -> valueLocations.Add(sourceLocation, restoredSourceHash)
      | PT.ItemKind.Fn -> fnLocations.Add(sourceLocation, restoredSourceHash)

  // Convert to immutable maps for efficient lookup.
  // All items (types, fns, values) are keyed by their content hash.
  // The ops contain Add*(item) followed by Set*Name(hash, loc);
  // we pair them to build ContentHash -> item maps.
  let typeMap =
    let mutable map = Map.empty<ContentHash, PT.PackageType.PackageType>
    let mutable pendingType : Option<PT.PackageType.PackageType> = None
    for op in ops do
      match op with
      | PT.PackageOp.AddType t -> pendingType <- Some t
      | PT.PackageOp.SetTypeName(hash, _loc) ->
        match pendingType with
        | Some t ->
          map <- Map.add hash { t with hash = hash } map
          pendingType <- None
        | None -> ()
      | _ -> ()
    map

  let fnMap =
    let mutable map = Map.empty<ContentHash, PT.PackageFn.PackageFn>
    let mutable pendingFn : Option<PT.PackageFn.PackageFn> = None
    for op in ops do
      match op with
      | PT.PackageOp.AddFn f -> pendingFn <- Some f
      | PT.PackageOp.SetFnName(hash, _loc) ->
        match pendingFn with
        | Some f ->
          map <- Map.add hash { f with hash = hash } map
          pendingFn <- None
        | None -> ()
      | _ -> ()
    map

  let valueMap =
    let mutable map = Map.empty<ContentHash, PT.PackageValue.PackageValue>
    let mutable pendingValue : Option<PT.PackageValue.PackageValue> = None
    for op in ops do
      match op with
      | PT.PackageOp.AddValue v -> pendingValue <- Some v
      | PT.PackageOp.SetValueName(hash, _loc) ->
        match pendingValue with
        | Some v ->
          map <- Map.add hash { v with hash = hash } map
          pendingValue <- None
        | None -> ()
      | _ -> ()
    map

  let typeLocMap = Map.ofSeq typeLocations
  let valueLocMap = Map.ofSeq valueLocations
  let fnLocMap = Map.ofSeq fnLocations

  // Build reverse multi-maps (id → all locations)
  let typeIdToLocs =
    typeLocations
    |> Seq.fold
      (fun acc (loc, id) ->
        let existing = Map.tryFind id acc |> Option.defaultValue []
        Map.add id (loc :: existing) acc)
      Map.empty
  let valueIdToLocs =
    valueLocations
    |> Seq.fold
      (fun acc (loc, id) ->
        let existing = Map.tryFind id acc |> Option.defaultValue []
        Map.add id (loc :: existing) acc)
      Map.empty
  let fnIdToLocs =
    fnLocations
    |> Seq.fold
      (fun acc (loc, id) ->
        let existing = Map.tryFind id acc |> Option.defaultValue []
        Map.add id (loc :: existing) acc)
      Map.empty

  { findType = fun (_, loc) -> Ply(Map.tryFind loc typeLocMap)
    findValue = fun (_, loc) -> Ply(Map.tryFind loc valueLocMap)
    findFn = fun (_, loc) -> Ply(Map.tryFind loc fnLocMap)

    getType = fun id -> Ply(Map.tryFind id typeMap)
    getValue = fun id -> Ply(Map.tryFind id valueMap)
    getFn = fun id -> Ply(Map.tryFind id fnMap)

    getTypeLocations =
      fun _branchId id -> Ply(Map.tryFind id typeIdToLocs |> Option.defaultValue [])
    getValueLocations =
      fun _branchId id -> Ply(Map.tryFind id valueIdToLocs |> Option.defaultValue [])
    getFnLocations =
      fun _branchId id -> Ply(Map.tryFind id fnIdToLocs |> Option.defaultValue [])

    // no need to support this for in-memory.
    search =
      fun (_, _query) ->
        // Simple in-memory search - just return all items with their locations
        // Could implement proper filtering if needed
        let typesWithLocs =
          typeMap
          |> Map.toList
          |> List.choose (fun (hash, t) ->
            match Map.tryFind hash typeIdToLocs |> Option.defaultValue [] with
            | loc :: _ ->
              Option.Some({ entity = t; location = loc } : PT.LocatedItem<_>)
            | [] -> Option.None)

        let valuesWithLocs =
          valueMap
          |> Map.toList
          |> List.choose (fun (hash, v) ->
            match Map.tryFind hash valueIdToLocs |> Option.defaultValue [] with
            | loc :: _ ->
              Option.Some({ entity = v; location = loc } : PT.LocatedItem<_>)
            | [] -> Option.None)

        let fnsWithLocs =
          fnMap
          |> Map.toList
          |> List.choose (fun (hash, f) ->
            match Map.tryFind hash fnIdToLocs |> Option.defaultValue [] with
            | loc :: _ ->
              Option.Some({ entity = f; location = loc } : PT.LocatedItem<_>)
            | [] -> Option.None)

        Ply
          { PT.Search.SearchResults.submodules = []
            types = typesWithLocs
            values = valuesWithLocs
            fns = fnsWithLocs }

    init = uply { return () } }


/// Combine two PackageManagers: check `overlay` first, then fall back to `fallback`.
/// This is used to layer transient/uncommitted definitions on top of persistent ones.
let combine
  (overlay : PT.PackageManager)
  (fallback : PT.PackageManager)
  : PT.PackageManager =
  { findType =
      fun (branchId, loc) ->
        uply {
          match! overlay.findType (branchId, loc) with
          | Some id -> return Some id
          | None -> return! fallback.findType (branchId, loc)
        }

    findValue =
      fun (branchId, loc) ->
        uply {
          match! overlay.findValue (branchId, loc) with
          | Some id -> return Some id
          | None -> return! fallback.findValue (branchId, loc)
        }

    findFn =
      fun (branchId, loc) ->
        uply {
          match! overlay.findFn (branchId, loc) with
          | Some id -> return Some id
          | None -> return! fallback.findFn (branchId, loc)
        }

    getType =
      fun id ->
        uply {
          match! overlay.getType id with
          | Some t -> return Some t
          | None -> return! fallback.getType id
        }

    getValue =
      fun id ->
        uply {
          match! overlay.getValue id with
          | Some v -> return Some v
          | None -> return! fallback.getValue id
        }

    getFn =
      fun id ->
        uply {
          match! overlay.getFn id with
          | Some f -> return Some f
          | None -> return! fallback.getFn id
        }

    getTypeLocations =
      fun branchId id ->
        uply {
          let! overlayLocs = overlay.getTypeLocations branchId id
          let! fallbackLocs = fallback.getTypeLocations branchId id
          return overlayLocs @ fallbackLocs
        }

    getValueLocations =
      fun branchId id ->
        uply {
          let! overlayLocs = overlay.getValueLocations branchId id
          let! fallbackLocs = fallback.getValueLocations branchId id
          return overlayLocs @ fallbackLocs
        }

    getFnLocations =
      fun branchId id ->
        uply {
          let! overlayLocs = overlay.getFnLocations branchId id
          let! fallbackLocs = fallback.getFnLocations branchId id
          return overlayLocs @ fallbackLocs
        }

    search =
      fun (branchId, query) ->
        uply {
          // Combine search results from both
          let! overlayResults = overlay.search (branchId, query)
          let! fallbackResults = fallback.search (branchId, query)

          return
            { PT.Search.SearchResults.submodules =
                List.append overlayResults.submodules fallbackResults.submodules
              types = List.append overlayResults.types fallbackResults.types
              values = List.append overlayResults.values fallbackResults.values
              fns = List.append overlayResults.fns fallbackResults.fns }
        }

    init =
      uply {
        do! overlay.init
        do! fallback.init
      } }


/// Create an in-memory PackageManager from PackageOps
/// (at time of writing, only really useful for tests and from-disk parsing)
let withExtraOps
  (basePM : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : PT.PackageManager =
  let opsPM = createInMemory ops
  combine opsPM basePM
