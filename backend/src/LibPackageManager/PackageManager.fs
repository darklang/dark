module LibPackageManager.PackageManager

open Prelude
open LibExecution.ProgramTypes

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibPackageManager.Caching

module PMPT = ProgramTypes
module PMRT = RuntimeTypes


// Per-branch cache of Harmful fn hashes (as underlying hex strings —
// PT.Hash and RT.Hash are distinct CLR types; storing strings avoids
// threading either wrapper through the cache layer).
//
// First call per branch loads the Set from the `deprecations` table;
// subsequent calls are O(1) lookups. Long-lived processes that mutate
// deprecation state on a branch (LSP, cloud) should invalidate by
// evicting the branch's entry — not implemented yet; short-lived CLIs
// rebuild the PM per invocation so they don't care.
let private harmfulCache =
  System.Collections.Concurrent.ConcurrentDictionary<PT.BranchId, Set<string>>()

let private loadHarmfulForBranch (branchId : PT.BranchId) : Set<string> =
  match harmfulCache.TryGetValue branchId with
  | true, cached -> cached
  | false, _ ->
    let branchChain =
      Branches.getBranchChain branchId |> Async.AwaitTask |> Async.RunSynchronously
    let harmful =
      Queries.getHarmfulFnHashes branchChain
      |> Async.AwaitTask
      |> Async.RunSynchronously
      |> Set.map (fun (PT.Hash h) -> h)
    harmfulCache[branchId] <- harmful
    harmful


// TODO: bring back eager loading
let rt : RT.PackageManager =
  { getType = withCache PMRT.Type.get
    getFn = withCache PMRT.Fn.get
    getValue = withCache PMRT.Value.get
    getBlob = PMRT.Blob.get

    isHarmful =
      fun branchId (RT.Hash h) -> Ply(Set.contains h (loadHarmfulForBranch branchId))

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
  let typeLocations = ResizeArray<PT.PackageLocation * Hash>()
  let valueLocations = ResizeArray<PT.PackageLocation * Hash>()
  let fnLocations = ResizeArray<PT.PackageLocation * Hash>()

  for op in ops do
    match op with
    | PT.PackageOp.SetName(loc, target) ->
      match target with
      | PT.PackageType h -> typeLocations.Add(loc, h)
      | PT.PackageValue h -> valueLocations.Add(loc, h)
      | PT.PackageFn h -> fnLocations.Add(loc, h)
    | PT.PackageOp.AddType _
    | PT.PackageOp.AddValue _
    | PT.PackageOp.AddFn _ -> ()

    // Deprecations don't affect in-memory location maps.
    | PT.PackageOp.Deprecate _
    | PT.PackageOp.Undeprecate _ -> ()

    // After propagation, dependents have new hashes.
    // For each repoint, update the location to point to toRef (the new version)
    | PT.PackageOp.PropagateUpdate(_, _, _, _, repoints) ->
      for repoint in repoints do
        match repoint.toRef with
        | PT.PackageType h -> typeLocations.Add(repoint.location, h)
        | PT.PackageValue h -> valueLocations.Add(repoint.location, h)
        | PT.PackageFn h -> fnLocations.Add(repoint.location, h)

    // For each repoint, point the location back to fromRef (the old version).
    // Then also restore the source item's location to its pre-propagation hash
    | PT.PackageOp.RevertPropagation(_,
                                     _,
                                     sourceLocation,
                                     restoredSourceRef,
                                     revertedRepoints) ->
      // Reverse the repoints: locations go back to fromRef
      for repoint in revertedRepoints do
        match repoint.fromRef with
        | PT.PackageType h -> typeLocations.Add(repoint.location, h)
        | PT.PackageValue h -> valueLocations.Add(repoint.location, h)
        | PT.PackageFn h -> fnLocations.Add(repoint.location, h)
      // Restore source location to committed hash
      match restoredSourceRef with
      | PT.PackageType h -> typeLocations.Add(sourceLocation, h)
      | PT.PackageValue h -> valueLocations.Add(sourceLocation, h)
      | PT.PackageFn h -> fnLocations.Add(sourceLocation, h)

  // Convert to immutable maps for efficient lookup.
  // All items (types, fns, values) are keyed by their hash.
  // The ops contain Add*(item) followed by Set*Name(hash, loc);
  // we pair them to build Hash -> item maps.
  let typeMap =
    let mutable map = Map.empty<Hash, PT.PackageType.PackageType>
    let mutable pendingType : Option<PT.PackageType.PackageType> = None
    for op in ops do
      match op with
      | PT.PackageOp.AddType t -> pendingType <- Some t
      | PT.PackageOp.SetName(_, PT.PackageType hash) ->
        match pendingType with
        | Some t ->
          map <- Map.add hash { t with hash = hash } map
          pendingType <- None
        | None -> ()
      | _ -> ()
    map

  let fnMap =
    let mutable map = Map.empty<Hash, PT.PackageFn.PackageFn>
    let mutable pendingFn : Option<PT.PackageFn.PackageFn> = None
    for op in ops do
      match op with
      | PT.PackageOp.AddFn f -> pendingFn <- Some f
      | PT.PackageOp.SetName(_, PT.PackageFn hash) ->
        match pendingFn with
        | Some f ->
          map <- Map.add hash { f with hash = hash } map
          pendingFn <- None
        | None -> ()
      | _ -> ()
    map

  let valueMap =
    let mutable map = Map.empty<Hash, PT.PackageValue.PackageValue>
    let mutable pendingValue : Option<PT.PackageValue.PackageValue> = None
    for op in ops do
      match op with
      | PT.PackageOp.AddValue v -> pendingValue <- Some v
      | PT.PackageOp.SetName(_, PT.PackageValue hash) ->
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
