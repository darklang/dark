module LibPackageManager.PackageManager

open Prelude

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


let pt : PT.PackageManager =
  { findType = withCache PMPT.Type.find
    findValue = withCache PMPT.Value.find
    findFn = withCache PMPT.Fn.find

    getType = withCache PMPT.Type.get
    getFn = withCache PMPT.Fn.get
    getValue = withCache PMPT.Value.get

    getTypeLocation = withCache PMPT.Type.getLocation
    getValueLocation = withCache PMPT.Value.getLocation
    getFnLocation = withCache PMPT.Fn.getLocation

    search = PMPT.search

    init = uply { return () } }


/// Create an in-memory PackageManager from a list of PackageOps.
/// This builds internal maps by applying each op sequentially.
/// Used for transient state during parsing, testing, etc.
let createInMemory (ops : List<PT.PackageOp>) : PT.PackageManager =
  // Build maps by applying each op
  let types = ResizeArray<PT.PackageType.PackageType>()
  let values = ResizeArray<PT.PackageValue.PackageValue>()
  let fns = ResizeArray<PT.PackageFn.PackageFn>()
  let typeLocations = ResizeArray<PT.PackageLocation * uuid>()
  let valueLocations = ResizeArray<PT.PackageLocation * uuid>()
  let fnLocations = ResizeArray<PT.PackageLocation * uuid>()

  for op in ops do
    match op with
    | PT.PackageOp.AddType t -> types.Add(t)
    | PT.PackageOp.SetTypeName(id, loc) -> typeLocations.Add(loc, id)
    | PT.PackageOp.AddValue v -> values.Add(v)
    | PT.PackageOp.SetValueName(id, loc) -> valueLocations.Add(loc, id)
    | PT.PackageOp.AddFn f -> fns.Add(f)
    | PT.PackageOp.SetFnName(id, loc) -> fnLocations.Add(loc, id)

  // Convert to immutable maps for efficient lookup
  let typeMap = types |> Seq.map (fun t -> t.id, t) |> Map.ofSeq
  let valueMap = values |> Seq.map (fun v -> v.id, v) |> Map.ofSeq
  let fnMap = fns |> Seq.map (fun f -> f.id, f) |> Map.ofSeq
  let typeLocMap = Map.ofSeq typeLocations
  let valueLocMap = Map.ofSeq valueLocations
  let fnLocMap = Map.ofSeq fnLocations

  // Build reverse maps (id → location)
  let typeIdToLoc = typeLocations |> Seq.map (fun (loc, id) -> id, loc) |> Map.ofSeq
  let valueIdToLoc =
    valueLocations |> Seq.map (fun (loc, id) -> id, loc) |> Map.ofSeq
  let fnIdToLoc = fnLocations |> Seq.map (fun (loc, id) -> id, loc) |> Map.ofSeq

  { findType = fun loc -> Ply(Map.tryFind loc typeLocMap)
    findValue = fun loc -> Ply(Map.tryFind loc valueLocMap)
    findFn = fun loc -> Ply(Map.tryFind loc fnLocMap)

    getType = fun id -> Ply(Map.tryFind id typeMap)
    getValue = fun id -> Ply(Map.tryFind id valueMap)
    getFn = fun id -> Ply(Map.tryFind id fnMap)

    getTypeLocation = fun id -> Ply(Map.tryFind id typeIdToLoc)
    getValueLocation = fun id -> Ply(Map.tryFind id valueIdToLoc)
    getFnLocation = fun id -> Ply(Map.tryFind id fnIdToLoc)

    // no need to support this for in-memory.
    search =
      fun _query ->
        // Simple in-memory search - just return all items with their locations
        // Could implement proper filtering if needed
        let typesWithLocs =
          types
          |> Seq.toList
          |> List.choose (fun t ->
            match Map.tryFind t.id typeIdToLoc with
            | Some loc ->
              Option.Some({ entity = t; location = loc } : PT.LocatedItem<_>)
            | None -> Option.None)

        let valuesWithLocs =
          values
          |> Seq.toList
          |> List.choose (fun v ->
            match Map.tryFind v.id valueIdToLoc with
            | Some loc ->
              Option.Some({ entity = v; location = loc } : PT.LocatedItem<_>)
            | None -> Option.None)

        let fnsWithLocs =
          fns
          |> Seq.toList
          |> List.choose (fun f ->
            match Map.tryFind f.id fnIdToLoc with
            | Some loc ->
              Option.Some({ entity = f; location = loc } : PT.LocatedItem<_>)
            | None -> Option.None)

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
      fun loc ->
        uply {
          match! overlay.findType loc with
          | Some id -> return Some id
          | None -> return! fallback.findType loc
        }

    findValue =
      fun loc ->
        uply {
          match! overlay.findValue loc with
          | Some id -> return Some id
          | None -> return! fallback.findValue loc
        }

    findFn =
      fun loc ->
        uply {
          match! overlay.findFn loc with
          | Some id -> return Some id
          | None -> return! fallback.findFn loc
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

    getTypeLocation =
      fun id ->
        uply {
          match! overlay.getTypeLocation id with
          | Some loc -> return Some loc
          | None -> return! fallback.getTypeLocation id
        }

    getValueLocation =
      fun id ->
        uply {
          match! overlay.getValueLocation id with
          | Some loc -> return Some loc
          | None -> return! fallback.getValueLocation id
        }

    getFnLocation =
      fun id ->
        uply {
          match! overlay.getFnLocation id with
          | Some loc -> return Some loc
          | None -> return! fallback.getFnLocation id
        }

    search =
      fun query ->
        uply {
          // Combine search results from both
          let! overlayResults = overlay.search query
          let! fallbackResults = fallback.search query

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


// TODO can we (somehow) abstract the algorithm of: phase 1, id stabilization, reparse for phase 2

/// Stabilize IDs in ops by matching them against a reference PackageManager
/// Used during two-phase parsing to ensure IDs from second pass match first pass
let stabilizeOpsAgainstPM
  (referencePM : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : Ply<List<PT.PackageOp>> =
  uply {
    let mutable result = []
    for op in List.rev ops do
      let! stabilizedOp =
        uply {
          match op with
          | PT.PackageOp.SetTypeName(_, loc) ->
            // Look up stable ID from reference PM
            let! stableIdOpt = referencePM.findType loc
            let stableId =
              stableIdOpt |> Option.defaultWith (fun () -> System.Guid.NewGuid())
            return PT.PackageOp.SetTypeName(stableId, loc)

          | PT.PackageOp.AddType typ ->
            // Find location for this type in current ops
            let typLoc =
              ops
              |> List.tryPick (function
                | PT.PackageOp.SetTypeName(id, loc) when id = typ.id -> Some loc
                | _ -> None)
            // Look up stable ID from reference PM using that location
            let! stableIdOpt =
              match typLoc with
              | Some loc -> referencePM.findType loc
              | None -> Ply(None)
            let stableId = stableIdOpt |> Option.defaultValue typ.id
            return PT.PackageOp.AddType { typ with id = stableId }

          | PT.PackageOp.SetValueName(_, loc) ->
            let! stableIdOpt = referencePM.findValue loc
            let stableId =
              stableIdOpt |> Option.defaultWith (fun () -> System.Guid.NewGuid())
            return PT.PackageOp.SetValueName(stableId, loc)

          | PT.PackageOp.AddValue value ->
            let valueLoc =
              ops
              |> List.tryPick (function
                | PT.PackageOp.SetValueName(id, loc) when id = value.id -> Some loc
                | _ -> None)
            let! stableIdOpt =
              match valueLoc with
              | Some loc -> referencePM.findValue loc
              | None -> Ply(None)
            let stableId = stableIdOpt |> Option.defaultValue value.id
            return PT.PackageOp.AddValue { value with id = stableId }

          | PT.PackageOp.SetFnName(_, loc) ->
            let! stableIdOpt = referencePM.findFn loc
            let stableId =
              stableIdOpt |> Option.defaultWith (fun () -> System.Guid.NewGuid())
            return PT.PackageOp.SetFnName(stableId, loc)

          | PT.PackageOp.AddFn fn ->
            let fnLoc =
              ops
              |> List.tryPick (function
                | PT.PackageOp.SetFnName(id, loc) when id = fn.id -> Some loc
                | _ -> None)
            let! stableIdOpt =
              match fnLoc with
              | Some loc -> referencePM.findFn loc
              | None -> Ply(None)
            let stableId = stableIdOpt |> Option.defaultValue fn.id
            return PT.PackageOp.AddFn { fn with id = stableId }
        }
      result <- stabilizedOp :: result
    return result
  }


/// Create an in-memory PackageManager from PackageOps
/// (at time of writing, only really useful for tests and from-disk parsing)
let withExtraOps
  (basePM : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : PT.PackageManager =
  let opsPM = createInMemory ops
  combine opsPM basePM
