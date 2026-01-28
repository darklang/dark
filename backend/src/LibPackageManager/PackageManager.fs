module LibPackageManager.PackageManager

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibPackageManager.Caching

module PMPT = ProgramTypes
module PMRT = RuntimeTypes
module SC = SignatureComparer


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

    | PT.PackageOp.ApproveItem _ -> ()
    | PT.PackageOp.RejectItem _ -> ()
    | PT.PackageOp.RequestNamingApproval _ -> ()
    | PT.PackageOp.WithdrawApprovalRequest _ -> ()
    | PT.PackageOp.RequestChanges _ -> ()

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

  { findType = fun (_accountId, _branchID, loc) -> Ply(Map.tryFind loc typeLocMap)
    findValue = fun (_accountId, _branchID, loc) -> Ply(Map.tryFind loc valueLocMap)
    findFn = fun (_accountId, _branchID, loc) -> Ply(Map.tryFind loc fnLocMap)

    getType = fun id -> Ply(Map.tryFind id typeMap)
    getValue = fun id -> Ply(Map.tryFind id valueMap)
    getFn = fun id -> Ply(Map.tryFind id fnMap)

    getTypeLocation =
      fun (_accountId, _branchID, id) -> Ply(Map.tryFind id typeIdToLoc)
    getValueLocation =
      fun (_accountId, _branchID, id) -> Ply(Map.tryFind id valueIdToLoc)
    getFnLocation = fun (_accountId, _branchID, id) -> Ply(Map.tryFind id fnIdToLoc)

    // no need to support this for in-memory.
    search =
      fun (_accountId, _branchID, _query) ->
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
      fun (accountID, branchID, loc) ->
        uply {
          match! overlay.findType (accountID, branchID, loc) with
          | Some id -> return Some id
          | None -> return! fallback.findType (accountID, branchID, loc)
        }

    findValue =
      fun (accountID, branchID, loc) ->
        uply {
          match! overlay.findValue (accountID, branchID, loc) with
          | Some id -> return Some id
          | None -> return! fallback.findValue (accountID, branchID, loc)
        }

    findFn =
      fun (accountID, branchID, loc) ->
        uply {
          match! overlay.findFn (accountID, branchID, loc) with
          | Some id -> return Some id
          | None -> return! fallback.findFn (accountID, branchID, loc)
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
      fun (accountID, branchID, id) ->
        uply {
          match! overlay.getTypeLocation (accountID, branchID, id) with
          | Some loc -> return Some loc
          | None -> return! fallback.getTypeLocation (accountID, branchID, id)
        }

    getValueLocation =
      fun (accountID, branchID, id) ->
        uply {
          match! overlay.getValueLocation (accountID, branchID, id) with
          | Some loc -> return Some loc
          | None -> return! fallback.getValueLocation (accountID, branchID, id)
        }

    getFnLocation =
      fun (accountID, branchID, id) ->
        uply {
          match! overlay.getFnLocation (accountID, branchID, id) with
          | Some loc -> return Some loc
          | None -> return! fallback.getFnLocation (accountID, branchID, id)
        }

    search =
      fun (accountID, branchID, query) ->
        uply {
          // Combine search results from both
          let! overlayResults = overlay.search (accountID, branchID, query)
          let! fallbackResults = fallback.search (accountID, branchID, query)

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
    let accountID : Option<PT.AccountID> = None
    for op in List.rev ops do
      let! stabilizedOp =
        uply {
          match op with
          | PT.PackageOp.SetTypeName(_, loc) ->
            // Look up stable ID from reference PM
            let! stableIdOpt = referencePM.findType (accountID, None, loc)
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
              | Some loc -> referencePM.findType (accountID, None, loc)
              | None -> Ply(None)
            let stableId = stableIdOpt |> Option.defaultValue typ.id
            return PT.PackageOp.AddType { typ with id = stableId }

          | PT.PackageOp.SetValueName(_, loc) ->
            let! stableIdOpt = referencePM.findValue (accountID, None, loc)
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
              | Some loc -> referencePM.findValue (accountID, None, loc)
              | None -> Ply(None)
            let stableId = stableIdOpt |> Option.defaultValue value.id
            return PT.PackageOp.AddValue { value with id = stableId }

          | PT.PackageOp.SetFnName(_, loc) ->
            let! stableIdOpt = referencePM.findFn (accountID, None, loc)
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
              | Some loc -> referencePM.findFn (accountID, None, loc)
              | None -> Ply(None)
            let stableId = stableIdOpt |> Option.defaultValue fn.id
            return PT.PackageOp.AddFn { fn with id = stableId }

          | PT.PackageOp.ApproveItem _ -> return op
          | PT.PackageOp.RejectItem _ -> return op
          | PT.PackageOp.RequestNamingApproval _ -> return op
          | PT.PackageOp.WithdrawApprovalRequest _ -> return op
          | PT.PackageOp.RequestChanges _ -> return op
        }
      result <- stabilizedOp :: result
    return result
  }


/// Stabilize IDs with edit detection - used for incremental changes against persistent PM.
/// Follows the immutability model: EVERY edit creates a new UUID, the name then points to
/// the new UUID. Old versions still exist.
///
/// Key behavior: Compares against the LAST APPROVED version, not the current (possibly pending) version.
/// This ensures that reverting to the approved state reuses the approved UUID (no new pending version).
///
/// Visibility rules (from deps-guide.md):
/// - Same branch, same owner: compatible changes auto-propagate, breaking = todos
/// - Different branch/owner: stay pinned until explicit upgrade
///
/// Parameters:
/// - referencePM: The package manager to look up existing items
/// - accountID: The user making the edit (for visibility filtering)
/// - branchID: The branch the edit is on (for visibility filtering)
/// - ops: The package operations to stabilize
///
/// Returns: (stabilized ops, detected edits)
/// - If identical to approved: reuse approved UUID (no edit)
/// - If different: new UUID, EditInfo records whether breaking (for todo creation)
let stabilizeOpsWithEditDetection
  (referencePM : PT.PackageManager)
  (_accountID : Option<PT.AccountID>)
  (branchID : Option<uuid>)
  (ops : List<PT.PackageOp>)
  : Ply<List<PT.PackageOp> * List<EditPropagation.EditInfo>> =
  uply {
    // PASS 1: Build the complete ID mapping (fresh ID -> stable ID) for ALL items
    // This pass does NOT compare bodies - it just establishes which fresh IDs map to which stable IDs.
    // IMPORTANT: We look up the LAST APPROVED version, not the current (possibly pending) version.
    // This ensures that reverting to the approved state reuses the approved UUID.
    let idMapping = System.Collections.Generic.Dictionary<uuid, uuid>()

    // Helper to look up the last APPROVED ID for a location (not pending)
    let lookupApprovedTypeId (typLoc : Option<PT.PackageLocation>) =
      uply {
        match typLoc with
        | Some loc ->
          let modulesStr = String.concat "." loc.modules
          return! Queries.getLastApprovedType branchID loc.owner modulesStr loc.name
        | None -> return None
      }

    let lookupApprovedValueId (valueLoc : Option<PT.PackageLocation>) =
      uply {
        match valueLoc with
        | Some loc ->
          let modulesStr = String.concat "." loc.modules
          return! Queries.getLastApprovedValue branchID loc.owner modulesStr loc.name
        | None -> return None
      }

    let lookupApprovedFnId (fnLoc : Option<PT.PackageLocation>) =
      uply {
        match fnLoc with
        | Some loc ->
          let modulesStr = String.concat "." loc.modules
          return! Queries.getLastApprovedFn branchID loc.owner modulesStr loc.name
        | None -> return None
      }

    // Build ID mapping for all items using LAST APPROVED versions
    do!
      ops
      |> Ply.List.iterSequentially (fun op ->
        uply {
          match op with
          | PT.PackageOp.AddType typ ->
            let typLoc =
              ops
              |> List.tryPick (function
                | PT.PackageOp.SetTypeName(id, loc) when id = typ.id -> Some loc
                | _ -> None)
            let! approvedIdOpt = lookupApprovedTypeId typLoc
            match approvedIdOpt with
            | Some approvedId -> idMapping[typ.id] <- approvedId
            | None -> idMapping[typ.id] <- typ.id

          | PT.PackageOp.AddValue value ->
            let valueLoc =
              ops
              |> List.tryPick (function
                | PT.PackageOp.SetValueName(id, loc) when id = value.id -> Some loc
                | _ -> None)
            let! approvedIdOpt = lookupApprovedValueId valueLoc
            match approvedIdOpt with
            | Some approvedId -> idMapping[value.id] <- approvedId
            | None -> idMapping[value.id] <- value.id

          | PT.PackageOp.AddFn fn ->
            let fnLoc =
              ops
              |> List.tryPick (function
                | PT.PackageOp.SetFnName(id, loc) when id = fn.id -> Some loc
                | _ -> None)
            let! approvedIdOpt = lookupApprovedFnId fnLoc
            match approvedIdOpt with
            | Some approvedId -> idMapping[fn.id] <- approvedId
            | None -> idMapping[fn.id] <- fn.id

          | _ -> ()
        })

    // PASS 1.5: Collect all UUIDs referenced in function/value bodies and add mappings
    // for any stable (non-fresh) UUIDs that have approved versions.
    // This handles the case where a body references a pending UUID from a different file.
    let! () =
      uply {
        // Collect all UUIDs from bodies
        let allReferencedUuids =
          ops
          |> List.collect (fun op ->
            match op with
            | PT.PackageOp.AddFn fn -> SC.collectReferencedUuids fn.body |> Set.toList
            | PT.PackageOp.AddValue value -> SC.collectReferencedUuids value.body |> Set.toList
            | _ -> [])
          |> Set.ofList

        // For each UUID not already in idMapping, look up if there's an approved version
        for uuid in allReferencedUuids do
          if not (idMapping.ContainsKey(uuid)) then
            // This UUID is from the database (not a fresh parsing UUID)
            // Look up its location to find the approved version
            let! fnLocOpt = Queries.getLocationByItemId uuid "fn"
            match fnLocOpt with
            | Some (owner, modules, name) ->
              let! approvedIdOpt = Queries.getLastApprovedFn branchID owner modules name
              match approvedIdOpt with
              | Some approvedId when approvedId <> uuid ->
                // Map pending UUID to approved UUID
                idMapping[uuid] <- approvedId
              | _ -> ()
            | None ->
              // Try as value
              let! valueLocOpt = Queries.getLocationByItemId uuid "value"
              match valueLocOpt with
              | Some (owner, modules, name) ->
                let! approvedIdOpt = Queries.getLastApprovedValue branchID owner modules name
                match approvedIdOpt with
                | Some approvedId when approvedId <> uuid ->
                  idMapping[uuid] <- approvedId
                | _ -> ()
              | None ->
                // Try as type
                let! typeLocOpt = Queries.getLocationByItemId uuid "type"
                match typeLocOpt with
                | Some (owner, modules, name) ->
                  let! approvedIdOpt = Queries.getLastApprovedType branchID owner modules name
                  match approvedIdOpt with
                  | Some approvedId when approvedId <> uuid ->
                    idMapping[uuid] <- approvedId
                  | _ -> ()
                | None -> ()
      }

    // PASS 2: Now that we have the complete mapping, compare bodies and detect edits.
    // For items that map to approved items, we compare content using the mapping
    // to normalize UUIDs in the new item's body.
    // If identical to approved: skip the op entirely (true no-op, revert scenario)
    // If different: create new UUID and record the edit
    let mutable edits : List<EditPropagation.EditInfo> = []
    // Track IDs that are identical to approved (should be filtered out)
    let identicalToApproved = System.Collections.Generic.HashSet<uuid>()
    // Track final IDs for SetXxxName ops (separate from idMapping which stays approved-only)
    // This maps fresh ID -> final ID (new UUID for changed items, approved for identical)
    let finalIdMapping = System.Collections.Generic.Dictionary<uuid, uuid>()
    // Track approved IDs for items that were reverted (to clean up edit_history)
    let mutable revertedApprovedIds : List<uuid> = []

    let! addOpsStabilized =
      ops
      |> Ply.List.mapSequentially (fun op ->
        uply {
          match op with
          | PT.PackageOp.AddType typ ->
            let approvedId = idMapping[typ.id]
            if approvedId = typ.id then
              // New item (no approved version exists) - keep original ID
              return Some(PT.PackageOp.AddType typ)
            else
              // Approved version exists - compare content against it
              let! existingTypeOpt = referencePM.getType approvedId
              match existingTypeOpt with
              | Some existingType ->
                // Types don't have expression bodies, so no mapping needed for comparison
                match SC.compareTypes existingType typ with
                | SC.Identical ->
                  // Content unchanged from approved - emit with approved UUID
                  // idMapping already points to approved (from PASS 1)
                  identicalToApproved.Add(typ.id) |> ignore<bool>
                  finalIdMapping[typ.id] <- approvedId
                  revertedApprovedIds <- approvedId :: revertedApprovedIds
                  return Some(PT.PackageOp.AddType { typ with id = approvedId })
                | SC.Compatible ->
                  // Content changed but compatible - create new pending UUID
                  // DON'T update idMapping - keep it at approved so other items reference approved
                  let newId = System.Guid.NewGuid()
                  finalIdMapping[typ.id] <- newId
                  edits <-
                    { EditPropagation.EditInfo.oldId = approvedId
                      newId = newId
                      isBreaking = false }
                    :: edits
                  return Some(PT.PackageOp.AddType { typ with id = newId })
                | SC.Breaking ->
                  // Breaking change - create new pending UUID and mark as breaking
                  // DON'T update idMapping - keep it at approved so other items reference approved
                  let newId = System.Guid.NewGuid()
                  finalIdMapping[typ.id] <- newId
                  edits <-
                    { EditPropagation.EditInfo.oldId = approvedId
                      newId = newId
                      isBreaking = true }
                    :: edits
                  return Some(PT.PackageOp.AddType { typ with id = newId })
              | None ->
                // Couldn't fetch approved - treat as new (keep fresh ID)
                finalIdMapping[typ.id] <- typ.id
                return Some(PT.PackageOp.AddType typ)

          | PT.PackageOp.AddValue value ->
            let approvedId = idMapping[value.id]
            if approvedId = value.id then
              // New item (no approved version exists) - keep original ID
              // Still transform body to replace any fresh UUIDs of other items
              let transformedBody = SC.transformExprUuids idMapping value.body
              finalIdMapping[value.id] <- value.id
              return Some(PT.PackageOp.AddValue { value with body = transformedBody })
            else
              // Approved version exists - compare content against it
              let! existingValueOpt = referencePM.getValue approvedId
              match existingValueOpt with
              | Some existingValue ->
                // Use mapping to normalize UUIDs in value body before comparing
                match SC.compareValuesWithMapping idMapping existingValue value with
                | SC.Identical ->
                  // Content unchanged from approved - emit with approved UUID
                  // idMapping already points to approved (from PASS 1)
                  identicalToApproved.Add(value.id) |> ignore<bool>
                  finalIdMapping[value.id] <- approvedId
                  revertedApprovedIds <- approvedId :: revertedApprovedIds
                  // Transform body and use approved ID
                  let transformedBody = SC.transformExprUuids idMapping value.body
                  return Some(PT.PackageOp.AddValue { value with id = approvedId; body = transformedBody })
                | SC.Compatible ->
                  // Content changed but compatible - create new pending UUID
                  // DON'T update idMapping - keep it at approved so other items reference approved
                  let newId = System.Guid.NewGuid()
                  finalIdMapping[value.id] <- newId
                  edits <-
                    { EditPropagation.EditInfo.oldId = approvedId
                      newId = newId
                      isBreaking = false }
                    :: edits
                  // Transform body and use new ID
                  let transformedBody = SC.transformExprUuids idMapping value.body
                  return Some(PT.PackageOp.AddValue { value with id = newId; body = transformedBody })
                | SC.Breaking ->
                  // Breaking change - create new pending UUID
                  // DON'T update idMapping - keep it at approved so other items reference approved
                  let newId = System.Guid.NewGuid()
                  finalIdMapping[value.id] <- newId
                  edits <-
                    { EditPropagation.EditInfo.oldId = approvedId
                      newId = newId
                      isBreaking = true }
                    :: edits
                  // Transform body and use new ID
                  let transformedBody = SC.transformExprUuids idMapping value.body
                  return Some(PT.PackageOp.AddValue { value with id = newId; body = transformedBody })
              | None ->
                // Couldn't fetch approved - treat as new
                finalIdMapping[value.id] <- value.id
                let transformedBody = SC.transformExprUuids idMapping value.body
                return Some(PT.PackageOp.AddValue { value with body = transformedBody })

          | PT.PackageOp.AddFn fn ->
            let approvedId = idMapping[fn.id]
            if approvedId = fn.id then
              // New item (no approved version exists) - keep original ID
              // Still transform body to replace any fresh UUIDs of other items
              let transformedBody = SC.transformExprUuids idMapping fn.body
              finalIdMapping[fn.id] <- fn.id
              return Some(PT.PackageOp.AddFn { fn with body = transformedBody })
            else
              // Approved version exists - compare content against it
              let! existingFnOpt = referencePM.getFn approvedId
              match existingFnOpt with
              | Some existingFn ->
                // Use mapping to normalize UUIDs in function body before comparing
                match SC.compareFnsWithMapping idMapping existingFn fn with
                | SC.Identical ->
                  // Content unchanged from approved - emit with approved UUID
                  // idMapping already points to approved (from PASS 1)
                  identicalToApproved.Add(fn.id) |> ignore<bool>
                  finalIdMapping[fn.id] <- approvedId
                  revertedApprovedIds <- approvedId :: revertedApprovedIds
                  // Transform body and use approved ID
                  let transformedBody = SC.transformExprUuids idMapping fn.body
                  return Some(PT.PackageOp.AddFn { fn with id = approvedId; body = transformedBody })
                | SC.Compatible ->
                  // Content changed but compatible - create new pending UUID
                  // DON'T update idMapping - keep it at approved so other items reference approved
                  let newId = System.Guid.NewGuid()
                  finalIdMapping[fn.id] <- newId
                  edits <-
                    { EditPropagation.EditInfo.oldId = approvedId
                      newId = newId
                      isBreaking = false }
                    :: edits
                  // Transform body and use new ID
                  let transformedBody = SC.transformExprUuids idMapping fn.body
                  return Some(PT.PackageOp.AddFn { fn with id = newId; body = transformedBody })
                | SC.Breaking ->
                  // Breaking change - create new pending UUID and mark as breaking
                  // DON'T update idMapping - keep it at approved so other items reference approved
                  let newId = System.Guid.NewGuid()
                  finalIdMapping[fn.id] <- newId
                  edits <-
                    { EditPropagation.EditInfo.oldId = approvedId
                      newId = newId
                      isBreaking = true }
                    :: edits
                  // Transform body and use new ID
                  let transformedBody = SC.transformExprUuids idMapping fn.body
                  return Some(PT.PackageOp.AddFn { fn with id = newId; body = transformedBody })
              | None ->
                // Couldn't fetch approved - treat as new
                finalIdMapping[fn.id] <- fn.id
                let transformedBody = SC.transformExprUuids idMapping fn.body
                return Some(PT.PackageOp.AddFn { fn with body = transformedBody })

          | _ -> return Some op
        })

    // PASS 3: Update SetXxxName ops using finalIdMapping, and filter out ops for identical items
    // Also collect locations to deprecate (items identical to approved)
    let mutable locationsToDeprecate : List<PT.PackageLocation * string> = []

    let result =
      addOpsStabilized
      |> List.choose (fun opOpt ->
        match opOpt with
        | None -> None
        | Some op ->
          match op with
          | PT.PackageOp.SetTypeName(origId, loc) ->
            // Skip if this item was identical to approved (location entry already exists)
            if identicalToApproved.Contains(origId) then
              locationsToDeprecate <- (loc, "type") :: locationsToDeprecate
              None
            else
              // Use finalIdMapping to get the actual ID for this item
              match finalIdMapping.TryGetValue(origId) with
              | true, newId -> Some(PT.PackageOp.SetTypeName(newId, loc))
              | false, _ -> Some op

          | PT.PackageOp.SetValueName(origId, loc) ->
            // Skip if this item was identical to approved (location entry already exists)
            if identicalToApproved.Contains(origId) then
              locationsToDeprecate <- (loc, "value") :: locationsToDeprecate
              None
            else
              // Use finalIdMapping to get the actual ID for this item
              match finalIdMapping.TryGetValue(origId) with
              | true, newId -> Some(PT.PackageOp.SetValueName(newId, loc))
              | false, _ -> Some op

          | PT.PackageOp.SetFnName(origId, loc) ->
            // Skip if this item was identical to approved (location entry already exists)
            if identicalToApproved.Contains(origId) then
              locationsToDeprecate <- (loc, "fn") :: locationsToDeprecate
              None
            else
              // Use finalIdMapping to get the actual ID for this item
              match finalIdMapping.TryGetValue(origId) with
              | true, newId -> Some(PT.PackageOp.SetFnName(newId, loc))
              | false, _ -> Some op

          | _ -> Some op)

    // PASS 4: Deprecate pending entries for items that were reverted to approved
    do!
      locationsToDeprecate
      |> Ply.List.iterSequentially (fun (loc, itemType) ->
        uply {
          let modulesStr = String.concat "." loc.modules
          let! _ = Queries.deprecatePendingEntriesForLocation loc.owner modulesStr loc.name itemType branchID
          return ()
        })

    // PASS 5: Clean up edit_history (and breaking_change_todos via CASCADE) for reverted items
    do!
      revertedApprovedIds
      |> Ply.List.iterSequentially (fun approvedId ->
        uply {
          let! _ = Queries.deleteEditHistoryForApprovedItem approvedId
          return ()
        })

    return (result, List.rev edits)
  }


/// Create an in-memory PackageManager from PackageOps
/// (at time of writing, only really useful for tests and from-disk parsing)
let withExtraOps
  (basePM : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : PT.PackageManager =
  let opsPM = createInMemory ops
  combine opsPM basePM
