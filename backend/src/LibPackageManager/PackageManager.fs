module LibPackageManager.PackageManager

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibPackageManager.Caching

module PMPT = LibPackageManager.ProgramTypes
module PMRT = LibPackageManager.RuntimeTypes


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

    search = LibPackageManager.ProgramTypes.search

    init = uply { return () } }


/// Extract location→ID mappings from PackageOps
let extractLocationMaps
  (ops : List<PT.PackageOp>)
  : Map<PT.PackageLocation, uuid>
    * Map<PT.PackageLocation, uuid>
    * Map<PT.PackageLocation, uuid> =
  let typeLocToId =
    ops
    |> List.choose (function
      | PT.PackageOp.SetTypeName(id, loc) -> Some(loc, id)
      | _ -> None)
    |> Map.ofList

  let valueLocToId =
    ops
    |> List.choose (function
      | PT.PackageOp.SetValueName(id, loc) -> Some(loc, id)
      | _ -> None)
    |> Map.ofList

  let fnLocToId =
    ops
    |> List.choose (function
      | PT.PackageOp.SetFnName(id, loc) -> Some(loc, id)
      | _ -> None)
    |> Map.ofList

  (typeLocToId, valueLocToId, fnLocToId)


/// Create an in-memory PackageManager from PackageOps (for tests)
let withExtraOps
  (basePM : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : PT.PackageManager =
  // Extract items and location mappings from ops
  let types =
    ops
    |> List.choose (function
      | PT.PackageOp.AddType t -> Some t
      | _ -> None)

  let values =
    ops
    |> List.choose (function
      | PT.PackageOp.AddValue v -> Some v
      | _ -> None)

  let fns =
    ops
    |> List.choose (function
      | PT.PackageOp.AddFn f -> Some f
      | _ -> None)

  // Build location→ID maps
  let (typeLocToId, valueLocToId, fnLocToId) = extractLocationMaps ops

  // Build reverse lookup maps (id -> location)
  let typeIdToLoc =
    typeLocToId |> Map.toList |> List.map (fun (loc, id) -> id, loc) |> Map.ofList
  let valueIdToLoc =
    valueLocToId |> Map.toList |> List.map (fun (loc, id) -> id, loc) |> Map.ofList
  let fnIdToLoc =
    fnLocToId |> Map.toList |> List.map (fun (loc, id) -> id, loc) |> Map.ofList

  { findType =
      fun (branchId, loc) ->
        match Map.tryFind loc typeLocToId with
        | Some id -> Ply(Some id)
        | None -> basePM.findType (branchId, loc)

    findValue =
      fun (branchId, loc) ->
        match Map.tryFind loc valueLocToId with
        | Some id -> Ply(Some id)
        | None -> basePM.findValue (branchId, loc)

    findFn =
      fun (branchId, loc) ->
        match Map.tryFind loc fnLocToId with
        | Some id -> Ply(Some id)
        | None -> basePM.findFn (branchId, loc)

    getType =
      fun id ->
        match types |> List.tryFind (fun t -> t.id = id) with
        | Some t -> Ply(Some t)
        | None -> basePM.getType id

    getValue =
      fun id ->
        match values |> List.tryFind (fun v -> v.id = id) with
        | Some v -> Ply(Some v)
        | None -> basePM.getValue id

    getFn =
      fun id ->
        match fns |> List.tryFind (fun f -> f.id = id) with
        | Some f -> Ply(Some f)
        | None -> basePM.getFn id

    getTypeLocation =
      fun (branchId, id) ->
        match Map.tryFind id typeIdToLoc with
        | Some loc -> Ply(Some loc)
        | None -> basePM.getTypeLocation (branchId, id)

    getValueLocation =
      fun (branchId, id) ->
        match Map.tryFind id valueIdToLoc with
        | Some loc -> Ply(Some loc)
        | None -> basePM.getValueLocation (branchId, id)

    getFnLocation =
      fun (branchId, id) ->
        match Map.tryFind id fnIdToLoc with
        | Some loc -> Ply(Some loc)
        | None -> basePM.getFnLocation (branchId, id)

    search = basePM.search
    init = basePM.init }
