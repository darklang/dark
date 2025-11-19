module LibPackageManager.PT.InMemory

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

module PT = LibExecution.ProgramTypes

/// Create an in-memory PackageManager from a list of PackageOps.
/// Uses mutable ConcurrentDictionaries so that applyOps can add new definitions.
/// Used for transient state during parsing, testing, etc.
let create (ops : List<PT.PackageOp>) : PT.PackageManager =
  // Use concurrent dictionaries for thread-safe mutable state
  let typeMap = ConcurrentDictionary<System.Guid, PT.PackageType.PackageType>()
  let valueMap = ConcurrentDictionary<System.Guid, PT.PackageValue.PackageValue>()
  let fnMap = ConcurrentDictionary<System.Guid, PT.PackageFn.PackageFn>()

  // Location → ID mappings
  let typeLocMap = ConcurrentDictionary<PT.PackageLocation, System.Guid>()
  let valueLocMap = ConcurrentDictionary<PT.PackageLocation, System.Guid>()
  let fnLocMap = ConcurrentDictionary<PT.PackageLocation, System.Guid>()

  // ID → Location reverse mappings
  let typeIdToLoc = ConcurrentDictionary<System.Guid, PT.PackageLocation>()
  let valueIdToLoc = ConcurrentDictionary<System.Guid, PT.PackageLocation>()
  let fnIdToLoc = ConcurrentDictionary<System.Guid, PT.PackageLocation>()

  // Apply an op to the mutable dictionaries
  let applyOp op =
    match op with
    | PT.PackageOp.AddType t -> typeMap.TryAdd(t.id, t) |> ignore<bool>
    | PT.PackageOp.SetTypeName(id, loc) ->
      typeLocMap.TryAdd(loc, id) |> ignore<bool>
      typeIdToLoc.TryAdd(id, loc) |> ignore<bool>
    | PT.PackageOp.AddValue v -> valueMap.TryAdd(v.id, v) |> ignore<bool>
    | PT.PackageOp.SetValueName(id, loc) ->
      valueLocMap.TryAdd(loc, id) |> ignore<bool>
      valueIdToLoc.TryAdd(id, loc) |> ignore<bool>
    | PT.PackageOp.AddFn f -> fnMap.TryAdd(f.id, f) |> ignore<bool>
    | PT.PackageOp.SetFnName(id, loc) ->
      fnLocMap.TryAdd(loc, id) |> ignore<bool>
      fnIdToLoc.TryAdd(id, loc) |> ignore<bool>

  // Apply initial ops
  for op in ops do
    applyOp op

  { findType =
      fun (_branchID, loc) ->
        let (found, id) = typeLocMap.TryGetValue loc
        Ply(if found then Some id else None)
    findValue =
      fun (_branchID, loc) ->
        let (found, id) = valueLocMap.TryGetValue loc
        Ply(if found then Some id else None)
    findFn =
      fun (_branchID, loc) ->
        let (found, id) = fnLocMap.TryGetValue loc
        Ply(if found then Some id else None)

    getType =
      fun id ->
        let (found, typ) = typeMap.TryGetValue id
        Ply(if found then Some typ else None)
    getValue =
      fun id ->
        let (found, value) = valueMap.TryGetValue id
        Ply(if found then Some value else None)
    getFn =
      fun id ->
        let (found, fn) = fnMap.TryGetValue id
        Ply(if found then Some fn else None)

    getTypeLocation =
      fun (_branchID, id) ->
        let (found, loc) = typeIdToLoc.TryGetValue id
        Ply(if found then Some loc else None)
    getValueLocation =
      fun (_branchID, id) ->
        let (found, loc) = valueIdToLoc.TryGetValue id
        Ply(if found then Some loc else None)
    getFnLocation =
      fun (_branchID, id) ->
        let (found, loc) = fnIdToLoc.TryGetValue id
        Ply(if found then Some loc else None)

    search =
      fun (_branchId, _query) ->
        let results : PT.Search.SearchResults =
          { submodules = []; types = []; values = []; fns = [] }
        Ply results

    applyOps =
      fun (_branchID, ops) ->
        for op in ops do
          applyOp op
        uply { return () }

    init = uply { return () } }
