module LibPackageManager.PT.InMemory

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Collections.Concurrent

open Prelude

module PT = LibExecution.ProgramTypes

// TODO: we actually need a special type here, which _maps to_ the 'normal' PM.

// let's run through some scenarios...
(*
- run-time: irrelevant. all sql-bound
- pre-dogfooding parsings
  - (all 2 phases, maybe with 'id stabilization')
  - canvases from disk
  - packages from disk
  - test module/file from disk
- supporting parsings while things are running normally
  - (start with normal SQL-bound PM)
  - parse once, get either ops or values
  - create in-mem thing here; apply ops (or add values)
  - parse again
  - ?? ID stabilization? idk. ugh just make hashes a real thing, as part of this PR.
  (2 phase)
*)



// hmm may need some _internal state_ vs exposed stuff separation
// and playback just affects the internal state in a pretty way

/// Create an in-memory PackageManager from a list of PackageOps.
/// This builds internal maps by applying each op sequentially.
/// Used for transient state during parsing, testing, etc.
let create () : PT.PackageManager =
  // Build maps by applying each op
  let types = ResizeArray<PT.PackageType.PackageType>()
  let values = ResizeArray<PT.PackageValue.PackageValue>()
  let fns = ResizeArray<PT.PackageFn.PackageFn>()
  let typeLocations = ResizeArray<PT.PackageLocation * uuid>()
  let valueLocations = ResizeArray<PT.PackageLocation * uuid>()
  let fnLocations = ResizeArray<PT.PackageLocation * uuid>()

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

  let applyOp op =
    match op with
    | PT.PackageOp.AddType t -> types.Add(t)
    | PT.PackageOp.SetTypeName(id, loc) -> typeLocations.Add(loc, id)
    | PT.PackageOp.AddValue v -> values.Add(v)
    | PT.PackageOp.SetValueName(id, loc) -> valueLocations.Add(loc, id)
    | PT.PackageOp.AddFn f -> fns.Add(f)
    | PT.PackageOp.SetFnName(id, loc) -> fnLocations.Add(loc, id)


  { findType = fun (_branchID, loc) -> Ply(Map.tryFind loc typeLocMap)
    findValue = fun (_branchID, loc) -> Ply(Map.tryFind loc valueLocMap)
    findFn = fun (_branchID, loc) -> Ply(Map.tryFind loc fnLocMap)

    getType = fun id -> Ply(Map.tryFind id typeMap)
    getValue = fun id -> Ply(Map.tryFind id valueMap)
    getFn = fun id -> Ply(Map.tryFind id fnMap)

    getTypeLocation = fun (_branchID, id) -> Ply(Map.tryFind id typeIdToLoc)
    getValueLocation = fun (_branchID, id) -> Ply(Map.tryFind id valueIdToLoc)
    getFnLocation = fun (_branchID, id) -> Ply(Map.tryFind id fnIdToLoc)

    // no need to support this for in-memory - right?
    search =
      fun (_branchId, _query) ->
        let results : PT.Search.SearchResults =
          { submodules = []; types = []; values = []; fns = [] }
        Ply results

    applyOps =
      // Applying ops to _branches_, for now, not supported for in-memory stuff.
      // (No reason we can't, just haven't bothered w/ the refactor yet)
      fun (_branchID, ops) ->
        for op in ops do
          applyOp op

        uply { return () }

    init = uply { return () } }
