module LibPackageManager.PackageManager

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibPackageManager.Caching

module PMRT = RT.SQL


// TODO: bring back eager loading
let rt : RT.PackageManager =
  { getType = withCache PMRT.Type.get
    getFn = withCache PMRT.Fn.get
    getValue = withCache PMRT.Value.get

    // CLEANUP maybe do some eager-loading
    init = uply { return () } }

let cachelessPM = PT.SQL.PM.pt

let pt : PT.PackageManager =
  { findType = withCache cachelessPM.findType
    findValue = withCache cachelessPM.findValue
    findFn = withCache cachelessPM.findFn

    getType = withCache cachelessPM.getType
    getFn = withCache cachelessPM.getFn
    getValue = withCache cachelessPM.getValue

    getTypeLocation = withCache cachelessPM.getTypeLocation
    getValueLocation = withCache cachelessPM.getValueLocation
    getFnLocation = withCache cachelessPM.getFnLocation

    // CLEANUP _should_ this be cached? for a bit?
    search = cachelessPM.search

    applyOps = cachelessPM.applyOps

    init = uply { return () } }




// /// Combine two PackageManagers: check `overlay` first, then fall back to `fallback`.
// /// This is used to layer transient/uncommitted definitions on top of persistent ones.
// let combine
//   (overlay : PT.PackageManager)
//   (fallback : PT.PackageManager)
//   : PT.PackageManager =
//   { findType =
//       fun (branchID, loc) ->
//         uply {
//           match! overlay.findType (branchID, loc) with
//           | Some id -> return Some id
//           | None -> return! fallback.findType (branchID, loc)
//         }

//     findValue =
//       fun (branchID, loc) ->
//         uply {
//           match! overlay.findValue (branchID, loc) with
//           | Some id -> return Some id
//           | None -> return! fallback.findValue (branchID, loc)
//         }

//     findFn =
//       fun (branchID, loc) ->
//         uply {
//           match! overlay.findFn (branchID, loc) with
//           | Some id -> return Some id
//           | None -> return! fallback.findFn (branchID, loc)
//         }

//     getType =
//       fun id ->
//         uply {
//           match! overlay.getType id with
//           | Some t -> return Some t
//           | None -> return! fallback.getType id
//         }

//     getValue =
//       fun id ->
//         uply {
//           match! overlay.getValue id with
//           | Some v -> return Some v
//           | None -> return! fallback.getValue id
//         }

//     getFn =
//       fun id ->
//         uply {
//           match! overlay.getFn id with
//           | Some f -> return Some f
//           | None -> return! fallback.getFn id
//         }

//     getTypeLocation =
//       fun (branchID, id) ->
//         uply {
//           match! overlay.getTypeLocation (branchID, id) with
//           | Some loc -> return Some loc
//           | None -> return! fallback.getTypeLocation (branchID, id)
//         }

//     getValueLocation =
//       fun (branchID, id) ->
//         uply {
//           match! overlay.getValueLocation (branchID, id) with
//           | Some loc -> return Some loc
//           | None -> return! fallback.getValueLocation (branchID, id)
//         }

//     getFnLocation =
//       fun (branchID, id) ->
//         uply {
//           match! overlay.getFnLocation (branchID, id) with
//           | Some loc -> return Some loc
//           | None -> return! fallback.getFnLocation (branchID, id)
//         }

//     search =
//       fun (branchID, query) ->
//         uply {
//           // Combine search results from both
//           let! overlay = overlay.search (branchID, query)
//           let! fallback = fallback.search (branchID, query)

//           let results = //PT.Search.SearchResults
//             { submodules = List.append overlay.submodules fallback.submodules
//               types = List.append overlay.types fallback.types
//               values = List.append overlay.values fallback.values
//               fns = List.append overlay.fns fallback.fns }

//           return results
//         }

//     init =
//       uply {
//         do! overlay.init
//         do! fallback.init
//       } }


// // TODO can we (somehow) abstract the algorithm of: phase 1, id stabilization, reparse for phase 2

// /// Stabilize IDs in ops by matching them against a reference PackageManager
// /// Used during two-phase parsing to ensure IDs from second pass match first pass
// let stabilizeOpsAgainstPM
//   (referencePM : PT.PackageManager)
//   (ops : List<PT.PackageOp>)
//   : Ply<List<PT.PackageOp>> =
//   uply {
//     let mutable result = []
//     for op in List.rev ops do
//       let! stabilizedOp =
//         uply {
//           match op with
//           | PT.PackageOp.SetTypeName(_, loc) ->
//             // Look up stable ID from reference PM
//             let! stableIdOpt = referencePM.findType (None, loc)
//             let stableId =
//               stableIdOpt |> Option.defaultWith (fun () -> System.Guid.NewGuid())
//             return PT.PackageOp.SetTypeName(stableId, loc)

//           | PT.PackageOp.AddType typ ->
//             // Find location for this type in current ops
//             let typLoc =
//               ops
//               |> List.tryPick (function
//                 | PT.PackageOp.SetTypeName(id, loc) when id = typ.id -> Some loc
//                 | _ -> None)
//             // Look up stable ID from reference PM using that location
//             let! stableIdOpt =
//               match typLoc with
//               | Some loc -> referencePM.findType (None, loc)
//               | None -> Ply(None)
//             let stableId = stableIdOpt |> Option.defaultValue typ.id
//             return PT.PackageOp.AddType { typ with id = stableId }

//           | PT.PackageOp.SetValueName(_, loc) ->
//             let! stableIdOpt = referencePM.findValue (None, loc)
//             let stableId =
//               stableIdOpt |> Option.defaultWith (fun () -> System.Guid.NewGuid())
//             return PT.PackageOp.SetValueName(stableId, loc)

//           | PT.PackageOp.AddValue value ->
//             let valueLoc =
//               ops
//               |> List.tryPick (function
//                 | PT.PackageOp.SetValueName(id, loc) when id = value.id -> Some loc
//                 | _ -> None)
//             let! stableIdOpt =
//               match valueLoc with
//               | Some loc -> referencePM.findValue (None, loc)
//               | None -> Ply(None)
//             let stableId = stableIdOpt |> Option.defaultValue value.id
//             return PT.PackageOp.AddValue { value with id = stableId }

//           | PT.PackageOp.SetFnName(_, loc) ->
//             let! stableIdOpt = referencePM.findFn (None, loc)
//             let stableId =
//               stableIdOpt |> Option.defaultWith (fun () -> System.Guid.NewGuid())
//             return PT.PackageOp.SetFnName(stableId, loc)

//           | PT.PackageOp.AddFn fn ->
//             let fnLoc =
//               ops
//               |> List.tryPick (function
//                 | PT.PackageOp.SetFnName(id, loc) when id = fn.id -> Some loc
//                 | _ -> None)
//             let! stableIdOpt =
//               match fnLoc with
//               | Some loc -> referencePM.findFn (None, loc)
//               | None -> Ply(None)
//             let stableId = stableIdOpt |> Option.defaultValue fn.id
//             return PT.PackageOp.AddFn { fn with id = stableId }
//         }
//       result <- stabilizedOp :: result
//     return result
//   }


// /// Create an in-memory PackageManager from PackageOps
// /// (at time of writing, only really useful for tests and from-disk parsing)
// let withExtraOps
//   (basePM : PT.PackageManager)
//   (ops : List<PT.PackageOp>)
//   : PT.PackageManager =
//   let opsPM = createInMemory ops
//   combine opsPM basePM
