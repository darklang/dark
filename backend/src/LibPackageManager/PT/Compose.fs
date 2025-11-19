module LibPackageManager.PT.Compose

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude


module PT = LibExecution.ProgramTypes

/// Combine two PackageManagers: check `b` first, then fall back to `a`.
/// Ops are applied to the top one, not the lower one.
///
/// Context:
/// This is used to layer transient/uncommitted definitions on top of persistent ones.
let compose (a : PT.PackageManager) (b : PT.PackageManager) : PT.PackageManager =
  { findType =
      fun (branchID, loc) ->
        uply {
          match! b.findType (branchID, loc) with
          | Some id -> return Some id
          | None -> return! a.findType (branchID, loc)
        }

    findValue =
      fun (branchID, loc) ->
        uply {
          match! b.findValue (branchID, loc) with
          | Some id -> return Some id
          | None -> return! a.findValue (branchID, loc)
        }

    findFn =
      fun (branchID, loc) ->
        uply {
          match! b.findFn (branchID, loc) with
          | Some id -> return Some id
          | None -> return! a.findFn (branchID, loc)
        }

    getType =
      fun id ->
        uply {
          match! b.getType id with
          | Some t -> return Some t
          | None -> return! a.getType id
        }

    getValue =
      fun id ->
        uply {
          match! b.getValue id with
          | Some v -> return Some v
          | None -> return! a.getValue id
        }

    getFn =
      fun id ->
        uply {
          match! b.getFn id with
          | Some f -> return Some f
          | None -> return! a.getFn id
        }

    getTypeLocation =
      fun (branchID, id) ->
        uply {
          match! b.getTypeLocation (branchID, id) with
          | Some loc -> return Some loc
          | None -> return! a.getTypeLocation (branchID, id)
        }

    getValueLocation =
      fun (branchID, id) ->
        uply {
          match! b.getValueLocation (branchID, id) with
          | Some loc -> return Some loc
          | None -> return! a.getValueLocation (branchID, id)
        }

    getFnLocation =
      fun (branchID, id) ->
        uply {
          match! b.getFnLocation (branchID, id) with
          | Some loc -> return Some loc
          | None -> return! a.getFnLocation (branchID, id)
        }

    search =
      fun (branchID, query) ->
        uply {
          // Combine search results from both
          let! b = b.search (branchID, query)
          let! a = a.search (branchID, query)

          let results : PT.Search.SearchResults =
            { submodules = List.append b.submodules a.submodules
              types = List.append b.types a.types
              values = List.append b.values a.values
              fns = List.append b.fns a.fns }

          return results
        }

    applyOps = b.applyOps

    init =
      uply {
        do! b.init
        do! a.init
      } }
