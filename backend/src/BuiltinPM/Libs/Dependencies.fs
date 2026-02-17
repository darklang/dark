/// Builtin functions for dependency tracking between package items.
/// Enables "what calls this?" and "what does this call?" queries.
module BuiltinPM.Libs.Dependencies

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PMPT = LibPackageManager.ProgramTypes
module Branches = LibPackageManager.Branches


let tupleVT = VT.tuple VT.uuid VT.string []

/// Try to get location for an item ID, checking all item types (fn, type, value)
let private getLocationAny
  (branchChain : List<PT.BranchId>)
  (id : uuid)
  : Ply<Option<PT.PackageLocation>> =
  uply {
    // Try fn first (most common)
    match! PMPT.Fn.getLocation branchChain id with
    | Some loc -> return Some loc
    | None ->
      // Try type
      match! PMPT.Type.getLocation branchChain id with
      | Some loc -> return Some loc
      | None ->
        // Try value
        return! PMPT.Value.getLocation branchChain id
  }


let fns : List<BuiltInFn> =
  [ { name = fn "depsGetDependents" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch context for scoping"
          Param.make "targetId" TUuid "The UUID to find dependents for" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Returns items that reference the given UUID (reverse dependencies), scoped to the branch chain."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DUuid targetId ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let! results =
              LibPackageManager.Queries.getDependents branchChain targetId

            let dvals =
              results
              |> List.map (fun ref ->
                DTuple(DUuid ref.itemId, DString(ref.itemKind.toString ()), []))
            return DList(tupleVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "depsGetDependencies" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch context for scoping"
          Param.make "sourceId" TUuid "The UUID to find dependencies for" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Returns all UUIDs that the given item references (forward dependencies), scoped to the branch chain."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DUuid sourceId ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let! results =
              LibPackageManager.Queries.getDependencies branchChain sourceId
            let dvals =
              results
              |> List.map (fun ref ->
                DTuple(DUuid ref.itemId, DString(ref.itemKind.toString ()), []))
            return DList(tupleVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "depsGetDependentsBatch" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch context for scoping"
          Param.make "targetIds" (TList TUuid) "List of UUIDs to find dependents for" ]
      returnType = TList(TTuple(TUuid, TUuid, [ TString ]))
      description =
        "Batch lookup of dependents, scoped to the branch chain. Returns (dependsOnId, itemId, kind) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, targetIds) ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let ids =
              targetIds
              |> List.choose (fun dval ->
                match dval with
                | DUuid id -> Some id
                | _ -> None)

            let! results =
              LibPackageManager.Queries.getDependentsBatch branchChain ids

            let resultVT = VT.tuple VT.uuid VT.uuid [ VT.string ]

            let dvals =
              results
              |> List.map (fun dep ->
                DTuple(
                  DUuid dep.dependsOnId,
                  DUuid dep.itemId,
                  [ DString(dep.itemKind.toString ()) ]
                ))

            return DList(resultVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "depsResolveLocations" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch ID for location lookup"
          Param.make "itemIds" (TList TUuid) "List of item UUIDs to resolve" ]
      returnType =
        TList(TTuple(TUuid, TCustomType(Ok PT2DT.PackageLocation.typeName, []), []))
      description =
        "Resolve UUIDs to PackageLocations. Returns (itemId, location) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, itemIds) ] ->
          uply {
            let ids =
              itemIds
              |> List.choose (fun item ->
                match item with
                | DUuid id -> Some id
                | _ -> None)

            let! branchChain = LibPackageManager.Branches.getBranchChain branchId

            let! results =
              ids
              |> List.map (fun id ->
                uply {
                  match! getLocationAny branchChain id with
                  | Some loc -> return Some(id, loc)
                  | None -> return None
                })
              |> Ply.List.flatten
              |> Ply.map (List.choose identity)

            let dvals =
              results
              |> List.map (fun (id, loc) ->
                DTuple(DUuid id, PT2DT.PackageLocation.toDT loc, []))

            return
              DList(
                VT.tuple VT.uuid (VT.known PT2DT.PackageLocation.knownType) [],
                dvals
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
