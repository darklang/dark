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
module NR = LibExecution.RuntimeTypes.NameResolution
module PMPT = LibPackageManager.ProgramTypes
module Branches = LibPackageManager.Branches


let contentHashVT = VT.known PT2DT.ContentHash.knownType
let tupleVT = VT.tuple contentHashVT VT.string []

/// Try to get location for an item hash, checking all item types (fn, type, value)
let private getLocationAny
  (branchChain : List<PT.BranchId>)
  (hash : PT.ContentHash)
  : Ply<Option<PT.PackageLocation>> =
  uply {
    // Try fn first (most common)
    match! PMPT.Fn.getLocations branchChain hash with
    | loc :: _ -> return Some loc
    | [] ->
      // Try type
      match! PMPT.Type.getLocations branchChain hash with
      | loc :: _ -> return Some loc
      | [] ->
        // Try value
        let! locs = PMPT.Value.getLocations branchChain hash
        return List.tryHead locs
  }


let fns : List<BuiltInFn> =
  [ { name = fn "depsGetDependents" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch context for scoping"
          Param.make
            "target"
            (TCustomType(NR.ok PT2DT.ContentHash.typeName, []))
            "The hash to find dependents for" ]
      returnType =
        TList(TTuple(TCustomType(NR.ok PT2DT.ContentHash.typeName, []), TString, []))
      description =
        "Returns items that reference the given hash (reverse dependencies), scoped to the branch chain."
      fn =
        (function
        | _, _, _, [ DUuid branchId; targetDval ] ->
          uply {
            let target = PT2DT.ContentHash.fromDT targetDval
            let! branchChain = Branches.getBranchChain branchId
            let! results = LibPackageManager.Queries.getDependents branchChain target

            let dvals =
              results
              |> List.map (fun ref ->
                DTuple(
                  PT2DT.ContentHash.toDT ref.itemHash,
                  DString(ref.itemKind.toString ()),
                  []
                ))
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
          Param.make
            "source"
            (TCustomType(NR.ok PT2DT.ContentHash.typeName, []))
            "The hash to find dependencies for" ]
      returnType =
        TList(TTuple(TCustomType(NR.ok PT2DT.ContentHash.typeName, []), TString, []))
      description =
        "Returns all hashes that the given item references (forward dependencies), scoped to the branch chain."
      fn =
        (function
        | _, _, _, [ DUuid branchId; sourceDval ] ->
          uply {
            let source = PT2DT.ContentHash.fromDT sourceDval
            let! branchChain = Branches.getBranchChain branchId
            let! results =
              LibPackageManager.Queries.getDependencies branchChain source
            let dvals =
              results
              |> List.map (fun ref ->
                DTuple(
                  PT2DT.ContentHash.toDT ref.itemHash,
                  DString(ref.itemKind.toString ()),
                  []
                ))
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
          Param.make
            "targets"
            (TList(TCustomType(NR.ok PT2DT.ContentHash.typeName, [])))
            "List of hashes to find dependents for" ]
      returnType =
        TList(
          TTuple(
            TCustomType(NR.ok PT2DT.ContentHash.typeName, []),
            TCustomType(NR.ok PT2DT.ContentHash.typeName, []),
            [ TString ]
          )
        )
      description =
        "Batch lookup of dependents, scoped to the branch chain. Returns (dependsOnHash, itemHash, kind) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, targets) ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let ids = targets |> List.map PT2DT.ContentHash.fromDT

            let! results =
              LibPackageManager.Queries.getDependentsBatch branchChain ids

            let resultVT = VT.tuple contentHashVT contentHashVT [ VT.string ]

            let dvals =
              results
              |> List.map (fun dep ->
                DTuple(
                  PT2DT.ContentHash.toDT dep.dependsOnHash,
                  PT2DT.ContentHash.toDT dep.itemHash,
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
          Param.make
            "itemHashes"
            (TList(TCustomType(NR.ok PT2DT.ContentHash.typeName, [])))
            "List of item hashes to resolve" ]
      returnType =
        TList(
          TTuple(
            TCustomType(NR.ok PT2DT.ContentHash.typeName, []),
            TCustomType(NR.ok PT2DT.PackageLocation.typeName, []),
            []
          )
        )
      description =
        "Resolve hashes to PackageLocations. Returns (itemHash, location) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, itemHashes) ] ->
          uply {
            let hashes = itemHashes |> List.map PT2DT.ContentHash.fromDT

            let! branchChain = LibPackageManager.Branches.getBranchChain branchId

            let! results =
              hashes
              |> List.map (fun hash ->
                uply {
                  match! getLocationAny branchChain hash with
                  | Some loc -> return Some(hash, loc)
                  | None -> return None
                })
              |> Ply.List.flatten
              |> Ply.map (List.choose identity)

            let dvals =
              results
              |> List.map (fun (hash, loc) ->
                DTuple(
                  PT2DT.ContentHash.toDT hash,
                  PT2DT.PackageLocation.toDT loc,
                  []
                ))

            return
              DList(
                VT.tuple contentHashVT (VT.known PT2DT.PackageLocation.knownType) [],
                dvals
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
