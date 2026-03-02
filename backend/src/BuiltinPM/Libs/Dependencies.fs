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


let tupleVT = VT.tuple VT.string VT.string []

/// Try to get location for an item ID, checking all item types (fn, type, value)
let private getLocationAny
  (branchChain : List<PT.BranchId>)
  (id : PT.ContentHash)
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
          Param.make "target" TString "The hash to find dependents for" ]
      returnType = TList(TTuple(TString, TString, []))
      description =
        "Returns items that reference the given hash (reverse dependencies), scoped to the branch chain."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString target ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let! results =
              LibPackageManager.Queries.getDependents
                branchChain
                (PT.ContentHash target)

            let dvals =
              results
              |> List.map (fun ref ->
                let (PT.ContentHash h) = ref.itemHash
                DTuple(DString h, DString(ref.itemKind.toString ()), []))
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
          Param.make "source" TString "The hash to find dependencies for" ]
      returnType = TList(TTuple(TString, TString, []))
      description =
        "Returns all hashes that the given item references (forward dependencies), scoped to the branch chain."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString source ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let! results =
              LibPackageManager.Queries.getDependencies
                branchChain
                (PT.ContentHash source)
            let dvals =
              results
              |> List.map (fun ref ->
                let (PT.ContentHash h) = ref.itemHash
                DTuple(DString h, DString(ref.itemKind.toString ()), []))
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
            (TList TString)
            "List of hashes to find dependents for" ]
      returnType = TList(TTuple(TString, TString, [ TString ]))
      description =
        "Batch lookup of dependents, scoped to the branch chain. Returns (dependsOnHash, itemHash, kind) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, targets) ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let ids =
              targets
              |> List.choose (fun dval ->
                match dval with
                | DString s -> Some(PT.ContentHash s)
                | _ -> None)

            let! results =
              LibPackageManager.Queries.getDependentsBatch branchChain ids

            let resultVT = VT.tuple VT.string VT.string [ VT.string ]

            let dvals =
              results
              |> List.map (fun dep ->
                let (PT.ContentHash dependsOn) = dep.dependsOnHash
                let (PT.ContentHash itemH) = dep.itemHash
                DTuple(
                  DString dependsOn,
                  DString itemH,
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
          Param.make "itemHashes" (TList TString) "List of item hashes to resolve" ]
      returnType =
        TList(
          TTuple(TString, TCustomType(NR.ok PT2DT.PackageLocation.typeName, []), [])
        )
      description =
        "Resolve hashes to PackageLocations. Returns (itemHash, location) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, itemHashes) ] ->
          uply {
            let hashes =
              itemHashes
              |> List.choose (fun item ->
                match item with
                | DString s -> Some(PT.ContentHash s)
                | _ -> None)

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
                let (PT.ContentHash h) = hash
                DTuple(DString h, PT2DT.PackageLocation.toDT loc, []))

            return
              DList(
                VT.tuple VT.string (VT.known PT2DT.PackageLocation.knownType) [],
                dvals
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
