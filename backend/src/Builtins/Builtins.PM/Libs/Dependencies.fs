/// Builtin functions for dependency tracking between package items.
/// Enables "what calls this?" and "what does this call?" queries.
module Builtins.PM.Libs.Dependencies

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module NR = LibExecution.RuntimeTypes.NameResolution
module PMPT = LibDB.ProgramTypes
module Branches = LibDB.Branches


let private hashType = TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])
let private locationType = TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), [])
let private itemKindType = TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), [])

let private hashVT = VT.known (PT2DT.Hash.knownType ())
let private locationVT = VT.known (PT2DT.PackageLocation.knownType ())
let private itemKindVT = VT.known (PT2DT.ItemKind.knownType ())

/// Try to get location for an item hash, checking all item types (fn, type, value)
let private getLocationAny
  (branchChain : List<PT.BranchId>)
  (hash : PT.Hash)
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


let fns () : List<BuiltInFn> =
  [ { name = fn "depsGetDependencies" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch context for scoping"
          Param.make "source" hashType "The hash to find dependencies for" ]
      returnType = TList(TTuple(hashType, TString, []))
      description =
        "Returns all hashes that the given item references (forward dependencies), scoped to the branch chain."
      fn =
        (function
        | _, _, _, [ DUuid branchId; sourceDval ] ->
          uply {
            let source = PT2DT.Hash.fromDT sourceDval
            let! branchChain = Branches.getBranchChain branchId
            let! results =
              LibDB.Queries.getDependencies branchChain source
            let dvals =
              results
              |> List.map (fun ref ->
                DTuple(
                  PT2DT.Hash.toDT ref.itemHash,
                  DString(ref.itemKind.toString ()),
                  []
                ))
            return DList(VT.tuple hashVT VT.string [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "depsGetDependents" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch context for scoping"
          Param.make
            "targets"
            (TList(TTuple(locationType, itemKindType, [])))
            "List of (location, itemKind) targets to find dependents for" ]
      returnType = TList(TTuple(hashType, locationType, [ itemKindType ]))
      description =
        "Returns items that reference any of the given (location, kind) targets (reverse dependencies), scoped to the branch chain. Returns (itemHash, itemLocation, kind) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, targets) ] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let targets =
              targets
              |> List.map (function
                | DTuple(locationDval, itemKindDval, []) ->
                  (PT2DT.ItemKind.fromDT itemKindDval,
                   PT2DT.PackageLocation.fromDT locationDval)
                | other ->
                  Exception.raiseInternal
                    "Invalid depsGetDependents target"
                    [ "target", other ])

            let! results =
              LibDB.Queries.getDependentsByKindedLocations
                branchChain
                targets

            let dvals =
              results
              |> List.map (fun dep ->
                DTuple(
                  PT2DT.Hash.toDT dep.itemHash,
                  PT2DT.PackageLocation.toDT dep.itemLocation,
                  [ PT2DT.ItemKind.toDT dep.itemKind ]
                ))

            return DList(VT.tuple hashVT locationVT [ itemKindVT ], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "depsResolveLocations" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch ID for location lookup"
          Param.make "itemHashes" (TList(hashType)) "List of item hashes to resolve" ]
      returnType = TList(TTuple(hashType, locationType, []))
      description =
        "Resolve hashes to PackageLocations. Returns (itemHash, location) tuples."
      fn =
        (function
        | _, _, _, [ DUuid branchId; DList(_, itemHashes) ] ->
          uply {
            let hashes = itemHashes |> List.map PT2DT.Hash.fromDT

            let! branchChain = LibDB.Branches.getBranchChain branchId

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
                DTuple(PT2DT.Hash.toDT hash, PT2DT.PackageLocation.toDT loc, []))

            return DList(VT.tuple hashVT locationVT [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
