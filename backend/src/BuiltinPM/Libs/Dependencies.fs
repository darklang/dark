/// Builtin functions for dependency tracking between package items.
/// Enables "what calls this?" and "what does this call?" queries.
module BuiltinPM.Libs.Dependencies

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module VT = LibExecution.ValueType


let tupleVT = VT.tuple VT.uuid VT.string []


let fns : List<BuiltInFn> =
  [ { name = fn "dependenciesGetDependents" 0
      typeParams = []
      parameters = [ Param.make "targetId" TUuid "The UUID to find dependents for" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Returns items that reference the given UUID (reverse dependencies)."
      fn =
        (function
        | _, _, _, [ DUuid targetId ] ->
          uply {
            let! results = LibPackageManager.Queries.getDependents targetId

            let dvals =
              results
              |> List.map (fun ref -> DTuple(DUuid ref.itemId, DString ref.kind, []))
            return DList(tupleVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dependenciesGetDependencies" 0
      typeParams = []
      parameters =
        [ Param.make "sourceId" TUuid "The UUID to find dependencies for" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Returns all UUIDs that the given item references (forward dependencies)"
      fn =
        (function
        | _, _, _, [ DUuid sourceId ] ->
          uply {
            let! results = LibPackageManager.Queries.getDependencies sourceId
            let dvals =
              results
              |> List.map (fun ref -> DTuple(DUuid ref.itemId, DString ref.kind, []))
            return DList(tupleVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dependenciesGetDependentsBatch" 0
      typeParams = []
      parameters =
        [ Param.make "targetIds" (TList TUuid) "List of UUIDs to find dependents for" ]
      returnType = TList(TTuple(TUuid, TUuid, [ TString ]))
      description =
        "Batch lookup of dependents. Returns (dependsOnId, itemId, kind) tuples."
      fn =
        (function
        | _, _, _, [ DList(_, targetIds) ] ->
          uply {
            let ids =
              targetIds
              |> List.choose (fun dval ->
                match dval with
                | DUuid id -> Some id
                | _ -> None)

            let! results = LibPackageManager.Queries.getDependentsBatch ids

            let resultVT = VT.tuple VT.uuid VT.uuid [ VT.string ]

            let dvals =
              results
              |> List.map (fun dep ->
                DTuple(
                  DUuid dep.dependsOnId,
                  DUuid dep.itemId,
                  [ DString dep.kind ]
                ))

            return DList(resultVT, dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "dependenciesResolveNames" 0
      typeParams = []
      parameters =
        [ Param.make "itemIds" (TList TUuid) "List of item UUIDs to resolve" ]
      returnType = TList(TTuple(TUuid, TString, []))
      description =
        "Resolve UUIDs to display names. Returns (itemId, displayName) tuples."
      fn =
        (function
        | _, _, _, [ DList(_, itemIds) ] ->
          uply {
            let ids =
              itemIds
              |> List.choose (fun item ->
                match item with
                | DUuid id -> Some id
                | _ -> None)

            let! results = LibPackageManager.Queries.resolveLocations ids

            let dvals =
              results
              |> List.map (fun loc ->
                let displayName =
                  if System.String.IsNullOrEmpty(loc.modules) then
                    $"{loc.owner}.{loc.name}"
                  else
                    $"{loc.owner}.{loc.modules}.{loc.name}"
                DTuple(DUuid loc.itemId, DString displayName, []))

            return DList(VT.tuple VT.uuid VT.string [], dvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
