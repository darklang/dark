/// Helper functions for stdlibs
module LibExecution.StdLib

open Prelude
open VendoredTablecloth
open RuntimeTypes

type TypeRenames = List<FQTypeName.StdlibTypeName * FQTypeName.StdlibTypeName>
type FnRenames = List<FQFnName.StdlibFnName * FQFnName.StdlibFnName>

/// All Libs should expose `contents`, which is a list of all the types and functions it provides
type Contents = List<BuiltInFn> * List<BuiltInType>


// To cut down on the amount of code, when we rename a function and make no other
// changes, we don't duplicate it. Instead, we rename it and add the rename to this
// list. At startup, the renamed functions are created and added to the list.
//
// Renames is old name first, new name second. The new one should still be in the
// codebase, the old one should not. If a function is renamed multiple times, add the
// latest rename first.
let renameFunctions
  (renames : FnRenames)
  (existing : List<BuiltInFn>)
  : List<BuiltInFn> =
  let existingMap = existing |> List.map (fun fn -> fn.name, fn) |> Map
  let newFns =
    renames
    |> List.fold Map.empty (fun renamedFns (oldName, newName) ->
      let newFn =
        Map.tryFind newName (Map.mergeFavoringLeft renamedFns existingMap)
        |> Exception.unwrapOptionInternal
          $"all fns should exist {oldName} -> {newName}"
          [ "oldName", oldName; "newName", newName ]
      Map.add
        oldName
        { newFn with name = oldName; deprecated = RenamedTo newName }
        renamedFns)
    |> Map.values
  existing @ newFns

let renameTypes
  (renames : TypeRenames)
  (existing : List<BuiltInType>)
  : List<BuiltInType> =
  let existingMap = existing |> List.map (fun typ -> typ.name, typ) |> Map
  let newTypes =
    renames
    |> List.fold Map.empty (fun renamedTypes (oldName, newName) ->
      let newType =
        Map.tryFind newName (Map.mergeFavoringLeft renamedTypes existingMap)
        |> Exception.unwrapOptionInternal
          $"all types should exist {oldName} -> {newName}"
          [ "oldName", oldName; "newName", newName ]
      Map.add
        oldName
        { newType with name = oldName; deprecated = RenamedTo newName }
        renamedTypes)
    |> Map.values
  existing @ newTypes

let checkFn (fn : BuiltInFn) : unit =
  // We can't do this until constants (eg Math.pi) are no longer implemented as functions
  // if fn.parameters = [] then
  //   Exception.raiseInternal $"function {fn.name} has no parameters" [ "fn", fn.name ]
  ()

/// Provided a list of library contents, combine them (handling renames)
let combine
  (libs : List<Contents>)
  (fnRenames : FnRenames)
  (typeRenames : TypeRenames)
  : Contents =
  let (fns, types) = List.unzip libs
  fns |> List.concat |> List.iter checkFn
  (fns |> List.concat |> renameFunctions fnRenames,
   types |> List.concat |> renameTypes typeRenames)



module Shortcuts =
  let fn' = FQFnName.stdlibFnName'
  let fn = FQFnName.stdlibFnName
  let fnNoMod = FQFnName.stdlibFnName' []

  let typ = FQTypeName.stdlibTypeName
  let typ' = FQTypeName.stdlibTypeName'

  let fqType (modules : List<string>) (typ : string) (version : int) : FQTypeName.T =
    FQTypeName.Stdlib(typ' modules typ version)

  let pkgTyp
    (owner : string)
    (modules : NonEmptyList<string>)
    (typ : string)
    (version : int)
    : FQTypeName.T =
    FQTypeName.Package
      { owner = owner; modules = modules; typ = typ; version = version }




  let incorrectArgs = Errors.incorrectArgs

  type Param = BuiltInParam


  let stdlibTypeRef
    (modul : string)
    (name : string)
    (version : int)
    : TypeReference =
    TCustomType(FQTypeName.Stdlib(typ modul name version), [])
