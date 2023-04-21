/// Helper functions for stdlibs
module LibExecution.StdLib

open Prelude
open VendoredTablecloth
open RuntimeTypes


// To cut down on the amount of code, when we rename a function and make no other
// changes, we don't duplicate it. Instead, we rename it and add the rename to this
// list. At startup, the renamed functions are created and added to the list.
//
// Renames is old name first, new name second. The new one should still be in the
// codebase, the old one should not. If a function is renamed multiple times, add the
// latest rename first.
let renameFunctions
  (renames : List<FQFnName.StdlibFnName * FQFnName.StdlibFnName>)
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

module Shortcuts =
  let fn = FQFnName.stdlibFnName
  let fnNoMod = FQFnName.stdlibFnName' []
  let typ = FQTypeName.stdlibTypeName

  let incorrectArgs = Errors.incorrectArgs


  let stdlibTypeRef
    (module' : string)
    (name : string)
    (version : int)
    : TypeReference =
    TCustomType(FQTypeName.Stdlib(typ module' name version), [])
