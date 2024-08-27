/// Helper functions for stdlibs
module LibExecution.Builtin

open Prelude
open RuntimeTypes

type FnRenames = List<FQFnName.Builtin * FQFnName.Builtin>


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
  let existingMap = existing |> Map.fromListBy _.name
  let newFns =
    renames
    |> List.fold
      (fun renamedFns (oldName, newName) ->
        let newFn =
          Map.find newName (Map.mergeFavoringLeft renamedFns existingMap)
          |> Exception.unwrapOptionInternal
            $"all fns should exist {oldName} -> {newName}"
            [ "oldName", oldName; "newName", newName ]

        Map.add
          oldName
          { newFn with
              name = oldName
              deprecated = RenamedTo(FQFnName.Builtin newName) }
          renamedFns)
      Map.empty
    |> Map.values
  existing @ newFns


let checkFn (fn : BuiltInFn) : unit =
  if fn.parameters = [] then
    Exception.raiseInternal $"function {fn.name} has no parameters" [ "fn", fn.name ]


/// Provided a list of library contents, combine them (handling renames)
let combine (libs : List<Builtins>) (fnRenames : FnRenames) : Builtins =
  let fns = libs |> List.map _.fns |> List.collect Map.values

  fns |> List.iter checkFn

  { constants =
      libs
      |> List.map _.constants
      |> List.collect Map.values
      |> Map.fromListBy _.name
    fns = fns |> renameFunctions fnRenames |> Map.fromListBy _.name }


let make (constants : List<BuiltInConstant>) (fns : List<BuiltInFn>) : Builtins =
  { constants = constants |> Map.fromListBy _.name
    fns = fns |> Map.fromListBy _.name }


module Shortcuts =
  let fn = FQFnName.builtin
  let constant = FQConstantName.builtin
  let incorrectArgs = RuntimeTypes.incorrectArgs

  type Param = BuiltInParam
