/// Helper functions for declaraing built-in functions and values
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

  { values =
      libs |> List.map _.values |> List.collect Map.values |> Map.fromListBy _.name
    fns = fns |> renameFunctions fnRenames |> Map.fromListBy _.name }


let make (values : List<BuiltInValue>) (fns : List<BuiltInFn>) : Builtins =
  { values = values |> Map.fromListBy _.name; fns = fns |> Map.fromListBy _.name }


module Shortcuts =
  let fn = FQFnName.builtin
  let value = FQValueName.builtin
  let incorrectArgs = RuntimeTypes.incorrectArgs

  /// Narrow an arbitrary-precision `Int` to a native `int64`, raising a Dark
  /// `OutOfRange` error (rather than letting a host overflow escape) when the
  /// value doesn't fit. Use at builtins that hand the value to int64-typed APIs.
  let intToInt64 (vm : VMState) (i : DarkInt) : int64 =
    match DarkInt.toInt64 i with
    | Some v -> v
    | None ->
      RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID

  /// Like `intToInt64`, but narrows to a native `int` (Int32).
  let intToInt32 (vm : VMState) (i : DarkInt) : int =
    match DarkInt.toInt32 i with
    | Some v -> v
    | None ->
      RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID

  type Param = BuiltInParam
