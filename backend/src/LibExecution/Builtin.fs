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
  // A rename can name a function that is itself the target of an earlier rename, so look in what
  // this fold has produced before falling back to the real ones. Merging the two maps to ask would
  // rebuild the whole table once per rename.
  let existingByName = Dictionary<FQFnName.Builtin, BuiltInFn>()
  existing |> List.iter (fun fn -> existingByName[fn.name] <- fn)

  let renamed = Dictionary<FQFnName.Builtin, BuiltInFn>()
  renames
  |> List.iter (fun (oldName, newName) ->
    let newFn =
      match renamed.TryGetValue newName with
      | true, fn -> fn
      | false, _ ->
        match existingByName.TryGetValue newName with
        | true, fn -> fn
        | false, _ ->
          Exception.raiseInternal
            $"all fns should exist {oldName} -> {newName}"
            [ "oldName", oldName; "newName", newName ]
    renamed[oldName] <-
      { newFn with
          name = oldName
          deprecated = RenamedTo(FQFnName.Builtin newName) })

  existing @ (renamed.Values |> List.ofSeq)


let checkFn (fn : BuiltInFn) : unit =
  if fn.parameters = [] then
    Exception.raiseInternal $"function {fn.name} has no parameters" [ "fn", fn.name ]


let private byName (items : List<'a>) (name : 'a -> 'k) : Dictionary<'k, 'a> =
  let d = Dictionary<'k, 'a>(List.length items)
  items |> List.iter (fun item -> d[name item] <- item)
  d


/// Provided a list of library contents, combine them (handling renames).
///
/// Straight from the source dictionaries into a pre-sized target one, with no F# list in between.
/// That matters because this runs at every nesting level: `Libs.List.builtins ()` builds a
/// `Dictionary` via `make`, `Builtins.Pure.Builtin.builtins ()` combines a couple of dozen of those,
/// and `PlatformSet.make` combines every one of THOSE. Materialising `lib.fns.Values |> List.ofSeq`
/// at each level costs every builtin a cons cell per level, and letting the target dictionary grow
/// from empty rehashes the whole set two or three times on the way up.
///
/// The rename path is unchanged and deliberately still list-based: `fnRenames` is empty everywhere
/// today, so the cost of converting to a list and back is paid by nobody, and the semantics there
/// (a rename whose target is itself the target of an earlier rename) are worth not re-deriving.
let combine (libs : List<Builtins>) (fnRenames : FnRenames) : Builtins =
  let fnCount = libs |> List.sumBy (fun lib -> lib.fns.Count)
  let valueCount = libs |> List.sumBy (fun lib -> lib.values.Count)

  let fns = Dictionary<FQFnName.Builtin, BuiltInFn>(fnCount)
  let values = Dictionary<FQValueName.Builtin, BuiltInValue>(valueCount)

  for lib in libs do
    for fn in lib.fns.Values do
      checkFn fn
      fns[fn.name] <- fn
    for value in lib.values.Values do
      values[value.name] <- value

  if List.isEmpty fnRenames then
    { values = values; fns = fns }
  else
    let renamed = fns.Values |> List.ofSeq |> renameFunctions fnRenames
    { values = values; fns = byName renamed _.name }


let make (values : List<BuiltInValue>) (fns : List<BuiltInFn>) : Builtins =
  { values = byName values _.name; fns = byName fns _.name }


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

  /// Converts a `float` to a `DInt`, truncating toward zero. NaN/Infinity have
  /// no Int representation, so `bigint f` would throw a host exception — surface
  /// a Dark `OutOfRange` error instead. Shared by the Float and Int builtins.
  let roundedToInt (vm : VMState) (rounded : float) : Ply<Dval> =
    if System.Double.IsNaN rounded || System.Double.IsInfinity rounded then
      RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID
    else
      rounded |> bigint |> Dval.int |> Ply

  type Param = BuiltInParam
