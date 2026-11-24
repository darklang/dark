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
  // Mutable for allocation on a hot path: every startup runs this over every builtin.
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
  let d = Dictionary<'k, 'a>()
  items |> List.iter (fun item -> d[name item] <- item)
  d


/// Provided a list of library contents, combine them (handling renames)
let combine (libs : List<Builtins>) (fnRenames : FnRenames) : Builtins =
  let fns = libs |> List.collect (fun lib -> lib.fns.Values |> List.ofSeq)

  fns |> List.iter checkFn

  { values =
      byName
        (libs |> List.collect (fun lib -> lib.values.Values |> List.ofSeq))
        _.name
    // Nearly every combine has no renames, and `renameFunctions` indexes every fn before it looks.
    fns =
      byName
        (if List.isEmpty fnRenames then fns else renameFunctions fnRenames fns)
        _.name }


let make (values : List<BuiltInValue>) (fns : List<BuiltInFn>) : Builtins =
  { values = byName values _.name; fns = byName fns _.name }


module Shortcuts =
  let fn = FQFnName.builtin
  let value = FQValueName.builtin
  let incorrectArgs = RuntimeTypes.incorrectArgs

  let outOfRange (kind : RuntimeError.BigInts.OutOfRangeKind) (value : Dval) : 'a =
    RuntimeError.BigInts.OutOfRange(kind, value) |> RuntimeError.BigInt |> raiseRTE

  /// Narrow an arbitrary-precision `BigInt` to a native `int64`, raising a Dark
  /// `OutOfRange` error (rather than letting a host overflow escape) when the
  /// value doesn't fit. Use at builtins that hand the value to int64-typed APIs.
  let bigIntToInt64 (i : DarkBigInt) : int64 =
    match DarkBigInt.toInt64 i with
    | Some v -> v
    | None ->
      let kind =
        RuntimeError.BigInts.BigIntToInt64(
          System.Int64.MinValue,
          System.Int64.MaxValue
        )
      outOfRange kind (DBigInt i)

  /// Narrow an `Int64` to a native `int` (Int32), raising a Dark `OutOfRange` error
  /// when the value doesn't fit.
  let int64ToInt32 (i : int64) : int =
    if i < int64 System.Int32.MinValue || i > int64 System.Int32.MaxValue then
      outOfRange
        (RuntimeError.BigInts.Int64ToInt32(
          int64 System.Int32.MinValue,
          int64 System.Int32.MaxValue
        ))
        (DInt64 i)
    else
      int i

  /// An `Int64` the kernel's `int` holds.
  let (|KernelInt|_|) (i : int64) : Option<int> =
    if i < int64 System.Int32.MinValue || i > int64 System.Int32.MaxValue then
      None
    else
      Some(int i)

  /// A capability token, or a descriptor number the kernel's `int` holds. Capability tokens
  /// cannot be narrowed to OS descriptor numbers.
  let (|KernelFd|_|) (i : DarkBigInt) : Option<HostTypes.FileDescriptor> =
    let value = DarkBigInt.toBigInt i
    if value >= HostDescriptors.tokenFloor then
      Some(HostTypes.FileDescriptor.Scoped value)
    else
      DarkBigInt.toInt32 i |> Option.map HostTypes.FileDescriptor.Raw

  /// Converts a `float` to a `DBigInt`, truncating toward zero. NaN/Infinity have
  /// no BigInt representation, so `bigint f` would throw a host exception — surface
  /// a Dark `OutOfRange` error instead. Shared by the Float64 and BigInt builtins.
  let roundedToBigInt (rounded : float) : Dval =
    if System.Double.IsNaN rounded || System.Double.IsInfinity rounded then
      outOfRange RuntimeError.BigInts.FloatHasNoInteger (DFloat64 rounded)
    else
      rounded |> bigint |> Dval.bigInt
  /// `roundedToBigInt` for an `Int64` result: out of its range is `OutOfRange` too.
  let roundedToInt64 (rounded : float) : Dval =
    // 2^63 is exact as a float; anything at or past it, or NaN, does not fit.
    if System.Double.IsNaN rounded || System.Double.IsInfinity rounded then
      outOfRange RuntimeError.BigInts.FloatHasNoInteger (DFloat64 rounded)
    elif
      not (rounded >= -9223372036854775808.0 && rounded < 9223372036854775808.0)
    then
      outOfRange
        (RuntimeError.BigInts.FloatOutsideInt64(
          System.Int64.MinValue,
          System.Int64.MaxValue
        ))
        (DFloat64 rounded)
    else
      rounded |> int64 |> Dval.dint64
  type Param = BuiltInParam
