/// Impure randomness builtins. Pure numeric/list ops live in their
/// type-specific files under `Builtins.Pure/Libs/`.
///
/// One primitive per KIND of draw, not one per width. The fixed-width variants (`Int8` through
/// `UInt32`) are Dark on top of `int64Random`, and `UInt64` is Dark on top of `intRandom`, since
/// its top half does not fit in an `Int64`. Nothing is lost by the move: those primitives already
/// draw uniformly over an inclusive range, so widening the bounds and narrowing the result back
/// leaves the reduction exactly where it was.
///
/// `listRandomElement` deliberately did NOT move. It draws from the cryptographic RNG while these
/// draw from a seeded `System.Random`, so reimplementing it as "random index, then getAt" would
/// swap one for the other with every signature identical. Same effect, same platform, different
/// guarantee, and nothing in the types says so.
module Builtins.Random.Libs.Random

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module VT = LibExecution.ValueType
module TypeChecker = LibExecution.TypeChecker


let fns () : List<BuiltInFn> =
  [ { name = fn "int64Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt64 ""; Param.make "end" TInt64 "" ]
      returnType = TInt64
      description =
        "Returns a random integer between <param start> and <param end> (inclusive). "
        + "NOT cryptographic: the draw comes from a generator seeded per call from the system "
        + "RNG, so one call carries about 31 bits of entropy however wide its return type. Use "
        + "it for sampling and shuffling, not for anything anyone would want to guess."
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          // .NET's "nextInt64" is exclusive, but we want inclusive.
          let correction : int64 = 1
          lower + randomSeeded().NextInt64(upper - lower + correction)
          |> DInt64
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Random ]
      deprecated = NotDeprecated }


    { name = fn "intRandom" 0
      typeParams = []
      parameters = [ Param.make "start" TInt ""; Param.make "end" TInt "" ]
      returnType = TInt
      description =
        "Returns a random integer between <param start> and <param end> (inclusive), at "
        + "arbitrary precision. NOT cryptographic, and the same roughly 31 bits of entropy per "
        + "call as the fixed-width draws, whatever the width of the range asked for."
      fn =
        (function
        | _, _, _, [| DInt a; DInt b |] ->
          let a = DarkInt.toBigInt a
          let b = DarkInt.toBigInt b
          let lower, upper = if a > b then (b, a) else (a, b)
          let range = upper - lower + bigint 1
          // Draw a uniform value in [0, range). Generate extra bytes so the
          // modulo bias is negligible (< 2^-64); the top byte stays 0 so the
          // BigInteger is read as non-negative.
          let numBytes = range.GetByteCount(true) + 8
          let buf = Array.zeroCreate (numBytes + 1)
          randomSeeded().NextBytes(buf)
          buf[numBytes] <- 0uy
          let r = System.Numerics.BigInteger(buf) % range
          Dval.int (lower + r) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Random ]
      deprecated = NotDeprecated }


    { name = fn "listRandomElement" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "list" (TList(TVariable "a")) "" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "Returns {{Some <var randomValue>}}, where <var randomValue> is a "
        + "randomly selected value in <param list>. Returns {{None}} if <param "
        + "list> is empty. Unlike the numeric draws beside it, this one is "
        + "CRYPTOGRAPHIC: the index comes straight from the system RNG."
      fn =
        let optType = VT.unknownTODO
        (function
        | _, _, _, [| DList(_, []) |] ->
          TypeChecker.DvalCreator.optionNone optType |> Ply
        | _, vm, _, [| DList(_, l) |] ->
          // Will return <= (length - 1).
          // Maximum value is Int64.MaxValue (half of UInt64.MaxValue),
          // not a real concern at expected list sizes.
          let index = RNG.GetInt32(l.Length)
          (List.tryItem index l)
          |> TypeChecker.DvalCreator.option vm.threadID optType
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = set [ Effect.Random ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
