/// Impure randomness builtins. Pure numeric/list ops live in their
/// type-specific files under `Builtins.Pure/Libs/`.
module Builtins.Random.Libs.Random

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module VT = LibExecution.ValueType
module TypeChecker = LibExecution.TypeChecker


let fns () : List<BuiltInFn> =
  [ { name = fn "intRandom" 0
      typeParams = []
      parameters = [ Param.make "start" TInt ""; Param.make "end" TInt "" ]
      returnType = TInt
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
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
        + "list> is empty."
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
      previewable = Impure
      callEffects = set [ Effect.Random ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
