/// Impure randomness builtins. Pure numeric/list ops live in their
/// type-specific files under `Builtins.Pure/Libs/`.
module Builtins.Random.Libs.Random

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects


let fns () : List<BuiltInFn> =
  [ { name = fn "bigIntRandom" 0
      typeParams = []
      parameters = [ Param.make "start" TBigInt ""; Param.make "end" TBigInt "" ]
      returnType = TBigInt
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | state, _, _, [| DBigInt a; DBigInt b |] ->
          let a = DarkBigInt.toBigInt a
          let b = DarkBigInt.toBigInt b
          let lower, upper = if a > b then (b, a) else (a, b)
          let range = upper - lower + bigint 1
          // Draw a uniform value in [0, range). Generate extra bytes so the
          // modulo bias is negligible (< 2^-64); the top byte stays 0 so the
          // BigInteger is read as non-negative. `List.shuffle` and `List.randomElement`
          // are Dark over this.
          let numBytes = range.GetByteCount(true) + 8
          let bytes = Array.zeroCreate<byte> numBytes
          LibExecution.HostRandom.fill state.random bytes
          let buf = Array.append bytes [| 0uy |]
          let r = System.Numerics.BigInteger(buf) % range
          Dval.bigInt (lower + r)
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Random ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
