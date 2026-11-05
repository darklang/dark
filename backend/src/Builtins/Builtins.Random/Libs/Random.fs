/// Impure randomness builtins. Pure numeric/list ops live in their
/// type-specific files under `Builtins.Pure/Libs/`.
module Builtins.Random.Libs.Random

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects


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
          // BigInteger is read as non-negative. The bytes come straight from the OS
          // generator: `List.shuffle` and `List.randomElement` are Dark over this, and
          // they always drew from it.
          let numBytes = range.GetByteCount(true) + 8
          let buf = Array.zeroCreate<byte> (numBytes + 1)
          RNG.Fill(System.Span<byte>(buf, 0, numBytes))
          buf[numBytes] <- 0uy
          let r = System.Numerics.BigInteger(buf) % range
          Dval.int (lower + r) |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Random ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
