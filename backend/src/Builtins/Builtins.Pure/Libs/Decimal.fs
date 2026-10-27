/// The two Decimal builtins Dark cannot do itself: taking a decimal apart into an integer and a
/// scale, and putting one back together. Parsing, printing and rounding are written in Dark on
/// top of these; arithmetic and ordering are the polymorphic operators in `NoModule`.
module Builtins.Pure.Libs.Decimal

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


/// 2^96, one past the largest magnitude a decimal's 96-bit integer holds.
let private magnitudeLimit = bigint.Pow(bigint 2, 96)

let private mask32 = bigint 0xFFFFFFFFu


let fns () : List<BuiltInFn> =
  [ { name = fn "decimalToParts" 0
      typeParams = []
      parameters = [ Param.make "d" TDecimal "" ]
      returnType = TTuple(TInt, TInt, [])
      description =
        "Split <param d> into {{(unscaled, scale)}}, where <param d> is {{unscaled / 10^scale}}. "
        + "The scale is as written, so {{1.10m}} is {{(110, 2)}} and {{1.1m}} is {{(11, 1)}}."
      fn =
        (function
        | _, _, _, [| DDecimal d |] ->
          let bits = System.Decimal.GetBits d
          let magnitude =
            (bigint (uint32 bits[2]) <<< 64)
            ||| (bigint (uint32 bits[1]) <<< 32)
            ||| bigint (uint32 bits[0])
          let unscaled = if d < 0m then -magnitude else magnitude
          DTuple(Dval.int unscaled, Dval.int (bigint (int d.Scale)), []) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "decimalFromParts" 0
      typeParams = []
      parameters = [ Param.make "unscaled" TInt ""; Param.make "scale" TInt "" ]
      returnType = TypeReference.option TDecimal
      description =
        "The decimal {{<param unscaled> / 10^<param scale>}}, keeping <param scale>. {{None}} "
        + "when <param scale> is outside 0 to 28 or <param unscaled> needs more than 96 bits."
      fn =
        (function
        | _, _, _, [| DInt unscaled; DInt scale |] ->
          let unscaled = DarkInt.toBigInt unscaled
          let scale = DarkInt.toBigInt scale
          let magnitude = bigint.Abs unscaled
          if
            scale < bigint.Zero || scale > bigint 28 || magnitude >= magnitudeLimit
          then
            Dval.optionNone KTDecimal |> Ply
          else
            let word (shift : int) = int (uint32 ((magnitude >>> shift) &&& mask32))
            System.Decimal(
              word 0,
              word 32,
              word 64,
              unscaled.Sign < 0,
              byte (int scale)
            )
            |> DDecimal
            |> Dval.optionSome KTDecimal
            |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
