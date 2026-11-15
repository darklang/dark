/// The Decimal builtins Dark cannot do itself: taking a decimal apart into an integer and a
/// scale, putting one back together, and arithmetic that returns an Error past the range
/// rather than raising. Parsing, printing and rounding are written in Dark on top of these; the
/// operators and ordering are the polymorphic ones in `NoModule`.
module Builtins.Pure.Libs.Decimal

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution


/// 2^96, one past the largest magnitude a decimal's 96-bit integer holds.
let private magnitudeLimit = bigint.Pow(bigint 2, 96)

let private mask32 = bigint 0xFFFFFFFFu


/// <param op> as `+`, `-`, `*` or `/` compute it, or where they raise, an Error of
/// <param errorType>: Overflow past the range, or DivisionByZero.
let private checkedOp
  (name : string)
  (what : string)
  (errorType : unit -> string)
  (op : decimal -> decimal -> decimal)
  : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters = [ Param.make "a" TDecimal ""; Param.make "b" TDecimal "" ]
    returnType =
      TypeReference.result
        TDecimal
        (TCustomType(NR.ok (FQTypeName.fqPackage (errorType ())), []))
    description = $"{what}, or an {{{{Error}}}} where the result is out of range"
    fn =
      (function
      | _, _, _, [| DDecimal a; DDecimal b |] ->
        let typeName = FQTypeName.fqPackage (errorType ())
        let errorKT = KTCustomType(typeName, [])
        let error caseName =
          DEnum(typeName, typeName, [], caseName, [])
          |> Dval.resultError KTDecimal errorKT
        let result =
          try
            DDecimal(op a b) |> Dval.resultOk KTDecimal errorKT
          with
          | :? System.OverflowException -> error "Overflow"
          | :? System.DivideByZeroException -> error "DivisionByZero"
        Ply result
      | _ -> incorrectArgs ())
    previewable = Pure
    callEffects = Set.empty
    deprecated = NotDeprecated }


let private overflow = PackageRefs.Type.Stdlib.Math.overflow
let private divisionError = PackageRefs.Type.Stdlib.Math.divisionError


let fns () : List<BuiltInFn> =
  [ { name = fn "decimalToParts" 0
      typeParams = []
      parameters = [ Param.make "d" TDecimal "" ]
      returnType = TTuple(TBigInt, TInt64, [])
      description =
        "Split <param d> into {{(unscaled, scale)}}, where <param d> is {{unscaled / 10^scale}}. "
        + "The scale is as written, so {{1.10dec}} is {{(110z, 2)}} and {{1.1dec}} is {{(11z, 1)}}."
      fn =
        (function
        | _, _, _, [| DDecimal d |] ->
          let bits = System.Decimal.GetBits d
          let magnitude =
            (bigint (uint32 bits[2]) <<< 64)
            ||| (bigint (uint32 bits[1]) <<< 32)
            ||| bigint (uint32 bits[0])
          let unscaled = if d < 0m then -magnitude else magnitude
          DTuple(Dval.bigInt unscaled, Dval.dint64 (int64 d.Scale), []) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "decimalFromParts" 0
      typeParams = []
      parameters = [ Param.make "unscaled" TBigInt ""; Param.make "scale" TInt64 "" ]
      returnType = TypeReference.option TDecimal
      description =
        "The decimal {{<param unscaled> / 10^<param scale>}}, keeping <param scale>. {{None}} "
        + "when <param scale> is outside 0 to 28 or <param unscaled> needs more than 96 bits."
      fn =
        (function
        | _, _, _, [| DBigInt unscaled; DInt64 scale |] ->
          let unscaled = DarkBigInt.toBigInt unscaled
          let magnitude = bigint.Abs unscaled
          if scale < 0L || scale > 28L || magnitude >= magnitudeLimit then
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
      deprecated = NotDeprecated }

    checkedOp "decimalCheckedAdd" "<param a> plus <param b>" overflow (+)
    checkedOp "decimalCheckedSubtract" "<param a> minus <param b>" overflow (-)
    checkedOp "decimalCheckedMultiply" "<param a> times <param b>" overflow (*)
    checkedOp
      "decimalCheckedDivide"
      "<param a> divided by <param b>"
      divisionError
      (/) ]

let builtins () = LibExecution.Builtin.make [] (fns ())
