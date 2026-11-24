module Builtins.Pure.Libs.Float64

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs


module ParseError =
  type ParseError = | BadFormat

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.floatParseError ())
    DEnum(typeName, typeName, [], caseName, fields)


let fns () : List<BuiltInFn> =
  [ { name = fn "float64Ceiling" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TInt64
      description = "Round up to an integer value"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> a |> System.Math.Ceiling |> roundedToInt64
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "float64Floor" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TInt64
      description =
        "Round down to an integer value.\n\nConsider <fn Float64.truncate> if "
        + "your goal is to discard the fractional part of a number: "
        + "{{Float64.floor(-1.9) == -2}} but {{Float64.truncate(-1.9) == -1}}"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> a |> System.Math.Floor |> roundedToInt64
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "float64Round" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TInt64
      description = "Round to the nearest integer value"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> a |> System.Math.Round |> roundedToInt64
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "float64Truncate" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TInt64
      description =
        "Discard the fractional portion of the float, rounding towards zero"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> a |> System.Math.Truncate |> roundedToInt64
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "float64Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TFloat64
      description = "Get the square root of a float"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> (DFloat64(System.Math.Sqrt a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float64Log" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TFloat64
      description = "The natural logarithm of a float"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> DFloat64(System.Math.Log a)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float64Log10" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TFloat64
      description = "The base-10 logarithm of a float"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> DFloat64(System.Math.Log10 a)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float64Exp" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TFloat64
      description = "e raised to the power of a float"
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> DFloat64(System.Math.Exp a)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float64Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TFloat64
          (TCustomType(
            { originalName = []
              resolved =
                Ok(FQTypeName.fqPackage (PackageRefs.Type.Stdlib.floatParseError ())) },
            []
          ))
      description =
        "Returns the <type Float64> value wrapped in a {{Result}} of the <type String>"
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.floatParseError ())
        let resultOk = Dval.resultOk KTFloat64 (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTFloat64 (KTCustomType(typeName, []))
        (function
        | _, _, _, [| DString s |] ->
          try
            float (s) |> DFloat64 |> resultOk
          with :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float64ToString" 0
      typeParams = []
      parameters = [ Param.make "f" TFloat64 "" ]
      returnType = TString
      description =
        "Stringify <param float> as the shortest decimal that parses back to the same value, e.g. `1.5`, `0.30000000000000004`, `1e+16`, `1e-5`."
      fn =
        (function
        | _, _, _, [| DFloat64 f |] -> (DString(Dval.float64ToString f))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float64ToBits" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TUInt64
      description =
        "Return the IEEE 754 binary64 bit pattern of <param a> as an unsigned 64-bit value: "
        + "the sign in bit 63, then 11 bits of exponent, then 52 of significand. Tells "
        + "apart what {{==}} cannot -- {{0.0}} and {{-0.0}} have different patterns, and "
        + "every NaN has a pattern despite equalling nothing, itself included."
      fn =
        (function
        | _, _, _, [| DFloat64 a |] ->
          System.BitConverter.DoubleToUInt64Bits a |> DUInt64
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float64FromBits" 0
      typeParams = []
      parameters = [ Param.make "bits" TUInt64 "" ]
      returnType = TFloat64
      description =
        "The Float64 whose IEEE 754 binary64 bit pattern is <param bits>, the inverse of "
        + "float64ToBits"
      fn =
        (function
        | _, _, _, [| DUInt64 bits |] ->
          System.BitConverter.UInt64BitsToDouble bits |> DFloat64
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
