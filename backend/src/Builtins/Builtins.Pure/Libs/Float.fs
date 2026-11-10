module Builtins.Pure.Libs.Float

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
  [ { name = fn "floatCeiling" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description = "Round up to an integer value"
      fn =
        (function
        | _, vm, _, [| DFloat a |] -> a |> System.Math.Ceiling |> roundedToInt64 vm
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "floatFloor" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description =
        "Round down to an integer value.\n\nConsider <fn Float.truncate> if "
        + "your goal is to discard the fractional part of a number: "
        + "{{Float.floor(-1.9) == -2}} but {{Float.truncate(-1.9) == -1}}"
      fn =
        (function
        | _, vm, _, [| DFloat a |] -> a |> System.Math.Floor |> roundedToInt64 vm
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "floatRound" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description = "Round to the nearest integer value"
      fn =
        (function
        | _, vm, _, [| DFloat a |] -> a |> System.Math.Round |> roundedToInt64 vm
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "floatTruncate" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description =
        "Discard the fractional portion of the float, rounding towards zero"
      fn =
        (function
        | _, vm, _, [| DFloat a |] -> a |> System.Math.Truncate |> roundedToInt64 vm
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "floatSqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TFloat
      description = "Get the square root of a float"
      fn =
        (function
        | _, _, _, [| DFloat a |] -> Ply(DFloat(System.Math.Sqrt a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "floatParse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TFloat
          (TCustomType(
            { originalName = []
              resolved =
                Ok(FQTypeName.fqPackage (PackageRefs.Type.Stdlib.floatParseError ())) },
            []
          ))
      description =
        "Returns the <type Float> value wrapped in a {{Result}} of the <type String>"
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.floatParseError ())
        let resultOk = Dval.resultOk KTFloat (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTFloat (KTCustomType(typeName, []))
        (function
        | _, _, _, [| DString s |] ->
          try
            float (s) |> DFloat |> resultOk |> Ply
          with :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "floatToString" 0
      typeParams = []
      parameters = [ Param.make "f" TFloat "" ]
      returnType = TString
      description =
        "Stringify <param float> as the shortest decimal that parses back to the same value, e.g. `1.5`, `0.30000000000000004`, `1e+16`, `1e-5`."
      fn =
        (function
        | _, _, _, [| DFloat f |] ->
          // TODO add tests from DvalRepr.Tests
          let result =
            if System.Double.IsPositiveInfinity f then "Infinity"
            else if System.Double.IsNegativeInfinity f then "-Infinity"
            else if System.Double.IsNaN f then "NaN"
            else floatToShortestString f
          Ply(DString result)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "floatToBits" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TUInt64
      description =
        "Return the IEEE 754 binary64 bit pattern of <param a> as an unsigned 64-bit value: "
        + "the sign in bit 63, then 11 bits of exponent, then 52 of significand. Tells "
        + "apart what {{==}} cannot -- {{0.0}} and {{-0.0}} have different patterns, and "
        + "every NaN has a pattern despite equalling nothing, itself included."
      fn =
        (function
        | _, _, _, [| DFloat a |] ->
          System.BitConverter.DoubleToUInt64Bits a |> DUInt64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
