module BuiltinExecution.Libs.Float

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs


module ParseError =
  type ParseError = | BadFormat

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []

    let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.floatParseError
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "floatCeiling" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description = "Round up to an integer value"
      fn =
        (function
        | _, _, _, [ DFloat a ] -> a |> System.Math.Ceiling |> int64 |> DInt64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatRoundUp" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description = "Round up to an integer value"
      fn =
        (function
        | _, _, _, [ DFloat a ] -> a |> System.Math.Ceiling |> int64 |> DInt64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatFloor" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description =
        "Round down to an integer value.

        Consider <fn Float.truncate> if your goal
        is to discard the fractional part of a number: {{Float.floor -1.9 == -2.0}}
        but {{Float.truncate -1.9 == -1.0}}"
      fn =
        (function
        | _, _, _, [ DFloat a ] -> a |> System.Math.Floor |> int64 |> DInt64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatRoundDown" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description =
        "Round down to an integer value.

         Consider <fn Float.truncate> if your goal is to discard the fractional part
         of a number: {{Float.floor -1.9 == -2.0}} but {{Float.truncate -1.9 ==
         -1.0}}"

      fn =
        (function
        | _, _, _, [ DFloat a ] -> a |> System.Math.Floor |> int64 |> DInt64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatRound" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description = "Round to the nearest integer value"
      fn =
        (function
        | _, _, _, [ DFloat a ] -> a |> System.Math.Round |> int64 |> DInt64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatTruncate" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description =
        "Discard the fractional portion of the float, rounding towards zero"
      fn =
        (function
        | _, _, _, [ DFloat a ] ->
          a |> System.Math.Truncate |> int64 |> DInt64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatSqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TFloat
      description = "Get the square root of a float"
      fn =
        (function
        | _, _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Sqrt a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatPower" 0
      typeParams = []
      parameters = [ Param.make "base" TFloat ""; Param.make "exponent" TFloat "" ]
      returnType = TFloat
      description = "Returns <param base> raised to the power of <param exponent>"
      fn =
        (function
        | _, _, _, [ DFloat base_; DFloat exp ] -> Ply(DFloat(base_ ** exp))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "^"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatDivide" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Divide <type Float> <param a> by <type Float> <param b>"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatAdd" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Add <type Float> <param a> to <type Float> <param b>"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatMultiply" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Multiply <type Float> <param a> by <type Float> <param b>"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "*"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatSubtract" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Subtract <type Float> <param b> from <type Float> <param a>"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatGreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is greater than b"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatGreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is greater than or equal to b"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatLessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is less than b"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatLessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is less than or equal to b"
      fn =
        (function
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatRoundTowardsZero" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt64
      description =
        "Discard the fractional portion of <type Float> <param a>, rounding towards zero."
      fn =
        (function
        | _, _, _, [ DFloat a ] ->
          a |> System.Math.Truncate |> int64 |> DInt64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatParse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TFloat
          (TCustomType(
            Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.floatParseError),
            []
          ))
      description =
        "Returns the <type Float> value wrapped in a {{Result}} of the <type String>"
      fn =
        let resultOk r = Dval.resultOk KTFloat KTString r |> Ply
        let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.floatParseError
        let resultError = Dval.resultError KTFloat (KTCustomType(typeName, []))
        (function
        | _, _, _, [ DString s ] ->
          try
            float (s) |> DFloat |> resultOk
          with :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatToString" 0
      typeParams = []
      parameters = [ Param.make "f" TFloat "" ]
      returnType = TString
      description = "Stringify <param float>"
      fn =
        (function
        | _, _, _, [ DFloat f ] ->
          // TODO add tests from DvalRepr.Tests
          let result =
            if System.Double.IsPositiveInfinity f then
              "Infinity"
            else if System.Double.IsNegativeInfinity f then
              "-Infinity"
            else if System.Double.IsNaN f then
              "NaN"
            else
              let result = sprintf "%.12g" f
              if result.Contains "." then result else $"{result}.0"
          Ply(DString result)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floatIsNaN" 0
      typeParams = []
      parameters = [ Param.make "f" TFloat "" ]
      returnType = TBool
      description = "Returns true if <param f> is NaN"
      fn =
        (function
        | _, _, _, [ DFloat f ] -> Ply(DBool(System.Double.IsNaN f))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
