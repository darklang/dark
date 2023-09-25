module BuiltinExecution.Libs.Float

open System

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Float" ]

let fns : List<BuiltInFn> =
  [ { name = fn "ceiling" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description = "Round up to an integer value"
      fn =
        (function
        | _, _, [ DFloat a ] -> a |> Math.Ceiling |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "roundUp" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description = "Round up to an integer value"
      fn =
        (function
        | _, _, [ DFloat a ] -> a |> Math.Ceiling |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "floor" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Round down to an integer value.

        Consider <fn Float.truncate> if your goal
        is to discard the fractional part of a number: {{Float.floor -1.9 == -2.0}}
        but {{Float.truncate -1.9 == -1.0}}"
      fn =
        (function
        | _, _, [ DFloat a ] -> a |> Math.Floor |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "roundDown" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Round down to an integer value.

         Consider <fn Float.truncate> if your goal is to discard the fractional part
         of a number: {{Float.floor -1.9 == -2.0}} but {{Float.truncate -1.9 ==
         -1.0}}"

      fn =
        (function
        | _, _, [ DFloat a ] -> a |> Math.Floor |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "round" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description = "Round to the nearest integer value"
      fn =
        (function
        | _, _, [ DFloat a ] -> a |> Math.Round |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "truncate" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Discard the fractional portion of the float, rounding towards zero"
      fn =
        (function
        | _, _, [ DFloat a ] -> a |> Math.Truncate |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TFloat
      description = "Get the square root of a float"
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(Math.Sqrt a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "power" 0
      typeParams = []
      parameters = [ Param.make "base" TFloat ""; Param.make "exponent" TFloat "" ]
      returnType = TFloat
      description = "Returns <param base> raised to the power of <param exponent>"
      fn =
        (function
        | _, _, [ DFloat base_; DFloat exp ] -> Ply(DFloat(base_ ** exp))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "^"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Divide <type Float> <param a> by <type Float> <param b>"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Add <type Float> <param a> to <type Float> <param b>"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Multiply <type Float> <param a> by <type Float> <param b>"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "*"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Subtract <type Float> <param b> from <type Float> <param a>"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is greater than b"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is greater than or equal to b"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is less than b"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is less than or equal to b"
      fn =
        (function
        | _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "roundTowardsZero" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Discard the fractional portion of <type Float> <param a>, rounding towards zero."
      fn =
        (function
        | _, _, [ DFloat a ] -> a |> Math.Truncate |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TFloat TString
      description =
        "Returns the <type Float> value wrapped in a {{Result}} of the <type String>"
      fn =
        let resultOk = Dval.resultOk VT.float VT.string
        let resultError = Dval.resultError VT.float VT.string
        (function
        | _, _, [ DString s ] ->
          (try
            float (s) |> DFloat |> resultOk
           with e ->
             "Expected a String representation of an IEEE float"
             |> DString
             |> resultError)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "f" TFloat "" ]
      returnType = TString
      description = "Stringify <param float>"
      fn =
        (function
        | _, _, [ DFloat f ] ->
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


    { name = fn "isNan" 0
      typeParams = []
      parameters = [ Param.make "f" TFloat "" ]
      returnType = TBool
      description = "Returns true if <param f> is NaN"
      fn =
        (function
        | _, _, [ DFloat f ] -> Ply(DBool(System.Double.IsNaN f))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
