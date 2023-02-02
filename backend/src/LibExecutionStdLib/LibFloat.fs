module LibExecutionStdLib.LibFloat

open LibExecution.RuntimeTypes
open Prelude
open System

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ { name = fn "Float" "ceiling" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description = "Round up to an integer value"
      fn =
        (function
        | _, [ DFloat a ] -> a |> Math.Ceiling |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "roundUp" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description = "Round up to an integer value"
      fn =
        (function
        | _, [ DFloat a ] -> a |> Math.Ceiling |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "floor" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Round down to an integer value.

        Consider <fn Float::truncate> if your goal
        is to discard the fractional part of a number: {{Float::floor -1.9 == -2.0}}
        but {{Float::truncate -1.9 == -1.0}}"
      fn =
        (function
        | _, [ DFloat a ] -> a |> Math.Floor |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "roundDown" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Round down to an integer value.

         Consider <fn Float::truncate> if your goal is to discard the fractional part
         of a number: {{Float::floor -1.9 == -2.0}} but {{Float::truncate -1.9 ==
         -1.0}}"

      fn =
        (function
        | _, [ DFloat a ] -> a |> Math.Floor |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "round" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description = "Round to the nearest integer value"
      fn =
        (function
        | _, [ DFloat a ] -> a |> Math.Round |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "truncate" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Discard the fractional portion of the float, rounding towards zero"
      fn =
        (function
        | _, [ DFloat a ] -> a |> Math.Truncate |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "absoluteValue" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the absolute value of <param a> (turning negative inputs into positive outputs)"
      fn =
        (function
        | _, [ DFloat a ] -> DFloat(Math.Abs a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "negate" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TFloat
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, [ DFloat a ] -> DFloat(a * -1.0) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "sqrt" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TFloat
      description = "Get the square root of a float"
      fn =
        (function
        | _, [ DFloat a ] -> Ply(DFloat(Math.Sqrt a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "power" 0
      parameters = [ Param.make "base" TFloat ""; Param.make "exponent" TFloat "" ]
      returnType = TFloat
      description = "Returns <param base> raised to the power of <param exponent>"
      fn =
        (function
        | _, [ DFloat base_; DFloat exp ] -> Ply(DFloat(base_ ** exp))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "^"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "divide" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Divide <type float> <param a> by <type float> <param b>"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DFloat(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "add" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Add <type float> <param a> to <type float> <param b>"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DFloat(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "multiply" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Multiply <type float> <param a> by <type float> <param b>"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DFloat(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "*"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "subtract" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description = "Subtract <type float> <param b> from <type float> <param a>"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DFloat(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "greaterThan" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is greater than b"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "greaterThanOrEqualTo" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is greater than b"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "lessThan" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is less than b"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "lessThanOrEqualTo" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TBool
      description = "Returns true if a is less than b"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "sum" 0
      parameters = [ Param.make "a" (TList TFloat) "" ]
      returnType = TFloat
      description = "Returns the sum of all the floats in the list"
      fn =
        (function
        | _, [ DList l as ldv ] ->
          let floats =
            List.map
              (fun f ->
                match f with
                | DFloat ft -> ft
                | t ->
                  Exception.raiseCode (
                    Errors.argumentWasnt "a list of floats" "a" ldv
                  ))
              l

          let sum = List.fold (fun acc elem -> acc + elem) 0.0 floats
          Ply(DFloat sum)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "min" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the lesser of <type float> <param a> and <type float> <param b>"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DFloat(Math.Min(a, b)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "max" 0
      parameters = [ Param.make "a" TFloat ""; Param.make "b" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the greater of <type float> <param a> and <type float> <param b>"
      fn =
        (function
        | _, [ DFloat a; DFloat b ] -> Ply(DFloat(Math.Max(a, b)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "clamp" 0
      parameters =
        [ Param.make "value" TFloat ""
          Param.make "limitA" TFloat ""
          Param.make "limitB" TFloat "" ]
      returnType = TFloat
      description =
        "If <param value> is within the range given by <param limitA> and <param
         limitB>, returns <param value>.

         If <param value> is outside the range, returns <param limitA> or <param
         limitB>, whichever is closer to <param value>.

         <param limitA> and <param limitB> can be provided in any order."
      fn =
        (function
        | _, [ DFloat v; DFloat a; DFloat b ] ->
          if System.Double.IsNaN a || System.Double.IsNaN b then
            Ply(DError(SourceNone, "clamp requires arguments to be valid numbers"))
          else
            let min, max = if a < b then (a, b) else (b, a)
            Ply(DFloat(Math.Clamp(v, min, max)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Float" "roundTowardsZero" 0
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TInt
      description =
        "Discard the fractional portion of <type float> <param a>, rounding towards zero."
      fn =
        (function
        | _, [ DFloat a ] -> a |> Math.Truncate |> int64 |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Float" "parse" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TFloat, TStr)
      description = "Returns the <type float> value of the <type string>"
      fn =
        (function
        | _, [ DStr s ] ->
          (try
            float (s) |> DFloat |> Ok |> DResult |> Ply
           with
           | e ->
             "Expected a string representation of an IEEE float"
             |> DStr
             |> Error
             |> DResult
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
