module LibExecutionStdLib.LibInt

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "Int" "mod" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

         The modulus <param b> must be 0 or negative.

         Use <fn Int::remainder> if you want the remainder after division, which has
         a different behavior for negative numbers."
      fn =
        (function
        | _, [ DInt v; DInt m as mdv ] ->
          if m <= 0L then
            err (Errors.argumentWasnt "positive" "b" mdv)
          else
            // dotnet returns negative mods, but OCaml did positive ones
            let result = v % m
            let result = if result < 0L then m + result else result
            Ply(DInt(result))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "%"
      previewable = Pure
      // TODO: Deprecate this when we can version infix operators and when infix operators support Result return types (https://github.com/darklang/dark/issues/4267)
      // The current function returns DError (it used to rollbar) on negative `b`.
      deprecated = NotDeprecated }


    // See above for when to uncomment this
    // TODO: A future version should support all non-zero modulus values and should include the infix "%"
    // { name = fn "Int" "mod" 1
    //   parameters = [ Param.make "value" TInt ""; Param.make "modulus" TInt "" ]
    //   returnType = TResult(TInt, TStr)
    //   description =
    //     "Returns the result of wrapping <param value> around so that {{0 <= res < modulus}}, as a <type Result>.
    //      If <param modulus> is positive, returns {{Ok res}}. Returns an {{Error}} if <param modulus> is {{0}} or negative.
    //     Use <fn Int::remainder> if you want the remainder after division, which has a different behavior for negative numbers."
    //   fn =
    //     (function
    //     | _, [ DInt v; DInt m ] ->
    //       (try
    //         Ply(DResult(Ok(DInt(v % m))))
    //        with
    //        | e ->
    //          if m <= 0L then
    //            Ply(
    //              DResult(
    //                Error(
    //                  DStr(
    //                    "`modulus` must be positive but was "
    //                    + LibExecution.DvalReprDeveloper.toRepr (DInt m)
    //                  )
    //                )
    //              )
    //            )
    //          else // In case there's another failure mode, rollbar
    //            Exception.raiseInternal "Unexpected failiure mode" [] e)
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplemented
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    { name = fn "Int" "remainder" 0
      parameters = [ Param.make "value" TInt ""; Param.make "divisor" TInt "" ]
      returnType = TResult(TInt, TStr)
      description =
        "Returns the integer remainder left over after dividing <param value> by
         <param divisor>, as a <type Result>.

         For example, {{Int::remainder 15 6 == Ok 3}}. The remainder will be
         negative only if {{<var value> < 0}}.

         The sign of <param divisor> doesn't influence the outcome.

         Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        (function
        | _, [ DInt v; DInt d ] ->
          (try
            v % d |> DInt |> Ok |> DResult |> Ply
           with
           | e ->
             if d = 0L then
               Ply(DResult(Error(DStr($"`divisor` must be non-zero"))))
             else
               Exception.raiseInternal
                 "unexpected failure case in Int::remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "add" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Adds two integers together"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DInt(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "subtract" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Subtracts two integers"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DInt(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "multiply" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Multiplies two integers"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DInt(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "*"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "power" 0
      parameters = [ Param.make "base" TInt ""; Param.make "exponent" TInt "" ]
      returnType = TInt
      description = "Raise <param base> to the power of <param exponent>"
      fn =
        (function
        | _, [ DInt number; DInt exp as expdv ] ->
          (try
            if exp < 0L then
              err (Errors.argumentWasnt "positive" "exponent" expdv)
            // Handle some edge cases around 1. We want to make this match
            // OCaml, so we have to support an exponent above int32, but
            // below int63. This only matters for 1 or -1, and otherwise a
            // number raised to an int63 exponent wouldn't fit in an int63
            else if number = 1L then
              Ply(DInt(1L))
            else if number = -1L && exp % 2L = 0L then
              Ply(DInt(1L))
            else if number = -1L then
              Ply(DInt(-1L))
            else
              (bigint number) ** (int exp) |> int64 |> DInt |> Ply
           with
           | _ -> err "Error raising to exponent")
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "^"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "divide" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Divides two integers"
      fn =
        (function
        | _, [ DInt a; DInt b ] ->
          if b = 0L then err "Division by zero" else Ply(DInt(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "absoluteValue" 0
      parameters = [ Param.make "a" TInt "" ]
      returnType = TInt
      description =
        "Returns the absolute value of <param a> (turning negative inputs into positive outputs)"
      fn =
        (function
        | _, [ DInt a ] -> Ply(DInt(abs a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "negate" 0
      parameters = [ Param.make "a" TInt "" ]
      returnType = TInt
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, [ DInt a ] -> Ply(DInt(-a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "greaterThan" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "greaterThanOrEqualTo" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "lessThan" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "lessThanOrEqualTo" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "random" 1
      parameters = [ Param.make "start" TInt ""; Param.make "end" TInt "" ]
      returnType = TInt
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, [ DInt a; DInt b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          // The next line is a fix to correct work when an upper bound equal '1'.
          // It is need because System.Random.NexInt64(Int64) _always_ returns 0 for 1
          // line from the official doc: "The _exclusive_ upper bound of the random number to be generated"
          let correction : int64 = if upper = 1 then 2 else 0
          lower + randomSeeded().NextInt64(upper - lower + correction) |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Int" "sqrt" 0
      parameters = [ Param.make "a" TInt "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int>"
      fn =
        (function
        | _, [ DInt a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "toFloat" 0
      parameters = [ Param.make "a" TInt "" ]
      returnType = TFloat
      description = "Converts an <type Int> to a <type Float>"
      fn =
        (function
        | _, [ DInt a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "sum" 0
      parameters = [ Param.make "a" (TList TInt) "" ]
      returnType = TInt
      description = "Returns the sum of all the ints in the list"
      fn =
        (function
        | _, [ DList l as ldv ] ->
          let ints =
            List.map
              (fun i ->
                match i with
                | DInt it -> it
                | _ ->
                  Exception.raiseCode (Errors.argumentWasnt "a list of ints" "a" ldv))
              l

          let sum = List.fold (fun acc elem -> acc + elem) 0L ints
          Ply(DInt sum)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "max" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Returns the higher of <param a> and <param b>"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DInt(max a b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "min" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Returns the lower of <param a> and <param b>"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DInt(min a b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "clamp" 0
      parameters =
        [ Param.make "value" TInt ""
          Param.make "limitA" TInt ""
          Param.make "limitB" TInt "" ]
      returnType = TInt
      description =
        "If <param value> is within the range given by <param limitA> and <param
         limitB>, returns <param value>.

         If <param value> is outside the range, returns <param limitA> or <param
         limitB>, whichever is closer to <param value>.

         <param limitA> and <param limitB> can be provided in any order."
      fn =
        (function
        | _, [ DInt v; DInt a; DInt b ] ->
          let min, max = if a < b then (a, b) else (b, a)

          if v < min then Ply(DInt min)
          else if v > max then Ply(DInt max)
          else Ply(DInt v)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "parse" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TInt, TStr)
      description = "Returns the <type int> value of a <type string>"
      fn =
        (function
        | _, [ DStr s ] ->
          (try
            s |> System.Convert.ToInt64 |> DInt |> Ok |> DResult |> Ply
           with
           | _e ->
             $"Expected to parse string with only numbers, instead got \"{s}\""
             |> DStr
             |> Error
             |> DResult
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "toString" 0
      parameters = [ Param.make "int" TInt "" ]
      returnType = TStr
      description = "Stringify <param int>"
      fn =
        (function
        | _, [ DInt int ] -> Ply(DStr(string int))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    ]
