module LibExecutionStdLib.LibInt

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "Int" "mod" 0
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description =
        "Returns the result of wrapping `a` around so that `0 <= res < b`.
         The modulus `b` must be 0 or negative.
         Use `Int::remainder` if you want the remainder after division, which has a different behavior for negative numbers."
      fn =
        (function
        | state, [ DInt v; DInt m as mdv ] ->
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
      // TODO: Deprecate this when we can version infix operators and when infix operators support Result return types.
      // The current function returns DError (it used to rollbar) on negative `b`.
      deprecated = NotDeprecated }
    (*  (* See above for when to uncomment this *)
  ; { name = fn "Int" "mod" 1
    ; infix_names = ["%_v1"]
    ; parameters = [Param.make "value" TInt ""; Param.make "modulus" TInt ""]
    ; returnType = TResult
    ; description =
        "Returns the result of wrapping `value` around so that `0 <= res < modulus`, as a Result.
         If `modulus` is positive, returns `Ok res`. Returns an `Error` if `modulus` is 0 or negative.
         Use `Int::remainder` if you want the remainder after division, which has a different behavior for negative numbers."
    ; fn =
        (* TODO: A future version should support all non-zero modulus values and should include the infix "%" *)

          (function
          | _, [DInt v; DInt m] ->
            ( try DResult (Ok (DInt (Dint.modulo_exn v m)))
              with e ->
                if m <= Dint.of_int 0
                then
                  DResult
                    (Error
                       (DStr
                          ( "`modulus` must be positive but was "
                          ^ Dval.to_developer_repr_v0 (DInt m) )))
                else (* In case there's another failure mode, rollbar *)
                  raise e )
          | _ ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
    ; previewable = Pure
    ; deprecated = NotDeprecated } *)


    { name = fn "Int" "remainder" 0
      parameters = [ Param.make "value" TInt ""; Param.make "divisor" TInt "" ]
      returnType = TResult(TInt, TStr)
      description =
        "Returns the integer remainder left over after dividing `value` by `divisor`, as a Result.
          For example, `Int::remainder 15 6 == Ok 3`. The remainder will be negative only if `value < 0`.
          The sign of `divisor` doesn't influence the outcome.
          Returns an `Error` if `divisor` is 0."
      fn =
        (function
        | _, [ DInt v; DInt d ] ->
          (try
            v % d |> DInt |> Ok |> DResult |> Ply
           with
           | e ->
             if d = 0L then
               Ply(DResult(Error(DStr($"`divisor` must be non-zero"))))

             else // In case there's another failure mode, rollbar
               raise e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
      description = "Raise `base` to the power of `exponent`"
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
        "Returns the absolute value of `a` (turning negative inputs into positive outputs)."
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
      description = "Returns the negation of `a`, `-a`."
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
      description = "Returns true if a is greater than b"
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
      description = "Returns true if a is greater than or equal to b"
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
      description = "Returns true if a is less than b"
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
      description = "Returns true if a is less than or equal to b"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Int" "random" 0
      parameters = [ Param.make "start" TInt ""; Param.make "end" TInt "" ]
      returnType = TInt
      description = "Returns a random integer between a and b (inclusive)"
      fn =
        (function
        | _, [ DInt a; DInt b ] -> a + randomSeeded().NextInt64(b - a) |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "Int" "random" 1) }


    { name = fn "Int" "random" 1
      parameters = [ Param.make "start" TInt ""; Param.make "end" TInt "" ]
      returnType = TInt
      description = "Returns a random integer between `start` and `end` (inclusive)."
      fn =
        (function
        | _, [ DInt a; DInt b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          lower + randomSeeded().NextInt64(upper - lower) |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Int" "sqrt" 0
      parameters = [ Param.make "a" TInt "" ]
      returnType = TFloat
      description = "Get the square root of an Int"
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
      description = "Converts an Int to a Float"
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
                | t -> Errors.throw (Errors.argumentWasnt "a list of ints" "a" ldv))
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
      description = "Returns the higher of a and b"
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
      description = "Returns the lower of `a` and `b`"
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
        "If `value` is within the range given by `limitA` and `limitB`, returns `value`.
   If `value` is outside the range, returns `limitA` or `limitB`, whichever is closer to `value`.
   `limitA` and `limitB` can be provided in any order."
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
      deprecated = NotDeprecated } ]
