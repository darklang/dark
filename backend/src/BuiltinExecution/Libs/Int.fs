module BuiltinExecution.Libs.Int

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

/// Used for values which are outside the range of expected values for some
/// reason. Really, any function using this should have a Result type instead.
let argumentWasntPositive (paramName : string) (dv : Dval) : string =
  let actual = LibExecution.DvalReprDeveloper.toRepr dv
  $"Expected `{paramName}` to be positive, but it was `{actual}`"

module IntRuntimeError =
  type Error =
    | DivideByZeroError
    | OutOfRange
    | NegativeExponent
    | NegativeModulus
    | ZeroModulus

  module RTE =
    let toRuntimeError (e : Error) : RuntimeError =
      let (caseName, fields) =
        match e with
        | DivideByZeroError -> "DivideByZeroError", []
        | OutOfRange -> "OutOfRange", []
        | NegativeExponent -> "NegativeExponent", []
        | NegativeModulus -> "NegativeModulus", []
        | ZeroModulus -> "ZeroModulus", []

      let typeName = RuntimeError.name [ "Int" ] "Error" 0

      DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError.intError


module IntParseError =
  type IntParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : IntParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName =
      TypeName.fqPackage "Darklang" [ "Stdlib"; "Int" ] "IntParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fn = fn [ "Int" ]

let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

         The modulus <param b> must be greater than 0.

         Use <fn Int.remainder> if you want the remainder after division, which has
         a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DInt v; DInt m ] ->
          if m = 0L then
            IntRuntimeError.Error.ZeroModulus
            |> IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else if m < 0L then
            IntRuntimeError.Error.NegativeModulus
            |> IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < 0L then m + result else result
            Ply(DInt(result))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "%"
      previewable = Pure
      // TODO: Deprecate this when we can version infix operators and when infix operators support Result return types (https://github.com/darklang/dark/issues/4267)
      // The current function returns an RTE (it used to rollbar) on negative `b`.
      deprecated = NotDeprecated }


    // See above for when to uncomment this
    // TODO: A future version should support all non-zero modulus values and should include the infix "%"
    // { name = fn "mod" 0
    //   parameters = [ Param.make "value" TInt ""; Param.make "modulus" TInt "" ]
    //   returnType = TypeReference.result TInt TString
    //   description =
    //     "Returns the result of wrapping <param value> around so that {{0 <= res < modulus}}, as a <type Result>.
    //      If <param modulus> is positive, returns {{Ok res}}. Returns an {{Error}} if <param modulus> is {{0}} or negative.
    //     Use <fn Int.remainder> if you want the remainder after division, which has a different behavior for negative numbers."
    //   fn =
    //     (function
    //     | _, [ DInt v; DInt m ] ->
    //       (try
    //         Ply(Dval.resultOk(DInt(v % m)))
    //        with
    //        | e ->
    //          if m <= 0L then
    //            Ply(
    //              DResult(
    //                Error(
    //                  DString(
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


    { name = fn "remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt ""; Param.make "divisor" TInt "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
         <param divisor>, as a <type Result>.

         For example, {{Int.remainder 15 6 == Ok 3}}. The remainder will be
         negative only if {{<var value> < 0}}.

         The sign of <param divisor> doesn't influence the outcome.

         Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt KTString r |> Ply
        (function
        | state, _, [ DInt v; DInt d ] ->
          (try
            v % d |> DInt |> resultOk
           with e ->
             if d = 0L then
               IntRuntimeError.Error.DivideByZeroError
               |> IntRuntimeError.RTE.toRuntimeError
               |> raiseRTE state.caller
               |> Ply
             else
               Exception.raiseInternal
                 "unexpected failure case in Int.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Adds two integers together"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] -> Ply(DInt(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Subtracts two integers"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] -> Ply(DInt(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Multiplies two integers"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] -> Ply(DInt(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "*"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt ""; Param.make "exponent" TInt "" ]
      returnType = TInt
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | state, _, [ DInt number; DInt exp ] ->
          (try
            if exp < 0L then
              IntRuntimeError.Error.NegativeExponent
              |> IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.caller
              |> Ply
            else
              (bigint number) ** (int exp) |> int64 |> DInt |> Ply
           with :? System.OverflowException ->
             IntRuntimeError.Error.OutOfRange
             |> IntRuntimeError.RTE.toRuntimeError
             |> raiseRTE state.caller
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "^"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Divides two integers"
      fn =
        (function
        | state, _, [ DInt a; DInt b ] ->
          if b = 0L then
            IntRuntimeError.Error.DivideByZeroError
            |> IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DInt(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TInt
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, [ DInt a ] -> Ply(DInt(-a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt ""; Param.make "end" TInt "" ]
      returnType = TInt
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DInt a; DInt b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          // .NET's "nextInt64" is exclusive,
          // but we'd rather an inclusive version of this function
          let correction : int64 = 1

          lower + randomSeeded().NextInt64(upper - lower + correction) |> DInt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int>"
      fn =
        (function
        | _, _, [ DInt a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TFloat
      description = "Converts an <type Int> to a <type Float>"
      fn =
        (function
        | _, _, [ DInt a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TInt
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "Int" ]
                  name = TypeName.TypeName "IntParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type Int> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let typeName = RuntimeError.name [ "Int" ] "IntParseError" 0
        let resultError = Dval.resultError KTInt (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Convert.ToInt64 |> DInt |> resultOk |> Ply
          with
          | :? System.FormatException ->
            IntParseError.BadFormat |> IntParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            IntParseError.OutOfRange |> IntParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, [ DInt int ] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
