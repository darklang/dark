module BuiltinExecution.Libs.Int64

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

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

      let typeName = RuntimeError.name [ "Int" ] "Error"

      DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError.intError


module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName = FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Int64" ] "ParseError"
    DEnum(typeName, typeName, [], caseName, fields)



let fns : List<BuiltInFn> =
  [ { name = fn "int64Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

         The modulus <param b> must be greater than 0.

         Use <fn Int64.remainder> if you want the remainder after division, which has
         a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DInt64 v; DInt64 m ] ->
          if m = 0L then
            IntRuntimeError.Error.ZeroModulus
            |> IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else if m < 0L then
            IntRuntimeError.Error.NegativeModulus
            |> IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            let result = v % m
            let result = if result < 0L then m + result else result
            Ply(DInt64(result))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "%"
      previewable = Pure
      // TODO: Deprecate this when we can version infix operators
      //  and when infix operators support Result return types
      //  (https://github.com/darklang/dark/issues/4267)
      // The current function returns an RTE (it used to rollbar) on negative `b`.
      deprecated = NotDeprecated }


    // See above for when to uncomment this
    // TODO: A future version should support all non-zero modulus values and should include the infix "%"
    // { name = fn "mod" 0
    //   parameters = [ Param.make "value" TInt64 ""; Param.make "modulus" TInt64 "" ]
    //   returnType = TypeReference.result TInt64 TString
    //   description =
    //     "Returns the result of wrapping <param value> around so that {{0 <= res < modulus}}, as a <type Result>.
    //      If <param modulus> is positive, returns {{Ok res}}. Returns an {{Error}} if <param modulus> is {{0}} or negative.
    //     Use <fn Int64.remainder> if you want the remainder after division, which has a different behavior for negative numbers."
    //   fn =
    //     (function
    //     | _, [ DInt64 v; DInt64 m ] ->
    //       (try
    //         Ply(Dval.resultOk(DInt64(v % m)))
    //        with
    //        | e ->
    //          if m <= 0L then
    //            Ply(
    //              DResult(
    //                Error(
    //                  DString(
    //                    "`modulus` must be positive but was "
    //                    + LibExecution.DvalReprDeveloper.toRepr (DInt64 m)
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


    { name = fn "int64Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt64 ""; Param.make "divisor" TInt64 "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
         <param divisor>, as a <type Result>.

         For example, {{Int64.remainder 15 6 == Ok 3}}. The remainder will be
         negative only if {{<var value> < 0}}.

         The sign of <param divisor> doesn't influence the outcome.

         Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt64 KTString r |> Ply
        (function
        | state, _, [ DInt64 v; DInt64 d ] ->
          (try
            v % d |> DInt64 |> resultOk
           with e ->
             if d = 0L then
               IntRuntimeError.Error.DivideByZeroError
               |> IntRuntimeError.RTE.toRuntimeError
               |> raiseRTE state.tracing.callStack
               |> Ply
             else
               Exception.raiseInternal
                 "unexpected failure case in Int64.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Adds two integers together"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] -> Ply(DInt64(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Subtracts two integers"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] -> Ply(DInt64(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Multiplies two integers"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] -> Ply(DInt64(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "*"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt64 ""; Param.make "exponent" TInt64 "" ]
      returnType = TInt64
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | state, _, [ DInt64 number; DInt64 exp ] ->
          (try
            if exp < 0L then
              IntRuntimeError.Error.NegativeExponent
              |> IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.tracing.callStack
              |> Ply
            else
              (bigint number) ** (int exp) |> int64 |> DInt64 |> Ply
           with :? System.OverflowException ->
             IntRuntimeError.Error.OutOfRange
             |> IntRuntimeError.RTE.toRuntimeError
             |> raiseRTE state.tracing.callStack
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "^"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Divides two integers"
      fn =
        (function
        | state, _, [ DInt64 a; DInt64 b ] ->
          if b = 0L then
            IntRuntimeError.Error.DivideByZeroError
            |> IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            Ply(DInt64(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TInt64
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, [ DInt64 a ] -> Ply(DInt64(-a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt64 ""; Param.make "end" TInt64 "" ]
      returnType = TInt64
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DInt64 a; DInt64 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          // .NET's "nextInt64" is exclusive,
          // but we'd rather an inclusive version of this function
          let correction : int64 = 1

          lower + randomSeeded().NextInt64(upper - lower + correction)
          |> DInt64
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "int64Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int64>"
      fn =
        (function
        | _, _, [ DInt64 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TFloat
      description = "Converts an <type Int64> to a <type Float>"
      fn =
        (function
        | _, _, [ DInt64 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Int64" ] "ParseError"
        TypeReference.result TInt64 (TCustomType(Ok errorType, []))
      description = "Returns the <type Int64> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let typeName = RuntimeError.name [ "Int64" ] "ParseError"
        let resultError = Dval.resultError KTInt64 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Convert.ToInt64 |> DInt64 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt64 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, [ DInt64 int ] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt64
      description = "Converts an Int8 to a 64-bit signed integer."
      fn =
        (function
        | _, _, [ DInt8 a ] -> DInt64(int64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt64
      description = "Converts a UInt8 to a 64-bit signed integer."
      fn =
        (function
        | _, _, [ DUInt8 a ] -> DInt64(int64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt64
      description = "Converts an Int16 to a 64-bit signed integer."
      fn =
        (function
        | _, _, [ DInt16 a ] -> DInt64(int64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TInt64
      description = "Converts a UInt16 to a 64-bit signed integer."
      fn =
        (function
        | _, _, [ DUInt16 a ] -> DInt64(int64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt64
      description = "Converts an Int32 to a 64-bit signed integer."
      fn =
        (function
        | _, _, [ DInt32 a ] -> DInt64(int64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TInt64
      description = "Converts a UInt32 to a 64-bit signed integer."
      fn =
        (function
        | _, _, [ DUInt32 a ] -> DInt64(int64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts a UInt64 to a 64-bit signed integer. Returns {{None}} if the value is greater than 9223372036854775807."
      fn =
        (function
        | _, _, [ DUInt64 a ] ->
          if (a > uint64 System.Int64.MaxValue) then
            Dval.optionNone KTInt64 |> Ply
          else
            Dval.optionSome KTInt64 (DInt64(int64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts an Int128 to a 64-bit signed integer. Returns {{None}} if the value is less than -9223372036854775808 or greater than 9223372036854775807."
      fn =
        (function
        | _, _, [ DInt128 a ] ->
          if
            (a < System.Int128.op_Implicit System.Int64.MinValue)
            || (a > System.Int128.op_Implicit System.Int64.MaxValue)
          then
            Dval.optionNone KTInt64 |> Ply
          else
            Dval.optionSome KTInt64 (DInt64(int64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts a UInt128 to a 64-bit signed integer. Returns {{None}} if the value is greater than 9223372036854775807."
      fn =
        (function
        | _, _, [ DUInt128 a ] ->
          if (a > 9223372036854775807Z) then
            Dval.optionNone KTInt64 |> Ply
          else
            Dval.optionSome KTInt64 (DInt64(int64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
