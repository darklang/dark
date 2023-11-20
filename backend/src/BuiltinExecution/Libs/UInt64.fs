module BuiltinExecution.Libs.UInt64

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


module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName =
      TypeName.fqPackage "Darklang" [ "Stdlib"; "UInt64" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fn = fn [ "UInt64" ]

let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

         The modulus <param b> must be greater than 0.

         Use <fn UInt64.remainder> if you want the remainder after division, which has
         a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DUInt64 v; DUInt64 m ] ->
          if m = 0UL then
            Int64.IntRuntimeError.Error.ZeroModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < 0UL then m + result else result
            Ply(DUInt64(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Adds 64-bit unsigned integers together"
      fn =
        (function
        | state, _, [ DUInt64 a; DUInt64 b ] ->
          try
            DUInt64(Checked.(+) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Subtracts 64-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt64 a; DUInt64 b ] ->
          try
            DUInt64(Checked.(-) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Multiplies 64-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt64 a; DUInt64 b ] ->
          try
            DUInt64(Checked.(*) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt64 ""; Param.make "exponent" TUInt64 "" ]
      returnType = TUInt64
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | state, _, [ DUInt64 number; DUInt64 exp ] ->
          (try
            (bigint number) ** (int exp) |> uint64 |> DUInt64 |> Ply
           with :? System.OverflowException ->
             Int64.IntRuntimeError.Error.OutOfRange
             |> Int64.IntRuntimeError.RTE.toRuntimeError
             |> raiseRTE state.caller
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Divides 64-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt64 a; DUInt64 b ] ->
          if b = 0UL then
            Int64.IntRuntimeError.Error.DivideByZeroError
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = a / b
            if
              result < System.UInt64.MinValue || result > System.UInt64.MaxValue
            then
              Int64.IntRuntimeError.Error.OutOfRange
              |> Int64.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.caller
              |> Ply
            else
              Ply(DUInt64(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt64 ""; Param.make "end" TUInt64 "" ]
      returnType = TUInt64
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DUInt64 a; DUInt64 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          // .NET's "nextUInt64" is exclusive,
          // but we'd rather an inclusive version of this function
          let correction : int = 1

          let lowerBound = max lower 0UL
          let upperBound = min upper (uint64 System.UInt64.MaxValue)
          let uint64Range = int upperBound - int lowerBound + correction

          let resultInt = randomSeeded().Next(uint64Range)

          let uint64Result = lowerBound + (uint64 resultInt)

          Ply(DUInt64(uint64Result))

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt64>"
      fn =
        (function
        | _, _, [ DUInt64 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TFloat
      description = "Converts an <type UInt64> to a <type Float>"
      fn =
        (function
        | _, _, [ DUInt64 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TUInt64
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "UInt64" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type UInt64> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt64 KTString
        let typeName = RuntimeError.name [ "UInt64" ] "ParseError" 0
        let resultError = Dval.resultError KTUInt64 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Convert.ToUInt64 |> DUInt64 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "int" TUInt64 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, [ DUInt64 int ] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TypeReference.option TUInt64
      description =
        "Converts an Int8 to a 64-bit usigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, [ DInt8 a ] ->
          if (a < 0y) then
            Dval.optionNone KTUInt64 |> Ply
          else
            Dval.optionSome KTUInt64 (DUInt64(uint64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TUInt64
      description = "Converts a UInt8 to a 64-bit usigned integer."
      fn =
        (function
        | _, _, [ DUInt8 a ] -> DUInt64(uint64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TypeReference.option TUInt64
      description =
        "Converts an Int16 to a 64-bit usigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, [ DInt16 a ] ->
          if (a < 0s) then
            Dval.optionNone KTUInt64 |> Ply
          else
            Dval.optionSome KTUInt64 (DUInt64(uint64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TUInt64
      description = "Converts a UInt16 to a 64-bit usigned integer."
      fn =
        (function
        | _, _, [ DUInt16 a ] -> DUInt64(uint64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TUInt64
      description =
        "Converts an Int32 to a 64-bit usigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, [ DInt32 a ] ->
          if (a < 0l) then
            Dval.optionNone KTUInt64 |> Ply
          else
            Dval.optionSome KTUInt64 (DUInt64(uint64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TUInt64
      description = "Converts a UInt32 to a 64-bit usigned integer."
      fn =
        (function
        | _, _, [ DUInt32 a ] -> DUInt64(uint64 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TUInt64
      description =
        "Converts an Int64 to a 64-bit usigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, [ DInt64 a ] ->
          if (a < 0L) then
            Dval.optionNone KTUInt64 |> Ply
          else
            Dval.optionSome KTUInt64 (DUInt64(uint64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TUInt64
      description =
        "Converts an Int128 to a 64-bit usigned integer. Returns {{None}} if the value is less than 0 or greater than 18446744073709551615."
      fn =
        (function
        | _, _, [ DInt128 a ] ->
          if
            (a < System.Int128.op_Implicit System.UInt64.MinValue)
            || (a > System.Int128.op_Implicit System.UInt64.MaxValue)
          then
            Dval.optionNone KTUInt64 |> Ply
          else
            Dval.optionSome KTUInt64 (DUInt64(uint64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TUInt64
      description =
        "Converts a UInt128 to a 64-bit usigned integer. Returns {{None}} if the value is less than 0 or greater than 18446744073709551615."
      fn =
        (function
        | _, _, [ DUInt128 a ] ->
          if
            (a < System.UInt128.op_Implicit System.UInt64.MinValue)
            || (a > System.UInt128.op_Implicit System.UInt64.MaxValue)
          then
            Dval.optionNone KTUInt64 |> Ply
          else
            Dval.optionSome KTUInt64 (DUInt64(uint64 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
