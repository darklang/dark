module BuiltinExecution.Libs.UInt32

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

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
      FQTypeName.fqPackage "Darklang" [ "Stdlib"; "UInt32" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fn = fn [ "UInt32" ]

let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn UInt32.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DUInt32 v; DUInt32 m ] ->
          if m = 0ul then
            Int64.IntRuntimeError.Error.ZeroModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < 0ul then m + result else result
            Ply(DUInt32(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Adds two 32-bit unsigned integers together"
      fn =
        (function
        | state, _, [ DUInt32 a; DUInt32 b ] ->
          try
            let result = Checked.(+) a b
            Ply(DUInt32(result))
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
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Subtracts two 32-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt32 a; DUInt32 b ] ->
          try
            let result = Checked.(-) a b
            Ply(DUInt32(result))
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
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Multiplies two 32-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt32 a; DUInt32 b ] ->
          try
            let result = Checked.(*) a b
            Ply(DUInt32(result))
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
      parameters = [ Param.make "base" TUInt32 ""; Param.make "exponent" TUInt32 "" ]
      returnType = TUInt32
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | state, _, [ DUInt32 number; DUInt32 exp ] ->
          (try
            (bigint number) ** (int exp) |> uint32 |> DUInt32 |> Ply
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
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Divides two 32-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt32 a; DUInt32 b ] ->
          if b = 0ul then
            Int64.IntRuntimeError.Error.DivideByZeroError
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = a / b
            if
              result < System.UInt32.MinValue || result > System.UInt32.MaxValue
            then
              Int64.IntRuntimeError.Error.OutOfRange
              |> Int64.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.caller
              |> Ply
            else
              Ply(DUInt32(uint32 result))

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TString
      description = "Stringify <param uint32>"
      fn =
        (function
        | _, _, [ DUInt32 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TFloat
      description = "Converts an <type UInt32> to a <type Float>"
      fn =
        (function
        | _, _, [ DUInt32 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt32 ""; Param.make "end" TUInt32 "" ]
      returnType = TUInt32
      description =
        "Returns a random integer32 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DUInt32 a; DUInt32 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let lowerBound = max lower 0ul
          let upperBound = min upper 4294967295ul
          let correctRange = 1

          let uint32Range = int upperBound - int lowerBound + correctRange

          let resultInt = randomSeeded().Next(uint32Range)

          let uint32Result = lowerBound + (uint32 resultInt)
          Ply(DUInt32(uint32Result))

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt32>"
      fn =
        (function
        | _, _, [ DUInt32 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.Package
            { owner = "Darklang"
              modules = [ "Stdlib"; "UInt32" ]
              name = "ParseError"
              version = 0 }
        TypeReference.result TUInt32 (TCustomType(Ok errorType, []))
      description = "Returns the <type UInt32> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt32 KTString
        let typeName = RuntimeError.name [ "UInt32" ] "ParseError" 0
        let resultError = Dval.resultError KTUInt32 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Convert.ToUInt32 |> DUInt32 |> resultOk |> Ply
          with
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply

          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TypeReference.option TUInt32
      description =
        "Converts an Int8 to a 32-bit unsigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, [ DInt8 a ] ->
          if (a < 0y) then
            Dval.optionNone KTUInt32 |> Ply
          else
            Dval.optionSome KTUInt32 (DUInt32(uint32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TUInt32
      description = "Converts a UInt8 to a 32-bit unsigned integer."
      fn =
        (function
        | _, _, [ DUInt8 a ] -> DUInt32(uint32 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TypeReference.option TUInt32
      description =
        "Converts an Int16 to a 32-bit unsigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, [ DInt16 a ] ->
          if (a < 0s) then
            Dval.optionNone KTUInt32 |> Ply
          else
            Dval.optionSome KTUInt32 (DUInt32(uint32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TUInt32
      description = "Converts a UInt16 to a 32-bit unsigned integer."
      fn =
        (function
        | _, _, [ DUInt16 a ] -> DUInt32(uint32 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts an Int32 to a 32-bit unsigned integer. Returns {{None}} if the value is less than 0"
      fn =
        (function
        | _, _, [ DInt32 a ] ->
          if (a < 0l) then
            Dval.optionNone KTUInt32 |> Ply
          else
            Dval.optionSome KTUInt32 (DUInt32(uint32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TUInt32
      description =
        "Converts an Int64 to a 32-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 4294967295."
      fn =
        (function
        | _, _, [ DInt64 a ] ->
          if
            (a < int64 System.UInt32.MinValue) || (a > int64 System.UInt32.MaxValue)
          then
            Dval.optionNone KTUInt32 |> Ply
          else
            Dval.optionSome KTUInt32 (DUInt32(uint32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TUInt32
      description =
        "Converts a UInt64 to a 32-bit unsigned integer. Returns {{None}} if the value is greater than 4294967295."
      fn =
        (function
        | _, _, [ DUInt64 a ] ->
          if (a > uint64 System.UInt32.MaxValue) then
            Dval.optionNone KTUInt32 |> Ply
          else
            Dval.optionSome KTUInt32 (DUInt32(uint32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TUInt32
      description =
        "Converts an Int128 to a 32-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 4294967295."
      fn =
        (function
        | _, _, [ DInt128 a ] ->
          if
            (a < System.Int128.op_Implicit System.UInt32.MinValue)
            || (a > System.Int128.op_Implicit System.UInt32.MaxValue)
          then
            Dval.optionNone KTUInt32 |> Ply
          else
            Dval.optionSome KTUInt32 (DUInt32(uint32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TUInt32
      description =
        "Converts a UInt128 to a 32-bit unsigned integer. Returns {{None}} if the value is greater than 4294967295."
      fn =
        (function
        | _, _, [ DUInt128 a ] ->
          if (a > System.UInt128.op_Implicit System.UInt32.MaxValue) then
            Dval.optionNone KTUInt32 |> Ply
          else
            Dval.optionSome KTUInt32 (DUInt32(uint32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, constants)
