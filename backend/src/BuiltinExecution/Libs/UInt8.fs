module BuiltinExecution.Libs.UInt8

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval


module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName = FQTypeName.fqPackage "Darklang" [ "Stdlib"; "UInt8" ] "ParseError"
    DEnum(typeName, typeName, [], caseName, fields)



let fns : List<BuiltInFn> =
  [ { name = fn "uint8Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn UInt8.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DUInt8 v; DUInt8 m ] ->
          if m = 0uy then
            Int64.IntRuntimeError.Error.ZeroModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            let result = v % m
            let result = if result < 0uy then m + result else result
            Ply(DUInt8(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Adds two 8-bit unsigned integers together"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          try
            DUInt8(Checked.(+) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Subtracts two 8-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          try
            DUInt8(Checked.(-) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Multiplies two 8-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          try
            DUInt8(Checked.(*) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8Power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt8 ""; Param.make "exponent" TUInt8 "" ]
      returnType = TUInt8
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | state, _, [ DUInt8 number; DUInt8 exp ] ->
          (try
            (bigint number) ** (int exp) |> uint8 |> DUInt8 |> Ply
           with :? System.OverflowException ->
             Int64.IntRuntimeError.Error.OutOfRange
             |> Int64.IntRuntimeError.RTE.toRuntimeError
             |> raiseRTE state.tracing.callStack
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Divides two 8-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          if b = 0uy then
            Int64.IntRuntimeError.Error.DivideByZeroError
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            let result = int a / int b
            if result < 0 || result > 255 then
              Int64.IntRuntimeError.Error.OutOfRange
              |> Int64.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.tracing.callStack
              |> Ply
            else
              Ply(DUInt8(uint8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TString
      description = "Converts an <type UInt8> to a <type String>"
      fn =
        (function
        | _, _, [ DUInt8 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TFloat
      description = "Converts an <type UInt8> to a <type Float>"
      fn =
        (function
        | _, _, [ DUInt8 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8Random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt8 ""; Param.make "end" TUInt8 "" ]
      returnType = TUInt8
      description =
        "Returns a random 8-bit unsigned integer (uint8) between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DUInt8 a; DUInt8 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let lowerBound = max lower 0uy
          let upperBound = min upper 255uy

          let uint8Range = int upperBound - int lowerBound + 1

          let resultInt = randomSeeded().Next(uint8Range)

          let uint8Result = lowerBound + (uint8 resultInt)

          uint8Result |> DUInt8 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "uint8Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt8>"
      fn =
        (function
        | _, _, [ DUInt8 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage "Darklang" [ "Stdlib"; "UInt8" ] "ParseError"
        TypeReference.result TUInt8 (TCustomType(Ok errorType, []))
      description = "Returns the <type UInt8> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt8 KTString
        let typeName = RuntimeError.name [ "UInt8" ] "ParseError"
        let resultError = Dval.resultError KTUInt8 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Byte.Parse |> DUInt8 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts an Int8 to an 8-bit unsigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, [ DInt8 a ] ->
          if a < 0y then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts an Int16 to an 8-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 255."
      fn =
        (function
        | _, _, [ DInt16 a ] ->
          if a < 0s || a > 255s then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts a UInt16 to an 8-bit unsigned integer. Returns {{None}} if the value is greater than 255."
      fn =
        (function
        | _, _, [ DUInt16 a ] ->
          if a > 255us then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts an Int32 to an 8-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 255."
      fn =
        (function
        | _, _, [ DInt32 a ] ->
          if a < 0l || a > 255l then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts a UInt32 to an 8-bit unsigned integer. Returns {{None}} if the value is greater than 255."
      fn =
        (function
        | _, _, [ DUInt32 a ] ->
          if a > 255ul then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts an Int64 to an 8-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 255."
      fn =
        (function
        | _, _, [ DInt64 a ] ->
          if a < 0L || a > 255L then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts a UInt64 to an 8-bit unsigned integer. Returns {{None}} if the value is greater than 255."
      fn =
        (function
        | _, _, [ DUInt64 a ] ->
          if a > 255UL then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts an Int128 to an 8-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 255."
      fn =
        (function
        | _, _, [ DInt128 a ] ->
          if a < 0Q || a > 255Q then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint8FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Converts a UInt128 to an 8-bit unsigned integer. Returns {{None}} if the value is greater than 255."
      fn =
        (function
        | _, _, [ DUInt128 a ] ->
          if a > 255Z then
            Dval.optionNone KTUInt8 |> Ply
          else
            Dval.optionSome KTUInt8 (DUInt8(uint8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
