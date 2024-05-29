module BuiltinExecution.Libs.Int8

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

    let typeName = FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Int8" ] "ParseError"
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "int8Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn Int8.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DInt8 v; DInt8 m ] ->
          if m = 0y then
            Int64.IntRuntimeError.Error.ZeroModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else if m < 0y then
            Int64.IntRuntimeError.Error.NegativeModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            let result = v % m
            let result = if result < 0y then m + result else result
            Ply(DInt8(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt8 ""; Param.make "divisor" TInt8 "" ]
      returnType = TypeReference.result TInt8 TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
        <param divisor>, as a <type Result>.

        For example, {{Int8.remainder 15 6 == Ok 3}}. The remainder will be
        negative only if {{<var value> < 0}}.

        The sign of <param divisor> doesn't influence the outcome.

        Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt8 KTString r |> Ply
        (function
        | state, _, [ DInt8 v; DInt8 d ] ->
          (try
            v % d |> DInt8 |> resultOk
           with e ->
             if d = 0y then
               Int64.IntRuntimeError.Error.DivideByZeroError
               |> Int64.IntRuntimeError.RTE.toRuntimeError
               |> raiseRTE state.tracing.callStack
               |> Ply
             else
               Exception.raiseInternal
                 "unexpected failure case in Int8.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Adds two 8-bit signed integers together"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          try
            DInt8(Checked.(+) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Subtracts two 8-bit signed integers"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          try
            DInt8(Checked.(-) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Multiplies two 8-bit signed integers"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          try
            DInt8(Checked.(*) a b) |> Ply
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt8 ""; Param.make "exponent" TInt8 "" ]
      returnType = TInt8
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | state, _, [ DInt8 number; DInt8 exp ] ->
          (try
            if exp < 0y then
              Int64.IntRuntimeError.Error.NegativeExponent
              |> Int64.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.tracing.callStack
              |> Ply
            else
              (bigint number) ** (int exp) |> int8 |> DInt8 |> Ply
           with :? System.OverflowException ->
             Int64.IntRuntimeError.Error.OutOfRange
             |> Int64.IntRuntimeError.RTE.toRuntimeError
             |> raiseRTE state.tracing.callStack
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Divides two 8-bit signed integers"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          if b = int8 0 then
            Int64.IntRuntimeError.Error.DivideByZeroError
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            let result = int a / int b
            if result < -128 || result > 127 then
              Int64.IntRuntimeError.Error.OutOfRange
              |> Int64.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.tracing.callStack
              |> Ply
            else
              Ply(DInt8(int8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt8
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | state, _, [ DInt8 a ] ->
          let result = -(int a)
          if result < -128 || result > 127 then
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            Ply(DInt8(int8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TString
      description = "Converts an <type Int8> to a <type String>"
      fn =
        (function
        | _, _, [ DInt8 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TFloat
      description = "Converts an <type Int8> to a <type Float>"
      fn =
        (function
        | _, _, [ DInt8 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt8 ""; Param.make "end" TInt8 "" ]
      returnType = TInt8
      description =
        "Returns a random 8-bit signed integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DInt8 a; DInt8 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let lowerBound = max lower -128y
          let upperBound = min upper 127y

          let int8Range = int upperBound - int lowerBound + 1

          let resultInt = randomSeeded().Next(int8Range)

          let int8Result = lowerBound + (int8 resultInt)

          int8Result |> DInt8 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "int8Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int8>"
      fn =
        (function
        | _, _, [ DInt8 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Int8" ] "ParseError"
        TypeReference.result TInt8 (TCustomType(Ok errorType, []))
      description = "Returns the <type Int8> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt8 KTString
        let typeName = RuntimeError.name [ "Int8" ] "ParseError"
        let resultError = Dval.resultError KTInt8 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.SByte.Parse |> DInt8 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts a UInt8 to an 8-bit signed integer. Returns {{None}} if the value is greater than 127."
      fn =
        (function
        | _, _, [ DUInt8 a ] ->
          if a > 127uy then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts an Int16 to an 8-bit signed integer. Returns {{None}} if the value is less than -128 or greater than 127."
      fn =
        (function
        | _, _, [ DInt16 a ] ->
          if a < -128s || a > 127s then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts a UInt16 to an 8-bit signed integer. Returns {{None}} if the value is greater than 127."
      fn =
        (function
        | _, _, [ DUInt16 a ] ->
          if a > 127us then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts an Int32 to an 8-bit signed integer. Returns {{None}} if the value is less than -128 or greater than 127."
      fn =
        (function
        | _, _, [ DInt32 a ] ->
          if a < -128l || a > 127l then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts a UInt32 to an 8-bit signed integer. Returns {{None}} if the value is greater than 127."
      fn =
        (function
        | _, _, [ DUInt32 a ] ->
          if a > 127ul then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts an Int64 to an 8-bit signed integer. Returns {{None}} if the value is less than -128 or greater than 127."
      fn =
        (function
        | _, _, [ DInt64 a ] ->
          if a < -128L || a > 127L then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts a UInt64 to an 8-bit signed integer. Returns {{None}} if the value is greater than 127."
      fn =
        (function
        | _, _, [ DUInt64 a ] ->
          if a > 127UL then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts an Int128 to an 8-bit signed integer. Returns {{None}} if the value is less than -128 or greater than 127."
      fn =
        (function
        | _, _, [ DInt128 a ] ->
          if a < -128Q || a > 127Q then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int8FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TInt8
      description =
        "Converts a UInt128 to an 8-bit signed integer. Returns {{None}} if the value is greater than 127."
      fn =
        (function
        | _, _, [ DUInt128 a ] ->
          if a > 127Z then
            Dval.optionNone KTInt8 |> Ply
          else
            Dval.optionSome KTInt8 (DInt8(int8 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
