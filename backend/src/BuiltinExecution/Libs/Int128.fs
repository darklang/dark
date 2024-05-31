module BuiltinExecution.Libs.Int128

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

    let typeName =
      FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Int128" ] "ParseError"
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "int128Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn Int128.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DInt128 v; DInt128 m ] ->
          if m = System.Int128.Zero then
            Int64.IntRuntimeError.Error.ZeroModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else if m < System.Int128.Zero then
            Int64.IntRuntimeError.Error.NegativeModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          else
            let result = v % m
            let result = if result < System.Int128.Zero then m + result else result
            Ply(DInt128(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt128 ""; Param.make "divisor" TInt128 "" ]
      returnType = TypeReference.result TInt128 TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
        <param divisor>, as a <type Result>.

        For example, {{Int128.remainder 15 6 == Ok 3}}. The remainder will be
        negative only if {{<var value> < 0}}.

        The sign of <param divisor> doesn't influence the outcome.

        Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt128 KTString r |> Ply
        (function
        | state, _, [ DInt128 v; DInt128 d ] ->
          (try
            v % d |> DInt128 |> resultOk
           with e ->
             if d = System.Int128.Zero then
               Int64.IntRuntimeError.Error.DivideByZeroError
               |> Int64.IntRuntimeError.RTE.toRuntimeError
               |> raiseRTE state.tracing.callStack
               |> Ply
             else
               Exception.raiseInternal
                 "unexpected failure case in Int128.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Adds two 128-bit signed integers together"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_CheckedAddition (a, b)
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Subtracts two 128-bit signed integers"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_CheckedSubtraction (a, b)
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }



    { name = fn "int128Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Multiplies two 128-bit signed integers"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_CheckedMultiply (a, b)
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // TODO: add power function


    { name = fn "int128Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Divides two 128-bit signed integers"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_Division (a, b)
            Ply(DInt128(result))
          with
          | :? System.DivideByZeroException ->
            Int64.IntRuntimeError.Error.DivideByZeroError
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
          | :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TInt128
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | state, _, [ DInt128 a ] ->
          try
            let result = System.Int128.op_CheckedUnaryNegation a
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.tracing.callStack
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TString
      description = "Converts an <type Int128> to a <type String>"
      fn =
        (function
        | _, _, [ DInt128 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TFloat
      description = "Converts an <type Int128> to a <type Float>"
      fn =
        (function
        | _, _, [ DInt128 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int128>"
      fn =
        (function
        | _, _, [ DInt128 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TInt128
          (TCustomType(
            Ok(FQTypeName.fqPackage "Darklang" [ "Stdlib"; "Int128" ] "ParseError"),
            []
          ))
      description = "Returns the <type Int128> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt128 KTString
        let typeName = RuntimeError.name [ "Int128" ] "ParseError"
        let resultError = Dval.resultError KTInt128 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Int128.Parse |> DInt128 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt128
      description = "Converts an Int8 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DInt8 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt128
      description = "Converts a UInt8 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DUInt8 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt128
      description = "Converts an Int16 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DInt16 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TInt128
      description = "Converts a UInt16 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DUInt16 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt128
      description = "Converts an Int32 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DInt32 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TInt128
      description = "Converts a UInt32 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DUInt32 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TInt128
      description = "Converts an Int64 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DInt64 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TInt128
      description = "Converts a UInt64 to a 128-bit signed integer."
      fn =
        (function
        | _, _, [ DUInt64 a ] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
