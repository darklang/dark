module BuiltinExecution.Libs.Int128

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let types : List<BuiltInType> = []

let modules = [ "Int128" ]
let fn = fn modules
let constant = constant modules

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
      TypeName.fqPackage "Darklang" [ "Stdlib"; "Int128" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
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
            |> raiseRTE state.caller
            |> Ply
          else if m < System.Int128.Zero then
            Int64.IntRuntimeError.Error.NegativeModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < System.Int128.Zero then m + result else result
            Ply(DInt128(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "remainder" 0
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
               |> raiseRTE state.caller
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


    { name = fn "add" 0
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
            |> raiseRTE state.caller
            |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
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
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }



    { name = fn "multiply" 0
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
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // TODO: add power function


    { name = fn "divide" 0
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
            |> raiseRTE state.caller
            |> Ply
          | :? System.OverflowException ->
            Int64.IntRuntimeError.Error.OutOfRange
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "negate" 0
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
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
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


    { name = fn "greaterThanOrEqualTo" 0
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


    { name = fn "lessThan" 0
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


    { name = fn "lessThanOrEqualTo" 0
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


    { name = fn "toString" 0
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


    { name = fn "toFloat" 0
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


    { name = fn "sqrt" 0
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


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TInt128
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "Int128" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type Int128> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt128 KTString
        let typeName = RuntimeError.name [ "Int128" ] "ParseError" 0
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


    { name = fn "fromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts an Int8 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DInt8 a ] ->
          if (a < -128y) || (a > 127y) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts a UInt8 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DUInt8 a ] ->
          if (a < 0uy) || (a > 255uy) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts an Int16 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DInt16 a ] ->
          if (a < System.Int16.MinValue) || (a > int16 System.Int16.MaxValue) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts a UInt16 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DUInt16 a ] ->
          if (a < System.UInt16.MinValue) || (a > System.UInt16.MaxValue) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts an Int32 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DInt32 a ] ->
          if (a < System.Int32.MinValue) || (a > System.Int32.MaxValue) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts a UInt32 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DUInt32 a ] ->
          if (a < System.UInt32.MinValue) || (a > System.UInt32.MaxValue) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts an Int64 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DInt64 a ] ->
          if (a < System.Int64.MinValue) || (a > System.Int64.MaxValue) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts a UInt64 to a 128-bit signed integer. Returns {{None}} if the value is less than −170141183460469231731687303715884105728 or greater than 170141183460469231731687303715884105727."
      fn =
        (function
        | _, _, [ DUInt64 a ] ->
          if (a < System.UInt64.MinValue) || (a > System.UInt64.MaxValue) then
            Dval.optionNone KTInt128 |> Ply
          else
            Dval.optionSome KTInt128 (DInt128(System.Int128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    ]

let contents = (fns, types, constants)
