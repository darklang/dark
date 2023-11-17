module BuiltinExecution.Libs.UInt128

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let types : List<BuiltInType> = []

let modules = [ "UInt128" ]
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
      TypeName.fqPackage "Darklang" [ "Stdlib"; "UInt128" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn UInt128.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DUInt128 v; DUInt128 m ] ->
          if m = System.UInt128.Zero then
            Int64.IntRuntimeError.Error.ZeroModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < System.UInt128.Zero then m + result else result
            Ply(DUInt128(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Adds two 128-bit unsigned integers together"
      fn =
        (function
        | state, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_CheckedAddition (a, b)
            Ply(DUInt128(result))
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
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Subtracts two 128-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_CheckedSubtraction (a, b)
            Ply(DUInt128(result))
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
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Multiplies two 128-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_CheckedMultiply (a, b)
            Ply(DUInt128(result))
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
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Divides two 128-bit unsigned integers"
      fn =
        (function
        | state, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_Division (a, b)
            Ply(DUInt128(result))
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


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TString
      description = "Converts an <type UInt128> to a <type String>"
      fn =
        (function
        | _, _, [ DUInt128 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TFloat
      description = "Converts an <type UInt128> to a <type Float>"
      fn =
        (function
        | _, _, [ DUInt128 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt128>"
      fn =
        (function
        | _, _, [ DUInt128 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TUInt128
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "UInt128" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type UInt128> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt128 KTString
        let typeName = RuntimeError.name [ "UInt128" ] "ParseError" 0
        let resultError = Dval.resultError KTUInt128 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.UInt128.Parse |> DUInt128 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TypeReference.option TUInt128
      description =
        "Converts a UInt8 to a 128-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 255."
      fn =
        (function
        | _, _, [ DUInt8 a ] ->
          if (a < 0uy) || (a > 255uy) then
            Dval.optionNone KTUInt128 |> Ply
          else
            Dval.optionSome KTUInt128 (DUInt128(System.UInt128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TypeReference.option TUInt128
      description =
        "Converts a UInt16 to a 128-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 65535."
      fn =
        (function
        | _, _, [ DUInt16 a ] ->
          if (a < System.UInt16.MinValue) || (a > System.UInt16.MaxValue) then
            Dval.optionNone KTUInt128 |> Ply
          else
            Dval.optionSome KTUInt128 (DUInt128(System.UInt128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TUInt128
      description =
        "Converts a UInt32 to a 128-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 4294967295."
      fn =
        (function
        | _, _, [ DUInt32 a ] ->
          if (a < System.UInt32.MinValue) || (a > System.UInt32.MaxValue) then
            Dval.optionNone KTUInt128 |> Ply
          else
            Dval.optionSome KTUInt128 (DUInt128(System.UInt128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TUInt128
      description =
        "Converts a UInt64 to a 128-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 18446744073709551615."
      fn =
        (function
        | _, _, [ DUInt64 a ] ->
          if (a < System.UInt64.MinValue) || (a > System.UInt64.MaxValue) then
            Dval.optionNone KTUInt128 |> Ply
          else
            Dval.optionSome KTUInt128 (DUInt128(System.UInt128.op_Implicit a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
