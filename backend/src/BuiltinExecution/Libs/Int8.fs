module BuiltinExecution.Libs.Int8

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

    let typeName = TypeName.fqPackage "Darklang" [ "Stdlib"; "Int8" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fn = fn [ "Int8" ]

let fns : List<BuiltInFn> =
  [ { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Adds two intergers8 together"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          let result = int a + int b
          if result < -128 || result > 127 then
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DInt8(int8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Subtracts two integers8"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          let result = int a - int b
          if result < -128 || result > 127 then
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DInt8(int8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Multiplies two integers8"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          let result = int a * int b
          if result < -128 || result > 127 then
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DInt8(int8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Divides two integers8"
      fn =
        (function
        | state, _, [ DInt8 a; DInt8 b ] ->
          if b = int8 0 then
            Int.IntRuntimeError.Error.DivideByZeroError
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = int a / int b
            if result < -128 || result > 127 then
              Int.IntRuntimeError.Error.OutOfRange
              |> Int.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.caller
              |> Ply
            else
              Ply(DInt8(int8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt8
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | state, _, [ DInt8 a ] ->
          let result = -(int a)
          if result < -128 || result > 127 then
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DInt8(int8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
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


    { name = fn "greaterThanOrEqualTo" 0
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


    { name = fn "lessThan" 0
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


    { name = fn "lessThanOrEqualTo" 0
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


    { name = fn "toString" 0
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


    { name = fn "toFloat" 0
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


    { name = fn "random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt8 ""; Param.make "end" TInt8 "" ]
      returnType = TInt8
      description =
        "Returns a random 8-bit integer (int8) between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DInt8 a; DInt8 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let lowerBound = max lower (int8 -128)
          let upperBound = min upper (int8 127)

          let int8Range = (int) upperBound - (int) lowerBound + 1

          let resultInt = randomSeeded().Next(int8Range)

          let int8Result = (lowerBound + (int8 resultInt))

          int8Result |> DInt8 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TInt8
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "Int8" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type Int8> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt8 KTString
        let typeName = RuntimeError.name [ "Int8" ] "ParseError" 0
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


    { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TInt8
      description = "Converts an int64 to int8"
      fn =
        (function
        | state, _, [ DInt a ] ->
          if a < -128L || a > 127L then
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DInt8(int8 a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
