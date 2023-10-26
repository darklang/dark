module BuiltinExecution.Libs.UInt16

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
      TypeName.fqPackage "Darklang" [ "Stdlib"; "UInt16" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fn = fn [ "UInt16" ]

let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn UInt16.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DUInt16 v; DUInt16 m ] ->
          if m = 0us then
            Int.IntRuntimeError.Error.ZeroModulus
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < 0us then m + result else result
            Ply(DUInt16(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Adds two 16 bits unsigned integers together"
      fn =
        (function
        | state, _, [ DUInt16 a; DUInt16 b ] ->
          try
            let result = Checked.(+) a b
            Ply(DUInt16(result))
          with :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Subtracts two 16 bits unsigned integers"
      fn =
        (function
        | state, _, [ DUInt16 a; DUInt16 b ] ->
          try
            let result = Checked.(-) a b
            Ply(DUInt16(result))
          with :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Multiplies two 16 bits unsigned integers"
      fn =
        (function
        | state, _, [ DUInt16 a; DUInt16 b ] ->
          try
            let result = Checked.(*) a b
            Ply(DUInt16(result))
          with :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Divides two 16 bits unsigned integers"
      fn =
        (function
        | state, _, [ DUInt16 a; DUInt16 b ] ->
          if b = 0us then
            Int.IntRuntimeError.Error.DivideByZeroError
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = a / b
            if
              result < System.UInt16.MinValue || result > System.UInt16.MaxValue
            then
              Int.IntRuntimeError.Error.OutOfRange
              |> Int.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.caller
              |> Ply
            else
              Ply(DUInt16(uint16 result))

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TString
      description = "Stringify <param uint16>"
      fn =
        (function
        | _, _, [ DUInt16 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TFloat
      description = "Converts an <type UInt16> to a <type Float>"
      fn =
        (function
        | _, _, [ DUInt16 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt16 ""; Param.make "end" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Returns a random integer16 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DUInt16 a; DUInt16 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let lowerBound = max lower 0us
          let upperBound = min upper 65535us
          let correctRange = 1

          let uint16Range = int upperBound - int lowerBound + correctRange

          let resultInt = randomSeeded().Next(uint16Range)

          let uint16Result = lowerBound + (uint16 resultInt)
          Ply(DUInt16(uint16Result))

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt16>"
      fn =
        (function
        | _, _, [ DUInt16 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TUInt16
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "UInt16" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type UInt16> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt16 KTString
        let typeName = RuntimeError.name [ "UInt16" ] "ParseError" 0
        let resultError = Dval.resultError KTUInt16 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Convert.ToUInt16 |> DUInt16 |> resultOk |> Ply
          with
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply

          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts an int64 to a 16 bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 65535."
      fn =
        (function
        | _, _, [ DInt a ] ->
          if
            a < int64 System.UInt16.MinValue || a > int64 System.UInt16.MaxValue
          then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
