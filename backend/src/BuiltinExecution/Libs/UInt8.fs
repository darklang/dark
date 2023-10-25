module BuiltinExecution.Libs.UInt8

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

    let typeName = TypeName.fqPackage "Darklang" [ "Stdlib"; "UInt8" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fn = fn [ "UInt8" ]

let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
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
            Int.IntRuntimeError.Error.ZeroModulus
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else if m < 0uy then
            Int.IntRuntimeError.Error.NegativeModulus
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < 0uy then m + result else result
            Ply(DUInt8(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TUInt8 ""; Param.make "divisor" TUInt8 "" ]
      returnType = TypeReference.result TUInt8 TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
        <param divisor>, as a <type Result>.

        For example, {{UInt8.remainder 15 6 == Ok 3}}. The remainder will be
        negative only if {{<var value> < 0}}.

        The sign of <param divisor> doesn't influence the outcome.

        Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk r = Dval.resultOk KTUInt8 KTString r |> Ply
        (function
        | state, _, [ DUInt8 v; DUInt8 d ] ->
          (try
            v % d |> DUInt8 |> resultOk
           with e ->
             if d = 0uy then
               Int.IntRuntimeError.Error.DivideByZeroError
               |> Int.IntRuntimeError.RTE.toRuntimeError
               |> raiseRTE state.caller
               |> Ply
             else
               Exception.raiseInternal
                 "unexpected failure case in UInt8.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Adds two 8 bits unsigned integers together"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          try
            DUInt8(Checked.(+) a b) |> Ply
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
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Subtracts two 8 bits unsigned integers"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          try
            DUInt8(Checked.(-) a b) |> Ply
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
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Multiplies two 8 bits unsigned integers"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          try
            DUInt8(Checked.(*) a b) |> Ply
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
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Divides two 8 bits unsigned integers"
      fn =
        (function
        | state, _, [ DUInt8 a; DUInt8 b ] ->
          if b = 0uy then
            Int.IntRuntimeError.Error.DivideByZeroError
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = int a / int b
            if result < 0 || result > 255 then
              Int.IntRuntimeError.Error.OutOfRange
              |> Int.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.caller
              |> Ply
            else
              Ply(DUInt8(uint8 result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
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


    { name = fn "greaterThanOrEqualTo" 0
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


    { name = fn "lessThan" 0
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


    { name = fn "lessThanOrEqualTo" 0
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


    { name = fn "toString" 0
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


    { name = fn "toFloat" 0
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


    { name = fn "random" 0
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

          let uint8Range = (int) upperBound - (int) lowerBound + 1

          let resultInt = randomSeeded().Next(uint8Range)

          let uint8Result = (lowerBound + (uint8 resultInt))

          uint8Result |> DUInt8 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
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
                  modules = [ "Stdlib"; "UInt8" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type UInt8> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt8 KTString
        let typeName = RuntimeError.name [ "UInt8" ] "ParseError" 0
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


    { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TUInt8
      description = "Converts an int64 to an 8 bit unsigned integer"
      fn =
        (function
        | state, _, [ DInt a ] ->
          if a < 0L || a > 255L then
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DUInt8(uint8 a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
