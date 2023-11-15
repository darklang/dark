module BuiltinExecution.Libs.Int32

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

    let typeName = TypeName.fqPackage "Darklang" [ "Stdlib"; "Int32" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fn = fn [ "Int32" ]

let fns : List<BuiltInFn> =
  [ { name = fn "mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

         The modulus <param b> must be greater than 0.

         Use <fn Int32.remainder> if you want the remainder after division, which has
         a different behavior for negative numbers."
      fn =
        (function
        | state, _, [ DInt32 v; DInt32 m ] ->
          if m = 0 then
            Int64.IntRuntimeError.Error.ZeroModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else if m < 0 then
            Int64.IntRuntimeError.Error.NegativeModulus
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            let result = v % m
            let result = if result < 0 then m + result else result
            Ply(DInt32(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt32 ""; Param.make "divisor" TInt32 "" ]
      returnType = TypeReference.result TInt32 TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
         <param divisor>, as a <type Result>.

         For example, {{Int32.remainder 15 6 == Ok 3}}. The remainder will be
         negative only if {{<var value> < 0}}.

         The sign of <param divisor> doesn't influence the outcome.

         Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt32 KTString r |> Ply
        (function
        | state, _, [ DInt32 v; DInt32 d ] ->
          (try
            v % d |> DInt32 |> resultOk
           with e ->
             if d = 0 then
               Int64.IntRuntimeError.Error.DivideByZeroError
               |> Int64.IntRuntimeError.RTE.toRuntimeError
               |> raiseRTE state.caller
               |> Ply
             else
               Exception.raiseInternal
                 "unexpected failure case in Int32.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Adds two 32-bit signed integers together"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] -> Ply(DInt32(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Subtracts two 32-bit signed integers"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] -> Ply(DInt32(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Multiplies two 32-bit signed integers"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] -> Ply(DInt32(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt32 ""; Param.make "exponent" TInt32 "" ]
      returnType = TInt32
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | state, _, [ DInt32 number; DInt32 exp ] ->
          (try
            if exp < 0 then
              Int64.IntRuntimeError.Error.NegativeExponent
              |> Int64.IntRuntimeError.RTE.toRuntimeError
              |> raiseRTE state.caller
              |> Ply
            else
              (bigint number) ** (int exp) |> int32 |> DInt32 |> Ply
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
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Divides two 32-bit signed integers"
      fn =
        (function
        | state, _, [ DInt32 a; DInt32 b ] ->
          if b = 0 then
            Int64.IntRuntimeError.Error.DivideByZeroError
            |> Int64.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            Ply(DInt32(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt32
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, [ DInt32 a ] -> Ply(DInt32(-a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt32 ""; Param.make "end" TInt32 "" ]
      returnType = TInt32
      description =
        "Returns a random integer32 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, [ DInt32 a; DInt32 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let correction : int32 = 1

          lower + randomSeeded().Next(upper - lower + correction) |> DInt32 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int32>"
      fn =
        (function
        | _, _, [ DInt32 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TFloat
      description = "Converts an <type Int32> to a <type Float>"
      fn =
        (function
        | _, _, [ DInt32 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TInt32
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "Int32" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type Int32> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt32 KTString
        let typeName = RuntimeError.name [ "Int32" ] "ParseError" 0
        let resultError = Dval.resultError KTInt32 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Convert.ToInt32 |> DInt32 |> resultOk |> Ply
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
      parameters = [ Param.make "int" TInt32 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, [ DInt32 int ] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts an int64 to a 32-bit signed integer. Returns {{None}} if the value is less than 0 or greater than 2147483647."
      fn =
        (function
        | _, _, [ DInt64 a ] ->
          if
            (a < int64 System.Int32.MinValue) || (a > int64 System.Int32.MaxValue)
          then
            Dval.optionNone KTInt32 |> Ply
          else
            Dval.optionSome KTInt32 (DInt32(int32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
