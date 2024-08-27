module BuiltinExecution.Libs.Int32

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs
module RTE = RuntimeError


module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.int32ParseError
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "int32Mod" 0
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
        | _, vm, _, [ DInt32 v; DInt32 m ] ->
          if m = 0 then
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.callStack
          else if m < 0 then
            RTE.Ints.NegativeModulus |> RTE.Int |> raiseRTE vm.callStack
          else
            let result = v % m
            let result = if result < 0 then m + result else result
            Ply(DInt32 result)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Remainder" 0
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
        | _, vm, _, [ DInt32 v; DInt32 d ] ->
          (try
            v % d |> DInt32 |> resultOk
           with e ->
             if d = 0 then
               RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.callStack
             else
               Exception.raiseInternal
                 "unexpected failure case in Int32.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Adds two 32-bit signed integers together"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DInt32(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Subtracts two 32-bit signed integers"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DInt32(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Multiplies two 32-bit signed integers"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DInt32(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt32 ""; Param.make "exponent" TInt32 "" ]
      returnType = TInt32
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | _, vm, _, [ DInt32 number; DInt32 exp ] ->
          (try
            if exp < 0 then
              RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.callStack
            else
              (bigint number) ** (int exp) |> int32 |> DInt32 |> Ply
           with :? System.OverflowException ->
             RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Divides two 32-bit signed integers"
      fn =
        (function
        | _, vm, _, [ DInt32 a; DInt32 b ] ->
          if b = 0 then
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.callStack
          else
            Ply(DInt32(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt32
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [ DInt32 a ] -> Ply(DInt32(-a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt32 ""; Param.make "end" TInt32 "" ]
      returnType = TInt32
      description =
        "Returns a random integer32 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let correction : int32 = 1

          lower + randomSeeded().Next(upper - lower + correction) |> DInt32 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "int32Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int32>"
      fn =
        (function
        | _, _, _, [ DInt32 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TFloat
      description = "Converts an <type Int32> to a <type Float>"
      fn =
        (function
        | _, _, _, [ DInt32 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType = FQTypeName.fqPackage PackageIDs.Type.Stdlib.int32ParseError

        TypeReference.result TInt32 (TCustomType(Ok errorType, []))
      description = "Returns the <type Int32> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt32 KTString
        let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.int32ParseError
        let resultError = Dval.resultError KTInt32 (KTCustomType(typeName, []))
        (function
        | _, _, _, [ DString s ] ->
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


    { name = fn "int32ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt32 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [ DInt32 int ] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt32
      description = "Converts an Int8 to a 32-bit signed integer."
      fn =
        (function
        | _, _, _, [ DInt8 a ] -> DInt32(int32 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt32
      description = "Converts a UInt8 to a 32-bit signed integer."
      fn =
        (function
        | _, _, _, [ DUInt8 a ] -> DInt32(int32 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt32
      description = "Converts an Int16 to a 32-bit signed integer."
      fn =
        (function
        | _, _, _, [ DInt16 a ] -> DInt32(int32 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TInt32
      description = "Converts a UInt16 to a 32-bit signed integer."
      fn =
        (function
        | _, _, _, [ DUInt16 a ] -> DInt32(int32 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts a UInt32 to a 32-bit signed integer. Returns {{None}} if the value is greater than 2147483647."
      fn =
        (function
        | _, _, _, [ DUInt32 a ] ->
          if (a > uint32 System.Int32.MaxValue) then
            Dval.optionNone KTInt32 |> Ply
          else
            Dval.optionSome KTInt32 (DInt32(int32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts an Int64 to a 32-bit signed integer. Returns {{None}} if the value is less than -2147483648 or greater than 2147483647."
      fn =
        (function
        | _, _, _, [ DInt64 a ] ->
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


    { name = fn "int32FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts a UInt64 to a 32-bit signed integer. Returns {{None}} if the value is greater than 2147483647."
      fn =
        (function
        | _, _, _, [ DUInt64 a ] ->
          if (a > uint64 System.Int32.MaxValue) then
            Dval.optionNone KTInt32 |> Ply
          else
            Dval.optionSome KTInt32 (DInt32(int32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts an Int128 to a 32-bit signed integer. Returns {{None}} if the value is less than -2147483648 or greater than 2147483647."
      fn =
        (function
        | _, _, _, [ DInt128 a ] ->
          if
            (a < System.Int128.op_Implicit System.Int32.MinValue)
            || (a > System.Int128.op_Implicit System.Int32.MaxValue)
          then
            Dval.optionNone KTInt32 |> Ply
          else
            Dval.optionSome KTInt32 (DInt32(int32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int32FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts a UInt128 to a 32-bit signed integer. Returns {{None}} if the value is greater than 2147483647."
      fn =
        (function
        | _, _, _, [ DUInt128 a ] ->
          if (a > 2147483647Z) then
            Dval.optionNone KTInt32 |> Ply
          else
            Dval.optionSome KTInt32 (DInt32(int32 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
