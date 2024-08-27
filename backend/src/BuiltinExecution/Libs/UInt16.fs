module BuiltinExecution.Libs.UInt16

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

    let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.uint16ParseError
    DEnum(typeName, typeName, [], caseName, fields)



let fns : List<BuiltInFn> =
  [ { name = fn "uint16Mod" 0
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
        | _, vm, _, [ DUInt16 v; DUInt16 m ] ->
          if m = 0us then
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.callStack |> Ply
          else
            let result = v % m
            let result = if result < 0us then m + result else result
            Ply(DUInt16(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Adds two 16-bit unsigned integers together"
      fn =
        (function
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          try
            let result = Checked.(+) a b
            Ply(DUInt16(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Subtracts two 16-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          try
            let result = Checked.(-) a b
            Ply(DUInt16(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Multiplies two 16-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          try
            let result = Checked.(*) a b
            Ply(DUInt16(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16Power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt16 ""; Param.make "exponent" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | _, vm, _, [ DUInt16 number; DUInt16 exp ] ->
          (try
            (bigint number) ** (int exp) |> uint16 |> DUInt16 |> Ply
           with :? System.OverflowException ->
             RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Divides two 16-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          if b = 0us then
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.callStack |> Ply
          else
            let result = a / b
            if
              result < System.UInt16.MinValue || result > System.UInt16.MaxValue
            then
              RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack |> Ply
            else
              Ply(DUInt16(uint16 result))

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TString
      description = "Stringify <param uint16>"
      fn =
        (function
        | _, _, _, [ DUInt16 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TFloat
      description = "Converts an <type UInt16> to a <type Float>"
      fn =
        (function
        | _, _, _, [ DUInt16 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16Random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt16 ""; Param.make "end" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Returns a random integer16 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] ->
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


    { name = fn "uint16Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt16>"
      fn =
        (function
        | _, _, _, [ DUInt16 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType = FQTypeName.fqPackage PackageIDs.Type.Stdlib.uint16ParseError
        TypeReference.result TUInt16 (TCustomType(Ok errorType, []))
      description = "Returns the <type UInt16> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt16 KTString
        let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.uint16ParseError
        let resultError = Dval.resultError KTUInt16 (KTCustomType(typeName, []))
        (function
        | _, _, _, [ DString s ] ->
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


    { name = fn "uint16FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts an Int8 to a 16-bit unsigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, _, [ DInt8 a ] ->
          if (a < 0y) then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TUInt16
      description = "Converts a UInt8 to a 16-bit unsigned integer."
      fn =
        (function
        | _, _, _, [ DUInt8 a ] -> DUInt16(uint16 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts an Int16 to a 16-bit unsigned integer. Returns {{None}} if the value is less than 0."
      fn =
        (function
        | _, _, _, [ DInt16 a ] ->
          if (a < 0s) then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts an Int32 to a 16-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 65535."
      fn =
        (function
        | _, _, _, [ DInt32 a ] ->
          if
            (a < int32 System.UInt16.MinValue) || (a > int32 System.UInt16.MaxValue)
          then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts a UInt32 to a 16-bit unsigned integer. Returns {{None}} if the value is greater than 65535."
      fn =
        (function
        | _, _, _, [ DUInt32 a ] ->
          if (a > uint32 System.UInt16.MaxValue) then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts an Int64 to a 16-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 65535."
      fn =
        (function
        | _, _, _, [ DInt64 a ] ->
          if
            (a < int64 System.UInt16.MinValue) || (a > int64 System.UInt16.MaxValue)
          then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts a UInt64 to a 16-bit unsigned integer. Returns {{None}} if the value is greater than 65535."
      fn =
        (function
        | _, _, _, [ DUInt64 a ] ->
          if (a > uint64 System.UInt16.MaxValue) then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts an Int128 to a 16-bit unsigned integer. Returns {{None}} if the value is less than 0 or greater than 65535."
      fn =
        (function
        | _, _, _, [ DInt128 a ] ->
          if
            (a < System.Int128.op_Implicit System.UInt16.MinValue)
            || (a > System.Int128.op_Implicit System.UInt16.MaxValue)
          then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint16FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts a UInt128 to a 16-bit unsigned integer. Returns {{None}} if the value is greater than 65535."
      fn =
        (function
        | _, _, _, [ DUInt128 a ] ->
          if (a > System.UInt128.op_Implicit System.UInt16.MaxValue) then
            Dval.optionNone KTUInt16 |> Ply
          else
            Dval.optionSome KTUInt16 (DUInt16(uint16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
