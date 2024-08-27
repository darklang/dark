module BuiltinExecution.Libs.Int16

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

    let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.int16ParseError
    DEnum(typeName, typeName, [], caseName, fields)



let fns : List<BuiltInFn> =
  [ { name = fn "int16Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn Int16.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [ DInt16 v; DInt16 m ] ->
          if m = 0s then
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.callStack
          else if m < 0s then
            RTE.Ints.NegativeModulus |> RTE.Int |> raiseRTE vm.callStack
          else
            let result = v % m
            let result = if result < 0s then m + result else result
            Ply(DInt16 result)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt16 ""; Param.make "divisor" TInt16 "" ]
      returnType = TypeReference.result TInt16 TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
        <param divisor>, as a <type Result>.

        For example, {{Int16.remainder 15 6 == Ok 3}}. The remainder will be
        negative only if {{<var value> < 0}}.

        The sign of <param divisor> doesn't influence the outcome.

        Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt16 KTString r |> Ply
        (function
        | _, vm, _, [ DInt16 v; DInt16 d ] ->
          (try
            v % d |> DInt16 |> resultOk
           with e ->
             if d = 0s then
               RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.callStack
             else
               Exception.raiseInternal
                 "unexpected failure case in Int16.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Adds two 16-bit signed integers together"
      fn =
        (function
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          try
            let result = Checked.(+) a b
            Ply(DInt16(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Subtracts two 16-bit signed integers"
      fn =
        (function
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          try
            let result = Checked.(-) a b
            Ply(DInt16(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "multiplies two 16-bit signed integers"
      fn =
        (function
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          try
            let result = Checked.(*) a b
            Ply(DInt16(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt16 ""; Param.make "exponent" TInt16 "" ]
      returnType = TInt16
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Return value wrapped in a {{Result}} "
      fn =
        (function
        | _, vm, _, [ DInt16 number; DInt16 exp ] ->
          (try
            if exp < 0s then
              RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.callStack
            else
              (bigint number) ** (int exp) |> int16 |> DInt16 |> Ply
           with :? System.OverflowException ->
             RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Divides two 16-bit signed integers"
      fn =
        (function
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          if b = 0s then
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.callStack
          else if a = int16 System.Int16.MinValue && b = -1s then
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
          else
            let result = a / b
            if result < System.Int16.MinValue || result > System.Int16.MaxValue then
              RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
            else
              Ply(DInt16(int16 result))

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt16
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, vm, _, [ DInt16 a ] ->
          if a = System.Int16.MinValue then
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
          else
            let result = -a
            Ply(DInt16 result)

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TString
      description = "Stringify <param int16>"
      fn =
        (function
        | _, _, _, [ DInt16 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TFloat
      description = "Converts an <type Int16> to a <type Float>"
      fn =
        (function
        | _, _, _, [ DInt16 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt16 ""; Param.make "end" TInt16 "" ]
      returnType = TInt16
      description =
        "Returns a random integer16 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DInt16 a; DInt16 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)

          let correctRange = 1

          int lower + randomSeeded().Next(int upper - int lower + correctRange)
          |> int16
          |> DInt16
          |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType = FQTypeName.fqPackage PackageIDs.Type.Stdlib.int16ParseError
        TypeReference.result TInt16 (TCustomType(Ok errorType, []))
      description = "Returns the <type Int16> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt16 KTString
        let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.int16ParseError
        let resultError = Dval.resultError KTInt16 (KTCustomType(typeName, []))
        (function
        | _, _, _, [ DString s ] ->
          try
            s |> System.Convert.ToInt16 |> DInt16 |> resultOk |> Ply
          with
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply

          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt16
      description = "Converts an Int8 to a 16-bit signed integer."
      fn =
        (function
        | _, _, _, [ DInt8 a ] -> DInt16(int16 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt16
      description = "Converts a UInt8 to a 16-bit signed integer."
      fn =
        (function
        | _, _, _, [ DUInt8 a ] -> DInt16(int16 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts a UInt16 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [ DUInt16 a ] ->
          if a > uint16 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an Int32 to a 16-bit signed integer. Returns {{None}} if the value is less than -32768 or greater than 32767"
      fn =
        (function
        | _, _, _, [ DInt32 a ] ->
          if a < int32 System.Int16.MinValue || a > int32 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts a UInt32 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [ DUInt32 a ] ->
          if a > uint32 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an Int64 to a 16-bit signed integer. Returns {{None}} if the value is less than -32768 or greater than 32767"
      fn =
        (function
        | _, _, _, [ DInt64 a ] ->
          if a < int64 System.Int16.MinValue || a > int64 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts a UInt64 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [ DUInt64 a ] ->
          if a > uint64 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an Int128 to a 16-bit signed integer. Returns {{None}} if the value is less than -32768 or greater than 32767"
      fn =
        (function
        | _, _, _, [ DInt128 a ] ->
          if
            a < System.Int128.op_Implicit System.Int16.MinValue
            || a > System.Int128.op_Implicit System.Int16.MaxValue
          then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an UInt128 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [ DUInt128 a ] ->
          if a > 32767Z then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
