module Builtins.Pure.Libs.UInt16

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution


module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.uint16ParseError ())
    DEnum(typeName, typeName, [], caseName, fields)



let fns () : List<BuiltInFn> =
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
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0us then m + result else result
            Ply(DUInt16(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Adds two 16-bit unsigned integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Subtracts two 16-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Multiplies two 16-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16Power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt16 ""; Param.make "exponent" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Raise <param base> to the power of <param exponent>.
        <param exponent> must to be positive.
        Overflow wraps around."
      fn =
        (function
        | _, _, _, [ DUInt16 number; DUInt16 exp ] ->
          // wrap on overflow via modular exponentiation
          let m = System.Numerics.BigInteger.Pow(bigint 2, 16)
          let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
          let r = ((r % m) + m) % m
          uint16 r |> DUInt16 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
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
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID
          else
            Ply(DUInt16(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.uint16ParseError ())
        TypeReference.result TUInt16 (TCustomType(NR.ok errorType, []))
      description = "Returns the <type UInt16> value of a <type String>"
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.uint16ParseError ())
        let resultOk = Dval.resultOk KTUInt16 (KTCustomType(typeName, []))
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise AND on two <type UInt16> values"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a &&& b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise OR on two <type UInt16> values"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a ||| b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise XOR on two <type UInt16> values"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a ^^^ b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise NOT on a <type UInt16> value"
      fn =
        (function
        | _, _, _, [ DUInt16 a ] -> Ply(DUInt16(~~~a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise left shift of a <type UInt16> value"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a <<< int b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "uint16ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise right shift of a <type UInt16> value"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DUInt16(a >>> int b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
