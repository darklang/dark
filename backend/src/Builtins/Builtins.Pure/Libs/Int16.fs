module Builtins.Pure.Libs.Int16

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

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int16ParseError ())
    DEnum(typeName, typeName, [], caseName, fields)



let fns () : List<BuiltInFn> =
  [ { name = fn "int16Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res <"
        + " b}}. The modulus <param b> must be greater than 0, and this raises "
        + "an error if it is not; <fn Int16.mod> returns a <type Result> "
        + "instead."
      fn =
        (function
        | _, vm, _, [| DInt16 a; DInt16 b |] ->
          IntDivision.moduloInt16 a b
          |> IntDivision.modulusOrRaise vm
          |> DInt16
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt16 ""; Param.make "divisor" TInt16 "" ]
      returnType = TInt16
      description =
        "Returns the integer remainder, with the sign of the dividend. "
        + "Raises an error if the divisor is zero."
      fn =
        (function
        | _, vm, _, [| DInt16 a; DInt16 b |] ->
          IntDivision.remainderInt16 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt16
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Adds two 16-bit signed integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Subtracts two 16-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Multiplies two 16-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt16 ""; Param.make "exponent" TInt16 "" ]
      returnType = TInt16
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around. Raises an error if <param exponent> is negative."
      fn =
        (function
        | _, vm, _, [| DInt16 number; DInt16 exp |] ->
          if exp < 0s then
            RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID
          else
            // wrap on overflow via modular exponentiation
            let m = System.Numerics.BigInteger.Pow(bigint 2, 16)
            let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
            let r = ((r % m) + m) % m
            let r = if r >= m / bigint 2 then r - m else r
            int16 r |> DInt16 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description =
        "Divides <param a> by <param b>, rounding towards zero. Dividing the "
        + "minimum value by {{-1}} wraps back to the minimum value. Raises an "
        + "error if <param b> is {{0}}; <fn Int16.divide> returns a <type "
        + "Result> instead."
      fn =
        (function
        | _, vm, _, [| DInt16 a; DInt16 b |] ->
          IntDivision.divideInt16 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt16
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt16
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Ply(DInt16(-a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TString
      description = "Stringify <param int16>"
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int16>"
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TFloat
      description = "Converts an <type Int16> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int16ParseError ())
        TypeReference.result TInt16 (TCustomType(NR.ok errorType, []))
      description = "Returns the <type Int16> value of a <type String>"
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int16ParseError ())
        let resultOk = Dval.resultOk KTInt16 (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTInt16 (KTCustomType(typeName, []))
        (function
        | _, _, _, [| DString s |] ->
          try
            s |> System.Convert.ToInt16 |> DInt16 |> resultOk |> Ply
          with
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply

          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply

        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt16
      description = "Converts an Int8 to a 16-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> DInt16(int16 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt16
      description = "Converts a UInt8 to a 16-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> DInt16(int16 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts a UInt16 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [| DUInt16 a |] ->
          if a > uint16 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an Int32 to a 16-bit signed integer. Returns {{None}} if the value is less than -32768 or greater than 32767"
      fn =
        (function
        | _, _, _, [| DInt32 a |] ->
          if a < int32 System.Int16.MinValue || a > int32 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts a UInt32 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [| DUInt32 a |] ->
          if a > uint32 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an Int64 to a 16-bit signed integer. Returns {{None}} if the value is less than -32768 or greater than 32767"
      fn =
        (function
        | _, _, _, [| DInt64 a |] ->
          if a < int64 System.Int16.MinValue || a > int64 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts a UInt64 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [| DUInt64 a |] ->
          if a > uint64 System.Int16.MaxValue then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an Int128 to a 16-bit signed integer. Returns {{None}} if the value is less than -32768 or greater than 32767"
      fn =
        (function
        | _, _, _, [| DInt128 a |] ->
          if
            a < System.Int128.op_Implicit System.Int16.MinValue
            || a > System.Int128.op_Implicit System.Int16.MaxValue
          then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts an UInt128 to a 16-bit signed integer. Returns {{None}} if the value is greater than 32767"
      fn =
        (function
        | _, _, _, [| DUInt128 a |] ->
          if a > 32767Z then
            Dval.optionNone KTInt16 |> Ply
          else
            Dval.optionSome KTInt16 (DInt16(int16 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Bitwise AND on two <type Int16> values"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Bitwise OR on two <type Int16> values"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Bitwise XOR on two <type Int16> values"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt16
      description = "Bitwise NOT on an <type Int16> value"
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Ply(DInt16(~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Bitwise left shift of an <type Int16> value"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Bitwise right shift of an <type Int16> value"
      fn =
        (function
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
