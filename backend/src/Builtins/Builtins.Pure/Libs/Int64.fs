module Builtins.Pure.Libs.Int64

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

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int64ParseError ())
    DEnum(typeName, typeName, [], caseName, fields)



let fns () : List<BuiltInFn> =
  [ { name = fn "int64Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res <"
        + " b}}. The modulus <param b> must be greater than 0, and this raises "
        + "an error if it is not; <fn Int64.mod> returns a <type Result> "
        + "instead."
      fn =
        (function
        | _, vm, _, [| DInt64 a; DInt64 b |] ->
          IntDivision.moduloInt64 a b
          |> IntDivision.modulusOrRaise vm
          |> Dval.dint64
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt64 ""; Param.make "divisor" TInt64 "" ]
      returnType = TInt64
      description =
        "Returns the integer remainder, with the sign of the dividend. "
        + "Raises an error if the divisor is zero."
      fn =
        (function
        | _, vm, _, [| DInt64 a; DInt64 b |] ->
          IntDivision.remainderInt64 a b
          |> IntDivision.divisionOrRaise vm
          |> Dval.dint64
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Adds two integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a + b))
        | _ -> incorrectArgs ())
      // CLEANUP: SQL pushdown for Int64 arithmetic does not match runtime
      // overflow semantics. Runtime evaluation wraps, but SQLite promotes
      // overflowing integer arithmetic to REAL for these add/sub/mul/div/pow specs.
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Subtracts two integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Multiplies two integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt64 ""; Param.make "exponent" TInt64 "" ]
      returnType = TInt64
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around. Raises an error if <param exponent> is negative."
      fn =
        (function
        | _, vm, _, [| DInt64 number; DInt64 exp |] ->
          if exp < 0L then
            RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID
          else
            // wrap on overflow via modular exponentiation, which stays cheap
            // even for huge exponents (no enormous bigint is built)
            let m = System.Numerics.BigInteger.Pow(bigint 2, 64)
            let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
            let r = ((r % m) + m) % m
            let r = if r >= m / bigint 2 then r - m else r
            int64 r |> Dval.dint64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description =
        "Divides <param a> by <param b>, rounding towards zero. Dividing the "
        + "minimum value by {{-1}} wraps back to the minimum value. Raises an "
        + "error if <param b> is {{0}}; <fn Int64.divide> returns a <type "
        + "Result> instead."
      fn =
        (function
        | _, vm, _, [| DInt64 a; DInt64 b |] ->
          IntDivision.divideInt64 a b
          |> IntDivision.divisionOrRaise vm
          |> Dval.dint64
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TInt64
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [| DInt64 a |] -> Ply(Dval.dint64 (-a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int64>"
      fn =
        (function
        | _, _, _, [| DInt64 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TFloat
      description = "Converts an <type Int64> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DInt64 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int64ParseError ())
        TypeReference.result TInt64 (TCustomType(NR.ok errorType, []))
      description = "Returns the <type Int64> value of a <type String>"
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int64ParseError ())
        let resultOk = Dval.resultOk KTInt64 (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTInt64 (KTCustomType(typeName, []))
        (function
        | _, _, _, [| DString s |] ->
          try
            s |> System.Convert.ToInt64 |> Dval.dint64 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt64 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DInt64 int |] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt64
      description = "Converts an Int8 to a 64-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Dval.dint64 (int64 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt64
      description = "Converts a UInt8 to a 64-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> Dval.dint64 (int64 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt64
      description = "Converts an Int16 to a 64-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Dval.dint64 (int64 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TInt64
      description = "Converts a UInt16 to a 64-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Dval.dint64 (int64 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt64
      description = "Converts an Int32 to a 64-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Dval.dint64 (int64 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TInt64
      description = "Converts a UInt32 to a 64-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Dval.dint64 (int64 a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts a UInt64 to a 64-bit signed integer. Returns {{None}} if the value is greater than 9223372036854775807."
      fn =
        (function
        | _, _, _, [| DUInt64 a |] ->
          if (a > uint64 System.Int64.MaxValue) then
            Dval.optionNone KTInt64 |> Ply
          else
            Dval.optionSome KTInt64 (DInt64(int64 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts an Int128 to a 64-bit signed integer. Returns {{None}} if the value is less than -9223372036854775808 or greater than 9223372036854775807."
      fn =
        (function
        | _, _, _, [| DInt128 a |] ->
          if
            (a < System.Int128.op_Implicit System.Int64.MinValue)
            || (a > System.Int128.op_Implicit System.Int64.MaxValue)
          then
            Dval.optionNone KTInt64 |> Ply
          else
            Dval.optionSome KTInt64 (DInt64(int64 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64FromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts a UInt128 to a 64-bit signed integer. Returns {{None}} if the value is greater than 9223372036854775807."
      fn =
        (function
        | _, _, _, [| DUInt128 a |] ->
          if (a > 9223372036854775807Z) then
            Dval.optionNone KTInt64 |> Ply
          else
            Dval.optionSome KTInt64 (DInt64(int64 a)) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Bitwise AND on two <type Int64> values"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Bitwise OR on two <type Int64> values"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Bitwise XOR on two <type Int64> values"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TInt64
      description = "Bitwise NOT on an <type Int64> value"
      fn =
        (function
        | _, _, _, [| DInt64 a |] -> Ply(Dval.dint64 (~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Bitwise left shift of an <type Int64> value"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 ""; Param.make "b" TInt64 "" ]
      returnType = TInt64
      description = "Bitwise right shift of an <type Int64> value"
      fn =
        (function
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
