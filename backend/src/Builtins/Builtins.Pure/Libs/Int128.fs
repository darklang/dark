module Builtins.Pure.Libs.Int128

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageRefs = LibExecution.PackageRefs
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution
module NoModule = Builtins.Pure.Libs.NoModule


module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int128ParseError ())
    DEnum(typeName, typeName, [], caseName, fields)


let fns () : List<BuiltInFn> =
  [ { name = fn "int128Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res <"
        + " b}}. The modulus <param b> must be greater than 0, and this raises "
        + "an error if it is not; <fn Int128.mod> returns a <type Result> "
        + "instead."
      fn =
        (function
        | _, vm, _, [| DInt128 a; DInt128 b |] ->
          IntDivision.moduloInt128 a b
          |> IntDivision.modulusOrRaise vm
          |> DInt128
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt128 ""; Param.make "divisor" TInt128 "" ]
      returnType = TInt128
      description =
        "Returns the integer remainder, with the sign of the dividend. "
        + "Raises an error if the divisor is zero."
      fn =
        (function
        | _, vm, _, [| DInt128 a; DInt128 b |] ->
          IntDivision.remainderInt128 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt128
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Adds two 128-bit signed integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Subtracts two 128-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Multiplies two 128-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt128 ""; Param.make "exponent" TInt128 "" ]
      returnType = TInt128
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around. Raises an error if <param exponent> is negative."
      fn =
        (function
        | _, vm, _, [| DInt128 number; DInt128 exp |] ->
          if exp < System.Int128.Zero then
            RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID
          else
            // `BigInteger.Parse(string n)`, not a conversion: there is no BigInteger
            // overload for Int128.
            NoModule.powSigned
              128
              (System.Numerics.BigInteger.Parse(string number))
              (System.Numerics.BigInteger.Parse(string exp))
            |> fun r -> System.Int128.Parse(r.ToString()) |> DInt128 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description =
        "Divides <param a> by <param b>, rounding towards zero. Dividing the "
        + "minimum value by {{-1}} wraps back to the minimum value. Raises an "
        + "error if <param b> is {{0}}; <fn Int128.divide> returns a <type "
        + "Result> instead."
      fn =
        (function
        | _, vm, _, [| DInt128 a; DInt128 b |] ->
          IntDivision.divideInt128 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt128
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TInt128
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [| DInt128 a |] -> Ply(DInt128(-a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TString
      description = "Converts an <type Int128> to a <type String>"
      fn =
        (function
        | _, _, _, [| DInt128 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TFloat
      description = "Converts an <type Int128> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DInt128 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int128>"
      fn =
        (function
        | _, _, _, [| DInt128 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TInt128
          (TCustomType(
            { originalName = []
              resolved =
                Ok(
                  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int128ParseError ())
                ) },
            []
          ))
      description = "Returns the <type Int128> value of a <type String>"
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int128ParseError ())
        let resultOk = Dval.resultOk KTInt128 (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTInt128 (KTCustomType(typeName, []))
        (function
        | _, _, _, [| DString s |] ->
          try
            s |> System.Int128.Parse |> DInt128 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt128
      description = "Converts an Int8 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt128
      description = "Converts a UInt8 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt128
      description = "Converts an Int16 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TInt128
      description = "Converts a UInt16 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt128
      description = "Converts an Int32 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TInt128
      description = "Converts a UInt32 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TInt128
      description = "Converts an Int64 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DInt64 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TInt128
      description = "Converts a UInt64 to a 128-bit signed integer."
      fn =
        (function
        | _, _, _, [| DUInt64 a |] -> DInt128(System.Int128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Bitwise AND on two <type Int128> values"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Bitwise OR on two <type Int128> values"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Bitwise XOR on two <type Int128> values"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TInt128
      description = "Bitwise NOT on an <type Int128> value"
      fn =
        (function
        | _, _, _, [| DInt128 a |] ->
          Ply(DInt128(System.Int128.op_OnesComplement (a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Bitwise left shift of an <type Int128> value"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Bitwise right shift of an <type Int128> value"
      fn =
        (function
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
