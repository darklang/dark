module Builtins.Pure.Libs.Int128

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError
module NoModule = Builtins.Pure.Libs.NoModule


let fns () : List<BuiltInFn> =
  [ { name = fn "int128Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0.\n\nUse <fn "
        + "Int128.checkedRemainder> if you want the remainder after division, which has "
        + "a different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [| DInt128 v; DInt128 m |] ->
          if m = System.Int128.Zero then
            RTE.BigInts.ZeroModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else if m < System.Int128.Zero then
            RTE.BigInts.NegativeModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < System.Int128.Zero then m + result else result
            Ply(DInt128(result))
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
            RTE.BigInts.NegativeExponent |> RTE.BigInt |> raiseRTE vm.threadID
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
      description = "Divides two 128-bit signed integers"
      fn =
        (function
        | _, vm, _, [| DInt128 a; DInt128 b |] ->
          if b = System.Int128.Zero then
            RTE.BigInts.DivideByZeroError |> RTE.BigInt |> raiseRTE vm.threadID
          elif a = System.Int128.MinValue && b = System.Int128.NegativeOne then
            // wraps back to MinValue (the division itself would throw)
            Ply(DInt128 System.Int128.MinValue)
          else
            Ply(DInt128(a / b))
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
