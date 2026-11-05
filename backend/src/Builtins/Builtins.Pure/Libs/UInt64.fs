module Builtins.Pure.Libs.UInt64

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint64Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0.\n\nUse <fn "
        + "UInt64.remainder> if you want the remainder after division, which has "
        + "a different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [| DUInt64 v; DUInt64 m |] ->
          if m = 0UL then
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0UL then m + result else result
            Ply(DUInt64(result))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Adds 64-bit unsigned integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Subtracts 64-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Multiplies 64-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64Power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt64 ""; Param.make "exponent" TUInt64 "" ]
      returnType = TUInt64
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around."
      fn =
        (function
        | _, _, _, [| DUInt64 number; DUInt64 exp |] ->
          // wrap on overflow via modular exponentiation, which stays cheap even
          // for huge exponents (no enormous bigint is built)
          let m = System.Numerics.BigInteger.Pow(bigint 2, 64)
          let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
          let r = ((r % m) + m) % m
          uint64 r |> DUInt64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Divides 64-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [| DUInt64 a; DUInt64 b |] ->
          if b = 0UL then
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID
          else
            Ply(DUInt64(a / b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }




    { name = fn "uint64Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt64>"
      fn =
        (function
        | _, _, _, [| DUInt64 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TFloat
      description = "Converts an <type UInt64> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DUInt64 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TUInt64 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DUInt64 int |] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Bitwise AND on two <type UInt64> values"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Bitwise OR on two <type UInt64> values"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Bitwise XOR on two <type UInt64> values"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TUInt64
      description = "Bitwise NOT on a <type UInt64> value"
      fn =
        (function
        | _, _, _, [| DUInt64 a |] -> Ply(DUInt64(~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Bitwise left shift of a <type UInt64> value"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 ""; Param.make "b" TUInt64 "" ]
      returnType = TUInt64
      description = "Bitwise right shift of a <type UInt64> value"
      fn =
        (function
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
