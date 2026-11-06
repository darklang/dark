module Builtins.Pure.Libs.UInt32

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint32Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0.\n\nUse <fn "
        + "UInt32.remainder> if you want the remainder after division, which has "
        + "a different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [| DUInt32 v; DUInt32 m |] ->
          if m = 0ul then
            RTE.BigInts.ZeroModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0ul then m + result else result
            Ply(DUInt32(result))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description =
        "Adds two 32-bit unsigned integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Subtracts two 32-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Multiplies two 32-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32Power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt32 ""; Param.make "exponent" TUInt32 "" ]
      returnType = TUInt32
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around."
      fn =
        (function
        | _, _, _, [| DUInt32 number; DUInt32 exp |] ->
          // wrap on overflow via modular exponentiation
          let m = System.Numerics.BigInteger.Pow(bigint 2, 32)
          let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
          let r = ((r % m) + m) % m
          uint32 r |> DUInt32 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Divides two 32-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [| DUInt32 a; DUInt32 b |] ->
          if b = 0ul then
            RTE.BigInts.DivideByZeroError |> RTE.BigInt |> raiseRTE vm.threadID
          else
            Ply(DUInt32(a / b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TString
      description = "Stringify <param uint32>"
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TFloat
      description = "Converts an <type UInt32> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt32>"
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Bitwise AND on two <type UInt32> values"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Bitwise OR on two <type UInt32> values"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Bitwise XOR on two <type UInt32> values"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TUInt32
      description = "Bitwise NOT on a <type UInt32> value"
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Ply(DUInt32(~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Bitwise left shift of a <type UInt32> value"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 ""; Param.make "b" TUInt32 "" ]
      returnType = TUInt32
      description = "Bitwise right shift of a <type UInt32> value"
      fn =
        (function
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
