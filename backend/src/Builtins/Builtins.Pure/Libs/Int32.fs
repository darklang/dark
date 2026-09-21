module Builtins.Pure.Libs.Int32

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

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int32ParseError ())
    DEnum(typeName, typeName, [], caseName, fields)


let fns () : List<BuiltInFn> =
  [ { name = fn "int32Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0.\n\nUse <fn "
        + "Int32.remainder> if you want the remainder after division, which has a "
        + "different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [| DInt32 v; DInt32 m |] ->
          if m = 0 then
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.threadID
          else if m < 0 then
            RTE.Ints.NegativeModulus |> RTE.Int |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0 then m + result else result
            Ply(DInt32 result)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt32 ""; Param.make "divisor" TInt32 "" ]
      returnType = TypeReference.result TInt32 TString
      description =
        "Returns the integer remainder left over after dividing <param value> "
        + "by <param divisor>, as a <type Result>.\n\nFor example, "
        + "{{Int32.remainder 15 6 == Ok 3}}. The remainder will be negative only "
        + "if {{<var value> < 0}}.\n\nThe sign of <param divisor> doesn't "
        + "influence the outcome.\n\nReturns an {{Error}} if <param divisor> is "
        + "{{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt32 KTString r |> Ply
        (function
        | _, vm, _, [| DInt32 v; DInt32 d |] ->
          (try
            v % d |> DInt32 |> resultOk
           with e ->
             if d = 0 then
               RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID
             else
               Exception.raiseInternal
                 "unexpected failure case in Int32.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Adds two 32-bit signed integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a + b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Subtracts two 32-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a - b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Multiplies two 32-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a * b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt32 ""; Param.make "exponent" TInt32 "" ]
      returnType = TInt32
      description =
        "Raise <param base> to the power of <param exponent>. <param exponent> "
        + "must to be positive. Overflow wraps around."
      fn =
        (function
        | _, vm, _, [| DInt32 number; DInt32 exp |] ->
          if exp < 0 then
            RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID
          else
            // wrap on overflow via modular exponentiation
            let m = System.Numerics.BigInteger.Pow(bigint 2, 32)
            let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
            let r = ((r % m) + m) % m
            let r = if r >= m / bigint 2 then r - m else r
            int32 r |> DInt32 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Divides two 32-bit signed integers"
      fn =
        (function
        | _, vm, _, [| DInt32 a; DInt32 b |] ->
          if b = 0 then
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID
          else if a = System.Int32.MinValue && b = -1 then
            // wraps back to MinValue (the division itself would throw)
            Ply(DInt32 System.Int32.MinValue)
          else
            Ply(DInt32(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt32
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Ply(DInt32(-a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int32>"
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int32ParseError ())

        TypeReference.result TInt32 (TCustomType(NR.ok errorType, []))
      description = "Returns the <type Int32> value of a <type String>"
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.int32ParseError ())
        let resultOk = Dval.resultOk KTInt32 (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTInt32 (KTCustomType(typeName, []))
        (function
        | _, _, _, [| DString s |] ->
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
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt32 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DInt32 int |] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise AND on two <type Int32> values"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a &&& b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise OR on two <type Int32> values"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a ||| b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise XOR on two <type Int32> values"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a ^^^ b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise NOT on an <type Int32> value"
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Ply(DInt32(~~~a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise left shift of an <type Int32> value"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a <<< int b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise right shift of an <type Int32> value"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a >>> int b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
