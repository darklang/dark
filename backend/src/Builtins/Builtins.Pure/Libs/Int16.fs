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
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0.\n\nUse <fn "
        + "Int16.remainder> if you want the remainder after division, which has a "
        + "different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [| DInt16 v; DInt16 m |] ->
          if m = 0s then
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.threadID
          else if m < 0s then
            RTE.Ints.NegativeModulus |> RTE.Int |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0s then m + result else result
            Ply(DInt16 result)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Remainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt16 ""; Param.make "divisor" TInt16 "" ]
      returnType = TypeReference.result TInt16 TString
      description =
        "Returns the integer remainder left over after dividing <param value> "
        + "by <param divisor>, as a <type Result>.\n\nFor example, "
        + "{{Int16.remainder 15 6 == Ok 3}}. The remainder will be negative only "
        + "if {{<var value> < 0}}.\n\nThe sign of <param divisor> doesn't "
        + "influence the outcome.\n\nReturns an {{Error}} if <param divisor> is "
        + "{{0}}."
      fn =
        let resultOk r = Dval.resultOk KTInt16 KTString r |> Ply
        (function
        | _, vm, _, [| DInt16 v; DInt16 d |] ->
          (try
            v % d |> DInt16 |> resultOk
           with e ->
             if d = 0s then
               RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID
             else
               Exception.raiseInternal
                 "unexpected failure case in Int16.remainder"
                 [ "v", v; "d", d ]
                 e)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt16 ""; Param.make "exponent" TInt16 "" ]
      returnType = TInt16
      description =
        "Raise <param base> to the power of <param exponent>. <param exponent> "
        + "must to be positive. Overflow wraps around."
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
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 ""; Param.make "b" TInt16 "" ]
      returnType = TInt16
      description = "Divides two 16-bit signed integers"
      fn =
        (function
        | _, vm, _, [| DInt16 a; DInt16 b |] ->
          if b = 0s then
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID
          else
            // -32768s / -1s divides in int32 then narrows, wrapping instead of throwing
            Ply(DInt16(a / b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
