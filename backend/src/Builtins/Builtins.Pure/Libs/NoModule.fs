module Builtins.Pure.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module ValueType = LibExecution.ValueType
module RTE = RuntimeError
module Interpreter = LibExecution.Interpreter
module Pending = LibExecution.Pending
module IntOps = LibExecution.IntOps


// Builtin types cannot express “both operands have the same numeric type,” so
// these operator signatures are intentionally loose. Runtime dispatch and the
// at-rest checker enforce the constraint.
let varA = TVariable "a"
let varB = TVariable "b"


/// Shared body for `equals` / `notEquals` builtins — VT-merge type
/// check + structural compare.
let private equalsBuiltinImpl (vm : VMState) (a : Dval) (b : Dval) : bool =
  let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
  match ValueType.merge vtA vtB with
  | Error _ -> RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB) |> raiseRTE vm.threadID
  | Ok _ -> Dval.equals a b


// Polymorphic numeric operators (`+`, `-`, `*`, `/`, `%`, and comparisons).
// Each inspects the runtime value type and operates on two values of the same
// numeric type, mirroring how `equals` already dispatches at runtime.

/// Raised when a numeric operator gets operands that aren't two values of the
/// same numeric type (e.g. `1L + 2.0`, `"a" + "b"`).
let private numericTypeError (vm : VMState) (a : Dval) (b : Dval) : 'a =
  RTE.NumericOperationOnIncompatibleTypes(Dval.toValueType a, Dval.toValueType b)
  |> raiseRTE vm.threadID

let private divideByZero (vm : VMState) : 'a =
  RTE.BigInts.DivideByZeroError |> RTE.BigInt |> raiseRTE vm.threadID

let private zeroModulus (vm : VMState) : 'a =
  RTE.BigInts.ZeroModulus |> RTE.BigInt |> raiseRTE vm.threadID

let private negativeModulus (vm : VMState) : 'a =
  RTE.BigInts.NegativeModulus |> RTE.BigInt |> raiseRTE vm.threadID

let private negativeExponent (vm : VMState) : 'a =
  RTE.BigInts.NegativeExponent |> RTE.BigInt |> raiseRTE vm.threadID

let private outOfRange (vm : VMState) : 'a =
  RTE.BigInts.OutOfRange |> RTE.BigInt |> raiseRTE vm.threadID

/// Decimal arithmetic raises rather than wrapping or rounding to infinity: a result past
/// 79,228,162,514,264,337,593,543,950,335 is out of range.
let private decimalChecked (vm : VMState) (op : unit -> decimal) : Ply<Dval> =
  try
    Ply(DDecimal(op ()))
  with :? System.OverflowException ->
    RTE.DecimalOutOfRange |> raiseRTE vm.threadID

/// A polymorphic operator: `IntOps` for one or two integers of one fixed-width type, which is
/// where their semantics live, and <param rest> for every other type.
let private withInts (op : int) (rest : BuiltInFnSig) : BuiltInFnSig =
  fun (struct (_, vm, _, args) as call) ->
    let outcome =
      match args with
      | [| a; b |] -> IntOps.binary op a b
      | [| a |] -> IntOps.unary op a
      | _ -> IntOps.NotInts
    match outcome with
    | IntOps.Value v -> Ply v
    | IntOps.Raises e -> e |> RTE.BigInt |> raiseRTE vm.threadID
    | IntOps.NotInts -> rest call


/// `unwrap` of a value with no read in flight at its top.
let private unwrapLanded (vm : VMState) (dval : Dval) : Ply<Dval> =
  match dval with

  // Success: extract `Some` out of an Option
  | DEnum(FQTypeName.Package(Hash id), _, _, "Some", [ value ]) when
    id = PackageRefs.Type.Stdlib.option ()
    ->
    Ply value

  // Success: extract `Ok` out of a Result
  | DEnum(FQTypeName.Package(Hash id), _, _, "Ok", [ value ]) when
    id = PackageRefs.Type.Stdlib.result ()
    ->
    Ply value

  // Error: expected Some, got None
  | DEnum(FQTypeName.Package(Hash id), _, _, "None", []) when
    id = PackageRefs.Type.Stdlib.option ()
    ->
    RuntimeError.Unwraps.GotNone |> RuntimeError.Unwrap |> raiseRTE vm.threadID

  // Error: expected Ok, got Error
  | DEnum(FQTypeName.Package(Hash id), _, _, "Error", [ value ]) when
    id = PackageRefs.Type.Stdlib.result ()
    ->
    RuntimeError.Unwraps.GotError value
    |> RuntimeError.Unwrap
    |> raiseRTE vm.threadID

  // Error: single dval, but not an Option or Result
  | otherDval ->
    RuntimeError.Unwraps.NonOptionOrResult otherDval
    |> RuntimeError.Unwrap
    |> raiseRTE vm.threadID


let fns () : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, vm, _, [| a; b |] -> equalsBuiltinImpl vm a b |> Dval.bool |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "notEquals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are not equal"
      fn =
        (function
        | _, vm, _, [| a; b |] -> equalsBuiltinImpl vm a b |> not |> Dval.bool |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Adds two values of the same type. Strings concatenate. For numbers: fixed-width "
        + "integer overflow wraps around, the arbitrary-precision BigInt grows instead of "
        + "overflowing, and float arithmetic follows IEEE (overflow to infinity)."
      fn =
        withInts IntOps.add (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.dbigint (DarkBigInt.add a b))
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a + b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(DFloat32(a + b))
          | _, vm, _, [| DDecimal a; DDecimal b |] ->
            decimalChecked vm (fun () -> a + b)
          // `+` concatenates Strings (`impl Add for String`), and this builtin is what `+`
          // lowers to while the package refs are not generated yet, which is how the stdlib's
          // own string concatenation loads on a fresh tree.
          | _, _, _, [| DString a; DString b |] -> Ply(DString(String.append a b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Subtracts two numbers of the same numeric type. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision BigInt grows instead of "
        + "overflowing; float arithmetic follows IEEE (overflow to infinity)."
      fn =
        withInts IntOps.subtract (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.dbigint (DarkBigInt.subtract a b))
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a - b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(DFloat32(a - b))
          | _, vm, _, [| DDecimal a; DDecimal b |] ->
            decimalChecked vm (fun () -> a - b)
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Multiplies two numbers of the same numeric type. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision BigInt grows instead of "
        + "overflowing; float arithmetic follows IEEE (overflow to infinity)."
      fn =
        withInts IntOps.multiply (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.dbigint (DarkBigInt.multiply a b))
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a * b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(DFloat32(a * b))
          | _, vm, _, [| DDecimal a; DDecimal b |] ->
            decimalChecked vm (fun () -> a * b)
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // TODO: make integer `/` and `%` checked by default: return a Result, and callers
    // unwrap. The interpreter may be a little slower, and the compiler should optimize
    // the unwrapping away. Until then, `checkedDivide` / `checkedMod` /
    // `checkedRemainder` on each integer type return a Result.
    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Divides two numbers of the same numeric type. Integer types use "
        + "integer division; dividing an integer by zero raises a runtime error. "
        + "Signed integer overflow (e.g. {{Int64.MinValue / -1}}) wraps around to "
        + "{{MinValue}}."
      fn =
        withInts IntOps.divide (function
          | _, vm, _, [| DBigInt a; DBigInt b |] ->
            if DarkBigInt.isZero b then
              divideByZero vm
            else
              Ply(Dval.dbigint (DarkBigInt.divide a b))
          // Float division by zero follows IEEE semantics (Infinity/NaN), as before
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a / b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(DFloat32(a / b))
          | _, vm, _, [| DDecimal a; DDecimal b |] ->
            if b = 0m then divideByZero vm else decimalChecked vm (fun () -> a / b)
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // TODO: make integer `/` and `%` checked by default: return a Result, and callers
    // unwrap. The interpreter may be a little slower, and the compiler should optimize
    // the unwrapping away. Until then, `checkedDivide` / `checkedMod` /
    // `checkedRemainder` on each integer type return a Result.
    { name = fn "modulo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Wraps <param a> around so that {{0 <= res < b}}, for two integers of the same "
        + "type or two decimals. The modulus <param b> must be greater than 0."
      fn =
        withInts IntOps.modulo (function
          | _, vm, _, [| DBigInt v; DBigInt m |] ->
            let m = DarkBigInt.toBigInt m
            if m = System.Numerics.BigInteger.Zero then
              zeroModulus vm
            elif m < System.Numerics.BigInteger.Zero then
              negativeModulus vm
            else
              let r = DarkBigInt.toBigInt v % m
              Ply(
                Dval.bigInt (
                  if r < System.Numerics.BigInteger.Zero then m + r else r
                )
              )
          | _, vm, _, [| DDecimal v; DDecimal m |] ->
            if m = 0m then
              zeroModulus vm
            elif m < 0m then
              negativeModulus vm
            else
              // exact: a decimal remainder is never rounded, and |r| < m, so m + r fits
              let r = v % m
              Ply(DDecimal(if r < 0m then m + r else r))
          // No Float arm, on purpose: a float `%` has to choose between flooring and
          // truncating, and what a zero divisor does when our own Float division returns
          // Infinity. Swift and Elm have no float modulo either. A Float pair takes the type
          // error below, which through the operator reads as a missing `Modulo` impl.
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "power" 0
      typeParams = []
      parameters = [ Param.make "base" varA ""; Param.make "exponent" varA "" ]
      returnType = varA
      description =
        "Raises a number to the power of another number of the same type. "
        + "Supported for every integer type and Float. "
        + "A negative integer exponent raises an error. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision BigInt grows instead."
      fn =
        withInts IntOps.power (function
          | _, vm, _, [| DBigInt number; DBigInt exp |] ->
            let number = DarkBigInt.toBigInt number
            let exp = DarkBigInt.toBigInt exp
            if exp < System.Numerics.BigInteger.Zero then
              negativeExponent vm
            elif exp > bigint System.Int32.MaxValue then
              // `**` needs an Int32 exponent; only trivial bases are representable.
              if number = System.Numerics.BigInteger.Zero then
                Ply(Dval.bigInt System.Numerics.BigInteger.Zero)
              elif number = System.Numerics.BigInteger.One then
                Ply(Dval.bigInt System.Numerics.BigInteger.One)
              elif number = System.Numerics.BigInteger.MinusOne then
                Ply(
                  Dval.bigInt (
                    if exp % (bigint 2) = System.Numerics.BigInteger.Zero then
                      System.Numerics.BigInteger.One
                    else
                      System.Numerics.BigInteger.MinusOne
                  )
                )
              else
                outOfRange vm
            else
              Ply(Dval.bigInt (number ** (int exp)))
          | _, _, _, [| DFloat number; DFloat exp |] -> Ply(DFloat(number ** exp))
          | _, _, _, [| DFloat32 number; DFloat32 exp |] ->
            Ply(DFloat32(float32 (float number ** float exp)))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Unary negation (`-x`). Supported for signed integer types and Float;
    // negating the minimum signed value wraps back to itself.
    { name = fn "negate" 0
      typeParams = []
      parameters = [ Param.make "a" varA "" ]
      returnType = varA
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        withInts IntOps.negate (function
          | _, _, _, [| DBigInt a |] -> Ply(Dval.dbigint (DarkBigInt.negate a))
          | _, _, _, [| DFloat a |] -> Ply(DFloat(-a))
          | _, _, _, [| DFloat32 a |] -> Ply(DFloat32(-a))
          | _, _, _, [| DDecimal a |] -> Ply(DDecimal(-a))
          | _, vm, _, [| a |] -> numericTypeError vm a a
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }
    // Bitwise operators.
    { name = fn "bitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise AND of two integers of the same type"
      fn =
        withInts IntOps.bitwiseAnd (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.bigInt (DarkBigInt.toBigInt a &&& DarkBigInt.toBigInt b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise OR of two integers of the same type"
      fn =
        withInts IntOps.bitwiseOr (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.bigInt (DarkBigInt.toBigInt a ||| DarkBigInt.toBigInt b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise XOR of two integers of the same type"
      fn =
        withInts IntOps.bitwiseXor (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.bigInt (DarkBigInt.toBigInt a ^^^ DarkBigInt.toBigInt b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Unary `~`. Two's complement, so `~x = -x - 1` on every signed type and on
    // the arbitrary-precision `BigInt`.
    { name = fn "bitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" varA "" ]
      returnType = varA
      description = "Bitwise NOT of an integer, {{~a}}"
      fn =
        withInts IntOps.bitwiseNot (function
          // `~~~` isn't defined on the 128-bit types; call the op directly.
          | _, _, _, [| DBigInt a |] ->
            Ply(
              Dval.bigInt (-(DarkBigInt.toBigInt a) - System.Numerics.BigInteger.One)
            )
          | _, vm, _, [| a |] -> numericTypeError vm a a
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "shiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise left shift of an integer by <param b> bits"
      fn =
        withInts IntOps.shiftLeft (function
          | _, vm, _, [| DBigInt a; DBigInt b |] ->
            let shift = bigIntToInt32 vm b
            if shift < 0 then
              outOfRange vm
            else
              Ply(Dval.bigInt (DarkBigInt.toBigInt a <<< shift))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "shiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description = "Bitwise right shift of an integer by <param b> bits"
      fn =
        withInts IntOps.shiftRight (function
          | _, vm, _, [| DBigInt a; DBigInt b |] ->
            let shift = bigIntToInt32 vm b
            if shift < 0 then
              outOfRange vm
            else
              Ply(Dval.bigInt (DarkBigInt.toBigInt a >>> shift))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        withInts IntOps.greaterThan (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.bool (DarkBigInt.compare a b > 0))
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a > b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(Dval.bool (a > b))
          | _, _, _, [| DDecimal a; DDecimal b |] -> Ply(Dval.bool (a > b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        withInts IntOps.greaterThanOrEqualTo (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.bool (DarkBigInt.compare a b >= 0))
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a >= b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(Dval.bool (a >= b))
          | _, _, _, [| DDecimal a; DDecimal b |] -> Ply(Dval.bool (a >= b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        withInts IntOps.lessThan (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.bool (DarkBigInt.compare a b < 0))
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a < b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(Dval.bool (a < b))
          | _, _, _, [| DDecimal a; DDecimal b |] -> Ply(Dval.bool (a < b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        withInts IntOps.lessThanOrEqualTo (function
          | _, _, _, [| DBigInt a; DBigInt b |] ->
            Ply(Dval.bool (DarkBigInt.compare a b <= 0))
          | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a <= b))
          | _, _, _, [| DFloat32 a; DFloat32 b |] -> Ply(Dval.bool (a <= b))
          | _, _, _, [| DDecimal a; DDecimal b |] -> Ply(Dval.bool (a <= b))
          | _, vm, _, [| a; b |] -> numericTypeError vm a b
          | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.moved "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
      fn =
        (function
        | _, _, _, [||] -> incorrectArgs ()
        | _, vm, _, [| DPromise _ as read |] ->
          // A `Some` or `Ok` built around a read hands the read inside back through its shell,
          // still in flight. Anything else is waited for: an error shows the value it got.
          match Pending.peek read with
          | DEnum(FQTypeName.Package(Hash id), _, _, "Some", [ value ]) when
            id = PackageRefs.Type.Stdlib.option ()
            ->
            Ply value
          | DEnum(FQTypeName.Package(Hash id), _, _, "Ok", [ value ]) when
            id = PackageRefs.Type.Stdlib.result ()
            ->
            Ply value
          | _ -> Interpreter.withValue vm read (fun landed -> unwrapLanded vm landed)
        | _, vm, _, [| dval |] -> unwrapLanded vm dval

        // Error: multiple arguments
        | _, vm, _, multipleArgs ->
          RuntimeError.Unwraps.MultipleArgs(List.ofArray multipleArgs)
          |> RuntimeError.Unwrap
          |> raiseRTE vm.threadID)

      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // The identity: a builtin's arguments land before it runs, and that wait is the point.
    { name = fn "runtimeForce" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "a") "" ]
      returnType = TVariable "a"
      description =
        "Returns <param value>, once it is a value: a read still in flight is waited for here"
      fn =
        (function
        | _, _, _, [| value |] -> Ply value
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
