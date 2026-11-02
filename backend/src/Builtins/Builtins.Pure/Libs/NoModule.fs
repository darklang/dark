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

let private negativeExponent (vm : VMState) : 'a =
  RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID

let private outOfRange (vm : VMState) : 'a =
  RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.threadID

// Fixed-width integer arithmetic wraps around on overflow (e.g.
// `127y + 1y == -128y`), so `+`, `-`, `*` use F#'s unchecked operators and
// `negate`/`divide` let `MinValue` wrap to itself rather than raising.

/// `number ^ exp` wrapped into a signed `bits`-wide integer.
/// Uses modular exponentiation so huge exponents stay cheap.
///
/// Not private: `Int128`/`UInt128` need the same arithmetic, and their builtins live in their
/// own files because this one's match has no 128-bit arms.
let powSigned (bits : int) (number : bigint) (exp : bigint) : bigint =
  let m = System.Numerics.BigInteger.Pow(bigint 2, bits)
  let r = System.Numerics.BigInteger.ModPow(number, exp, m)
  let r = ((r % m) + m) % m
  if r >= m / bigint 2 then r - m else r

/// `number ^ exp` wrapped into an unsigned `bits`-wide integer. Public for the same reason
/// as `powSigned`.
let powUnsigned (bits : int) (number : bigint) (exp : bigint) : bigint =
  let m = System.Numerics.BigInteger.Pow(bigint 2, bits)
  let r = System.Numerics.BigInteger.ModPow(number, exp, m)
  ((r % m) + m) % m


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
        + "integer overflow wraps around, the arbitrary-precision Int grows instead of "
        + "overflowing, and float arithmetic follows IEEE (overflow to infinity)."
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a + b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a + b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a + b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a + b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a + b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a + b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a + b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a + b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a + b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a + b))
        | _, _, _, [| DInt a; DInt b |] -> Ply(Dval.dint (DarkInt.add a b))
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a + b))
        // `+` concatenates Strings (`impl Add for String`), and this builtin is what `+`
        // lowers to while the package refs are not generated yet, which is how the stdlib's
        // own string concatenation loads on a fresh tree.
        | _, _, _, [| DString a; DString b |] ->
          Ply(DString(String.normalize (a + b)))
        | _, vm, _, [| a; b |] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      // CLEANUP: SQL pushdown for fixed-width integer arithmetic does not match
      // runtime overflow semantics. Runtime evaluation wraps, but SQLite promotes
      // overflowing integer arithmetic to REAL for these add/sub/mul/div/pow specs.
      // DInt is arbitrary-precision, so it has no wrap behavior to preserve.
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Subtracts two numbers of the same numeric type. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision Int grows instead of "
        + "overflowing; float arithmetic follows IEEE (overflow to infinity)."
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a - b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a - b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a - b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a - b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a - b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a - b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a - b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a - b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a - b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a - b))
        | _, _, _, [| DInt a; DInt b |] -> Ply(Dval.dint (DarkInt.subtract a b))
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a - b))
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
        + "overflow wraps around; the arbitrary-precision Int grows instead of "
        + "overflowing; float arithmetic follows IEEE (overflow to infinity)."
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a * b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a * b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a * b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a * b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a * b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a * b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a * b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a * b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a * b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a * b))
        | _, _, _, [| DInt a; DInt b |] -> Ply(Dval.dint (DarkInt.multiply a b))
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a * b))
        | _, vm, _, [| a; b |] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


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
        // Each integer arm is the `IntDivision` fn the typed builtin uses, so `/`
        // and `Int64.divide` agree by construction.
        (function
        // Float division by zero follows IEEE semantics (Infinity/NaN), as before
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a / b))
        | _, vm, _, [| DInt8 a; DInt8 b |] ->
          IntDivision.divideInt8 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt8
          |> Ply
        | _, vm, _, [| DUInt8 a; DUInt8 b |] ->
          IntDivision.divideUInt8 a b
          |> IntDivision.divisionOrRaise vm
          |> DUInt8
          |> Ply
        | _, vm, _, [| DInt16 a; DInt16 b |] ->
          IntDivision.divideInt16 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt16
          |> Ply
        | _, vm, _, [| DUInt16 a; DUInt16 b |] ->
          IntDivision.divideUInt16 a b
          |> IntDivision.divisionOrRaise vm
          |> DUInt16
          |> Ply
        | _, vm, _, [| DInt32 a; DInt32 b |] ->
          IntDivision.divideInt32 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt32
          |> Ply
        | _, vm, _, [| DUInt32 a; DUInt32 b |] ->
          IntDivision.divideUInt32 a b
          |> IntDivision.divisionOrRaise vm
          |> DUInt32
          |> Ply
        | _, vm, _, [| DInt64 a; DInt64 b |] ->
          IntDivision.divideInt64 a b
          |> IntDivision.divisionOrRaise vm
          |> Dval.dint64
          |> Ply
        | _, vm, _, [| DUInt64 a; DUInt64 b |] ->
          IntDivision.divideUInt64 a b
          |> IntDivision.divisionOrRaise vm
          |> DUInt64
          |> Ply
        | _, vm, _, [| DInt128 a; DInt128 b |] ->
          IntDivision.divideInt128 a b
          |> IntDivision.divisionOrRaise vm
          |> DInt128
          |> Ply
        | _, vm, _, [| DUInt128 a; DUInt128 b |] ->
          IntDivision.divideUInt128 a b
          |> IntDivision.divisionOrRaise vm
          |> DUInt128
          |> Ply
        | _, vm, _, [| DInt a; DInt b |] ->
          IntDivision.divideInt a b
          |> IntDivision.divisionOrRaise vm
          |> Dval.dint
          |> Ply
        | _, vm, _, [| a; b |] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "modulo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Wraps <param a> around so that {{0 <= res < b}}, for two integers of "
        + "the same type. The modulus <param b> must be greater than 0."
      fn =
        // `IntDivision`, as for `divide` above
        (function
        // No Float arm, on purpose: a float `%` has to choose between flooring and
        // truncating, and what a zero divisor does when our own Float division returns
        // Infinity. Swift and Elm have no float modulo either. A Float pair takes the type
        // error below, which through the operator reads as a missing `Modulo` impl.
        | _, vm, _, [| DInt8 a; DInt8 b |] ->
          IntDivision.moduloInt8 a b |> IntDivision.modulusOrRaise vm |> DInt8 |> Ply
        | _, vm, _, [| DUInt8 a; DUInt8 b |] ->
          IntDivision.moduloUInt8 a b
          |> IntDivision.modulusOrRaise vm
          |> DUInt8
          |> Ply
        | _, vm, _, [| DInt16 a; DInt16 b |] ->
          IntDivision.moduloInt16 a b
          |> IntDivision.modulusOrRaise vm
          |> DInt16
          |> Ply
        | _, vm, _, [| DUInt16 a; DUInt16 b |] ->
          IntDivision.moduloUInt16 a b
          |> IntDivision.modulusOrRaise vm
          |> DUInt16
          |> Ply
        | _, vm, _, [| DInt32 a; DInt32 b |] ->
          IntDivision.moduloInt32 a b
          |> IntDivision.modulusOrRaise vm
          |> DInt32
          |> Ply
        | _, vm, _, [| DUInt32 a; DUInt32 b |] ->
          IntDivision.moduloUInt32 a b
          |> IntDivision.modulusOrRaise vm
          |> DUInt32
          |> Ply
        | _, vm, _, [| DInt64 a; DInt64 b |] ->
          IntDivision.moduloInt64 a b
          |> IntDivision.modulusOrRaise vm
          |> Dval.dint64
          |> Ply
        | _, vm, _, [| DUInt64 a; DUInt64 b |] ->
          IntDivision.moduloUInt64 a b
          |> IntDivision.modulusOrRaise vm
          |> DUInt64
          |> Ply
        | _, vm, _, [| DInt128 a; DInt128 b |] ->
          IntDivision.moduloInt128 a b
          |> IntDivision.modulusOrRaise vm
          |> DInt128
          |> Ply
        | _, vm, _, [| DUInt128 a; DUInt128 b |] ->
          IntDivision.moduloUInt128 a b
          |> IntDivision.modulusOrRaise vm
          |> DUInt128
          |> Ply
        | _, vm, _, [| DInt a; DInt b |] ->
          IntDivision.moduloInt a b
          |> IntDivision.modulusOrRaise vm
          |> Dval.dint
          |> Ply
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
        + "Supported for every integer type and Float (not Int128/UInt128). "
        + "A negative integer exponent raises an error. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision Int grows instead."
      fn =
        // Fixed-width powers wrap, so they use modular exponentiation
        // (`powSigned`/`powUnsigned`) — this stays cheap even for huge
        // exponents instead of building an enormous bigint.
        (function
        | _, vm, _, [| DInt8 number; DInt8 exp |] ->
          if exp < 0y then
            negativeExponent vm
          else
            Ply(DInt8(int8 (powSigned 8 (bigint number) (bigint exp))))
        | _, _, _, [| DUInt8 number; DUInt8 exp |] ->
          Ply(DUInt8(uint8 (powUnsigned 8 (bigint number) (bigint exp))))
        | _, vm, _, [| DInt16 number; DInt16 exp |] ->
          if exp < 0s then
            negativeExponent vm
          else
            Ply(DInt16(int16 (powSigned 16 (bigint number) (bigint exp))))
        | _, _, _, [| DUInt16 number; DUInt16 exp |] ->
          Ply(DUInt16(uint16 (powUnsigned 16 (bigint number) (bigint exp))))
        | _, vm, _, [| DInt32 number; DInt32 exp |] ->
          if exp < 0l then
            negativeExponent vm
          else
            Ply(DInt32(int32 (powSigned 32 (bigint number) (bigint exp))))
        | _, _, _, [| DUInt32 number; DUInt32 exp |] ->
          Ply(DUInt32(uint32 (powUnsigned 32 (bigint number) (bigint exp))))
        | _, _, _, [| DUInt64 number; DUInt64 exp |] ->
          Ply(DUInt64(uint64 (powUnsigned 64 (bigint number) (bigint exp))))
        | _, vm, _, [| DInt64 number; DInt64 exp |] ->
          if exp < 0L then
            negativeExponent vm
          else
            Ply(Dval.dint64 (int64 (powSigned 64 (bigint number) (bigint exp))))
        | _, vm, _, [| DInt number; DInt exp |] ->
          let number = DarkInt.toBigInt number
          let exp = DarkInt.toBigInt exp
          if exp < System.Numerics.BigInteger.Zero then
            negativeExponent vm
          elif exp > bigint System.Int32.MaxValue then
            // `**` needs an Int32 exponent; only trivial bases are representable.
            if number = System.Numerics.BigInteger.Zero then
              Ply(Dval.int System.Numerics.BigInteger.Zero)
            elif number = System.Numerics.BigInteger.One then
              Ply(Dval.int System.Numerics.BigInteger.One)
            elif number = System.Numerics.BigInteger.MinusOne then
              Ply(
                Dval.int (
                  if exp % (bigint 2) = System.Numerics.BigInteger.Zero then
                    System.Numerics.BigInteger.One
                  else
                    System.Numerics.BigInteger.MinusOne
                )
              )
            else
              outOfRange vm
          else
            Ply(Dval.int (number ** (int exp)))
        | _, _, _, [| DFloat number; DFloat exp |] -> Ply(DFloat(number ** exp))
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
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DInt8(-a))
        | _, _, _, [| DInt16 a |] -> Ply(DInt16(-a))
        | _, _, _, [| DInt32 a |] -> Ply(DInt32(-a))
        | _, _, _, [| DInt64 a |] -> Ply(Dval.dint64 (-a))
        | _, _, _, [| DInt128 a |] -> Ply(DInt128(-a))
        | _, _, _, [| DInt a |] -> Ply(Dval.dint (DarkInt.negate a))
        | _, _, _, [| DFloat a |] -> Ply(DFloat(-a))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a &&& b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a &&& b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a &&& b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a &&& b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a &&& b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a &&& b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a &&& b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a &&& b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a &&& b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a &&& b))
        | _, _, _, [| DInt a; DInt b |] ->
          Ply(Dval.int (DarkInt.toBigInt a &&& DarkInt.toBigInt b))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a ||| b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a ||| b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a ||| b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a ||| b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a ||| b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a ||| b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a ||| b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a ||| b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a ||| b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a ||| b))
        | _, _, _, [| DInt a; DInt b |] ->
          Ply(Dval.int (DarkInt.toBigInt a ||| DarkInt.toBigInt b))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a ^^^ b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a ^^^ b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a ^^^ b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a ^^^ b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a ^^^ b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a ^^^ b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a ^^^ b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a ^^^ b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a ^^^ b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a ^^^ b))
        | _, _, _, [| DInt a; DInt b |] ->
          Ply(Dval.int (DarkInt.toBigInt a ^^^ DarkInt.toBigInt b))
        | _, vm, _, [| a; b |] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Unary `~`. Two's complement, so `~x = -x - 1` on every signed type and on
    // the arbitrary-precision `Int`.
    { name = fn "bitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" varA "" ]
      returnType = varA
      description = "Bitwise NOT of an integer, {{~a}}"
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DInt8(~~~a))
        | _, _, _, [| DUInt8 a |] -> Ply(DUInt8(~~~a))
        | _, _, _, [| DInt16 a |] -> Ply(DInt16(~~~a))
        | _, _, _, [| DUInt16 a |] -> Ply(DUInt16(~~~a))
        | _, _, _, [| DInt32 a |] -> Ply(DInt32(~~~a))
        | _, _, _, [| DUInt32 a |] -> Ply(DUInt32(~~~a))
        | _, _, _, [| DInt64 a |] -> Ply(Dval.dint64 (~~~a))
        | _, _, _, [| DUInt64 a |] -> Ply(DUInt64(~~~a))
        // `~~~` isn't defined on the 128-bit types; call the op directly.
        | _, _, _, [| DInt128 a |] -> Ply(DInt128(System.Int128.op_OnesComplement a))
        | _, _, _, [| DUInt128 a |] ->
          Ply(DUInt128(System.UInt128.op_OnesComplement a))
        | _, _, _, [| DInt a |] ->
          Ply(Dval.int (-(DarkInt.toBigInt a) - System.Numerics.BigInteger.One))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a <<< int b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a <<< int b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a <<< int b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a <<< int b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a <<< int b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a <<< int b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a <<< int b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a <<< int b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a <<< int b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a <<< int b))
        | _, vm, _, [| DInt a; DInt b |] ->
          let shift = intToInt32 vm b
          if shift < 0 then
            outOfRange vm
          else
            Ply(Dval.int (DarkInt.toBigInt a <<< shift))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a >>> int b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a >>> int b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(DInt16(a >>> int b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a >>> int b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a >>> int b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(DUInt32(a >>> int b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.dint64 (a >>> int b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(DUInt64(a >>> int b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(DInt128(a >>> int b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(DUInt128(a >>> int b))
        | _, vm, _, [| DInt a; DInt b |] ->
          let shift = intToInt32 vm b
          if shift < 0 then
            outOfRange vm
          else
            Ply(Dval.int (DarkInt.toBigInt a >>> shift))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(Dval.bool (a > b))
        | _, _, _, [| DInt a; DInt b |] -> Ply(Dval.bool (DarkInt.compare a b > 0))
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a > b))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(Dval.bool (a >= b))
        | _, _, _, [| DInt a; DInt b |] -> Ply(Dval.bool (DarkInt.compare a b >= 0))
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a >= b))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(Dval.bool (a < b))
        | _, _, _, [| DInt a; DInt b |] -> Ply(Dval.bool (DarkInt.compare a b < 0))
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a < b))
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
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DInt16 a; DInt16 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DUInt32 a; DUInt32 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DInt64 a; DInt64 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DUInt64 a; DUInt64 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DInt128 a; DInt128 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DUInt128 a; DUInt128 b |] -> Ply(Dval.bool (a <= b))
        | _, _, _, [| DInt a; DInt b |] -> Ply(Dval.bool (DarkInt.compare a b <= 0))
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(Dval.bool (a <= b))
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
