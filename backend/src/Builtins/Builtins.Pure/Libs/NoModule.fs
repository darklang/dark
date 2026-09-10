module Builtins.Pure.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module ValueType = LibExecution.ValueType
module RTE = RuntimeError


// Builtin types cannot express “both operands have the same numeric type,” so
// these operator signatures are intentionally loose. Runtime dispatch and the
// at-rest checker enforce the constraint.
let varA = TVariable "a"
let varB = TVariable "b"



// ── numeric conversion, once instead of 116 times ────────────────────────────
//
// `Int8.fromInt64`, `UInt32.toFloat`, `Int.fromUInt128` and the rest of the tower were a builtin
// each: every integer type against every source, in both directions, each one a cast and a range
// check written out longhand. `convert` and `tryConvert` replace all of them, and the split
// between the two is the only thing that needed thinking about.
//
// Some conversions WIDEN and cannot fail (`Int64.fromInt8`); they return the value.
// Others NARROW and can (`Int8.fromInt64` is None outside -128..127); they return an Option.
// One builtin cannot be both without either making the widening half lossy at every call site or
// having a return type that depends on the type argument, which Dark does not have. So: two.
//
// The Dark wrappers keep their exact names and signatures and delegate to whichever is right, so
// `Stdlib.Int8.fromInt64` still returns an Option because its own signature says so, and no caller
// changes.

// BigInteger <-> 128-bit: no `bigint`/cast operator covers these. Same idiom as `Libs/Int.fs`,
// which needed them first.
let private i128ToBig (a : System.Int128) : bigint =
  System.Numerics.BigInteger.op_Implicit a
let private u128ToBig (a : System.UInt128) : bigint =
  System.Numerics.BigInteger.op_Implicit a
let private bigToI128 (b : bigint) : System.Int128 =
  System.Numerics.BigInteger.op_Explicit b
let private bigToU128 (b : bigint) : System.UInt128 =
  System.Numerics.BigInteger.op_Explicit b

/// Every numeric Dval as a bigint. `None` for a non-numeric, and for `Float`, which is not an
/// integer and is handled separately at both ends.
let private numericAsBigInt (d : Dval) : Option<bigint> =
  match d with
  | DInt8 v -> Some(bigint v)
  | DUInt8 v -> Some(bigint v)
  | DInt16 v -> Some(bigint v)
  | DUInt16 v -> Some(bigint v)
  | DInt32 v -> Some(bigint v)
  | DUInt32 v -> Some(bigint v)
  | DInt64 v -> Some(bigint v)
  | DUInt64 v -> Some(bigint v)
  | DInt128 v -> Some(i128ToBig v)
  | DUInt128 v -> Some(u128ToBig v)
  | DInt di -> Some(DarkInt.toBigInt di)
  | _ -> None

/// The `KnownType` an Option of the target carries. Separate from `ofBigInt` because a `None`
/// still has to say what it is a `None` OF, and at that point there is no value to read it from.
let private knownTypeOf (target : TypeReference) : Option<KnownType> =
  match target with
  | TInt8 -> Some KTInt8
  | TUInt8 -> Some KTUInt8
  | TInt16 -> Some KTInt16
  | TUInt16 -> Some KTUInt16
  | TInt32 -> Some KTInt32
  | TUInt32 -> Some KTUInt32
  | TInt64 -> Some KTInt64
  | TUInt64 -> Some KTUInt64
  | TInt128 -> Some KTInt128
  | TUInt128 -> Some KTUInt128
  | TInt -> Some KTInt
  | TFloat -> Some KTFloat
  | _ -> None

// Range bounds as bigints, built ONCE. Writing `bigint System.SByte.MinValue` inline inside
// `ofBigInt` allocates two BigIntegers on every conversion, and conversions are on a hot path.
let private i8Lo, i8Hi = bigint System.SByte.MinValue, bigint System.SByte.MaxValue
let private u8Lo, u8Hi = bigint System.Byte.MinValue, bigint System.Byte.MaxValue
let private i16Lo, i16Hi = bigint System.Int16.MinValue, bigint System.Int16.MaxValue
let private u16Lo, u16Hi = bigint System.UInt16.MinValue, bigint System.UInt16.MaxValue
let private i32Lo, i32Hi = bigint System.Int32.MinValue, bigint System.Int32.MaxValue
let private u32Lo, u32Hi = bigint System.UInt32.MinValue, bigint System.UInt32.MaxValue
let private i64Lo, i64Hi = bigint System.Int64.MinValue, bigint System.Int64.MaxValue
let private u64Lo, u64Hi = bigint System.UInt64.MinValue, bigint System.UInt64.MaxValue
let private i128Lo, i128Hi =
  i128ToBig System.Int128.MinValue, i128ToBig System.Int128.MaxValue
let private u128Lo, u128Hi =
  u128ToBig System.UInt128.MinValue, u128ToBig System.UInt128.MaxValue

/// Build a value of the target type from a bigint, `None` when it does not fit.
///
/// `TInt` never fails: it is arbitrary precision. `TFloat` never fails either, though it can lose
/// precision, which is what a float conversion is.
let private ofBigInt (target : TypeReference) (b : bigint) : Option<Dval> =
  match target with
  | TInt8 -> if b >= i8Lo && b <= i8Hi then Some(DInt8(sbyte b)) else None
  | TUInt8 -> if b >= u8Lo && b <= u8Hi then Some(DUInt8(uint8 b)) else None
  | TInt16 -> if b >= i16Lo && b <= i16Hi then Some(DInt16(int16 b)) else None
  | TUInt16 -> if b >= u16Lo && b <= u16Hi then Some(DUInt16(uint16 b)) else None
  | TInt32 -> if b >= i32Lo && b <= i32Hi then Some(DInt32(int32 b)) else None
  | TUInt32 -> if b >= u32Lo && b <= u32Hi then Some(DUInt32(uint32 b)) else None
  | TInt64 -> if b >= i64Lo && b <= i64Hi then Some(Dval.dint64 (int64 b)) else None
  | TUInt64 -> if b >= u64Lo && b <= u64Hi then Some(DUInt64(uint64 b)) else None
  | TInt128 -> if b >= i128Lo && b <= i128Hi then Some(DInt128(bigToI128 b)) else None
  | TUInt128 -> if b >= u128Lo && b <= u128Hi then Some(DUInt128(bigToU128 b)) else None
  | TInt -> Some(Dval.dint (DarkInt.ofBigInt b))
  | TFloat -> Some(DFloat(float b))
  | _ -> None

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
  RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID

let private zeroModulus (vm : VMState) : 'a =
  RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.threadID

let private negativeModulus (vm : VMState) : 'a =
  RTE.Ints.NegativeModulus |> RTE.Int |> raiseRTE vm.threadID

let private negativeExponent (vm : VMState) : 'a =
  RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID

let private outOfRange (vm : VMState) : 'a =
  RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.threadID

// Fixed-width integer arithmetic wraps around on overflow (e.g.
// `127y + 1y == -128y`), so `+`, `-`, `*` use F#'s unchecked operators and
// `negate`/`divide` let `MinValue` wrap to itself rather than raising.

/// `number ^ exp` wrapped into a signed `bits`-wide integer.
/// Uses modular exponentiation so huge exponents stay cheap.
let private powSigned (bits : int) (number : bigint) (exp : bigint) : bigint =
  let m = System.Numerics.BigInteger.Pow(bigint 2, bits)
  let r = System.Numerics.BigInteger.ModPow(number, exp, m)
  let r = ((r % m) + m) % m
  if r >= m / bigint 2 then r - m else r

/// `number ^ exp` wrapped into an unsigned `bits`-wide integer.
let private powUnsigned (bits : int) (number : bigint) (exp : bigint) : bigint =
  let m = System.Numerics.BigInteger.Pow(bigint 2, bits)
  let r = System.Numerics.BigInteger.ModPow(number, exp, m)
  ((r % m) + m) % m


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
      sqlSpec = SqlBinOp "="
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
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Adds two numbers of the same numeric type. Fixed-width integer "
        + "overflow wraps around; the arbitrary-precision Int grows instead of "
        + "overflowing; float arithmetic follows IEEE (overflow to infinity)."
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
        | _, vm, _, [| a; b |] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      // CLEANUP: SQL pushdown for fixed-width integer arithmetic does not match
      // runtime overflow semantics. Runtime evaluation wraps, but SQLite promotes
      // overflowing integer arithmetic to REAL for these add/sub/mul/div/pow specs.
      // DInt is arbitrary-precision, so it has no wrap behavior to preserve.
      sqlSpec = SqlBinOp "+"
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
      sqlSpec = SqlBinOp "-"
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
      sqlSpec = SqlBinOp "*"
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
        // Unsigned division can't overflow. Signed division overflows only on
        // `MinValue / -1`, whose wrapped result is `MinValue` itself. For the
        // narrow Int8/Int16 the division happens in Int32 space and narrows
        // back, wrapping naturally; for Int32/Int64/Int128 the hardware/runtime
        // would throw, so we return `MinValue` explicitly.
        (function
        | _, vm, _, [| DInt8 a; DInt8 b |] ->
          if b = 0y then divideByZero vm else Ply(DInt8(a / b))
        | _, vm, _, [| DUInt8 a; DUInt8 b |] ->
          if b = 0uy then divideByZero vm else Ply(DUInt8(a / b))
        | _, vm, _, [| DInt16 a; DInt16 b |] ->
          if b = 0s then divideByZero vm else Ply(DInt16(a / b))
        | _, vm, _, [| DUInt16 a; DUInt16 b |] ->
          if b = 0us then divideByZero vm else Ply(DUInt16(a / b))
        | _, vm, _, [| DInt32 a; DInt32 b |] ->
          if b = 0l then
            divideByZero vm
          elif a = System.Int32.MinValue && b = -1l then
            Ply(DInt32 System.Int32.MinValue)
          else
            Ply(DInt32(a / b))
        | _, vm, _, [| DUInt32 a; DUInt32 b |] ->
          if b = 0ul then divideByZero vm else Ply(DUInt32(a / b))
        | _, vm, _, [| DInt64 a; DInt64 b |] ->
          if b = 0L then
            divideByZero vm
          elif a = System.Int64.MinValue && b = -1L then
            Ply(DInt64 System.Int64.MinValue)
          else
            Ply(Dval.dint64 (a / b))
        | _, vm, _, [| DUInt64 a; DUInt64 b |] ->
          if b = 0UL then divideByZero vm else Ply(DUInt64(a / b))
        | _, vm, _, [| DInt128 a; DInt128 b |] ->
          if b = System.Int128.Zero then
            divideByZero vm
          elif a = System.Int128.MinValue && b = System.Int128.NegativeOne then
            Ply(DInt128 System.Int128.MinValue)
          else
            Ply(DInt128(a / b))
        | _, vm, _, [| DUInt128 a; DUInt128 b |] ->
          if b = System.UInt128.Zero then divideByZero vm else Ply(DUInt128(a / b))
        | _, vm, _, [| DInt a; DInt b |] ->
          if DarkInt.isZero b then
            divideByZero vm
          else
            Ply(Dval.dint (DarkInt.divide a b))
        // Float division by zero follows IEEE semantics (Infinity/NaN), as before
        | _, _, _, [| DFloat a; DFloat b |] -> Ply(DFloat(a / b))
        | _, vm, _, [| a; b |] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "modulo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Wraps <param a> around so that {{0 <= res < b}}, for two numbers of "
        + "the same numeric type. The modulus <param b> must be greater than 0."
      fn =
        (function
        | _, vm, _, [| DInt8 v; DInt8 m |] ->
          if m = 0y then
            zeroModulus vm
          elif m < 0y then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt8(if r < 0y then m + r else r))
        | _, vm, _, [| DUInt8 v; DUInt8 m |] ->
          if m = 0uy then zeroModulus vm else Ply(DUInt8(v % m))
        | _, vm, _, [| DInt16 v; DInt16 m |] ->
          if m = 0s then
            zeroModulus vm
          elif m < 0s then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt16(if r < 0s then m + r else r))
        | _, vm, _, [| DUInt16 v; DUInt16 m |] ->
          if m = 0us then zeroModulus vm else Ply(DUInt16(v % m))
        | _, vm, _, [| DInt32 v; DInt32 m |] ->
          if m = 0l then
            zeroModulus vm
          elif m < 0l then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt32(if r < 0l then m + r else r))
        | _, vm, _, [| DUInt32 v; DUInt32 m |] ->
          if m = 0ul then zeroModulus vm else Ply(DUInt32(v % m))
        | _, vm, _, [| DInt64 v; DInt64 m |] ->
          if m = 0L then
            zeroModulus vm
          elif m < 0L then
            negativeModulus vm
          else
            let r = v % m
            Ply(Dval.dint64 (if r < 0L then m + r else r))
        | _, vm, _, [| DUInt64 v; DUInt64 m |] ->
          if m = 0UL then zeroModulus vm else Ply(DUInt64(v % m))
        | _, vm, _, [| DInt128 v; DInt128 m |] ->
          if m = System.Int128.Zero then
            zeroModulus vm
          elif m < System.Int128.Zero then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt128(if r < System.Int128.Zero then m + r else r))
        | _, vm, _, [| DUInt128 v; DUInt128 m |] ->
          if m = System.UInt128.Zero then zeroModulus vm else Ply(DUInt128(v % m))
        | _, vm, _, [| DInt v; DInt m |] ->
          let m = DarkInt.toBigInt m
          if m = System.Numerics.BigInteger.Zero then
            zeroModulus vm
          elif m < System.Numerics.BigInteger.Zero then
            negativeModulus vm
          else
            let r = DarkInt.toBigInt v % m
            Ply(Dval.int (if r < System.Numerics.BigInteger.Zero then m + r else r))
        | _, vm, _, [| DFloat v; DFloat m |] ->
          if m = 0.0 then
            zeroModulus vm
          elif m < 0.0 then
            negativeModulus vm
          else
            let r = v % m
            Ply(DFloat(if r < 0.0 then m + r else r))
        | _, vm, _, [| a; b |] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "%"
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
        + "Integer exponents must be non-negative. Fixed-width integer overflow "
        + "wraps around; the arbitrary-precision Int grows instead."
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
      sqlSpec = SqlFunction "POWER"
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
      sqlSpec = NotQueryable
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
      sqlSpec = SqlBinOp ">"
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
      sqlSpec = SqlBinOp ">="
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
      sqlSpec = SqlBinOp "<"
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
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
      fn =
        (function
        | _, _, _, [||] -> incorrectArgs ()
        | _, vm, _, [| dval |] ->
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
            RuntimeError.Unwraps.GotNone
            |> RuntimeError.Unwrap
            |> raiseRTE vm.threadID

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

        // Error: multiple arguments
        | _, vm, _, multipleArgs ->
          RuntimeError.Unwraps.MultipleArgs(List.ofArray multipleArgs)
          |> RuntimeError.Unwrap
          |> raiseRTE vm.threadID)

      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "convert" 0
      typeParams = [ "target" ]
      parameters = [ Param.make "a" varA "" ]
      returnType = TVariable "target"
      description =
        "Converts a number to another numeric type that is guaranteed to hold it. Use <fn "
        + "tryConvert> when the target is narrower than the source and the conversion can fail."
      fn =
        (function
        | _, vm, [ target ], [| a |] ->
          match a, target with
          // A float target never fails and is the only case that is not an integer at both
          // ends, so it is here rather than inside `ofBigInt`'s integer path.
          | DFloat f, TFloat -> Ply(DFloat f)
          | DFloat f, TInt ->
            // `bigint f` truncates toward zero; `roundedToInt` adds the NaN/Infinity guard so
            // those raise an Int error rather than a host exception. Same as `intFromFloat` did.
            roundedToInt vm f
          | _ ->
            match numericAsBigInt a with
            | None -> incorrectArgs ()
            | Some b ->
              match ofBigInt target b with
              | Some converted -> Ply converted
              | None ->
                // The caller asked for a widening that is not one. A Dark wrapper picks
                // `convert` or `tryConvert` from its own signature, so reaching this means the
                // wrapper is wrong, not the program.
                RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.threadID
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "tryConvert" 0
      typeParams = [ "target" ]
      parameters = [ Param.make "a" varA "" ]
      returnType = TypeReference.option (TVariable "target")
      description =
        "Converts a number to a narrower numeric type, answering {{None}} when it does not fit. "
        + "Use <fn convert> when the target is guaranteed to hold the value."
      fn =
        (function
        | _, _, [ target ], [| a |] ->
          match numericAsBigInt a, knownTypeOf target with
          | None, _
          | _, None -> incorrectArgs ()
          | Some b, Some kt ->
            match ofBigInt target b with
            | Some converted -> Dval.optionSome kt converted |> Ply
            | None -> Dval.optionNone kt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
