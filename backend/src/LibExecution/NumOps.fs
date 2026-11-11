/// The operators on the builtin numeric types, each defined once: what it returns, how it wraps
/// or rounds, and where it raises. The types are the ten fixed-width integers, `BigInt`, `Float`,
/// `Float32` and `Decimal`.
///
/// Everything that runs a numeric operator comes here. The VM answers `a / b` from the registers
/// with it (`FastOps`), and the polymorphic builtins (`Builtin.divide` ...) that the stdlib's
/// `impl Divide for Int32` and the rest call are made of it. So there is no second statement of
/// any of these to drift from, and the set is small and fixed: seventeen operators, which a
/// compiler maps onto its own.
///
/// What each one does on the fixed-width integers:
/// - `+ - *` wrap around (`127y + 1y == -128y`).
/// - `/` truncates toward zero and raises on a zero divisor. `MIN / -1` wraps back to `MIN`.
/// - `%` is Euclidean: `0 <= a % b < b`. It raises on a zero modulus, and on a negative one.
/// - `**` wraps around. It raises on a negative exponent.
/// - `<<` and `>>` take the count modulo the width, as .NET does; `>>` is arithmetic on the
///   signed types.
/// - unary `-` is for the signed types only and wraps `MIN` back to itself; `~` is for all ten.
///
/// On the others:
/// - `BigInt` has all seventeen, and grows rather than wrapping. `/` and `%` raise as the
///   integers do; `**` raises on a negative exponent, and on one too big for an `Int32` unless
///   the base is 0, 1 or -1. `<<` and `>>` raise on a count that is negative or past an `Int32`.
/// - `Float` and `Float32` have `+ - * / **`, the comparisons and unary `-`, all as IEEE has
///   them: dividing by zero gives an infinity or NaN, and NaN compares false.
/// - `Decimal` has `+ - * / %`, the comparisons and unary `-`. It raises rather than rounding a
///   result past its range, and `/` and `%` raise on zero as the integers do.
module LibExecution.NumOps

open Prelude
open RuntimeTypes

// The operators, as constants so the tables can switch on them. `FastOps` takes its tags for
// them from here.
[<Literal>]
let Add = 0

[<Literal>]
let Subtract = 1

[<Literal>]
let LessThan = 2

[<Literal>]
let LessThanOrEqualTo = 3

[<Literal>]
let GreaterThan = 4

[<Literal>]
let GreaterThanOrEqualTo = 5

[<Literal>]
let Multiply = 20

[<Literal>]
let Negate = 21

[<Literal>]
let BitwiseAnd = 22

[<Literal>]
let BitwiseOr = 23

[<Literal>]
let BitwiseXor = 24

[<Literal>]
let BitwiseNot = 25

[<Literal>]
let ShiftLeft = 26

[<Literal>]
let ShiftRight = 27

[<Literal>]
let Divide = 29

[<Literal>]
let Modulo = 30

[<Literal>]
let Power = 31

let add = Add
let subtract = Subtract
let lessThan = LessThan
let lessThanOrEqualTo = LessThanOrEqualTo
let greaterThan = GreaterThan
let greaterThanOrEqualTo = GreaterThanOrEqualTo
let multiply = Multiply
let negate = Negate
let bitwiseAnd = BitwiseAnd
let bitwiseOr = BitwiseOr
let bitwiseXor = BitwiseXor
let bitwiseNot = BitwiseNot
let shiftLeft = ShiftLeft
let shiftRight = ShiftRight
let divide = Divide
let modulo = Modulo
let power = Power


/// What an operator gives: a value, an error, or nothing to say, when the operands are not two
/// numbers of one type or the type has no such operator.
[<Struct>]
type Outcome =
  | Value of value : Dval
  | Raises of error : RuntimeError.Error
  | Unsupported

// The errors, each built once rather than on every raise.
let divideByZero = Raises(RuntimeError.BigInt RuntimeError.BigInts.DivideByZeroError)
let zeroModulus = Raises(RuntimeError.BigInt RuntimeError.BigInts.ZeroModulus)
let negativeModulus =
  Raises(RuntimeError.BigInt RuntimeError.BigInts.NegativeModulus)
let negativeExponent =
  Raises(RuntimeError.BigInt RuntimeError.BigInts.NegativeExponent)
let outOfRange = Raises(RuntimeError.BigInt RuntimeError.BigInts.OutOfRange)
let decimalOutOfRange = Raises RuntimeError.DecimalOutOfRange


/// `base ** exponent` in 64 wrapping bits, by squaring, from `acc`. Wrapping multiplication in 64
/// bits keeps the low bits of the true power, and `2^bits` divides `2^64`, so narrowing this to a
/// narrower type gives that type's wrapped power, with or without a sign extended into the high
/// bits.
let rec powWrap64 (acc : uint64) (b : uint64) (e : uint64) : uint64 =
  if e = 0UL then
    acc
  else
    powWrap64 (if e &&& 1UL = 1UL then acc * b else acc) (b * b) (e >>> 1)

/// `powWrap64` for `Int128`, whose own multiplication wraps.
let rec powWrapInt128
  (acc : System.Int128)
  (b : System.Int128)
  (e : System.Int128)
  : System.Int128 =
  if e = System.Int128.Zero then
    acc
  else
    powWrapInt128
      (if e &&& System.Int128.One = System.Int128.One then acc * b else acc)
      (b * b)
      (e >>> 1)

/// `powWrap64` for `UInt128`, whose own multiplication wraps.
let rec powWrapUInt128
  (acc : System.UInt128)
  (b : System.UInt128)
  (e : System.UInt128)
  : System.UInt128 =
  if e = System.UInt128.Zero then
    acc
  else
    powWrapUInt128
      (if e &&& System.UInt128.One = System.UInt128.One then acc * b else acc)
      (b * b)
      (e >>> 1)


/// One type's binary operators. <param div>, <param pow> and the shifts are the parts that differ
/// by type; the rest is the same F# operator on every one of them. The answer is given to
/// <param value>, <param fail> or <param none>, so a caller gets it in the shape it wants with
/// nothing in between: an `Outcome` for the builtins, a `voption` for the VM.
let inline binaryOn
  ([<InlineIfLambda>] mk : ^T -> Dval)
  ([<InlineIfLambda>] div : ^T -> ^T -> ^T)
  ([<InlineIfLambda>] pow : ^T -> ^T -> ^T)
  ([<InlineIfLambda>] shl : ^T -> ^T -> ^T)
  ([<InlineIfLambda>] shr : ^T -> ^T -> ^T)
  ([<InlineIfLambda>] value : Dval -> 'R)
  ([<InlineIfLambda>] fail : Outcome -> 'R)
  (none : 'R)
  (op : int)
  (x : ^T)
  (y : ^T)
  : 'R =
  let zero = LanguagePrimitives.GenericZero< ^T>
  // A `match` on constants, so a jump table rather than a chain of comparisons.
  match op with
  | Add -> value (mk (x + y))
  | Subtract -> value (mk (x - y))
  | Multiply -> value (mk (x * y))
  | LessThan -> value (Dval.bool (x < y))
  | LessThanOrEqualTo -> value (Dval.bool (x <= y))
  | GreaterThan -> value (Dval.bool (x > y))
  | GreaterThanOrEqualTo -> value (Dval.bool (x >= y))
  | Divide -> if y = zero then fail divideByZero else value (mk (div x y))
  | Modulo ->
    // Unsigned, `y < zero` and `r < zero` are never true.
    if y = zero then
      fail zeroModulus
    elif y < zero then
      fail negativeModulus
    else
      let r = x % y
      value (mk (if r < zero then y + r else r))
  | Power -> if y < zero then fail negativeExponent else value (mk (pow x y))
  | BitwiseAnd -> value (mk (x &&& y))
  | BitwiseOr -> value (mk (x ||| y))
  | BitwiseXor -> value (mk (x ^^^ y))
  | ShiftLeft -> value (mk (shl x y))
  | ShiftRight -> value (mk (shr x y))
  | _ -> none

let inline int8Op value fail (none : 'R) (op : int) (x : int8) (y : int8) : 'R =
  binaryOn
    Dval.dint8
    // in int32, so -128y / -1y wraps rather than throws
    (fun p q -> int8 (int p / int q))
    (fun p q -> int8 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline uint8Op value fail (none : 'R) (op : int) (x : uint8) (y : uint8) : 'R =
  binaryOn
    Dval.duint8
    (fun p q -> p / q)
    (fun p q -> uint8 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline int16Op value fail (none : 'R) (op : int) (x : int16) (y : int16) : 'R =
  binaryOn
    DInt16
    // in int32, as for Int8
    (fun p q -> int16 (int p / int q))
    (fun p q -> int16 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline uint16Op
  value
  fail
  (none : 'R)
  (op : int)
  (x : uint16)
  (y : uint16)
  : 'R =
  binaryOn
    DUInt16
    (fun p q -> p / q)
    (fun p q -> uint16 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline int32Op value fail (none : 'R) (op : int) (x : int32) (y : int32) : 'R =
  binaryOn
    DInt32
    // the division itself would throw
    (fun p q -> if p = System.Int32.MinValue && q = -1 then p else p / q)
    (fun p q -> int32 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline uint32Op
  value
  fail
  (none : 'R)
  (op : int)
  (x : uint32)
  (y : uint32)
  : 'R =
  binaryOn
    DUInt32
    (fun p q -> p / q)
    (fun p q -> uint32 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline int64Op value fail (none : 'R) (op : int) (x : int64) (y : int64) : 'R =
  binaryOn
    Dval.dint64
    (fun p q -> if p = System.Int64.MinValue && q = -1L then p else p / q)
    (fun p q -> int64 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline uint64Op
  value
  fail
  (none : 'R)
  (op : int)
  (x : uint64)
  (y : uint64)
  : 'R =
  binaryOn
    DUInt64
    (fun p q -> p / q)
    (fun p q -> powWrap64 1UL p q)
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline int128Op
  value
  fail
  (none : 'R)
  (op : int)
  (x : System.Int128)
  (y : System.Int128)
  : 'R =
  binaryOn
    DInt128
    (fun p q ->
      if p = System.Int128.MinValue && q = System.Int128.NegativeOne then
        p
      else
        p / q)
    (fun p q -> powWrapInt128 System.Int128.One p q)
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y

let inline uint128Op
  value
  fail
  (none : 'R)
  (op : int)
  (x : System.UInt128)
  (y : System.UInt128)
  : 'R =
  binaryOn
    DUInt128
    (fun p q -> p / q)
    (fun p q -> powWrapUInt128 System.UInt128.One p q)
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    value
    fail
    none
    op
    x
    y




/// `-a` (signed only) and `~a` on a fixed-width integer. Anything else is `Unsupported`.
let intUnary (op : int) (a : Dval) : Outcome =
  if op = negate then
    match a with
    | DInt8 x -> Value(DInt8(-x))
    | DInt16 x -> Value(DInt16(-x))
    | DInt32 x -> Value(DInt32(-x))
    | DInt64 x -> Value(Dval.dint64 (-x))
    | DInt128 x -> Value(DInt128(-x))
    | _ -> Unsupported
  elif op = bitwiseNot then
    match a with
    | DInt8 x -> Value(DInt8(~~~x))
    | DUInt8 x -> Value(DUInt8(~~~x))
    | DInt16 x -> Value(DInt16(~~~x))
    | DUInt16 x -> Value(DUInt16(~~~x))
    | DInt32 x -> Value(DInt32(~~~x))
    | DUInt32 x -> Value(DUInt32(~~~x))
    | DInt64 x -> Value(Dval.dint64 (~~~x))
    | DUInt64 x -> Value(DUInt64(~~~x))
    // `~~~` isn't defined on the 128-bit types
    | DInt128 x -> Value(DInt128(System.Int128.op_OnesComplement x))
    | DUInt128 x -> Value(DUInt128(System.UInt128.op_OnesComplement x))
    | _ -> Unsupported
  else
    Unsupported


/// A `BigInt` shift count: `<<` and `>>` need an `Int32` one, and a negative one would reverse
/// the direction.
let private shiftCount (b : DarkBigInt) : int voption =
  match DarkBigInt.toInt32 b with
  | Some count when count >= 0 -> ValueSome count
  | _ -> ValueNone

/// `BigInt`'s operators. Arbitrary precision, so nothing wraps.
let bigIntOp (op : int) (a : DarkBigInt) (b : DarkBigInt) : Outcome =
  match op with
  | Add -> Value(Dval.dbigint (DarkBigInt.add a b))
  | Subtract -> Value(Dval.dbigint (DarkBigInt.subtract a b))
  | Multiply -> Value(Dval.dbigint (DarkBigInt.multiply a b))
  | LessThan -> Value(Dval.bool (DarkBigInt.compare a b < 0))
  | LessThanOrEqualTo -> Value(Dval.bool (DarkBigInt.compare a b <= 0))
  | GreaterThan -> Value(Dval.bool (DarkBigInt.compare a b > 0))
  | GreaterThanOrEqualTo -> Value(Dval.bool (DarkBigInt.compare a b >= 0))
  | Divide ->
    if DarkBigInt.isZero b then
      divideByZero
    else
      Value(Dval.dbigint (DarkBigInt.divide a b))
  | Modulo ->
    let m = DarkBigInt.toBigInt b
    if m.IsZero then
      zeroModulus
    elif m.Sign < 0 then
      negativeModulus
    else
      let r = DarkBigInt.toBigInt a % m
      Value(Dval.bigInt (if r.Sign < 0 then m + r else r))
  | Power ->
    let number = DarkBigInt.toBigInt a
    let exp = DarkBigInt.toBigInt b
    if exp.Sign < 0 then
      negativeExponent
    elif exp > bigint System.Int32.MaxValue then
      // `**` needs an Int32 exponent; only trivial bases are representable.
      if number.IsZero || number.IsOne then
        Value(Dval.bigInt number)
      elif number = System.Numerics.BigInteger.MinusOne then
        Value(
          Dval.bigInt (if exp.IsEven then System.Numerics.BigInteger.One else number)
        )
      else
        outOfRange
    else
      Value(Dval.bigInt (number ** (int exp)))
  | BitwiseAnd ->
    Value(Dval.bigInt (DarkBigInt.toBigInt a &&& DarkBigInt.toBigInt b))
  | BitwiseOr -> Value(Dval.bigInt (DarkBigInt.toBigInt a ||| DarkBigInt.toBigInt b))
  | BitwiseXor ->
    Value(Dval.bigInt (DarkBigInt.toBigInt a ^^^ DarkBigInt.toBigInt b))
  | ShiftLeft ->
    match shiftCount b with
    | ValueSome count -> Value(Dval.bigInt (DarkBigInt.toBigInt a <<< count))
    | ValueNone -> outOfRange
  | ShiftRight ->
    match shiftCount b with
    | ValueSome count -> Value(Dval.bigInt (DarkBigInt.toBigInt a >>> count))
    | ValueNone -> outOfRange
  | _ -> Unsupported

/// `Float`'s operators, as IEEE has them.
let inline floatOp
  ([<InlineIfLambda>] value : Dval -> 'R)
  (none : 'R)
  (op : int)
  (x : float)
  (y : float)
  : 'R =
  match op with
  | Add -> value (DFloat(x + y))
  | Subtract -> value (DFloat(x - y))
  | Multiply -> value (DFloat(x * y))
  | Divide -> value (DFloat(x / y))
  | Power -> value (DFloat(x ** y))
  | LessThan -> value (Dval.bool (x < y))
  | LessThanOrEqualTo -> value (Dval.bool (x <= y))
  | GreaterThan -> value (Dval.bool (x > y))
  | GreaterThanOrEqualTo -> value (Dval.bool (x >= y))
  // No `%`, on purpose: a float `%` has to choose between flooring and truncating, and what a
  // zero divisor does when Float division returns Infinity. Swift and Elm have no float modulo
  // either.
  | _ -> none

/// `Float32`'s operators, as IEEE has them. `**` is computed in `Float`.
let inline float32Op
  ([<InlineIfLambda>] value : Dval -> 'R)
  (none : 'R)
  (op : int)
  (x : float32)
  (y : float32)
  : 'R =
  match op with
  | Add -> value (DFloat32(x + y))
  | Subtract -> value (DFloat32(x - y))
  | Multiply -> value (DFloat32(x * y))
  | Divide -> value (DFloat32(x / y))
  | Power -> value (DFloat32(float32 (float x ** float y)))
  | LessThan -> value (Dval.bool (x < y))
  | LessThanOrEqualTo -> value (Dval.bool (x <= y))
  | GreaterThan -> value (Dval.bool (x > y))
  | GreaterThanOrEqualTo -> value (Dval.bool (x >= y))
  | _ -> none

/// A `Decimal` result, or `DecimalOutOfRange` past 79,228,162,514,264,337,593,543,950,335.
/// Its own function, so the `try` stays out of the callers.
let private decimalChecked (op : int) (x : decimal) (y : decimal) : Outcome =
  try
    match op with
    | Add -> Value(DDecimal(x + y))
    | Subtract -> Value(DDecimal(x - y))
    | Multiply -> Value(DDecimal(x * y))
    | _ -> Value(DDecimal(x / y))
  with :? System.OverflowException ->
    decimalOutOfRange

/// `Decimal`'s operators. Exact where the result fits, and raising where it does not.
let decimalOp (op : int) (x : decimal) (y : decimal) : Outcome =
  match op with
  | Add
  | Subtract
  | Multiply -> decimalChecked op x y
  | Divide -> if y = 0m then divideByZero else decimalChecked op x y
  | Modulo ->
    if y = 0m then
      zeroModulus
    elif y < 0m then
      negativeModulus
    else
      // exact: a decimal remainder is never rounded, and |r| < y, so y + r fits
      let r = x % y
      Value(DDecimal(if r < 0m then y + r else r))
  | LessThan -> Value(Dval.bool (x < y))
  | LessThanOrEqualTo -> Value(Dval.bool (x <= y))
  | GreaterThan -> Value(Dval.bool (x > y))
  | GreaterThanOrEqualTo -> Value(Dval.bool (x >= y))
  | _ -> Unsupported


/// An `Outcome` handed on as <param value>, <param fail> or <param none> would have it.
let inline private relay
  ([<InlineIfLambda>] value : Dval -> 'R)
  ([<InlineIfLambda>] fail : Outcome -> 'R)
  (none : 'R)
  (outcome : Outcome)
  : 'R =
  match outcome with
  | Value v -> value v
  | Raises _ -> fail outcome
  | Unsupported -> none

/// A binary operator on two numbers of one builtin numeric type. Any other pair is <param none>.
let inline binaryWith
  ([<InlineIfLambda>] value : Dval -> 'R)
  ([<InlineIfLambda>] fail : Outcome -> 'R)
  (none : 'R)
  (op : int)
  (a : Dval)
  (b : Dval)
  : 'R =
  // Nested matches, not `match a, b with`: the pair is an allocation per operator.
  match a with
  | DBigInt x ->
    (match b with
     | DBigInt y -> relay value fail none (bigIntOp op x y)
     | _ -> none)
  | DFloat x ->
    (match b with
     | DFloat y -> floatOp value none op x y
     | _ -> none)
  | DFloat32 x ->
    (match b with
     | DFloat32 y -> float32Op value none op x y
     | _ -> none)
  | DDecimal x ->
    (match b with
     | DDecimal y -> relay value fail none (decimalOp op x y)
     | _ -> none)
  | DInt8 x ->
    (match b with
     | DInt8 y -> int8Op value fail none op x y
     | _ -> none)
  | DUInt8 x ->
    (match b with
     | DUInt8 y -> uint8Op value fail none op x y
     | _ -> none)
  | DInt16 x ->
    (match b with
     | DInt16 y -> int16Op value fail none op x y
     | _ -> none)
  | DUInt16 x ->
    (match b with
     | DUInt16 y -> uint16Op value fail none op x y
     | _ -> none)
  | DInt32 x ->
    (match b with
     | DInt32 y -> int32Op value fail none op x y
     | _ -> none)
  | DUInt32 x ->
    (match b with
     | DUInt32 y -> uint32Op value fail none op x y
     | _ -> none)
  | DInt64 x ->
    (match b with
     | DInt64 y -> int64Op value fail none op x y
     | _ -> none)
  | DUInt64 x ->
    (match b with
     | DUInt64 y -> uint64Op value fail none op x y
     | _ -> none)
  | DInt128 x ->
    (match b with
     | DInt128 y -> int128Op value fail none op x y
     | _ -> none)
  | DUInt128 x ->
    (match b with
     | DUInt128 y -> uint128Op value fail none op x y
     | _ -> none)
  | _ -> none

/// A binary operator on two numbers of one builtin numeric type. Any other pair is `Unsupported`.
let binary (op : int) (a : Dval) (b : Dval) : Outcome =
  binaryWith Value (fun outcome -> outcome) Unsupported op a b

/// `binary`'s value, and `ValueNone` wherever it has none: the VM's shape, with no `Outcome`
/// between the table and it.
let inline tryBinary (op : int) (a : Dval) (b : Dval) : Dval voption =
  binaryWith ValueSome (fun _ -> ValueNone) ValueNone op a b


/// `-a` and `~a` on a builtin numeric. `-` is for every type but the unsigned integers, and `~`
/// is for the integers and `BigInt`. Anything else is `Unsupported`.
let unary (op : int) (a : Dval) : Outcome =
  if op = negate then
    match a with
    | DBigInt x -> Value(Dval.dbigint (DarkBigInt.negate x))
    | DFloat x -> Value(DFloat(-x))
    | DFloat32 x -> Value(DFloat32(-x))
    | DDecimal x -> Value(DDecimal(-x))
    | _ -> intUnary op a
  elif op = bitwiseNot then
    match a with
    | DBigInt x ->
      Value(Dval.bigInt (-(DarkBigInt.toBigInt x) - System.Numerics.BigInteger.One))
    | _ -> intUnary op a
  else
    Unsupported
