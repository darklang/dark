/// The operators on the ten fixed-width integer types, each defined once: what it returns, how
/// it wraps, and where it raises.
///
/// Everything that runs an integer operator comes here. The VM answers `a / b` from the registers
/// with it (`FastOps`), and the polymorphic builtins (`Builtin.divide` ...) that the stdlib's
/// `impl Divide for Int32` and the rest call are made of it. So there is no second statement of
/// any of these to drift from, and the set is small and fixed: seventeen operators, which a
/// compiler maps onto its own.
///
/// What each one does:
/// - `+ - *` wrap around (`127y + 1y == -128y`).
/// - `/` truncates toward zero and raises on a zero divisor. `MIN / -1` wraps back to `MIN`.
/// - `%` is Euclidean: `0 <= a % b < b`. It raises on a zero modulus, and on a negative one.
/// - `**` wraps around. It raises on a negative exponent.
/// - `<<` and `>>` take the count modulo the width, as .NET does; `>>` is arithmetic on the
///   signed types.
/// - unary `-` is for the signed types only and wraps `MIN` back to itself; `~` is for all ten.
module LibExecution.IntOps

open Prelude
open RuntimeTypes

// The operators, as constants so `binaryOn` can switch on them. `FastOps` takes its tags for
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


/// What an operator gives two integers: a value, an error, or nothing to say, when they are not
/// two integers of one fixed-width type or the operator is not one of these.
[<Struct>]
type Outcome =
  | Value of value : Dval
  | Raises of error : RuntimeError.BigInts.Error
  | NotInts


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
/// by type; the rest is the same F# operator on every one of them.
let inline binaryOn
  ([<InlineIfLambda>] mk : ^T -> Dval)
  ([<InlineIfLambda>] div : ^T -> ^T -> ^T)
  ([<InlineIfLambda>] pow : ^T -> ^T -> ^T)
  ([<InlineIfLambda>] shl : ^T -> ^T -> ^T)
  ([<InlineIfLambda>] shr : ^T -> ^T -> ^T)
  (op : int)
  (x : ^T)
  (y : ^T)
  : Outcome =
  let zero = LanguagePrimitives.GenericZero< ^T>
  // A `match` on constants, so a jump table rather than a chain of comparisons.
  match op with
  | Add -> Value(mk (x + y))
  | Subtract -> Value(mk (x - y))
  | Multiply -> Value(mk (x * y))
  | LessThan -> Value(Dval.bool (x < y))
  | LessThanOrEqualTo -> Value(Dval.bool (x <= y))
  | GreaterThan -> Value(Dval.bool (x > y))
  | GreaterThanOrEqualTo -> Value(Dval.bool (x >= y))
  | Divide ->
    if y = zero then
      Raises RuntimeError.BigInts.DivideByZeroError
    else
      Value(mk (div x y))
  | Modulo ->
    // Unsigned, `y < zero` and `r < zero` are never true.
    if y = zero then
      Raises RuntimeError.BigInts.ZeroModulus
    elif y < zero then
      Raises RuntimeError.BigInts.NegativeModulus
    else
      let r = x % y
      Value(mk (if r < zero then y + r else r))
  | Power ->
    if y < zero then
      Raises RuntimeError.BigInts.NegativeExponent
    else
      Value(mk (pow x y))
  | BitwiseAnd -> Value(mk (x &&& y))
  | BitwiseOr -> Value(mk (x ||| y))
  | BitwiseXor -> Value(mk (x ^^^ y))
  | ShiftLeft -> Value(mk (shl x y))
  | ShiftRight -> Value(mk (shr x y))
  | _ -> NotInts

let inline int8Op (op : int) (x : int8) (y : int8) : Outcome =
  binaryOn
    DInt8
    // in int32, so -128y / -1y wraps rather than throws
    (fun p q -> int8 (int p / int q))
    (fun p q -> int8 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline uint8Op (op : int) (x : uint8) (y : uint8) : Outcome =
  binaryOn
    DUInt8
    (fun p q -> p / q)
    (fun p q -> uint8 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline int16Op (op : int) (x : int16) (y : int16) : Outcome =
  binaryOn
    DInt16
    // in int32, as for Int8
    (fun p q -> int16 (int p / int q))
    (fun p q -> int16 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline uint16Op (op : int) (x : uint16) (y : uint16) : Outcome =
  binaryOn
    DUInt16
    (fun p q -> p / q)
    (fun p q -> uint16 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline int32Op (op : int) (x : int32) (y : int32) : Outcome =
  binaryOn
    DInt32
    // the division itself would throw
    (fun p q -> if p = System.Int32.MinValue && q = -1 then p else p / q)
    (fun p q -> int32 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline uint32Op (op : int) (x : uint32) (y : uint32) : Outcome =
  binaryOn
    DUInt32
    (fun p q -> p / q)
    (fun p q -> uint32 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline int64Op (op : int) (x : int64) (y : int64) : Outcome =
  binaryOn
    Dval.dint64
    (fun p q -> if p = System.Int64.MinValue && q = -1L then p else p / q)
    (fun p q -> int64 (powWrap64 1UL (uint64 p) (uint64 q)))
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline uint64Op (op : int) (x : uint64) (y : uint64) : Outcome =
  binaryOn
    DUInt64
    (fun p q -> p / q)
    (fun p q -> powWrap64 1UL p q)
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y

let inline int128Op (op : int) (x : System.Int128) (y : System.Int128) : Outcome =
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
    op
    x
    y

let inline uint128Op (op : int) (x : System.UInt128) (y : System.UInt128) : Outcome =
  binaryOn
    DUInt128
    (fun p q -> p / q)
    (fun p q -> powWrapUInt128 System.UInt128.One p q)
    (fun p q -> p <<< int q)
    (fun p q -> p >>> int q)
    op
    x
    y


/// A binary operator on two integers of one fixed-width type. Any other pair is `NotInts`.
let inline binary (op : int) (a : Dval) (b : Dval) : Outcome =
  // Nested matches, not `match a, b with`: the pair is an allocation per operator.
  match a with
  | DInt8 x ->
    (match b with
     | DInt8 y -> int8Op op x y
     | _ -> NotInts)
  | DUInt8 x ->
    (match b with
     | DUInt8 y -> uint8Op op x y
     | _ -> NotInts)
  | DInt16 x ->
    (match b with
     | DInt16 y -> int16Op op x y
     | _ -> NotInts)
  | DUInt16 x ->
    (match b with
     | DUInt16 y -> uint16Op op x y
     | _ -> NotInts)
  | DInt32 x ->
    (match b with
     | DInt32 y -> int32Op op x y
     | _ -> NotInts)
  | DUInt32 x ->
    (match b with
     | DUInt32 y -> uint32Op op x y
     | _ -> NotInts)
  | DInt64 x ->
    (match b with
     | DInt64 y -> int64Op op x y
     | _ -> NotInts)
  | DUInt64 x ->
    (match b with
     | DUInt64 y -> uint64Op op x y
     | _ -> NotInts)
  | DInt128 x ->
    (match b with
     | DInt128 y -> int128Op op x y
     | _ -> NotInts)
  | DUInt128 x ->
    (match b with
     | DUInt128 y -> uint128Op op x y
     | _ -> NotInts)
  | _ -> NotInts


/// `-a` (signed only) and `~a` on a fixed-width integer. Anything else is `NotInts`.
let unary (op : int) (a : Dval) : Outcome =
  if op = negate then
    match a with
    | DInt8 x -> Value(DInt8(-x))
    | DInt16 x -> Value(DInt16(-x))
    | DInt32 x -> Value(DInt32(-x))
    | DInt64 x -> Value(Dval.dint64 (-x))
    | DInt128 x -> Value(DInt128(-x))
    | _ -> NotInts
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
    | _ -> NotInts
  else
    NotInts
