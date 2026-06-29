module Builtins.Pure.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module ValueType = LibExecution.ValueType
module RTE = RuntimeError


/// Structural equality. Walks two Dvals in parallel and returns
/// true iff every reachable leaf compares equal. Type errors
/// (callers passing structurally-incompatible Dvals) return false
/// rather than raising — the caller's responsibility to type-check
/// up front via VT.merge.
///
/// Blob comparison is identity-based: same-hash Persistents are
/// equal; same-UUID Ephemerals are equal; mixed cases (different
/// Ephemerals, Ephemeral vs Persistent) are false even when bytes
/// match. Byte-equality across freshly-built ephemerals would
/// require dereferencing — for Persistents that's an async
/// `package_blobs` lookup, which would force the entire `equals`
/// surface to be Ply-shaped just for one rare case. Callers that
/// want byte-equality across ephemerals should `Blob.promote` both
/// sides first; same bytes → same hash → equal as Persistents.
///
/// Streams compare by reference identity on their `lockObj`. Same
/// handle → true (preserves reflexivity). Different handles → false;
/// cross-handle equality would require draining, which violates
/// single-consumer semantics.
let rec equals (a : Dval) (b : Dval) : bool =
  let r = equals

  match a, b with
  | DUnit, DUnit -> true

  | DBool a, DBool b -> a = b

  | DInt8 a, DInt8 b -> a = b
  | DUInt8 a, DUInt8 b -> a = b
  | DInt16 a, DInt16 b -> a = b
  | DUInt16 a, DUInt16 b -> a = b
  | DInt32 a, DInt32 b -> a = b
  | DUInt32 a, DUInt32 b -> a = b
  | DInt64 a, DInt64 b -> a = b
  | DUInt64 a, DUInt64 b -> a = b
  | DInt128 a, DInt128 b -> a = b
  | DUInt128 a, DUInt128 b -> a = b
  | DInt a, DInt b -> a = b

  | DFloat a, DFloat b -> a = b

  | DChar a, DChar b -> a = b
  | DString a, DString b -> a = b

  | DDateTime a, DDateTime b -> a = b

  | DUuid a, DUuid b -> a = b

  | DList(typA, a), DList(typB, b) ->
    Result.isOk (ValueType.merge typA typB)
    && a.Length = b.Length
    && List.forall2 r a b

  | DTuple(a1, a2, a3), DTuple(b1, b2, b3) ->
    a3.Length = b3.Length && r a1 b1 && r a2 b2 && List.forall2 r a3 b3

  | DDict(typeA, a), DDict(typeB, b) ->
    Result.isOk (ValueType.merge typeA typeB)
    && Map.count a = Map.count b
    && (a
        |> Map.toSeq
        |> Seq.forall (fun (k, va) ->
          match Map.find k b with
          | Some vb -> r va vb
          | None -> false))

  | DRecord(_, typeNameA, typeArgsA, fieldsA),
    DRecord(_, typeNameB, typeArgsB, fieldsB) ->
    typeNameA = typeNameB
    && typeArgsA.Length = typeArgsB.Length
    && List.forall2
      (fun ta tb -> Result.isOk (ValueType.merge ta tb))
      typeArgsA
      typeArgsB
    && Map.count fieldsA = Map.count fieldsB
    && (fieldsA
        |> Map.toSeq
        |> Seq.forall (fun (k, va) ->
          match Map.find k fieldsB with
          | Some vb -> r va vb
          | None -> false))

  | DEnum(_, typeNameA, typeArgsA, caseNameA, fieldsA),
    DEnum(_, typeNameB, typeArgsB, caseNameB, fieldsB) ->
    typeNameA = typeNameB
    && typeArgsA.Length = typeArgsB.Length
    && List.forall2
      (fun ta tb -> Result.isOk (ValueType.merge ta tb))
      typeArgsA
      typeArgsB
    && caseNameA = caseNameB
    && fieldsA.Length = fieldsB.Length
    && List.forall2 r fieldsA fieldsB

  | DApplicable a, DApplicable b ->
    match a, b with
    // CLEANUP exprId is a partial check — fully checking LambdaImpl
    // equality needs lambda-internal-state work. Today this is
    // "same-source-position lambdas compare equal."
    | AppLambda a, AppLambda b -> a.exprId = b.exprId
    | AppNamedFn a, AppNamedFn b ->
      // `location` is excluded: it records how the function was named at the
      // reference site (for error messages), not its identity. Two references
      // to the same function by different names are the same function.
      a.name = b.name
      && a.typeArgs = b.typeArgs
      && a.argsSoFar = b.argsSoFar
      && a.typeSymbolTable = b.typeSymbolTable
    | _ -> false

  | DDB a, DDB b -> a = b

  | DBlob refA, DBlob refB ->
    // Identity-based: same hash (Persistent) or same UUID (Ephemeral).
    // Different ephemerals never compare equal — promote first if you
    // want byte-equality.
    match refA, refB with
    | Persistent(h1, l1), Persistent(h2, l2) -> h1 = h2 && l1 = l2
    | Ephemeral e1, Ephemeral e2 -> e1.id = e2.id
    | _ -> false

  | DStream(_, _, lockA), DStream(_, _, lockB) ->
    // Reference equality on lockObj — same-handle preserves
    // reflexivity. Cross-handle compare without consuming the streams
    // is fundamentally impossible under the single-consumer rule.
    System.Object.ReferenceEquals(lockA, lockB)

  // exhaustiveness — type mismatches return false; caller VT-merges
  // up front to convert to a clean RTE.
  | DUnit, _
  | DBool _, _
  | DInt8 _, _
  | DUInt8 _, _
  | DInt16 _, _
  | DUInt16 _, _
  | DInt32 _, _
  | DUInt32 _, _
  | DInt64 _, _
  | DUInt64 _, _
  | DInt128 _, _
  | DUInt128 _, _
  | DInt _, _
  | DFloat _, _
  | DChar _, _
  | DString _, _
  | DDateTime _, _
  | DUuid _, _
  | DList _, _
  | DTuple _, _
  | DDict _, _
  | DRecord _, _
  | DEnum _, _
  | DApplicable _, _
  | DDB _, _
  | DBlob _, _
  | DStream _, _ -> false


let varA = TVariable "a"
let varB = TVariable "b"


/// Shared body for `equals` / `notEquals` builtins — VT-merge type
/// check + structural compare.
let private equalsBuiltinImpl (vm : VMState) (a : Dval) (b : Dval) : bool =
  let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
  match ValueType.merge vtA vtB with
  | Error _ -> RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB) |> raiseRTE vm.threadID
  | Ok _ -> equals a b


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

let private outOfRange (vm : VMState) : 'a =
  RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.threadID

let private negativeExponent (vm : VMState) : 'a =
  RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID

/// Runs an integer computation that may overflow, converting the .NET
/// OverflowException into a clean `OutOfRange` RTE. Used by `/` and `^`, where
/// overflow is still an error (`+`, `-`, `*` wrap instead): `Int64.MinValue / -1L`
/// and oversized `^` results would otherwise escape as an uncaught exception.
let private overflowChecked (vm : VMState) (f : unit -> Dval) : Ply<Dval> =
  try
    Ply(f ())
  with :? System.OverflowException ->
    outOfRange vm


let fns () : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, vm, _, [ a; b ] -> equalsBuiltinImpl vm a b |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "="
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "notEquals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are not equal"
      fn =
        (function
        | _, vm, _, [ a; b ] -> equalsBuiltinImpl vm a b |> not |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Adds two numbers of the same numeric type. Fixed-width integer overflow
         raises a runtime error; the arbitrary-precision Int grows instead of
         overflowing; float arithmetic follows IEEE (overflow to infinity)."
      fn =
        (function
        | _, vm, _, [ DInt8 a; DInt8 b ] ->
          overflowChecked vm (fun () -> DInt8(Checked.(+) a b))
        | _, vm, _, [ DUInt8 a; DUInt8 b ] ->
          overflowChecked vm (fun () -> DUInt8(Checked.(+) a b))
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          overflowChecked vm (fun () -> DInt16(Checked.(+) a b))
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          overflowChecked vm (fun () -> DUInt16(Checked.(+) a b))
        | _, vm, _, [ DInt32 a; DInt32 b ] ->
          overflowChecked vm (fun () -> DInt32(Checked.(+) a b))
        | _, vm, _, [ DUInt32 a; DUInt32 b ] ->
          overflowChecked vm (fun () -> DUInt32(Checked.(+) a b))
        | _, vm, _, [ DInt64 a; DInt64 b ] ->
          overflowChecked vm (fun () -> DInt64(Checked.(+) a b))
        | _, vm, _, [ DUInt64 a; DUInt64 b ] ->
          overflowChecked vm (fun () -> DUInt64(Checked.(+) a b))
        | _, vm, _, [ DInt128 a; DInt128 b ] ->
          overflowChecked vm (fun () ->
            DInt128(System.Int128.op_CheckedAddition (a, b)))
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          overflowChecked vm (fun () ->
            DUInt128(System.UInt128.op_CheckedAddition (a, b)))
        | _, _, _, [ DInt a; DInt b ] -> Ply(DInt(DarkInt.add a b))
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a + b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "+"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Subtracts two numbers of the same numeric type. Fixed-width integer
         overflow raises a runtime error; the arbitrary-precision Int grows
         instead of overflowing; float arithmetic follows IEEE (overflow to
         infinity)."
      fn =
        (function
        | _, vm, _, [ DInt8 a; DInt8 b ] ->
          overflowChecked vm (fun () -> DInt8(Checked.(-) a b))
        | _, vm, _, [ DUInt8 a; DUInt8 b ] ->
          overflowChecked vm (fun () -> DUInt8(Checked.(-) a b))
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          overflowChecked vm (fun () -> DInt16(Checked.(-) a b))
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          overflowChecked vm (fun () -> DUInt16(Checked.(-) a b))
        | _, vm, _, [ DInt32 a; DInt32 b ] ->
          overflowChecked vm (fun () -> DInt32(Checked.(-) a b))
        | _, vm, _, [ DUInt32 a; DUInt32 b ] ->
          overflowChecked vm (fun () -> DUInt32(Checked.(-) a b))
        | _, vm, _, [ DInt64 a; DInt64 b ] ->
          overflowChecked vm (fun () -> DInt64(Checked.(-) a b))
        | _, vm, _, [ DUInt64 a; DUInt64 b ] ->
          overflowChecked vm (fun () -> DUInt64(Checked.(-) a b))
        | _, vm, _, [ DInt128 a; DInt128 b ] ->
          overflowChecked vm (fun () ->
            DInt128(System.Int128.op_CheckedSubtraction (a, b)))
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          overflowChecked vm (fun () ->
            DUInt128(System.UInt128.op_CheckedSubtraction (a, b)))
        | _, _, _, [ DInt a; DInt b ] -> Ply(DInt(DarkInt.subtract a b))
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a - b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "-"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Multiplies two numbers of the same numeric type. Fixed-width integer
         overflow raises a runtime error; the arbitrary-precision Int grows
         instead of overflowing; float arithmetic follows IEEE (overflow to
         infinity)."
      fn =
        (function
        | _, vm, _, [ DInt8 a; DInt8 b ] ->
          overflowChecked vm (fun () -> DInt8(Checked.(*) a b))
        | _, vm, _, [ DUInt8 a; DUInt8 b ] ->
          overflowChecked vm (fun () -> DUInt8(Checked.(*) a b))
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          overflowChecked vm (fun () -> DInt16(Checked.(*) a b))
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          overflowChecked vm (fun () -> DUInt16(Checked.(*) a b))
        | _, vm, _, [ DInt32 a; DInt32 b ] ->
          overflowChecked vm (fun () -> DInt32(Checked.(*) a b))
        | _, vm, _, [ DUInt32 a; DUInt32 b ] ->
          overflowChecked vm (fun () -> DUInt32(Checked.(*) a b))
        | _, vm, _, [ DInt64 a; DInt64 b ] ->
          overflowChecked vm (fun () -> DInt64(Checked.(*) a b))
        | _, vm, _, [ DUInt64 a; DUInt64 b ] ->
          overflowChecked vm (fun () -> DUInt64(Checked.(*) a b))
        | _, vm, _, [ DInt128 a; DInt128 b ] ->
          overflowChecked vm (fun () ->
            DInt128(System.Int128.op_CheckedMultiply (a, b)))
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          overflowChecked vm (fun () ->
            DUInt128(System.UInt128.op_CheckedMultiply (a, b)))
        | _, _, _, [ DInt a; DInt b ] -> Ply(DInt(DarkInt.multiply a b))
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a * b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "*"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Divides two numbers of the same numeric type. Integer types use integer
         division; dividing an integer by zero raises a runtime error. Signed
         integer overflow (e.g. {{Int64.MinValue / -1}}) also raises a runtime
         error."
      fn =
        // Unsigned division can't overflow. Signed division overflows only on
        // `MinValue / -1`. For Int32/Int64/Int128 that throws (caught by
        // `overflowChecked`); for the narrow Int8/Int16 it silently wraps
        // (division happens in Int32 space), so those need an explicit guard.
        (function
        | _, vm, _, [ DInt8 a; DInt8 b ] ->
          if b = 0y then divideByZero vm
          elif a = System.SByte.MinValue && b = -1y then outOfRange vm
          else Ply(DInt8(a / b))
        | _, vm, _, [ DUInt8 a; DUInt8 b ] ->
          if b = 0uy then divideByZero vm else Ply(DUInt8(a / b))
        | _, vm, _, [ DInt16 a; DInt16 b ] ->
          if b = 0s then divideByZero vm
          elif a = System.Int16.MinValue && b = -1s then outOfRange vm
          else Ply(DInt16(a / b))
        | _, vm, _, [ DUInt16 a; DUInt16 b ] ->
          if b = 0us then divideByZero vm else Ply(DUInt16(a / b))
        | _, vm, _, [ DInt32 a; DInt32 b ] ->
          if b = 0l then
            divideByZero vm
          else
            overflowChecked vm (fun () -> DInt32(a / b))
        | _, vm, _, [ DUInt32 a; DUInt32 b ] ->
          if b = 0ul then divideByZero vm else Ply(DUInt32(a / b))
        | _, vm, _, [ DInt64 a; DInt64 b ] ->
          if b = 0L then
            divideByZero vm
          else
            overflowChecked vm (fun () -> DInt64(a / b))
        | _, vm, _, [ DUInt64 a; DUInt64 b ] ->
          if b = 0UL then divideByZero vm else Ply(DUInt64(a / b))
        | _, vm, _, [ DInt128 a; DInt128 b ] ->
          if b = System.Int128.Zero then
            divideByZero vm
          else
            overflowChecked vm (fun () -> DInt128(a / b))
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          if b = System.UInt128.Zero then divideByZero vm else Ply(DUInt128(a / b))
        | _, vm, _, [ DInt a; DInt b ] ->
          if DarkInt.isZero b then divideByZero vm else Ply(DInt(DarkInt.divide a b))
        // Float division by zero follows IEEE semantics (Infinity/NaN), as before
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DFloat(a / b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "/"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "modulo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = varA
      description =
        "Wraps <param a> around so that {{0 <= res < b}}, for two numbers of the
         same numeric type. The modulus <param b> must be greater than 0."
      fn =
        (function
        | _, vm, _, [ DInt8 v; DInt8 m ] ->
          if m = 0y then
            zeroModulus vm
          elif m < 0y then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt8(if r < 0y then m + r else r))
        | _, vm, _, [ DUInt8 v; DUInt8 m ] ->
          if m = 0uy then zeroModulus vm else Ply(DUInt8(v % m))
        | _, vm, _, [ DInt16 v; DInt16 m ] ->
          if m = 0s then
            zeroModulus vm
          elif m < 0s then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt16(if r < 0s then m + r else r))
        | _, vm, _, [ DUInt16 v; DUInt16 m ] ->
          if m = 0us then zeroModulus vm else Ply(DUInt16(v % m))
        | _, vm, _, [ DInt32 v; DInt32 m ] ->
          if m = 0l then
            zeroModulus vm
          elif m < 0l then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt32(if r < 0l then m + r else r))
        | _, vm, _, [ DUInt32 v; DUInt32 m ] ->
          if m = 0ul then zeroModulus vm else Ply(DUInt32(v % m))
        | _, vm, _, [ DInt64 v; DInt64 m ] ->
          if m = 0L then
            zeroModulus vm
          elif m < 0L then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt64(if r < 0L then m + r else r))
        | _, vm, _, [ DUInt64 v; DUInt64 m ] ->
          if m = 0UL then zeroModulus vm else Ply(DUInt64(v % m))
        | _, vm, _, [ DInt128 v; DInt128 m ] ->
          if m = System.Int128.Zero then
            zeroModulus vm
          elif m < System.Int128.Zero then
            negativeModulus vm
          else
            let r = v % m
            Ply(DInt128(if r < System.Int128.Zero then m + r else r))
        | _, vm, _, [ DUInt128 v; DUInt128 m ] ->
          if m = System.UInt128.Zero then zeroModulus vm else Ply(DUInt128(v % m))
        | _, vm, _, [ DInt v; DInt m ] ->
          let m = DarkInt.toBigInt m
          if m = System.Numerics.BigInteger.Zero then
            zeroModulus vm
          elif m < System.Numerics.BigInteger.Zero then
            negativeModulus vm
          else
            let r = DarkInt.toBigInt v % m
            Ply(Dval.int (if r < System.Numerics.BigInteger.Zero then m + r else r))
        | _, vm, _, [ DFloat v; DFloat m ] ->
          if m = 0.0 then
            zeroModulus vm
          elif m < 0.0 then
            negativeModulus vm
          else
            let r = v % m
            Ply(DFloat(if r < 0.0 then m + r else r))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "%"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "power" 0
      typeParams = []
      parameters = [ Param.make "base" varA ""; Param.make "exponent" varA "" ]
      returnType = varA
      description =
        "Raises a number to the power of another number of the same type.
         Supported for every integer type and Float (not Int128/UInt128).
         Integer exponents must be non-negative. Fixed-width integer overflow
         raises a runtime error; the arbitrary-precision Int grows instead."
      fn =
        (function
        | _, vm, _, [ DInt8 number; DInt8 exp ] ->
          if exp < 0y then
            negativeExponent vm
          else
            overflowChecked vm (fun () ->
              (bigint number) ** (int exp) |> int8 |> DInt8)
        | _, vm, _, [ DUInt8 number; DUInt8 exp ] ->
          overflowChecked vm (fun () ->
            (bigint number) ** (int exp) |> uint8 |> DUInt8)
        | _, vm, _, [ DInt16 number; DInt16 exp ] ->
          if exp < 0s then
            negativeExponent vm
          else
            overflowChecked vm (fun () ->
              (bigint number) ** (int exp) |> int16 |> DInt16)
        | _, vm, _, [ DUInt16 number; DUInt16 exp ] ->
          overflowChecked vm (fun () ->
            (bigint number) ** (int exp) |> uint16 |> DUInt16)
        | _, vm, _, [ DInt32 number; DInt32 exp ] ->
          if exp < 0l then
            negativeExponent vm
          else
            overflowChecked vm (fun () ->
              (bigint number) ** (int exp) |> int32 |> DInt32)
        | _, vm, _, [ DUInt32 number; DUInt32 exp ] ->
          overflowChecked vm (fun () ->
            (bigint number) ** (int exp) |> uint32 |> DUInt32)
        | _, vm, _, [ DUInt64 number; DUInt64 exp ] ->
          if exp > uint64 System.Int32.MaxValue then
            match number with
            | 0UL -> Ply(DUInt64 0UL)
            | 1UL -> Ply(DUInt64 1UL)
            | _ -> outOfRange vm
          elif exp > 63UL && number > 1UL then
            outOfRange vm
          else
            overflowChecked vm (fun () ->
              (bigint number) ** (int exp) |> uint64 |> DUInt64)
        | _, vm, _, [ DInt64 number; DInt64 exp ] ->
          if exp < 0L then
            negativeExponent vm
          elif exp > int64 System.Int32.MaxValue then
            // `bigint ** int` needs an Int32 exponent; an exponent this large
            // overflows Int64 unless the base is trivial.
            match number with
            | 0L -> Ply(DInt64 0L)
            | 1L -> Ply(DInt64 1L)
            | -1L -> Ply(DInt64(if exp % 2L = 0L then 1L else -1L))
            | _ -> outOfRange vm
          elif exp > 63L && (number > 1L || number < -1L) then
            // Avoid constructing enormous bigints that can never fit in Int64.
            outOfRange vm
          else
            // `int64` narrowing throws OverflowException when the result
            // exceeds Int64 range.
            overflowChecked vm (fun () ->
              (bigint number) ** (int exp) |> int64 |> DInt64)
        | _, vm, _, [ DInt number; DInt exp ] ->
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
        | _, _, _, [ DFloat number; DFloat exp ] -> Ply(DFloat(number ** exp))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "POWER"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Unary negation (`-x`). Supported for signed integer types and Float;
    // negating the minimum signed value overflows and raises.
    { name = fn "negate" 0
      typeParams = []
      parameters = [ Param.make "a" varA "" ]
      returnType = varA
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, vm, _, [ DInt8 a ] ->
          let result = -(int a)
          if result < -128 || result > 127 then
            outOfRange vm
          else
            Ply(DInt8(int8 result))
        | _, vm, _, [ DInt16 a ] ->
          if a = System.Int16.MinValue then outOfRange vm else Ply(DInt16(-a))
        | _, vm, _, [ DInt32 a ] ->
          if a = System.Int32.MinValue then outOfRange vm else Ply(DInt32(-a))
        | _, vm, _, [ DInt64 a ] ->
          if a = System.Int64.MinValue then outOfRange vm else Ply(DInt64(-a))
        | _, vm, _, [ DInt128 a ] ->
          overflowChecked vm (fun () ->
            DInt128(System.Int128.op_CheckedUnaryNegation a))
        | _, _, _, [ DInt a ] -> Ply(DInt(DarkInt.negate a))
        | _, _, _, [ DFloat a ] -> Ply(DFloat(-a))
        | _, vm, _, [ a ] -> numericTypeError vm a a
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a > b))
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b > 0))
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a > b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a >= b))
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b >= 0))
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a >= b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp ">="
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a < b))
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b < 0))
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a < b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt8 a; DInt8 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DUInt8 a; DUInt8 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DInt16 a; DInt16 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DUInt16 a; DUInt16 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DInt32 a; DInt32 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DUInt32 a; DUInt32 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DInt64 a; DInt64 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DUInt64 a; DUInt64 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a <= b))
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b <= 0))
        | _, _, _, [ DFloat a; DFloat b ] -> Ply(DBool(a <= b))
        | _, vm, _, [ a; b ] -> numericTypeError vm a b
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<="
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
      fn =
        (function
        | _, _, _, [] -> incorrectArgs ()
        | _, vm, _, [ dval ] ->
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
          RuntimeError.Unwraps.MultipleArgs multipleArgs
          |> RuntimeError.Unwrap
          |> raiseRTE vm.threadID)

      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
