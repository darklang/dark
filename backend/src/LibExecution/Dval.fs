/// Simple pass-through functions for creating Dvals
module LibExecution.Dval

open Prelude

open LibExecution.RuntimeTypes
module VT = ValueType


let int8 (i : int8) = DInt8 i
let uint8 (i : uint8) = DUInt8 i
let int16 (i : int16) = DInt16 i
let uint16 (i : uint16) = DUInt16 i
let int32 (i : int32) = DInt32 i
let uint32 (i : uint32) = DUInt32 i
let int64 (i : int64) = DInt64 i
let uint64 (i : uint64) = DUInt64 i
let int128 (i : System.Int128) = DInt128 i
let uint128 (i : System.UInt128) = DUInt128 i

/// The default `Int`. Normalizes the bigint into `DarkInt` (small values use the
/// Finite/int64 representation; only overflow uses Infinite/bigint).
let int (b : bigint) : Dval = RuntimeTypes.Dval.int b

/// The numeric value of an `Int` Dval as a bigint.
let asBigInt (dv : Dval) : bigint = RuntimeTypes.Dval.asBigInt dv

let string (s : string) = DString s

let uuid (s : System.Guid) = DUuid s

let list (typ : KnownType) (list : List<Dval>) : Dval = DList(VT.known typ, list)

let dict (typ : KnownType) (entries : List<string * Dval>) : Dval =
  DDict(VT.known typ, Map entries)

let dictFromMap (typ : KnownType) (entries : Map<string, Dval>) : Dval =
  DDict(VT.known typ, entries)


let optionType () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.option ())

let optionSome (innerType : KnownType) (dv : Dval) : Dval =
  DEnum(optionType (), optionType (), [ VT.known innerType ], "Some", [ dv ])

let optionNone (innerType : KnownType) : Dval =
  DEnum(optionType (), optionType (), [ VT.known innerType ], "None", [])

let option (innerType : KnownType) (dv : Option<Dval>) : Dval =
  match dv with
  | Some dv -> optionSome innerType dv
  | None -> optionNone innerType



let resultType () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.result ())


let resultOk (okType : KnownType) (errorType : KnownType) (dvOk : Dval) : Dval =
  DEnum(
    resultType (),
    resultType (),
    [ ValueType.Known okType; ValueType.Known errorType ],
    "Ok",
    [ dvOk ]
  )

let resultError
  (okType : KnownType)
  (errorType : KnownType)
  (dvError : Dval)
  : Dval =

  DEnum(
    resultType (),
    resultType (),
    [ ValueType.Known okType; ValueType.Known errorType ],
    "Error",
    [ dvError ]
  )

let result
  (okType : KnownType)
  (errorType : KnownType)
  (dv : Result<Dval, Dval>)
  : Dval =
  match dv with
  | Ok dv -> resultOk okType errorType dv
  | Error dv -> resultError okType errorType dv


/// Can this Dval be stored as the evaluated body of a package value?
///
/// The persist path in `Seed.fs` runs each package value's body and
/// writes the resulting Dval as `package_values.rt_dval` bytes. The
/// binary serializer raises on exactly two shapes — we'd rather
/// report "cannot store this kind of value in a val" up front than
/// surface a deep-stack serialize exception.
///
/// Rejected:
/// - `DStream` — the pull fn is a closure bound to this exeState.
/// - `DBlob(Ephemeral _)` — the bytes are VM-local (inline, GC-bound);
///   promote to `Persistent` first. Most call paths already promote
///   (see `Blob.promote` for the val-commit / DB-write path and
///   `LibDB.Tracing.prepareDvalForStorage` for the trace path); this
///   branch is a safety net.
///
/// Walks containers so one bad leaf anywhere in the tree disqualifies
/// the whole value.
let rec isPersistable (dv : Dval) : bool =
  match dv with
  // These two shapes `raiseFormatError` in the Dval binary serializer
  // (see LibSerialization/Binary/Serializers/RT/Dval.fs). Catch them
  // here so the Seed.fs evaluator can report a clean reason instead
  // of surfacing a deep-stack raise.
  | DStream _ -> false
  | DBlob(Ephemeral _) -> false

  | DUnit
  | DBool _
  | DInt8 _
  | DUInt8 _
  | DInt16 _
  | DUInt16 _
  | DInt32 _
  | DUInt32 _
  | DInt64 _
  | DUInt64 _
  | DInt128 _
  | DUInt128 _
  | DInt _
  | DFloat _
  | DChar _
  | DString _
  | DDateTime _
  | DUuid _
  // DApplicable and DDB serialize successfully — lambdas store their
  // instruction stream, DB handles store as a string identifier.
  // Demo handlers persist lambdas as vals; user DBs rely on the DDB path.
  | DApplicable _
  | DDB _
  | DBlob(Persistent _) -> true

  | DList(_, items) -> items |> List.forall isPersistable
  | DTuple(a, b, rest) ->
    isPersistable a && isPersistable b && List.forall isPersistable rest
  | DDict(_, entries) -> entries |> Map.values |> Seq.forall isPersistable
  | DRecord(_, _, _, fields) -> fields |> Map.values |> Seq.forall isPersistable
  | DEnum(_, _, _, _, fields) -> fields |> List.forall isPersistable


/// Human-readable explanation of why [isPersistable] rejected a value.
/// Returns the first offending shape found — good enough for an error
/// message pointing the user at the problem.
let rec nonPersistableReason (dv : Dval) : Option<string> =
  match dv with
  | DStream _ ->
    Some "stream values can't be stored in a `val` — drain to a Blob or List first"
  | DBlob(Ephemeral _) ->
    Some
      "ephemeral blob can't be stored in a `val` — promote to persistent (serialize) first"

  | DList(_, items) -> items |> List.tryPick nonPersistableReason
  | DTuple(a, b, rest) -> [ a; b ] @ rest |> List.tryPick nonPersistableReason
  | DDict(_, entries) -> entries |> Map.values |> Seq.tryPick nonPersistableReason
  | DRecord(_, _, _, fields) ->
    fields |> Map.values |> Seq.tryPick nonPersistableReason
  | DEnum(_, _, _, _, fields) -> fields |> List.tryPick nonPersistableReason

  | _ -> None


let byteArrayToDvalList (bytes : byte[]) : Dval =
  bytes
  |> Array.toList
  |> List.map (fun b -> DUInt8(byte b))
  |> fun dvalList -> DList(VT.uint8, dvalList)

let dlistToByteArray (dvalList : List<Dval>) : byte[] =
  dvalList
  |> List.map (fun dval ->
    match dval with
    | DUInt8 b -> b
    | _ -> (Exception.raiseInternal "Invalid type in byte list") [])
  |> Array.ofList


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
    | AppNamedFn a, AppNamedFn b -> a = b
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
