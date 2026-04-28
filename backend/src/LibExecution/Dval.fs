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
/// - `DBlob(Ephemeral _)` — the raw bytes live in exeState.blobStore;
///   promote to `Persistent` first. Most call paths already promote
///   (see `promoteBlobs`); this branch is a safety net.
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
  | DFloat _
  | DChar _
  | DString _
  | DDateTime _
  | DUuid _
  // DApplicable and DDB serialize successfully — lambdas store their
  // instruction stream, DB handles store as a string identifier.
  // Demo handlers persist lambdas as vals; canvas-local DBs rely on
  // the DDB path.
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
