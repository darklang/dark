/// The core types and functions used by the Dark language's runtime.
///
/// This format is lossy, relative to the ProgramTypes; use IDs to refer back.
/// CLEANUP we could realistically expand upon this a bit,
///   excluding things like enum field names, fn param names, etc.
///   (referring back to PT by index or something)
module LibExecution.RuntimeTypes

open Prelude

// Aliases for the .NET mutable collection types used across the runtime
// state. We can't `open System.Collections.Generic` because it shadows
// F#'s native `list` with `System.Collections.Generic.List`.
type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
type HashSet<'a> = System.Collections.Generic.HashSet<'a>
type Stack<'a> = System.Collections.Generic.Stack<'a>


type BranchId = uuid

/// Structural hash of a package item's content (shape, not name/location).
type Hash =
  | Hash of string
  // F#'s default ToString() on unions uses reflection (StructuredPrintfImpl),
  // which fails under AOT trimming. Provide an explicit override so callers
  // like `string (h : Hash)` don't go through that path.
  override this.ToString() = let (Hash s) = this in s

module Hash =
  let empty : Hash = Hash ""
  let toHexString (Hash h) : string = h

let builtinNamePattern = @"^(__|[a-z])[a-z0-9A-Z_]\w*$"
let valueNamePattern = @"^[a-z][a-z0-9A-Z_']*$"

let assertBuiltin
  (name : string)
  (version : int)
  (nameValidator : string -> unit)
  : unit =
  nameValidator name
  assert_ "version can't be negative" [ "version", version ] (version >= 0)


/// Fully-Qualified Type Name
///
/// Used to reference a type defined in a Package
module FQTypeName =
  /// The hash of a type in the package manager
  type Package = Hash

  type FQTypeName = Package of Package

  let package (h : string) : Package = Hash h

  let fqPackage (h : string) : FQTypeName = Package(Hash h)


/// A Fully-Qualified Value Name
///
/// Used to reference a value defined by the runtime or in a Package
module FQValueName =
  /// A value built into the runtime
  type Builtin = { name : string; version : int }

  /// The hash of a value in the package manager
  type Package = Hash

  type FQValueName =
    | Builtin of Builtin
    | Package of Package

  let assertValueName (name : string) : unit =
    assertRe "Value name must match" valueNamePattern name

  let builtin (name : string) (version : int) : Builtin =
    assertBuiltin name version assertValueName
    { name = name; version = version }

  let package (h : string) : Package = Hash h

  let fqPackage (h : string) : FQValueName = Package(Hash h)


/// A Fully-Qualified Function Name
///
/// Used to reference a function defined by the runtime or in a Package
module FQFnName =
  /// A function built into the runtime
  type Builtin = { name : string; version : int }

  type Package = Hash

  type FQFnName =
    | Builtin of Builtin
    | Package of Package

  let assertBuiltinFnName (name : string) : unit =
    assertRe $"Fn name must match" builtinNamePattern name

  let builtin (name : string) (version : int) : Builtin =
    assertBuiltin name version assertBuiltinFnName
    { name = name; version = version }

  let package (h : string) : Package = Hash h

  let fqBuiltin (name : string) (version : int) : FQFnName =
    Builtin { name = name; version = version }

  let fqPackage (h : string) : FQFnName = Package(Hash h)



/// TODO include "ParseTime" in name (requires a lot of boring work in many files)
type NameResolutionError =
  | NotFound
  | InvalidName

type NameResolution<'a> =
  { originalName : List<string>; resolved : Result<'a, NameResolutionError> }

module NameResolution =
  let ok (value : 'a) : NameResolution<'a> =
    { originalName = []; resolved = Ok value }


/// A KnownType represents the type of a dval.
///
/// Many KnownTypes (such as lists and records) have nested types. Often, these
/// nested types are unknown (such as the contents of an empty list, or the
/// `Result.Error` type for `Ok 5`). As such, KnownTypes always nest ValueTypes
/// (an optional form of KnownType).
type KnownType =
  | KTUnit
  | KTBool
  | KTInt8
  | KTUInt8
  | KTInt16
  | KTUInt16
  | KTInt32
  | KTUInt32
  | KTInt64
  | KTUInt64
  | KTInt128
  | KTUInt128
  // arbitrary-precision integer; the default `Int`
  | KTInt
  | KTFloat
  | KTChar
  | KTString
  | KTUuid
  | KTDateTime

  /// Immutable byte sequence.
  | KTBlob

  /// Lazy value sequence parameterised over element type.
  | KTStream of ValueType

  /// `let empty =    []` // KTList Unknown
  /// `let intList = [1]` // KTList (ValueType.Known KTInt)
  | KTList of ValueType

  /// Intuitively, since `Dval`s generate `KnownType`s, you would think that we can
  /// use `KnownType`s in a `KTTuple`.
  ///
  /// However, we sometimes construct a KTTuple to repesent the type of a Tuple
  /// which doesn't exist. For example, in `List.zip [] []`, we create the result
  /// from the types of the two lists, which themselves might be (and likely are)
  /// `Unknown`.
  | KTTuple of ValueType * ValueType * List<ValueType>

  /// let f = (fun x -> x)        // KTFn([Unknown], Unknown)
  /// let intF = (fun (x: Int) -> x) // KTFn([Known KTInt], Unknown)
  ///
  /// Note that we could theoretically know some return types by analyzing the
  /// code or type signatures of functions. We don't do this yet as it's
  /// complicated. When we do decide to do this, some incorrect programs may stop
  /// functioning (see example). Our goal is for correctly typed functions to
  /// stay working so this might be ok.
  ///
  /// For example:
  ///   let z1 = (fun x -> 5)
  ///   let z2 = (fun x -> "str")
  /// `[z1, z2]` is allowed now but might not be allowed later
  | KTFn of args : NEList<ValueType> * ret : ValueType

  /// At time of writing, all DBs are of a specific type, and DBs may only be
  /// referenced directly, but we expect to eventually allow references to DBs
  /// where the type may be unknown
  /// List.head ([]: List<DB<'a>>) // KTDB (Unknown)
  | KTDB of ValueType

  /// let n = None          // type args: [Unknown]
  /// let s = Some(5)       // type args: [Known KTInt64]
  /// let o = Ok (5)        // type args: [Known KTInt64, Unknown]
  /// let e = Error ("str") // type args: [Unknown, Known KTString]
  | KTCustomType of FQTypeName.FQTypeName * typeArgs : List<ValueType>

  /// let myDict = {} // KTDict Unknown
  | KTDict of ValueType

/// Represents the actual type of a Dval
///
/// "Unknown" represents the concept of "bottom" in
///   type system / data flow analysis / lattices
and [<RequireQualifiedAccess>] ValueType =
  | Unknown
  | Known of KnownType




/// The payload of the default `Int` (`DInt`).
/// Int64-range values are `Finite`; larger values are `Infinite`.
/// Use `DarkInt.ofBigInt` to preserve that invariant.
///
/// CLEANUP: because this is a DU, small Ints still carry space for the bigint
/// case plus a tag, not just the Int64 value. If profiling shows this matters,
/// revisit the representation or consider caching common small values.
[<Struct; RequireQualifiedAccess>]
type DarkInt =
  | Finite of finite : int64
  | Infinite of infinite : bigint

module DarkInt =
  let private int64Min = bigint System.Int64.MinValue
  let private int64Max = bigint System.Int64.MaxValue

  /// Smart constructor enforcing the invariant: a value fitting Int64 becomes
  /// `Finite`, only overflow becomes `Infinite`.
  let ofBigInt (b : bigint) : DarkInt =
    if b >= int64Min && b <= int64Max then
      DarkInt.Finite(int64 b)
    else
      DarkInt.Infinite b

  let toBigInt (di : DarkInt) : bigint =
    match di with
    | DarkInt.Finite i -> bigint i
    | DarkInt.Infinite b -> b

  /// Checked narrowing to Int64: `Some` only when the value fits. An `Infinite`
  /// is by invariant always outside Int64 range, so it is always `None`.
  let toInt64 (di : DarkInt) : int64 option =
    match di with
    | DarkInt.Finite i -> Some i
    | DarkInt.Infinite _ -> None

  /// Checked narrowing to a native `int` (Int32): `Some` only when the value
  /// fits `[Int32.MinValue, Int32.MaxValue]`.
  let toInt32 (di : DarkInt) : int option =
    match di with
    | DarkInt.Finite i when
      i >= int64 System.Int32.MinValue && i <= int64 System.Int32.MaxValue
      ->
      Some(int i)
    | _ -> None

  let isZero (di : DarkInt) : bool =
    match di with
    | DarkInt.Finite i -> i = 0L
    | DarkInt.Infinite _ -> false // 0 is always Finite, so an Infinite is never zero

  /// Compare by numeric value (NOT by case tag — an Infinite is always outside
  /// Int64 range, so tag order would be wrong).
  /// Both operands out of Int64 range, or one of each. Split out so the Finite/Finite path below
  /// doesn't have a local function or a tuple in it, either of which is an allocation per call.
  let private compareViaBigInt (a : DarkInt) (b : DarkInt) : int =
    let bx = toBigInt a
    let by = toBigInt b
    if bx < by then -1
    elif bx > by then 1
    else 0

  let compare (a : DarkInt) (b : DarkInt) : int =
    // Nested matches rather than `match a, b with`. The tuple form reads better and allocates the
    // pair: `DarkInt` is a struct carrying a bigint's worth of space, so the pair is about 64 bytes,
    // on a function that runs once per comparison in a script.
    match a with
    | DarkInt.Finite x ->
      match b with
      | DarkInt.Finite y ->
        if x < y then -1
        elif x > y then 1
        else 0
      | DarkInt.Infinite _ -> compareViaBigInt a b
    | DarkInt.Infinite _ -> compareViaBigInt a b

  // Arithmetic: int64 fast path on Finite/Finite, promoting to bigint only on
  // overflow; bigint otherwise. Results normalize through `ofBigInt`.
  let add (a : DarkInt) (b : DarkInt) : DarkInt =
    match a with
    | DarkInt.Finite x ->
      match b with
      | DarkInt.Finite y ->
        try
          DarkInt.Finite(Checked.(+) x y)
        with :? System.OverflowException ->
          ofBigInt (bigint x + bigint y)
      | DarkInt.Infinite _ -> ofBigInt (toBigInt a + toBigInt b)
    | DarkInt.Infinite _ -> ofBigInt (toBigInt a + toBigInt b)

  let subtract (a : DarkInt) (b : DarkInt) : DarkInt =
    match a with
    | DarkInt.Finite x ->
      match b with
      | DarkInt.Finite y ->
        try
          DarkInt.Finite(Checked.(-) x y)
        with :? System.OverflowException ->
          ofBigInt (bigint x - bigint y)
      | DarkInt.Infinite _ -> ofBigInt (toBigInt a - toBigInt b)
    | DarkInt.Infinite _ -> ofBigInt (toBigInt a - toBigInt b)

  let multiply (a : DarkInt) (b : DarkInt) : DarkInt =
    match a with
    | DarkInt.Finite x ->
      match b with
      | DarkInt.Finite y ->
        try
          DarkInt.Finite(Checked.(*) x y)
        with :? System.OverflowException ->
          ofBigInt (bigint x * bigint y)
      | DarkInt.Infinite _ -> ofBigInt (toBigInt a * toBigInt b)
    | DarkInt.Infinite _ -> ofBigInt (toBigInt a * toBigInt b)

  /// Integer division; caller must ensure the divisor is non-zero.
  let divide (a : DarkInt) (b : DarkInt) : DarkInt =
    match a with
    // Int64 division overflows only on MinValue / -1; promote that case.
    | DarkInt.Finite x ->
      match b with
      | DarkInt.Finite y ->
        try
          DarkInt.Finite(x / y)
        with :? System.OverflowException ->
          ofBigInt (bigint x / bigint y)
      | DarkInt.Infinite _ -> ofBigInt (toBigInt a / toBigInt b)
    | DarkInt.Infinite _ -> ofBigInt (toBigInt a / toBigInt b)

  let negate (a : DarkInt) : DarkInt =
    match a with
    | DarkInt.Finite x ->
      if x = System.Int64.MinValue then
        ofBigInt (-(bigint x))
      else
        DarkInt.Finite(-x)
    | DarkInt.Infinite b -> ofBigInt (-b)


type TypeReference =
  | TUnit
  | TBool
  | TInt8
  | TUInt8
  | TInt16
  | TUInt16
  | TInt32
  | TUInt32
  | TInt64
  | TUInt64
  | TInt128
  | TUInt128
  // arbitrary-precision integer; the default `Int`
  | TInt
  | TFloat
  | TChar
  | TString
  | TUuid
  | TDateTime
  | TBlob
  | TStream of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TList of TypeReference
  | TDict of TypeReference // CLEANUP add key type
  | TFn of NEList<TypeReference> * TypeReference
  | TCustomType of
    NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference>
  | TVariable of string
  | TDB of TypeReference


  member this.isFn() : bool =
    match this with
    | TFn _ -> true
    | _ -> false

  member this.isConcrete() : bool =
    let rec isConcrete (t : TypeReference) : bool =
      match t with
      | TUnit
      | TBool
      | TInt8
      | TUInt8
      | TInt16
      | TUInt16
      | TInt32
      | TUInt32
      | TInt64
      | TUInt64
      | TInt128
      | TUInt128
      | TInt
      | TFloat
      | TChar
      | TString
      | TUuid
      | TDateTime
      | TBlob -> true

      | TStream t -> isConcrete t

      | TTuple(t1, t2, ts) ->
        isConcrete t1 && isConcrete t2 && List.forall isConcrete ts
      | TList t -> isConcrete t
      | TDict t -> isConcrete t

      | TCustomType(_, ts) -> List.forall isConcrete ts

      | TFn(ts, t) -> NEList.forall isConcrete ts && isConcrete t

      | TDB t -> isConcrete t

      | TVariable _ -> false

    isConcrete this


/// Our record/tracking of any type arguments in scope
///
/// i.e. within the execution of
///   `let serialize<'a> (x : 'a) : string = ...`,
/// called with inputs
///   `serialize<int> 1`,
/// we would have a TypeSymbolTable of
///  { "a" => TInt64 }
/// At most two entries in practice. The interpreter's own instrumentation reports a maximum of 2 and
/// a mean of 1.23 over a real workload, and as an `FSharpMap` this was 47% of everything the
/// interpreter allocated, because a one-entry map is three heap objects.
///
/// So the first two entries live in the struct and allocate nothing; above two it falls back to a
/// Map, which in this codebase has never been observed to happen. Copying is 48 bytes of stack on a
/// path that was doing three heap allocations.
///
/// Equality is by content, not field-by-field: two tables with the same pairs must compare equal
/// whichever way they were built, since a `Dval` can carry one (inside `DApplicable`) and Dvals are
/// compared.
[<Struct; NoComparison; CustomEquality>]
type TypeSymbolTable =
  {
    Count : int
    K0 : string
    V0 : ValueType
    K1 : string
    V1 : ValueType
    /// Only meaningful when `Count` is above 2. `Map.empty` otherwise, which is a singleton.
    Rest : Map<string, ValueType>
  }

  member this.TryFind(name : string) : ValueType voption =
    if this.Count > 0 && this.K0 = name then
      ValueSome this.V0
    elif this.Count > 1 && this.K1 = name then
      ValueSome this.V1
    elif this.Count > 2 then
      let mutable found = Unchecked.defaultof<ValueType>
      if this.Rest.TryGetValue(name, &found) then ValueSome found else ValueNone
    else
      ValueNone

  member this.ToList() : List<string * ValueType> =
    match this.Count with
    | 0 -> []
    | 1 -> [ (this.K0, this.V0) ]
    | 2 -> [ (this.K0, this.V0); (this.K1, this.V1) ]
    | _ -> (this.K0, this.V0) :: (this.K1, this.V1) :: Map.toList this.Rest

  override this.Equals(o : obj) : bool =
    match o with
    | :? TypeSymbolTable as other ->
      if this.Count <> other.Count then
        false
      else
        this.ToList()
        |> List.forall (fun (k, v) ->
          match other.TryFind k with
          | ValueSome v' -> v = v'
          | ValueNone -> false)
    | _ -> false

  override this.GetHashCode() : int =
    // Order-independent, so two tables with the same pairs hash alike however they were built.
    this.ToList() |> List.fold (fun acc (k, v) -> acc ^^^ (hash k * 31 + hash v)) 0


/// Operations on `TypeSymbolTable`. Every one of these is allocation-free at two entries or fewer,
/// which the instrumentation says is always.
module TST =
  let empty : TypeSymbolTable =
    { Count = 0
      K0 = Unchecked.defaultof<string>
      V0 = Unchecked.defaultof<ValueType>
      K1 = Unchecked.defaultof<string>
      V1 = Unchecked.defaultof<ValueType>
      Rest = Map.empty }

  let inline isEmpty (t : TypeSymbolTable) : bool = t.Count = 0
  let inline count (t : TypeSymbolTable) : int = t.Count
  let inline tryFind (name : string) (t : TypeSymbolTable) : ValueType voption =
    t.TryFind name
  let inline containsKey (name : string) (t : TypeSymbolTable) : bool =
    (t.TryFind name).IsSome
  let inline toList (t : TypeSymbolTable) : List<string * ValueType> = t.ToList()

  let add (name : string) (vt : ValueType) (t : TypeSymbolTable) : TypeSymbolTable =
    if t.Count > 0 && t.K0 = name then
      { t with V0 = vt }
    elif t.Count > 1 && t.K1 = name then
      { t with V1 = vt }
    elif t.Count > 2 && Map.containsKey name t.Rest then
      { t with Rest = Map.add name vt t.Rest }
    elif t.Count = 0 then
      { t with Count = 1; K0 = name; V0 = vt }
    elif t.Count = 1 then
      { t with Count = 2; K1 = name; V1 = vt }
    else
      { t with Count = t.Count + 1; Rest = Map.add name vt t.Rest }

  let remove (name : string) (t : TypeSymbolTable) : TypeSymbolTable =
    if t.Count > 0 && t.K0 = name then
      // Slide the second entry down so the invariant "slots fill from 0" holds.
      if t.Count = 1 then
        empty
      elif t.Count = 2 then
        { t with Count = 1; K0 = t.K1; V0 = t.V1 }
      else
        match Map.toList t.Rest with
        | (k, v) :: _ ->
          { t with
              K0 = t.K1
              V0 = t.V1
              K1 = k
              V1 = v
              Rest = Map.remove k t.Rest
              Count = t.Count - 1 }
        | [] -> { t with Count = 1; K0 = t.K1; V0 = t.V1 }
    elif t.Count > 1 && t.K1 = name then
      if t.Count = 2 then
        { t with Count = 1; K1 = Unchecked.defaultof<string> }
      else
        match Map.toList t.Rest with
        | (k, v) :: _ ->
          { t with K1 = k; V1 = v; Rest = Map.remove k t.Rest; Count = t.Count - 1 }
        | [] -> { t with Count = 1; K1 = Unchecked.defaultof<string> }
    elif t.Count > 2 && Map.containsKey name t.Rest then
      { t with Rest = Map.remove name t.Rest; Count = t.Count - 1 }
    else
      t

  /// `Map.remove` rebuilds the search path even when the key isn't there, and the names stripped from
  /// a table are a function's own type variables, which the caller usually hasn't bound. Checking
  /// first allocates nothing.
  let removeIfPresent (name : string) (t : TypeSymbolTable) : TypeSymbolTable =
    if containsKey name t then remove name t else t

  let ofList (pairs : List<string * ValueType>) : TypeSymbolTable =
    pairs |> List.fold (fun acc (k, v) -> add k v acc) empty

  let fold
    (f : 'st -> string -> ValueType -> 'st)
    (state : 'st)
    (t : TypeSymbolTable)
    : 'st =
    let mutable acc = state
    if t.Count > 0 then acc <- f acc t.K0 t.V0
    if t.Count > 1 then acc <- f acc t.K1 t.V1
    if t.Count > 2 then acc <- Map.fold f acc t.Rest
    acc

  let map (f : ValueType -> 'a) (t : TypeSymbolTable) : Map<string, 'a> =
    t.ToList() |> List.map (fun (k, v) -> (k, f v)) |> Map.ofList

  /// Right wins on conflict, and an entry that's already there with an equal value is left alone, so
  /// a merge that changes nothing returns the left table untouched.
  let mergeFavoringRight
    (l : TypeSymbolTable)
    (r : TypeSymbolTable)
    : TypeSymbolTable =
    if r.Count = 0 then
      l
    elif l.Count = 0 then
      r
    else
      fold
        (fun acc k v ->
          match acc.TryFind k with
          | ValueSome existing when existing = v -> acc
          | _ -> add k v acc)
        l
        r



// ------------
// Instructions ("bytecode")
// ------------
[<Measure>]
type register

type Register = int //<register> // TODO: unit of measure

/// The LHS pattern in
/// - a `let` binding (in `let x = 1`, the `x`)
/// - a lambda (in `fn (x, y) -> x + y`, the `(x, y)`
type LetPattern =
  /// `let x = 1`
  | LPVariable of extractTo : Register

  /// `let _ = 1`
  | LPWildcard

  /// `let (x, y) = (1, 2)`
  | LPTuple of first : LetPattern * second : LetPattern * theRest : List<LetPattern>

  /// `let () = ()`
  | LPUnit


type MatchPattern =
  | MPUnit
  | MPBool of bool
  | MPInt8 of int8
  | MPUInt8 of uint8
  | MPInt16 of int16
  | MPUInt16 of uint16
  | MPInt32 of int32
  | MPUInt32 of uint32
  | MPInt64 of int64
  | MPUInt64 of uint64
  | MPInt128 of System.Int128
  | MPUInt128 of System.UInt128
  | MPInt of bigint
  | MPFloat of float
  | MPChar of string
  | MPString of string
  | MPList of List<MatchPattern>
  | MPListCons of head : MatchPattern * tail : MatchPattern
  | MPTuple of
    first : MatchPattern *
    second : MatchPattern *
    theRest : List<MatchPattern>
  | MPEnum of caseName : string * fields : List<MatchPattern>
  | MPVariable of Register
  | MPOr of NEList<MatchPattern>


type StringSegment =
  | Text of string
  | Interpolated of Register


/// In-process blob whose bytes live directly on the Dval.
///
/// This deliberately avoids any current-scope or external ephemeral-blob
/// store lookup: if the Dval is reachable, the bytes are reachable; when
/// the Dval is collected, the bytes can be collected too.
///
/// `id` is the blob's runtime identity. Dark equality and ordering for
/// ephemeral blobs use this id (`NoModule.equals`, `List.compareDval`),
/// not byte equality. `Blob.newEphemeral` mints a fresh id and takes
/// ownership of the byte array, which callers must treat as immutable.
///
/// Custom `Equals`/`GetHashCode` key on `id`, so F# `=` (reachable on any
/// Dval containing a blob) agrees with Dark equality and hashing never
/// scans the byte array. `NoComparison` is required alongside
/// `CustomEquality`; it costs nothing here, since F# structural comparison
/// of blobs is never used — Dark ordering is hand-coded in `List.compareDval`
/// (by `id`) and Dval itself is already `NoComparison`.
///
/// CLEANUP: `bytes` can still be mutated if a caller keeps or receives the
/// array, and a hand-built record can reuse an `id`. Both are by
/// convention — not reachable from Dark, only from future internal code.
[<CustomEquality; NoComparison>]
type EphemeralBlob =
  { id : uuid
    bytes : byte[] }

  override this.Equals(o : obj) : bool =
    match o with
    | :? EphemeralBlob as other -> this.id = other.id
    | _ -> false
  override this.GetHashCode() : int = this.id.GetHashCode()

/// Where the bytes of a DBlob live. Ephemeral refs carry their bytes
/// inline ([EphemeralBlob]); persistent refs hold a content hash and
/// resolve via the package manager's `blobs` lookup against
/// `package_blobs`.
///
/// TODO BEAM-style sub-blob sharing: add
///   `| Subblob of parent: BlobRef * offset: int64 * length: int64`
/// so `Bytes.slice` returns a view rather than copying. `readBlobBytes`
/// would walk to the root; promotion would promote just the slice
/// (default) or the parent. Skip until a profile shows slice-copy is hot.
type BlobRef =
  | Ephemeral of EphemeralBlob
  | Persistent of hash : string * length : int64


type Instruction =
  // == Simple register operations ==
  /// Push a value into a register
  | LoadVal of loadTo : Register * Dval

  | CopyVal of copyTo : Register * copyFrom : Register

  // TODO: update both of these to take a _single_ arg,
  // and replace the 'rhs' component with a 'jumpIfFalse' component
  // hmm or maybe jumpIfTrue.
  // the point here is to allow for short-circuiting, allowing the RHS instructions to be skipped
  // if the first argument resolves the condition.
  // So I guess Or needs jumpIfTrue, and And needs jumpIfFalse.
  // and the jumpIfFalse/jumpIfTrue might have a 0-instr skip for the RHS.
  | Or of createTo : Register * lhs : Register * rhs : Register
  | And of createTo : Register * lhs : Register * rhs : Register

  // == Working with Basic Types ==
  | CreateString of createTo : Register * segments : List<StringSegment>

  // == Working with Variables ==
  /// Extract values in a Register to 0 or more registers, per the pattern.
  /// (e.g. `let (x, y) = (1, 2)`)
  ///
  /// Errors if the pattern doesn't match the value.
  | CheckLetPatternAndExtractVars of valueReg : Register * pat : LetPattern


  // == Flow Control ==

  // -- Jumps --
  /// Go `n` instructions forward, if the value in the register is `false`
  | JumpByIfFalse of instrsToJump : int * conditionReg : Register

  /// Go `n` instructions forward, unconditionally
  | JumpBy of instrsToJump : int


  // -- Match --
  /// Check if the value in the noted register the noted pattern,
  /// and extract values to registers per the nested patterns.
  | CheckMatchPatternAndExtractVars of
    /// what we're matching against
    valueReg : Register *
    pat : MatchPattern *
    /// jump over the current `match` expr's instructions if it doesn't match
    /// (to the next case, or to the "unmatched" instruction)
    failJump : int

  /// Could not find matching case in a match expression
  /// CLEANUP we probably need a way to reference back to PT so we can get useful RTEs
  /// TODO probably better as a usage of a broader "Fail" error case.
  | MatchUnmatched of valueReg : Register


  // == Working with Collections ==
  | CreateTuple of
    createTo : Register *
    first : Register *
    second : Register *
    theRest : List<Register>

  /// Create a list, and type-check to ensure the items are of a consistent type
  | CreateList of createTo : Register * itemsToAdd : List<Register>

  /// Create a dict, and type-check to ensure the entries are of a consistent type
  | CreateDict of createTo : Register * entries : List<string * Register>


  // == Working with Custom Data ==
  // -- Records --
  | CreateRecord of
    createTo : Register *
    typeName : FQTypeName.FQTypeName *
    typeArgs : List<TypeReference> *
    fields : List<string * Register>

  | CloneRecordWithUpdates of
    createTo : Register *
    originalRecordReg : Register *
    updates : List<string * Register>

  | GetRecordField of
    targetReg : Register *
    recordReg : Register *
    fieldName : string

  // -- Enums --
  | CreateEnum of
    createTo : Register *
    typeName : FQTypeName.FQTypeName *
    typeArgs : List<TypeReference> *
    caseName : string *
    fields : List<Register>


  | LoadValue of createTo : Register * FQValueName.FQValueName

  // == Working with things that Apply ==

  | CreateLambda of createTo : Register * lambda : LambdaImpl

  /// Apply some args (and maybe type args) to something
  /// (a named function, or lambda, etc)
  | Apply of
    createTo : Register *
    thingToApply : Register *
    typeArgs : List<TypeReference> *
    args : NEList<Register>

  // == Errors ==
  | RaiseNRE of List<string> * NameResolutionError

  | VarNotFound of targetRegIfDB : Register * name : string

  | CheckIfFirstExprIsUnit of Register

and Instructions =
  {
    /// How many registers are used in evaluating these instructions
    registerCount : int

    /// The instructions themselves
    instructions : List<Instruction>

    /// The register that will hold the result of the instructions
    resultIn : Register
  }


and DvalMap = Map<string, Dval>


/// Lambdas are a bit special:
/// they have to close over variables, and have their own set of instructions, not embedded in the main set
///
/// Note to self: trying to remove typeSymbolTable here
/// causes all sorts of scoping issues. Beware.
and LambdaImpl =
  {
    // -- Things we know as soon as we create the lambda --
    // maybe we need the TL ID as well?
    // CLEANUP maybe incldue word 'source' or something in this field name?
    exprId : id

    /// How should the arguments be deconstructed?
    patterns : NEList<LetPattern>

    /// When the lambda is defined,
    /// we need to "close over" any symbols 'above' that are referenced.
    ///
    /// e.g. in
    /// ```fsharp
    /// let a = 1
    /// let incr = fn x -> x + a
    /// incr 2
    /// ```
    /// , the lambda `fn x -> x + a` closes over `a`,
    /// which we record as `[(1, 2)]`
    /// (copy from register '1' above into register '2' in this CF)
    ///
    /// PT2RT has the duty of creating and passing in (PT2RT-only)
    /// symtable for the evaluation of the expr on the RHS
    registersToCloseOver : List<Register * Register>

    /// For a self-recursive nested function, the register in this frame that holds
    /// the function value itself, so the body can call itself by name. Unlike
    /// closed-over registers (captured by value when the lambda is created), this is
    /// filled at call time with the lambda value - which doesn't exist yet at
    /// creation. `None` for ordinary, non-recursive lambdas.
    selfRegister : Option<Register>

    instructions : Instructions
  }


and ApplicableNamedFn =
  { name : FQFnName.FQFnName

    typeSymbolTable : TypeSymbolTable

    // CLEANUP maybe this could be List<ValueType>?
    typeArgs : List<TypeReference>

    argsSoFar : List<Dval> }

and ApplicableLambda =
  {
    /// The lambda's ID, corresponding to the PT.Expr
    /// (the actual implementation is stored in the VMState)
    ///
    /// CLEANUP including the TLID of the expr here would be useful
    /// (exprId alone isn't enough to perform equality checks, etc.)
    exprId : id

    /// We _could_ have this be List<Register * Register>
    /// , but we run some risk of the register's value changing
    /// between the time we create the lambda and the time we apply it.
    /// (even though, at time of writing, this seems impossible.)
    closedRegisters : List<Register * Dval>

    /// A cache/copy of the type symbol table[1] when the lambda was created.
    ///
    /// [1] the `name: String -> Type` lookup of resolved generics
    /// for e.g. `Option<'a>`
    typeSymbolTable : TypeSymbolTable

    argsSoFar : List<Dval>
  }


/// Any thing that can be applied,
/// along with anything needed within their application closure
/// TODO: follow up with typeSymbols
and Applicable =
  | AppLambda of ApplicableLambda
  | AppNamedFn of ApplicableNamedFn




// We use NoComparison here to avoid accidentally using structural comparison
and [<NoComparison>] Dval =
  | DUnit

  // Simple types
  | DBool of bool

  | DInt8 of int8
  | DUInt8 of uint8
  | DInt16 of int16
  | DUInt16 of uint16
  | DInt32 of int32
  | DUInt32 of uint32
  | DInt64 of int64
  | DUInt64 of uint64
  | DInt128 of System.Int128
  | DUInt128 of System.UInt128
  // The default `Int`: arbitrary precision, with the Finite/Infinite split
  // contained in `DarkInt`.
  // CLEANUP: small Ints carry the memory overhead of the bigint case plus a tag.
  // Revisit the representation if profiling shows Int memory or GC pressure.
  | DInt of DarkInt

  | DFloat of double

  | DChar of string // TextElements (extended grapheme clusters) are provided as strings
  | DString of string

  | DDateTime of DarkDateTime.T
  | DUuid of System.Guid

  // Compound types
  | DList of ValueType * List<Dval>
  | DTuple of first : Dval * second : Dval * theRest : List<Dval>
  | DDict of
    // This is the type of the _values_, not the keys. Once users can specify the
    // key type, we likely will need to add a `keyType: ValueType` field here. TODO
    valueType : ValueType *
    entries : DvalMap

  // TODO: go through all instances of DRecord and DEnum
  // and make sure the typeNames are in the correct order

  // -- custom types --
  | DRecord of
    sourceTypeName : FQTypeName.FQTypeName *
    runtimeTypeName : FQTypeName.FQTypeName *
    // CLEANUP
    // Do we need to split this into sourceTypeArgs and runtimeTypeArgs?
    // What are we even using the source stuff for? error-reporting?
    // Could source stuff be erased in PT2RT, if we dealt with alias-resolution there?
    typeArgs : List<ValueType> *
    fields : DvalMap

  | DEnum of
    sourceTypeName : FQTypeName.FQTypeName *
    runtimeTypeName : FQTypeName.FQTypeName *
    typeArgs : List<ValueType> *
    caseName : string *
    fields : List<Dval>

  | DApplicable of Applicable

  // References
  | DDB of name : string

  /// Byte sequence, immutable by convention. Ephemeral blobs carry their
  /// bytes inline (lifetime is GC); `Blob.readBytes` returns the array to
  /// in-engine consumers, which treat it as read-only. Persistent blobs
  /// hold a content hash and resolve via `package_blobs`. See [BlobRef].
  | DBlob of BlobRef

  /// Lazy, single-consumer, non-persistable sequence. The inner
  /// [StreamImpl] is the lazy tree (FromIO plus Mapped / Filtered /
  /// Take / Concat transforms). The `disposed` flag + `lockObj` live
  /// alongside for lifecycle management — `disposed` short-circuits
  /// subsequent pulls once the stream drains, `lockObj` doubles as
  /// the GC finalizer target so abandoned streams still release
  /// their IO source.
  | DStream of StreamImpl * disposed : bool ref * lockObj : obj


/// Lazy sequence producer. [FromIO] is the leaf — a pull-based
/// producer. Mapped / Filtered / Take / Concat wrap other StreamImpls
/// as transformation nodes; each is pull-driven and does no work
/// until the enclosing [DStream] is drained via `readStreamNext` or
/// `readStreamChunk`.
///
/// Mapped/Filtered hold pre-bound `Dval -> Ply<...>` closures rather
/// than the raw [Applicable]. The builtin wrapper (Stream.map etc.)
/// closes over `exeState`/`vmState` when constructing the closure, so
/// the drain path in Dval.fs stays decoupled from Execution — Dval.fs
/// can't call Exe.executeApplicable directly since it sits earlier in
/// the dependency chain.
///
/// TODO Mapped/Filtered closures hold a reference to the originating
/// `ExecutionState`. If a Stream ever outlives its execution (long-lived
/// debug pause, returned-from-handler-and-stashed), the closure pins
/// stale state. Today this can't manifest — DStream isn't persistable
/// (`isPersistable` rejects, binary-serialise raises), so a stream
/// returned from a handler dies with its VM. Fix when it becomes a
/// real path: pass `state` as a parameter to `next`/`nextChunk` rather
/// than capturing it.
///
/// Take tracks both the original `n` (for introspection/printing) and
/// a mutable `remaining` counter that decrements on each pull.
///
/// Concat's `streams` list is a mutable ref — drain pops exhausted
/// heads so subsequent pulls skip over them without re-entering.
/// TODO this is unsafe under shared access — two callers pulling from
/// the same Concat impl would see torn views. Today the single-consumer
/// invariant prevents that, but the type system doesn't enforce it.
/// Fold this into the broader single-consumer enforcement work below.
///
/// Has a custom `Equals` that always returns false — `FromIO`'s pull
/// fn is a closure, so there's no sensible structural-equality story,
/// and we don't want `NoEquality` propagating up into Dval/CallFrame.
/// Callers that need "same stream" semantics must compare by reference
/// via the wrapping DStream's `lockObj`. Most callers shouldn't compare
/// streams at all.
and [<CustomEquality; NoComparison>] StreamImpl =
  /// Lazy pull producer.
  ///
  /// `next` yields the next element or None on exhaustion. `disposer`,
  /// when present, is invoked exactly once when the wrapping DStream
  /// is disposed — either via `streamClose`, when drain-to-end trips
  /// the `disposed` flag, or when the `StreamFinalizer` GCs the
  /// unreachable DStream. Used by IO-backed sources (HttpClient.stream,
  /// file reads) to release the underlying HttpResponseMessage /
  /// FileStream / etc.
  ///
  /// Optional `nextChunk` lets byte-stream producers avoid per-byte
  /// Ply/Dval boxing. `nextChunk maxBytes` fills up to `maxBytes` into
  /// a fresh byte[] and returns it (or None on exhaustion). Consumers
  /// that want bulk bytes (`streamToBlob`, SSE-byte accumulator) take
  /// this path; byte-by-byte `next` stays authoritative for element-
  /// wise pulls (`streamNext` on `Stream<UInt8>`). Non-byte streams
  /// leave this `None`.
  ///
  /// TODO no backpressure: a producer faster than its consumer fills
  /// memory. Today HTTP is network-bounded and in-process producers
  /// (`streamFromList` over a huge list, `streamUnfold` with no
  /// termination) are the only unbounded paths. Becomes load-bearing
  /// if anyone adds a "buffer N elements ahead" or "merge multiple
  /// streams" combinator.
  | FromIO of
    next : (unit -> Ply<Option<Dval>>) *
    elemType : ValueType *
    disposer : (unit -> unit) option *
    nextChunk : (int -> Ply<Option<byte[]>>) option
  | Mapped of src : StreamImpl * fn : (Dval -> Ply<Dval>) * elemType : ValueType
  | Filtered of src : StreamImpl * pred : (Dval -> Ply<bool>)
  | Take of src : StreamImpl * n : int64 * remaining : int64 ref
  | Concat of streams : StreamImpl list ref

  override this.Equals(_other : obj) : bool = false
  override this.GetHashCode() : int = 0


and DvalTask = Ply<Dval>



and ThreadID = uuid

and BuiltInParam =
  { name : string
    typ : TypeReference
    blockArgs : List<string>
    description : string }

  static member make
    (name : string)
    (typ : TypeReference)
    (description : string)
    : BuiltInParam =
    assert_ "make called on TFn" [ "name", name ] (not (typ.isFn ()))
    { name = name; typ = typ; description = description; blockArgs = [] }

  static member makeWithArgs
    (name : string)
    (typ : TypeReference)
    (description : string)
    (blockArgs : List<string>)
    : BuiltInParam =
    assert_ "makeWithArgs not called on TFn" [ "name", name ] (typ.isFn ())
    { name = name; typ = typ; description = description; blockArgs = blockArgs }


module StreamImpl =
  /// The element type emitted by this stream. Walks the tree for
  /// transforms that inherit from their source (Filtered, Take,
  /// Concat) and reports the head element type for Mapped. Concat
  /// over an empty list has no known element type.
  ///
  /// CLEANUP this walks the full transform tree on every call. Hot
  /// for type-checking `streamMap` results and `toValueType` on
  /// long pipelines. Cheap fix: cache at construction (the type is
  /// invariant once the StreamImpl is built — Concat's head doesn't
  /// change once frozen at construction). 🔧
  let rec elemType (impl : StreamImpl) : ValueType =
    match impl with
    | FromIO(_, t, _, _) -> t
    | Mapped(_, _, t) -> t
    | Filtered(src, _) -> elemType src
    | Take(src, _, _) -> elemType src
    | Concat streams ->
      match streams.Value with
      | [] -> ValueType.Unknown
      | first :: _ -> elemType first


module RuntimeError =
  module TypeChecking =
    type TypeCheckPathPart =
      | ListType

      | DictValueType
      // CLEANUP add DictKeyType here, once Dicts support non-string keys

      | TupleLength of expected : int * actual : int
      | TupleAtIndex of int

      | TypeArgLength of
        typeName : FQTypeName.FQTypeName *
        expected : int *
        actual : int
      | TypeArg of
        typeName : FQTypeName.FQTypeName *
        typeArgIndex : int *
        typeArgCount : int


    type ReverseTypeCheckPath = List<TypeCheckPathPart>


  module Bools =
    type Error =
      | AndOnlySupportsBooleans of gotLeft : ValueType * gotRight : ValueType
      | OrOnlySupportsBooleans of gotRight : ValueType * gotLeft : ValueType
      | ConditionRequiresBool of actualValueType : ValueType * actualValue : Dval

  module Ints =
    type Error =
      | DivideByZeroError
      | OutOfRange // CLEANUP consider including the out-of-range value
      | NegativeExponent
      | NegativeModulus
      | ZeroModulus

  module Strings =
    type Error = NonStringInInterpolation of vt : ValueType * dv : Dval


  module Lists =
    type Error =
      | TriedToAddMismatchedData of
        index : int *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

  module Dicts =
    type Error =
      | TriedToAddKeyAfterAlreadyPresent of key : string

      | TriedToAddMismatchedData of
        key : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval


  module Lets =
    // TODO consider some kinda _path_ thing like with JSON errors:
    // type Details =
    //   /// Unit pattern does not match
    //   | UnitPatternDoesNotMatch

    //   /// Tuple pattern does not match
    //   | TuplePatternDoesNotMatch

    //   /// Tuple pattern has wrong number of elements
    //   | TuplePatternWrongLength of expected: Int * actual: Int

    // maybe it'd be better to present:
    // - top-level path we're matching against
    // - the path to failure
    // - (?) ??

    type Error =
      /// Could not decompose `{someFn dval}` with pattern `{someFn pat}` in `let` expression
      | PatternDoesNotMatch of dval : Dval * pat : LetPattern

  module Matches =
    // TODO "When condition should be a boolean" -- this could warn _or_ error -- which do we want?
    // CLEANUP "Match must have at least one case"
    type Error =
      /// Could not find matching case for the given value
      | MatchUnmatched of unmatchedValue : Dval

  module Enums =
    type Error =
      | ConstructionWrongNumberOfFields of
        typeName : FQTypeName.FQTypeName *
        caseName : string *
        expectedFieldCount : int *
        actualFieldCount : int

      | ConstructionCaseNotFound of
        typeName : FQTypeName.FQTypeName *
        caseName : string

      | ConstructionFieldOfWrongType of
        caseName : string *
        fieldIndex : int *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval


  module Records =
    // CLEANUP _maybe_ "Record must have at least one field" (Q: for defs, or instances?)
    // I'm not totally convinced, though - `type WIP = {}` seems useful.
    // Later note -- this^ should be in some separate error tree for _dev-time_ errors

    type Error =
      // -- Creation --
      | CreationTypeNotRecord of name : FQTypeName.FQTypeName
      | CreationEmptyKey // I'm not quite sure how this can be reached(?)
      | CreationMissingField of fieldName : string
      | CreationDuplicateField of fieldName : string
      | CreationFieldNotExpected of fieldName : string
      | CreationFieldOfWrongType of
        fieldName : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      // -- Update --
      | UpdateNotRecord of actualType : ValueType
      | UpdateEmptyKey
      | UpdateDuplicateField of fieldName : string
      | UpdateFieldNotExpected of fieldName : string
      | UpdateFieldOfWrongType of
        fieldName : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      // -- Field Access --
      | FieldAccessEmptyFieldName
      | FieldAccessFieldNotFound of fieldName : string
      | FieldAccessNotRecord of actualType : ValueType


  /// Errors that occur when trying to apply a function or lambda
  module Applications =
    type Error =
      | ExpectedApplicableButNot of actualTyp : ValueType * actualValue : Dval

      // specific to fns
      | WrongNumberOfTypeArgsForFn of
        fn : FQFnName.FQFnName *
        expected : int *
        actual : int

      | CannotApplyTypeArgsMoreThanOnce

      | TooManyArgsForFn of fn : FQFnName.FQFnName * expected : int * actual : int

      | FnParameterNotExpectedType of
        fnName : FQFnName.FQFnName *
        paramIndex : int *
        paramName : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      | FnResultNotExpectedType of
        fnName : FQFnName.FQFnName *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      // specific to lambdas
      | CannotApplyTypeArgsToLambda
      | TooManyArgsForLambda of lambdaExprId : id * expected : int * actual : int

  module Statements =
    type Error =
      | FirstExpressionMustBeUnit of
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval


  module Unwraps =
    type Error =
      | GotNone
      | GotError of err : Dval
      | NonOptionOrResult of actual : Dval
      | MultipleArgs of args : List<Dval>

  module Jsons =
    type Error =
      | UnsupportedType of TypeReference
      | CannotSerializeValue of Dval

  module CLIs =
    type Error =
      | NoExpressionsToExecute
      | NonIntReturned of actuallyReturned : Dval



  /// RuntimeError is the major way of representing errors that occur at runtime.
  /// Most are focused on user errors, such as trying to put an Int in a list of Bools.
  /// Some cases represent internal failures, not at the fault of a user.
  ///
  /// These are not to be confused with Results, which should be used
  /// in functions to represent _expected_ cases of failure.
  ///
  /// See `docs/errors.md` for more discussion.
  type Error =
    | Bool of Bools.Error
    | Int of Ints.Error
    | String of Strings.Error

    | List of Lists.Error
    | Dict of Dicts.Error

    | Let of Lets.Error
    | VariableNotFound of attemptedVarName : string

    | EqualityCheckOnIncompatibleTypes of left : ValueType * right : ValueType
    | NumericOperationOnIncompatibleTypes of left : ValueType * right : ValueType

    | IfConditionNotBool of actualValue : Dval * actualValueType : ValueType

    | Match of Matches.Error

    | ParseTimeNameResolution of originalName : List<string> * NameResolutionError

    | TypeNotFound of name : FQTypeName.FQTypeName
    | FnNotFound of name : FQFnName.FQFnName
    | ValueNotFound of name : FQValueName.FQValueName

    /// Raised when calling a package fn whose hash is currently marked
    /// `Harmful` by a `Deprecate` op. Overridable via `ExecutionState.allowHarmful`
    /// (e.g. `run --allow-harmful`, `eval --allow-harmful`).
    | DeprecatedItemHalted of target : FQFnName.Package

    | WrongNumberOfTypeArgsForType of
      fn : FQTypeName.FQTypeName *
      expected : int *
      actual : int

    | Enum of Enums.Error
    | Record of Records.Error

    | Apply of Applications.Error

    | Unwrap of Unwraps.Error

    | Json of Jsons.Error


    // stuff that isn't _quite _ "core", and maybe should belong elsewhere
    // , once RTEs are (somehow) more extensible

    | CLI of CLIs.Error

    | DBSetOfWrongType of expected : TypeReference * actual : ValueType

    | Statement of Statements.Error

    /// SQL compiler errors when compiling lambdas to SQL queries
    | SqlCompiler of errMsg : string

    // punting these until DBs are supported again
    // - bring back this RTE where/when relevant "Attempting to access field '{fieldName}' of a Datastore (use `DB.*` standard library functions to interact with Datastores. Field access only work with records)"
    // - in backend/src/LibCloud/SqlCompiler.fs:
    //   - 1223: | SqlCompilerException errStr -> return Error(RuntimeError.oldError errStr)
    //   - 1224: // return Error(RuntimeError.oldError (errStr + $"\n\nIn body: {body}"))
    //   - | SqlCompiler of SqlCompiler.Error // -- or maybe this should happen during PT2RT? hmm.


    /// Sometimes, very-unexpected things happen. This is a catch-all for those.
    ///
    /// For local/private runtimes+hosting, allow users to see the details,
    /// but (TODO) for _our_ hosting, users shouldn't see the whole call stack or
    /// whatever, for (our) safety. Perhaps we can provide an opaque ID refer to
    /// the error in a support ticket.
    | UncaughtException of msg : string * metadata : List<string * Dval>


// CLEANUP the ThreadID isn't useful yet -- consider abandoning for now.
exception RuntimeErrorException of Option<ThreadID> * rte : RuntimeError.Error


let raiseRTE (threadId : ThreadID) (rte : RuntimeError.Error) : 'a =
  raise (RuntimeErrorException(Some threadId, rte))

let raiseUntargetedRTE (rte : RuntimeError.Error) : 'a =
  raise (RuntimeErrorException(None, rte))



type ExecutionPoint =
  /// User is executing some "arbitrary" expression, passed in by a user.
  /// This should only be at the `entrypoint` of a CallStack.
  ///
  /// Executing some top-level handler,
  /// such as a saved Script, an HTTP handler, or a Cron.
  | Source

  // Executing some function
  | Function of FQFnName.FQFnName

  /// Executing some lambda
  | Lambda of parent : ExecutionPoint * lambdaExprId : id


/// Not: in reverse order
type CallStack = List<ExecutionPoint>

module CallStack =
  let entrypoint (cs : CallStack) : Option<ExecutionPoint> = List.last cs

  let last (cs : CallStack) : Option<ExecutionPoint> = List.head cs


/// Internally in the runtime, we allow throwing RuntimeErrorExceptions. At the
/// boundary, typically in Execution.fs, we will catch the exception, and return
/// this type.
/// CLEANUP return a call stack or vmstate, or something, here
type ExecutionResult = Result<Dval, RuntimeError.Error * CallStack>

/// IncorrectArgs should never happen, as all functions are type-checked before
/// calling. If it does happen, it means that the type parameters in the Fn structure
/// do not match the args expected in the Builtin function definition.
/// CLEANUP should this take more args, so we can find the error? Maybe just the fn name?
let incorrectArgs () = Exception.raiseInternal "IncorrectArgs" []




// Used to mark whether a function/type has been deprecated, and if so,
// details about possible replacements/alternatives, and reasoning
type Deprecation<'name> =
  | NotDeprecated

  // The exact same thing is available under a new, preferred name
  | RenamedTo of 'name

  /// This has been deprecated and has a replacement we can suggest
  | ReplacedBy of 'name

  /// This has been deprecated and not replaced, provide a message for the user
  | DeprecatedBecause of reason : string


module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference }

  type EnumCase = { name : string; fields : List<TypeReference> }

  type Definition =
    | Alias of TypeReference
    | Record of NEList<RecordField>
    | Enum of NEList<EnumCase>

  type T = { typeParams : List<string>; definition : Definition }



/// One shared `ValueType.Known` wrapper per scalar type.
///
/// The `KT*` cases are nullary, so F# already shares those; it was the `Known` around them that got
/// rebuilt. Two places convert to a ValueType on every call -- `Dval.toValueType` from a value, and
/// `TypeReference.toVT` from a declaration -- and between them they run several times per argument
/// per function call. They now hand back the same objects, which also means the identity check at
/// the top of `ValueType.merge` fires between a value's type and its declared type.
module KnownVT =
  let unit : ValueType = ValueType.Known KTUnit
  let bool : ValueType = ValueType.Known KTBool
  let int8 : ValueType = ValueType.Known KTInt8
  let uint8 : ValueType = ValueType.Known KTUInt8
  let int16 : ValueType = ValueType.Known KTInt16
  let uint16 : ValueType = ValueType.Known KTUInt16
  let int32 : ValueType = ValueType.Known KTInt32
  let uint32 : ValueType = ValueType.Known KTUInt32
  let int64 : ValueType = ValueType.Known KTInt64
  let uint64 : ValueType = ValueType.Known KTUInt64
  let int128 : ValueType = ValueType.Known KTInt128
  let uint128 : ValueType = ValueType.Known KTUInt128
  let int : ValueType = ValueType.Known KTInt
  let float : ValueType = ValueType.Known KTFloat
  let char : ValueType = ValueType.Known KTChar
  let string : ValueType = ValueType.Known KTString
  let dateTime : ValueType = ValueType.Known KTDateTime
  let uuid : ValueType = ValueType.Known KTUuid
  let blob : ValueType = ValueType.Known KTBlob


// Functions for working with Dark runtime values
module Dval =
  // Interned scalars. A Dval is immutable and compared structurally, so nothing can tell a shared
  // instance from a fresh one, and the values a program actually computes cluster hard at the small
  // end: loop counters, indices, lengths, flags.
  //
  // `DInt` is the expensive one. `DarkInt` is a struct DU whose `Infinite` case carries a `bigint`,
  // so a `DInt` object is 56 bytes whether the number needs them or not -- adding two small integers
  // was the second-largest builtin in the profile at 1.1 MB. `DBool` is only 24 bytes but there are
  // just two of them in the universe, so caching them is free.
  //
  // The tables cost about 90 KB at startup, once.
  let dTrue : Dval = DBool true
  let dFalse : Dval = DBool false
  let inline bool (b : bool) : Dval = if b then dTrue else dFalse

  [<Literal>]
  let private smallLo = -128L

  [<Literal>]
  let private smallHi = 1023L

  let private smallInts : Dval[] =
    Array.init (int (smallHi - smallLo) + 1) (fun i ->
      DInt(DarkInt.Finite(int64 i + smallLo)))

  let private smallInt64s : Dval[] =
    Array.init (int (smallHi - smallLo) + 1) (fun i -> DInt64(int64 i + smallLo))

  /// `DInt`, sharing one instance for the small values.
  let dint (di : DarkInt) : Dval =
    match di with
    | DarkInt.Finite i when i >= smallLo && i <= smallHi ->
      smallInts[int (i - smallLo)]
    | _ -> DInt di

  /// `DInt64`, sharing one instance for the small values.
  let dint64 (i : int64) : Dval =
    if i >= smallLo && i <= smallHi then
      smallInt64s[int (i - smallLo)]
    else
      DInt64 i

  /// Constructs an `Int` Dval from a bigint, normalizing through `DarkInt`.
  let int (b : bigint) : Dval = dint (DarkInt.ofBigInt b)

  /// The numeric value of an `Int` Dval as a bigint.
  let asBigInt (dv : Dval) : bigint =
    match dv with
    | DInt i -> DarkInt.toBigInt i
    | _ -> Exception.raiseInternal "asBigInt called on a non-Int Dval" []


  // `ValueType.Known KTBool` and friends are constants, but the `Known` wrapper was
  // allocated fresh on every call. `toValueType` runs at least twice per argument per
  // function call -- once for inference, once for the parameter type check -- so these were
  // among the most frequently allocated objects in the interpreter. Nullary DU cases like
  // `KTBool` are already singletons; only the wrapper was being rebuilt.
  let private vtUnit = KnownVT.unit
  let private vtBool = KnownVT.bool
  let private vtInt8 = KnownVT.int8
  let private vtUInt8 = KnownVT.uint8
  let private vtInt16 = KnownVT.int16
  let private vtUInt16 = KnownVT.uint16
  let private vtInt32 = KnownVT.int32
  let private vtUInt32 = KnownVT.uint32
  let private vtInt64 = KnownVT.int64
  let private vtUInt64 = KnownVT.uint64
  let private vtInt128 = KnownVT.int128
  let private vtUInt128 = KnownVT.uint128
  let private vtInt = KnownVT.int
  let private vtFloat = KnownVT.float
  let private vtChar = KnownVT.char
  let private vtString = KnownVT.string
  let private vtDateTime = KnownVT.dateTime
  let private vtUuid = KnownVT.uuid

  // The scalar cases above hand back a shared wrapper. The container cases could not, because their
  // ValueType depends on the element type -- so `Known(KTList t)` was two objects, built fresh, on
  // every call. `toValueType` runs at least three times per package call (argument inference, the
  // parameter check, the return check), and those three stages together were 1.79 MB on the reference
  // workload; with the container cases stubbed out to a constant they measured 0.03 MB. So this is
  // essentially all of it.
  //
  // Memoized on the element type's *identity*, not its structure. That's what makes the lookup cheap,
  // and it's enough: a list carries one `ValueType` object for its whole life, and the scalar element
  // types are the shared wrappers above, so the same instance comes back round every time.
  //
  // `ConditionalWeakTable` rather than a dictionary so a ValueType built from user data doesn't pin
  // itself for the life of the process, and because it's thread-safe -- tests run VMs in parallel.
  let private listVTs =
    System.Runtime.CompilerServices.ConditionalWeakTable<ValueType, ValueType>()

  let private dictVTs =
    System.Runtime.CompilerServices.ConditionalWeakTable<ValueType, ValueType>()

  let private customVTs =
    System.Runtime.CompilerServices.ConditionalWeakTable<FQTypeName.FQTypeName, ValueType>()

  let rec toValueType (dv : Dval) : ValueType =
    match dv with
    | DUnit -> vtUnit

    | DBool _ -> vtBool

    | DInt8 _ -> vtInt8
    | DUInt8 _ -> vtUInt8
    | DInt16 _ -> vtInt16
    | DUInt16 _ -> vtUInt16
    | DInt32 _ -> vtInt32
    | DUInt32 _ -> vtUInt32
    | DInt64 _ -> vtInt64
    | DUInt64 _ -> vtUInt64
    | DInt128 _ -> vtInt128
    | DUInt128 _ -> vtUInt128
    | DInt _ -> vtInt
    | DFloat _ -> vtFloat
    | DChar _ -> vtChar
    | DString _ -> vtString
    | DDateTime _ -> vtDateTime
    | DUuid _ -> vtUuid

    | DList(t, _) ->
      let mutable hit = Unchecked.defaultof<ValueType>
      if listVTs.TryGetValue(t, &hit) then
        hit
      else
        let vt = ValueType.Known(KTList t)
        listVTs.AddOrUpdate(t, vt)
        vt

    | DDict(t, _) ->
      let mutable hit = Unchecked.defaultof<ValueType>
      if dictVTs.TryGetValue(t, &hit) then
        hit
      else
        let vt = ValueType.Known(KTDict t)
        dictVTs.AddOrUpdate(t, vt)
        vt
    | DTuple(first, second, theRest) ->
      ValueType.Known(
        KTTuple(toValueType first, toValueType second, List.map toValueType theRest)
      )

    // Only the un-parameterised case is memoized: with type args the key would have to be the whole
    // (name, args) shape, and structural hashing of that costs more than the two objects it saves.
    | DRecord(_, typeName, typeArgs, _) ->
      if List.isEmpty typeArgs then
        let mutable hit = Unchecked.defaultof<ValueType>
        if customVTs.TryGetValue(typeName, &hit) then
          hit
        else
          let vt = KTCustomType(typeName, []) |> ValueType.Known
          customVTs.AddOrUpdate(typeName, vt)
          vt
      else
        KTCustomType(typeName, typeArgs) |> ValueType.Known

    | DEnum(_, typeName, typeArgs, _, _) ->
      if List.isEmpty typeArgs then
        let mutable hit = Unchecked.defaultof<ValueType>
        if customVTs.TryGetValue(typeName, &hit) then
          hit
        else
          let vt = KTCustomType(typeName, []) |> ValueType.Known
          customVTs.AddOrUpdate(typeName, vt)
          vt
      else
        KTCustomType(typeName, typeArgs) |> ValueType.Known

    | DApplicable applicable ->
      match applicable with
      | AppLambda _lambda ->
        // TODO something
        //   KTFn(
        //     NEList.map (fun _ -> ValueType.Unknown) lambda.parameters,
        //     ValueType.Unknown
        //   )
        //   |> ValueType.Known
        ValueType.Unknown

      // TODO look up type, etc
      // (probably forces us to make this fn async?)
      | AppNamedFn _named -> ValueType.Unknown

    // CLEANUP follow up when DDB carries a typeReference (the name alone doesn't pin the
    // element type, so it stays Unknown — permissive against any declared DB<_>).
    | DDB _ -> ValueType.Unknown

    | DBlob _ -> ValueType.Known KTBlob

    | DStream(impl, _, _) -> ValueType.Known(KTStream(StreamImpl.elemType impl))


  /// Generic Dval-rewriting walker. At each node, asks `f`:
  ///   - `Some dv'` → substitute, do not recurse further into the original
  ///   - `None`     → recurse into containers and rebuild
  ///
  /// Recurses into DList/DTuple/DDict/DRecord/DEnum and into
  /// DApplicable closures (closed registers + partially-applied args).
  /// The closure recursion is required for persistence boundaries —
  /// a lambda capturing an ephemeral blob or a stream must have its
  /// environment rewritten alongside the rest of the value graph.
  ///
  /// Callers that only want to rewrite specific leaf shapes return
  /// `None` everywhere else; the walker handles the structural
  /// recursion and container rebuilds.
  ///
  /// Preserves structural sharing: when `f` returns `None` for every
  /// reachable leaf, every container returns its original reference
  /// (compared with `obj.ReferenceEquals`) rather than reconstructing.
  /// Map containers in particular avoid the O(N log N) `Map.ofList`
  /// rebuild path.
  let rewriteWith (f : Dval -> Ply.Ply<Dval option>) (dv : Dval) : Ply.Ply<Dval> =
    let inline same (a : obj) (b : obj) = obj.ReferenceEquals(a, b)

    let rec go (dv : Dval) : Ply.Ply<Dval> =
      uply {
        let! substituted = f dv
        match substituted with
        | Some dv' -> return dv'
        | None ->
          match dv with
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
          | DDB _
          | DStream _
          | DBlob _ -> return dv

          | DList(vt, items) ->
            let! items' = walkItems items
            return (if same items' items then dv else DList(vt, items'))

          | DTuple(a, b, rest) ->
            let! a' = go a
            let! b' = go b
            let! rest' = walkItems rest
            if same a' a && same b' b && same rest' rest then
              return dv
            else
              return DTuple(a', b', rest')

          | DDict(vt, entries) ->
            let! entries' = walkMap entries
            return (if same entries' entries then dv else DDict(vt, entries'))

          | DRecord(src, rt, typeArgs, fields) ->
            let! fields' = walkMap fields
            return
              (if same fields' fields then
                 dv
               else
                 DRecord(src, rt, typeArgs, fields'))

          | DEnum(src, rt, typeArgs, caseName, fields) ->
            let! fields' = walkItems fields
            return
              (if same fields' fields then
                 dv
               else
                 DEnum(src, rt, typeArgs, caseName, fields'))

          | DApplicable(AppLambda lambda) ->
            let! cr' = walkRegisters lambda.closedRegisters
            let! args' = walkItems lambda.argsSoFar
            if same cr' lambda.closedRegisters && same args' lambda.argsSoFar then
              return dv
            else
              return
                DApplicable(
                  AppLambda { lambda with closedRegisters = cr'; argsSoFar = args' }
                )

          | DApplicable(AppNamedFn namedFn) ->
            let! args' = walkItems namedFn.argsSoFar
            if same args' namedFn.argsSoFar then
              return dv
            else
              return DApplicable(AppNamedFn { namedFn with argsSoFar = args' })
      }

    /// Walk a `List<Dval>` element by element; return the original list
    /// reference if every child walked to itself, else build a new list
    /// with the walked results.
    and walkItems (xs : List<Dval>) : Ply.Ply<List<Dval>> =
      uply {
        match xs with
        | [] -> return xs
        | _ ->
          let arr = List.toArray xs
          let mutable changed = false
          for i in 0 .. arr.Length - 1 do
            let! y = go arr[i]
            if not (same y arr[i]) then
              changed <- true
              arr[i] <- y
          return (if changed then List.ofArray arr else xs)
      }

    /// Walk a `List<Register * Dval>` (lambda closed registers); only
    /// the Dval can change.
    and walkRegisters (rs : List<Register * Dval>) : Ply.Ply<List<Register * Dval>> =
      uply {
        match rs with
        | [] -> return rs
        | _ ->
          let arr = List.toArray rs
          let mutable changed = false
          for i in 0 .. arr.Length - 1 do
            let r, v = arr[i]
            let! v' = go v
            if not (same v' v) then
              changed <- true
              arr[i] <- (r, v')
          return (if changed then List.ofArray arr else rs)
      }

    /// Walk a `Map<string, Dval>`; return the original Map reference
    /// when nothing changes, otherwise reuse the original map and
    /// replace only the changed entries.
    and walkMap (m : Map<string, Dval>) : Ply.Ply<Map<string, Dval>> =
      uply {
        let mutable acc = m
        for KeyValue(k, v) in m do
          let! v' = go v
          if not (same v' v) then acc <- Map.add k v' acc
        return acc
      }

    go dv



// ------------
// Package-Space
// ------------
module PackageType =
  type PackageType = { hash : Hash; declaration : TypeDeclaration.T }

module PackageValue =
  type PackageValue = { hash : Hash; body : Dval }

module PackageFn =
  type Parameter = { name : string; typ : TypeReference }

  type PackageFn =
    { hash : Hash
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference

      // CLEANUP consider renaming - just `instructions` maybe?
      body : Instructions }


/// Functionality written in Dark stored and managed outside of user space
///
/// Note: it may be tempting to think these shouldn't return Options,
/// but if/when Package items may live (for some time) only on local systems,
/// there's a chance some code will be committed, referencing something
/// not yet in the Cloud PM.
/// (though, we'll likely demand deps. in the PM before committing something upstream...)
type PackageManager =
  {
    getType : FQTypeName.Package -> Ply<Option<PackageType.PackageType>>
    getValue : FQValueName.Package -> Ply<Option<PackageValue.PackageValue>>
    getFn : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>>

    /// Content-addressed blob bytes by SHA-256 hash. Returns [None]
    /// for missing hashes.
    getBlob : string -> Ply<Option<byte[]>>

    /// Insert bytes into `package_blobs` keyed by SHA-256 hash. Uses
    /// INSERT OR IGNORE — same hash = same content (content-addressing
    /// invariant), so a second insert is a cheap no-op.
    persistBlob : string -> byte[] -> Ply<unit>

    /// Is this package fn hash marked Harmful on the given branch chain?
    /// Branch-scoped because deprecation state flows through branches; the
    /// other PM lookups are content-addressed and need no branch.
    /// Only fns participate — see DeprecationKind.Harmful for why.
    /// Synchronous: every implementation computes this without I/O (the DB-backed one reads a per-branch
    /// cache), so returning `Ply<bool>` would cost a computation-expression bind on every package call.
    isHarmful : BranchId -> FQFnName.Package -> bool

    init : Ply<unit>
  }

  static member empty =
    { getType = (fun _ -> Ply None)
      getFn = (fun _ -> Ply None)
      getValue = (fun _ -> Ply None)
      getBlob = (fun _ -> Ply None)
      persistBlob = (fun _ _ -> uply { return () })
      isHarmful = (fun _ _ -> false)

      init = uply { return () } }

  /// Allows you to side-load a few 'extras' in-memory, along
  /// the normal fetching functionality. (Mostly helpful for tests)
  static member withExtras
    (types : List<PackageType.PackageType>)
    (values : List<PackageValue.PackageValue>)
    (fns : List<PackageFn.PackageFn>)
    (pm : PackageManager)
    : PackageManager =
    let typeMap = types |> List.map (fun t -> t.hash, t) |> Map.ofList
    let valueMap = values |> List.map (fun v -> v.hash, v) |> Map.ofList
    let fnMap = fns |> List.map (fun f -> f.hash, f) |> Map.ofList

    { getType =
        fun id ->
          match Map.tryFind id typeMap with
          | Some t -> Some t |> Ply
          | None -> pm.getType id
      getValue =
        fun id ->
          match Map.tryFind id valueMap with
          | Some v -> Some v |> Ply
          | None -> pm.getValue id
      getFn =
        fun id ->
          match Map.tryFind id fnMap with
          | Some f -> Some f |> Ply
          | None -> pm.getFn id
      getBlob = pm.getBlob
      persistBlob = pm.persistBlob
      isHarmful = pm.isHarmful
      init = pm.init }


// ------------
// User Space
// ------------
module DB =
  // CLEANUP consider making typ a ValueType instead
  type T = { tlid : tlid; name : string; typ : TypeReference; version : int }

// ------------
// Builtins, Execution State, Package Manager
// A bunch of tangled things we need to `and` together
// ------------

/// <summary>
/// Used to mark whether a function can be run on the client rather than backend.
/// </summary>
/// <remarks>
/// The runtime needs to know whether to save a function's results when it
/// runs. Pure functions that can be run on the client do not need to have
/// their results saved.
/// In addition, some functions can be run without side-effects; to give
/// the user a good experience, we can run them as soon as they are added.
/// this includes DateTime.now and Int.random.
/// </remarks>
type Previewable =
  /// The same inputs will always yield the same outputs,
  /// so we don't need to save results. e.g. `DateTime.addSeconds`
  | Pure

  /// Output may vary with the same inputs, though we can safely preview.
  /// e.g. `DateTime.now`. We should save the results.
  | ImpurePreviewable

  /// Can only be run on the server. e.g. `DB.update`
  /// We should save the results.
  | Impure


/// Used to mark whether a function has an equivalent that can be
/// used within a Sqlite query.
type SqlSpec =
  /// Can be implemented, but we haven't yet
  | NotYetImplemented

  /// This is not a function which can be queried
  | NotQueryable

  /// A query function (it can't be called inside a query, but its argument can be a query)
  | QueryFunction

  /// Can be implemented by a given builtin operator with 1 arg (eg `@ x`)
  | SqlUnaryOp of string

  /// Can be implemented by a given builtin operator with 2 args (eg `x + y`)
  | SqlBinOp of string

  /// Can be implemented by a given builtin function
  | SqlFunction of string

  /// Can be implemented by a given builtin function with extra arguments that go first
  | SqlFunctionWithPrefixArgs of string * List<string>

  /// Can be implemented by a given builtin function with extra arguments that go last
  | SqlFunctionWithSuffixArgs of string * List<string>

  /// Can be implemented by given callback that receives 1 SQLified-string argument
  /// | SqlCallback of (string -> string)
  /// Can be implemented by given callback that receives 2 SQLified-string argument
  | SqlCallback2 of (string -> string -> string)

  member this.isQueryable() : bool =
    match this with
    | NotYetImplemented
    | NotQueryable
    | QueryFunction -> false
    | SqlUnaryOp _
    | SqlBinOp _
    | SqlFunction _
    | SqlFunctionWithPrefixArgs _
    | SqlFunctionWithSuffixArgs _
    | SqlCallback2 _ -> true


module Tracing =
  /// Record the source expression of an error.
  /// This is to show the code that was responsible for it.
  /// TODO maybe rename to ExprLocation
  type Source = ExecutionPoint * Option<id>

  type FunctionRecord = Source * FQFnName.FQFnName

  type LoadFnResult =
    FunctionRecord -> NEList<Dval> -> Option<Dval * NodaTime.Instant>

  type StoreFnResult = FunctionRecord -> NEList<Dval> -> Dval -> unit

  /// Fired when a new call frame is pushed (Function or Lambda).
  /// Carries the frame's uuid, the executionPoint of the new frame, and
  /// the args bound into it. The uuid lets the tracer associate this entry
  /// with the matching exit (storeFnResult for fns, storeLambdaResult for
  /// lambdas).
  type StoreFrameEntry = uuid -> ExecutionPoint -> List<Dval> -> unit

  /// Fired when a Lambda call frame returns. Function frames return via
  /// storeFnResult, which already includes args + result. Lambdas don't
  /// fire storeFnResult, so this is the corresponding exit hook for them.
  type StoreLambdaResult = uuid -> Dval -> unit

  /// Set of callbacks used to trace the interpreter, and other context needed to run code
  type Tracing =
    {
      loadFnResult : LoadFnResult
      storeFnResult : StoreFnResult
      storeFrameEntry : StoreFrameEntry
      storeLambdaResult : StoreLambdaResult
      /// When true, the interpreter skips firing all tracer hooks
      /// (storeFrameEntry, storeFnResult, storeLambdaResult) and the
      /// associated pendingCallArgs bookkeeping.
      skipTracing : bool
    }


// -- The VM --
type Registers = Dval array

type InstrData =
  {
    instructions : Instruction array

    /// The register that the result of the block will be in
    resultReg : Register
  }

type CallFrame =
  {
    mutable id : uuid

    /// (Id * where to put result in parent * pc of parent to return to)
    ///
    /// A struct option of a struct tuple: the root frame aside, one of these is built for every function
    /// call in the program, and the reference-typed form was two allocations each time.
    mutable parent : voption<struct (uuid * Register * int)>

    mutable executionPoint : ExecutionPoint

    /// The instructions this frame runs, resolved once when the frame is pushed.
    ///
    /// A reference to the single shared `InstrData` for the fn or lambda, not a copy: the caches in
    /// `ExecutionState` and `VMState` still hold one each, and every frame running the same function
    /// points at it. Holding it here keeps the interpreter loop from binding a cache lookup with `let!`
    /// on every iteration.
    mutable instrData : InstrData

    /// The declared return type, for the check that runs when this frame returns.
    ///
    /// Resolved at push time for the same reason as `instrData`: otherwise every frame return re-fetches
    /// the function purely to read one field, and binds it, which costs a continuation closure per return.
    /// `ValueNone` for the root frame and for lambdas, neither of which declares one.
    mutable expectedReturnType : TypeReference voption

    /// What instruction index we are currently 'at'
    mutable programCounter : int

    mutable typeSymbolTable : TypeSymbolTable

    mutable registers : Registers
  }

/// Synchronous regions of the Apply path that the allocation counters attribute to.
module ApplyStage =
  let names : string[] =
    [| "pkg.tstShadow"
       "pkg.infer"
       "pkg.typeCheckArgs"
       "pkg.frame"
       "bi.tstShadow"
       "bi.args"
       "lambda.frame"
       "applyArgs"
       "pkg.typeCheckRun"
       "bi.typeCheckRun"
       "bi.checkResult"
       "frame.returnTypeCheck"
       "frame.pop"
       "pkg.fetch"
       "bi.fnLookup"
       "lambda.registers"
       "lambda.tst"
       "lambda.execPoint"
       "pkg.fetchOnly"
       "pkg.frameTst"
       "z.applyTotal"
       "z.lambdaTotal"
       "z.biTotal" |]

  [<Literal>]
  let PkgTstShadow = 0
  [<Literal>]
  let PkgInfer = 1
  [<Literal>]
  let PkgTypeCheckArgs = 2
  [<Literal>]
  let PkgFrame = 3
  [<Literal>]
  let BiTstShadow = 4
  [<Literal>]
  let BiArgs = 5
  [<Literal>]
  let LambdaFrame = 6
  [<Literal>]
  let ApplyArgs = 7
  [<Literal>]
  let PkgTypeCheckRun = 8
  [<Literal>]
  let BiTypeCheckRun = 9
  [<Literal>]
  let BiCheckResult = 10
  [<Literal>]
  let FrameReturnTypeCheck = 11
  [<Literal>]
  let FramePop = 12
  [<Literal>]
  let PkgFetch = 13
  [<Literal>]
  let BiFnLookup = 14
  [<Literal>]
  let LambdaRegisters = 15
  [<Literal>]
  let LambdaTst = 16
  [<Literal>]
  let LambdaExecPoint = 17
  [<Literal>]
  let PkgFetchOnly = 18
  [<Literal>]
  let PkgFrameTst = 19
  // Coarse brackets, used to find allocation the fine-grained stages don't cover. `z.` so they sort
  // last and read as scaffolding rather than as findings.
  [<Literal>]
  let ApplyTotal = 20
  [<Literal>]
  let LambdaTotal = 21
  [<Literal>]
  let BiTotal = 22


/// Stable index and display name per `Instruction` case, used by the per-opcode allocation counters on
/// `InterpreterStats`. The DU's compiler-generated `Tag` isn't accessible from here, so the mapping is
/// written out; `names` and `index` must stay in the same order.
module Opcode =
  let names : string[] =
    [| "LoadVal"
       "CopyVal"
       "Or"
       "And"
       "CreateString"
       "CheckLetPatternAndExtractVars"
       "JumpByIfFalse"
       "JumpBy"
       "CheckMatchPatternAndExtractVars"
       "MatchUnmatched"
       "CreateTuple"
       "CreateList"
       "CreateDict"
       "CreateRecord"
       "CloneRecordWithUpdates"
       "GetRecordField"
       "CreateEnum"
       "LoadValue"
       "CreateLambda"
       "Apply"
       "RaiseNRE"
       "VarNotFound"
       "CheckIfFirstExprIsUnit" |]

  let index (i : Instruction) : int =
    match i with
    | LoadVal _ -> 0
    | CopyVal _ -> 1
    | Or _ -> 2
    | And _ -> 3
    | CreateString _ -> 4
    | CheckLetPatternAndExtractVars _ -> 5
    | JumpByIfFalse _ -> 6
    | JumpBy _ -> 7
    | CheckMatchPatternAndExtractVars _ -> 8
    | MatchUnmatched _ -> 9
    | CreateTuple _ -> 10
    | CreateList _ -> 11
    | CreateDict _ -> 12
    | CreateRecord _ -> 13
    | CloneRecordWithUpdates _ -> 14
    | GetRecordField _ -> 15
    | CreateEnum _ -> 16
    | LoadValue _ -> 17
    | CreateLambda _ -> 18
    | Apply _ -> 19
    | RaiseNRE _ -> 20
    | VarNotFound _ -> 21
    | CheckIfFirstExprIsUnit _ -> 22


/// Every `InterpreterStats` created while telemetry is on, so the process can total them at exit. A VM is
/// created per `executeFunction` and the stats hang off the VM, so there is otherwise no way to ask "how
/// many instructions did that command run?": the object is gone by the time anyone could look.
///
/// Instrumentation only: the bag stays empty and nothing is enabled when telemetry is off.
module InterpreterStatsSink =
  let all = System.Collections.Concurrent.ConcurrentBag<obj>()

  /// Nothing ever removes from the bag, and a VM is created per `executeFunction`, so a long-lived
  /// process with telemetry on (`serve`, or a long interactive session) would accumulate one entry per
  /// call forever. A one-shot CLI command creates a handful, so a cap costs nothing there and bounds the
  /// leak everywhere else. Past the cap we stop recording rather than evicting: the exit dump wants the
  /// run's first VMs, not its last.
  let maxRetained = 10_000

  let mutable private retained = 0

  let add (s : obj) : unit =
    if retained < maxRetained then
      retained <- retained + 1
      all.Add s


/// Lightweight interpreter performance counters.
/// Incremented during execution, read/reset via builtins.
/// Per-builtin timing uses a dictionary keyed by name; overhead is ~1 Stopwatch
/// call per builtin invocation, only when detailedTiming is true.
type InterpreterStats =
  {
    /// When false, all counting is skipped (zero overhead in hot loop)
    mutable enabled : bool
    mutable instructionCount : int64
    mutable builtinCallCount : int64
    mutable packageCallCount : int64
    mutable framePushCount : int64
    mutable packageFnLoadCount : int64

    /// When true, per-builtin cumulative timing is collected (requires enabled)
    mutable detailedTiming : bool
    /// Cumulative microseconds per builtin name
    builtinTiming : Dictionary<string, int64>
    /// Call count per builtin name
    builtinCounts : Dictionary<string, int64>
    /// Cumulative microseconds per package fn hash
    packageFnTiming : Dictionary<string, int64>
    /// Call count per package fn hash
    packageFnCounts : Dictionary<string, int64>
    /// Timestamp when each frame was pushed (for measuring total fn time)
    framePushTimestamps : Dictionary<uuid, int64>

    /// Bytes allocated while executing each opcode, indexed by the `Instruction` DU tag, plus how many of
    /// each ran. Neither instruction count nor call count predicts wall time well, because the interpreter
    /// is allocation-bound; this says *which* opcodes produce the garbage.
    ///
    /// A `GC.GetAllocatedBytesForCurrentThread()` is ~4 ns against a ~6.7 us instruction, so unlike a
    /// Stopwatch read (~1.27 us on an HPET host) this is affordable per instruction.
    allocByOpcode : int64[]
    countByOpcode : int64[]

    /// Total register slots allocated across every frame pushed. Each frame gets its own
    /// `Array.zeroCreate registerCount`, so this times 8 bytes is the register-file share of Apply's
    /// allocation -- the leading suspect for its ~5.6 KB per call.
    mutable registersAllocated : int64

    /// Bytes allocated inside builtin bodies (`fn.fn`), separated from the interpreter machinery around
    /// them. The per-opcode counter can't answer this: Apply's arm awaits, so its delta spans nested
    /// execution and over-counts (it reports more than the process allocates in total).
    mutable builtinBodyAlloc : int64

    /// Bytes allocated per builtin name. Unlike `builtinTiming` this doesn't need `detailedTiming`: an
    /// allocation read is ~4 ns where a Stopwatch read is ~1.27 us on an HPET host, so it's affordable
    /// on every call.
    builtinAlloc : Dictionary<string, int64>

    /// Calls per builtin name, so the allocation above can be read per call rather than in aggregate.
    /// Like `builtinAlloc` and unlike `builtinCounts`, this doesn't need `detailedTiming`.
    builtinCallsByName : Dictionary<string, int64>

    /// Bytes allocated in named *synchronous* regions of the Apply path, indexed by `ApplyStage`.
    ///
    /// Only synchronous regions: a bracket spanning an `await` measures the nested execution that resumes
    /// inside it, not the region. That artifact made both the per-opcode Apply counter and the
    /// builtin-body counter over-report (the former claimed more bytes than the process allocated in
    /// total; the latter attributed 97% to the root script-runner builtin, which encloses everything).
    allocByStage : int64[]

    /// How many times each stage's bracket ran, alongside the bytes.
    ///
    /// Without this, the only denominator available is the total call count, and dividing by it
    /// silently assumes the cost is spread evenly. It isn't: `pkg.fetchOnly` looked like 20 bytes on
    /// each of 41,402 package calls and was really ~15 KB on each of 52 cold ones, with the other
    /// 41,350 free. Two separate pieces of work got aimed at the wrong thing before that turned up.
    countByStage : int64[]

    /// Type-symbol-table size at each frame push, summed and maxed. The TST is an immutable F# Map that a
    /// frame inherits from its parent, so if bindings accumulate down the call stack every merge on it is
    /// O(k) in a growing k -- which would make Apply's cost superlinear in depth rather than constant.
    mutable tstSizeSum : int64
    mutable tstSizeMax : int64
  }

  static member create() =
    let s =
      { enabled = Telemetry.isEnabled ()
        instructionCount = 0L
        builtinCallCount = 0L
        packageCallCount = 0L
        framePushCount = 0L
        packageFnLoadCount = 0L
        // Off even when counting is on: per-call timing costs a `Stopwatch.GetTimestamp()`, which is ~1.27us
        // on an HPET host. Turn it on deliberately, per run, via `Builtin.interpreterStatsEnableDetailedTiming`.
        detailedTiming = false
        builtinTiming = Dictionary()
        builtinCounts = Dictionary()
        packageFnTiming = Dictionary()
        packageFnCounts = Dictionary()
        framePushTimestamps = Dictionary()
        allocByOpcode = Array.zeroCreate 32
        countByOpcode = Array.zeroCreate 32
        registersAllocated = 0L
        builtinBodyAlloc = 0L
        builtinAlloc = Dictionary()
        builtinCallsByName = Dictionary()
        allocByStage = Array.zeroCreate 32
        countByStage = Array.zeroCreate 32
        tstSizeSum = 0L
        tstSizeMax = 0L }

    if s.enabled then InterpreterStatsSink.add (box s)
    s

  member this.reset() =
    this.instructionCount <- 0L
    this.builtinCallCount <- 0L
    this.packageCallCount <- 0L
    this.framePushCount <- 0L
    this.packageFnLoadCount <- 0L
    this.builtinTiming.Clear()
    this.builtinCounts.Clear()
    this.packageFnTiming.Clear()
    this.packageFnCounts.Clear()
    this.framePushTimestamps.Clear()
    this.registersAllocated <- 0L
    this.builtinBodyAlloc <- 0L
    this.builtinAlloc.Clear()
    this.builtinCallsByName.Clear()
    this.tstSizeSum <- 0L
    this.tstSizeMax <- 0L
    System.Array.Clear(this.allocByStage, 0, this.allocByStage.Length)
    System.Array.Clear(this.countByStage, 0, this.countByStage.Length)
    System.Array.Clear(this.allocByOpcode, 0, this.allocByOpcode.Length)
    System.Array.Clear(this.countByOpcode, 0, this.countByOpcode.Length)

  member private this.addTiming
    (timingDict : Dictionary<string, int64>)
    (countDict : Dictionary<string, int64>)
    (name : string)
    (elapsedTicks : int64)
    =
    // Raw ticks, not microseconds. The reporting boundary (`interpreterStatsGet`) documents that these
    // accumulators hold ticks and converts once there; dividing here both contradicted that -- so the
    // report multiplied ticks-per-ns by a value already in microseconds and under-reported by 1000x on a
    // 1 GHz clocksource -- and put a division in the hot path that the comment there says it avoids.
    match timingDict.TryGetValue(name) with
    | true, v -> timingDict[name] <- v + elapsedTicks
    | false, _ -> timingDict[name] <- elapsedTicks
    match countDict.TryGetValue(name) with
    | true, v -> countDict[name] <- v + 1L
    | false, _ -> countDict[name] <- 1L

  member this.recordBuiltin(name : string, elapsedTicks : int64) =
    this.addTiming this.builtinTiming this.builtinCounts name elapsedTicks

  member this.recordPackageFn(hash : string, elapsedTicks : int64) =
    this.addTiming this.packageFnTiming this.packageFnCounts hash elapsedTicks

type VMState =
  {
    mutable threadID : uuid

    callFrames : Dictionary<uuid, CallFrame>
    mutable currentFrameID : uuid

    // The inst data for each fn/lambda/etc. is stored here, so that
    // it doesn't have to be copied into each CallFrame.
    rootInstrData : Option<tlid> * InstrData
    /// Per-VM memoization of InstrData derived from `exeState.lambdaInstrCache`.
    mutable lambdaInstrDataCache : Dictionary<id, InstrData>

    /// Memoized `ExecutionPoint`s for lambda frames, keyed on the lambda's expression id and
    /// holding the calling frame's execution point it was derived from. See the note at the use
    /// site in `applyInstruction`.
    lambdaEpCache : Dictionary<id, struct (ExecutionPoint * ExecutionPoint)>

    /// Performance counters — incremented during execution
    stats : InterpreterStats

    /// Set by the instruction that wants to push a frame, read by the loop that pushes it. Lives here
    /// rather than as a local in `executeInner` because a mutable local the Ply builder's continuations
    /// capture becomes a heap ref cell, allocated once per frame activation.
    mutable frameToPush : CallFrame voption

    /// Source of frame ids. Per-VM rather than global because a VM's interpreter loop is single-threaded,
    /// so this needs no synchronization -- and a process-global counter would need it: tests run VMs in
    /// parallel, and a non-atomic shared increment hands two frames the same id, which silently drops one
    /// from `callFrames` and fails the parent lookup on return.
    mutable frameIdCounter : int64

    /// The value the root frame returned, set when it pops. On the VM rather than a local of the
    /// interpreter loop for the same reason as `pendingCallArgs`: a local is a field in every
    /// continuation the builder makes for the loop body.
    mutable finalResult : Dval voption

    /// Arguments of calls whose frames are still running, keyed by frame id, so the tracer can pair
    /// them with the result when the frame returns. Empty and untouched when tracing is off.
    ///
    /// On the VM rather than a local of the interpreter loop so that the parts of the loop that run
    /// outside the computation expression can reach it.
    pendingCallArgs : Dictionary<uuid, Dval list>

    /// Scratch space for the bindings a match pattern produces, reused across every `match` the VM
    /// evaluates. As a returned `List<Register * Dval>` this was a tuple and a cons per bound
    /// variable per pattern *tried*, which came to 11.6% of allocation.
    ///
    /// A buffer rather than writing straight into the registers, because a pattern can fail halfway:
    /// the caller applies these only once the whole pattern has matched, and an or-pattern's failed
    /// alternative truncates back to where it started.
    matchBindings : ResizeArray<struct (Register * Dval)>

    /// Popped frames, with their register files still attached, bucketed by register count and handed
    /// back out to the next push of that size. A frame and its registers are what pushing a call costs,
    /// and nothing holds either past the pop: a lambda copies the values it closes over, a partial
    /// application copies its args, the tracer is handed lists, and the parent link carries a frame id
    /// rather than a reference. Per-VM, so single-threaded and needing no synchronization.
    framePool : Dictionary<int, Stack<CallFrame>>
  }

  static member create(instrs : Option<tlid> * Instructions) : VMState =
    let tlid, instrs = instrs

    let rootCallFrameID = System.Guid.NewGuid()

    let rootInstrData : InstrData =
      { instructions = List.toArray instrs.instructions
        resultReg = instrs.resultIn }

    let rootCallFrame : CallFrame =
      { id = rootCallFrameID
        executionPoint = Source
        instrData = rootInstrData
        expectedReturnType = ValueNone
        programCounter = 0
        registers = Array.zeroCreate instrs.registerCount
        typeSymbolTable = TST.empty
        parent = ValueNone }

    { threadID = System.Guid.NewGuid()
      currentFrameID = rootCallFrameID
      callFrames =
        let d = Dictionary()
        d[rootCallFrameID] <- rootCallFrame
        d
      rootInstrData = (tlid, rootInstrData)
      lambdaInstrDataCache = Dictionary()
      lambdaEpCache = Dictionary()
      stats = InterpreterStats.create ()
      frameToPush = ValueNone
      frameIdCounter = 0L
      finalResult = ValueNone
      matchBindings = ResizeArray()
      pendingCallArgs = Dictionary()
      framePool = Dictionary() }

  static member createWithoutTLID(instrs : Instructions) : VMState =
    VMState.create (None, instrs)

  static member creatWithTLID (tlid : tlid) (instrs : Instructions) : VMState =
    VMState.create (Some tlid, instrs)



// -- Builtins --
type BuiltInValue =
  { name : FQValueName.Builtin
    typ : TypeReference
    description : string
    deprecated : Deprecation<FQValueName.FQValueName>
    body : Dval }

/// A built-in standard library function
///
/// (Generally shouldn't be accessed directly,
/// except by a single stdlib Package fn that wraps it)
type BuiltInFn =
  {
    name : FQFnName.Builtin
    typeParams : List<string>
    parameters : List<BuiltInParam> // TODO: should be NEList but there's so much to change!
    returnType : TypeReference
    description : string
    previewable : Previewable
    deprecated : Deprecation<FQFnName.FQFnName>
    sqlSpec : SqlSpec
    /// The capabilities this builtin needs — pure (`noCaps`) by default; effectful builtins declare
    /// their need. The call-site gate checks PRESENCE against this; nuanced builtins (http/file/exec)
    /// additionally enforce the specific scope (URL/path/args) in their own body.
    capabilities : Capabilities.Capabilities
    fn : BuiltInFnSig
  }

and BuiltInFnSig =
  // (exeState * vmState * typeArgs * fnArgs) -> result
  //
  // A *struct* tuple. As a reference tuple this was built on the heap on every builtin call, and at
  // 9.5% of the interpreter's allocation profile it was the last fixed cost of making one. The price
  // is that every implementation's pattern has to say `struct (...)`.
  (struct (ExecutionState * VMState * List<TypeReference> * List<Dval>)) -> DvalTask


/// Functionally written in F# and shipped with the executable
/// Both are `Dictionary` rather than `Map`. There are ~800 builtins, the set is fixed once the
/// process starts, and it is looked up on every builtin call. An F# `Map` charged for that twice: a
/// tree node per entry per rebuild (and `combine` rebuilds at every nesting level, so each builtin
/// was inserted into a balanced tree three times), and an O(log n) walk of generic structural
/// comparisons on every lookup.
and Builtins =
  { values : Dictionary<FQValueName.Builtin, BuiltInValue>
    fns : Dictionary<FQFnName.Builtin, BuiltInFn> }





/// Every part of a user's program. Single-instance Dark today —
/// no per-scope state, just the user-defined DB set.
and Program = { dbs : Map<string, DB.T> }


// Used for testing
// CLEANUP maybe this belongs in Execution rather than RuntimeTypes?
// and taken out of ExecutionState, where it's not really used?
and TestContext =
  { mutable sideEffectCount : int

    mutable exceptionReports : List<string * string * Metadata>
    mutable expectedExceptionCount : int
    postTestExecutionHook : TestContext -> unit }


and ExceptionReporter = ExecutionState -> VMState -> Metadata -> exn -> Ply<unit>

and Notifier = ExecutionState -> VMState -> string -> Metadata -> Ply<unit>

/// All state set when starting an execution; non-changing
/// (as opposed to the VMState, which changes as the execution progresses)
and ExecutionState =
  { // -- Set consistently across a runtime --
    tracing : Tracing.Tracing
    test : TestContext

    /// Lambda instructions registered by `CreateLambda`, looked up on `Apply`.
    /// Shared across every VM spawned under this execution so that lambdas
    /// created in one VM (e.g. an `eval` expression) remain findable when
    /// invoked from another (e.g. an httpServerServe request handler VM).
    lambdaInstrCache :
      System.Collections.Concurrent.ConcurrentDictionary<id, LambdaImpl>

    /// Memoization of `InstrData` derived from package function bodies.
    /// Shared across VMs for the same reason as `lambdaInstrCache`.
    packageFnInstrCache :
      System.Collections.Concurrent.ConcurrentDictionary<FQFnName.Package, InstrData>

    /// Called to report exceptions
    reportException : ExceptionReporter

    /// Called to notify that something of interest (that isn't an exception)
    /// has happened.
    ///
    /// Useful for tracking behaviour we want to deprecate, understanding what
    /// users are doing, etc.
    notify : Notifier

    // -- Set per-execution --
    branchId : BranchId
    program : Program

    types : Types
    fns : Functions
    values : Values

    /// The capabilities the running code is allowed — read by the call-site gate (an uncovered builtin
    /// call is denied). Callers set it: `eval`/host use the configured grant (allCaps by default), `dark
    /// run` uses NONE.
    grantedCaps : Capabilities.Capabilities

    /// Content-addressed persistent blob store (`package_blobs`).
    /// Ephemeral blobs carry their bytes inline and need no store;
    /// promotion (see `Blob.promote`) writes them here.
    ///
    /// Orphan reclaim TODOs (persistent blobs only):
    ///   - `package_blobs` orphan reclaim runs via the `pm-sweep-blobs`
    ///     CLI command, which scans `package_values.rt_dval` only —
    ///     `trace_data` and User DB rows don't hold blob refs today,
    ///     but any new referencing table needs wiring into the sweep.
    ///     TODO turn the sweep into "scan a list of (table, column)
    ///     pairs" defined alongside the schema so new blob-holding
    ///     columns register themselves.
    ///   - No reverse-index table (`package_blob_refs`) — the sweep
    ///     is O(N+M). Fine at current scale; revisit at higher
    ///     package counts.
    blobs : Blobs

    /// Escape hatch for `Harmful`-marked fns: when true, the interpreter
    /// still sees `fns.isHarmful` return true, but proceeds anyway (and
    /// can still `notify` for observability). Tests, sandboxes, security
    /// research set this; `run --allow-harmful` / `eval --allow-harmful`
    /// toggle it for one-offs.
    allowHarmful : bool

    /// The account this run is attributed to (the developer behind a
    /// commit / script run / handler invocation). `None` means
    /// unattributed — outer-CLI bootstrapping, tests, anonymous
    /// builtin invocations all leave this empty. The trace insert
    /// reads it; commit ops carry the same id separately.
    accountID : Option<System.Guid>
  }


and Types = { package : FQTypeName.Package -> Ply<Option<PackageType.PackageType>> }

and Values =
  { builtIn : Dictionary<FQValueName.Builtin, BuiltInValue>
    package : FQValueName.Package -> Ply<Option<PackageValue.PackageValue>> }

/// Blob-byte access wired onto the ExecutionState. `get` resolves a
/// content-addressed hash to bytes (or None if the hash is missing);
/// `persist` writes bytes to `package_blobs` via INSERT OR IGNORE.
/// Needed inside builtins that manipulate blobs — eg.
/// `Blob.toHex : Blob -> String` has to dereference its arg.
and Blobs =
  { get : string -> Ply<Option<byte[]>>; persist : string -> byte[] -> Ply<unit> }

and Functions =
  {
    builtIn : Dictionary<FQFnName.Builtin, BuiltInFn>
    package : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>>
    /// `PackageManager.isHarmful` with the state's branchId pre-applied.
    isHarmful : FQFnName.Package -> bool
  }



module Types =
  let empty = { package = (fun _ -> Ply None) }

  let find
    (types : Types)
    (name : FQTypeName.FQTypeName)
    : Ply<Option<TypeDeclaration.T>> =
    match name with
    | FQTypeName.Package pkg ->
      types.package pkg |> Ply.map (Option.map _.declaration)

  /// Swap concrete types for type parameters
  /// CLEANUP consider accepting a pre-zipped list instead
  let rec substitute
    (typeParams : List<string>)
    (typeArguments : List<TypeReference>)
    (typ : TypeReference)
    : TypeReference =
    let r = substitute typeParams typeArguments
    match typ with
    | TUnit
    | TBool
    | TInt8
    | TUInt8
    | TInt16
    | TUInt16
    | TInt32
    | TUInt32
    | TInt64
    | TUInt64
    | TInt128
    | TUInt128
    | TInt
    | TFloat
    | TChar
    | TString
    | TUuid
    | TDateTime
    | TBlob -> typ

    | TStream t -> TStream(r t)
    | TTuple(t1, t2, rest) -> TTuple(r t1, r t2, List.map r rest)
    | TList t -> TList(r t)
    | TDict t -> TDict(r t)

    | TCustomType(typ, args) -> TCustomType(typ, List.map r args)

    | TFn(args, ret) -> TFn(NEList.map r args, r ret)
    | TDB inner -> TDB(r inner)

    | TVariable v ->
      if typeParams.Length = typeArguments.Length then
        List.zip typeParams typeArguments
        |> List.find (fun (param, _) -> param = v)
        |> Option.map snd
        |> Exception.unwrapOptionInternal
          "No type argument found for type parameter"
          []
      else
        Exception.raiseInternal
          $"typeParams and typeArguments have different lengths"
          [ "typeParams", typeParams; "typeArguments", typeArguments ]


module TypeReference =
  let result (t1 : TypeReference) (t2 : TypeReference) : TypeReference =
    TCustomType(
      { originalName = []
        resolved = Ok(FQTypeName.fqPackage (PackageRefs.Type.Stdlib.result ())) },
      [ t1; t2 ]
    )

  let option (t : TypeReference) : TypeReference =
    TCustomType(
      { originalName = []
        resolved = Ok(FQTypeName.fqPackage (PackageRefs.Type.Stdlib.option ())) },
      [ t ]
    )

  let rec unwrapAlias (types : Types) (typ : TypeReference) : Ply<TypeReference> =
    match typ with
    | TCustomType({ resolved = Ok outerTypeName }, outerTypeArgs) ->
      uply {
        match! Types.find types outerTypeName with
        | Some { definition = TypeDeclaration.Alias typ; typeParams = typeParams } ->
          let typ = Types.substitute typeParams outerTypeArgs typ
          return! unwrapAlias types typ
        | _ -> return typ
      }
    | _ -> Ply typ


  /// A declared `TypeReference` as a `ValueType`.
  ///
  /// Three things were wrong with this, and together they were the top of the allocation profile for
  /// an HTTP request at about 18%.
  ///
  /// The `let r = toVT types tst` alias was a closure over both arguments, built on entry to every
  /// call including the scalar ones that never used it. Each recursion now passes them explicitly.
  ///
  /// The whole body was one `uply`, so `TString` -- which cannot be an alias and cannot await --
  /// still entered the builder. Only the cases that can genuinely need the store do now.
  ///
  /// And every scalar case built a fresh `Known` wrapper. They come from `KnownVT` instead, which is
  /// the same object `Dval.toValueType` returns, so comparing a value's type against its declared
  /// type can settle on reference equality.
  let rec toVT
    (types : Types)
    (tst : TypeSymbolTable)
    (typeRef : TypeReference)
    : Ply<ValueType> =
    match typeRef with
    | TUnit -> Ply KnownVT.unit
    | TBool -> Ply KnownVT.bool
    | TInt8 -> Ply KnownVT.int8
    | TUInt8 -> Ply KnownVT.uint8
    | TInt16 -> Ply KnownVT.int16
    | TUInt16 -> Ply KnownVT.uint16
    | TInt32 -> Ply KnownVT.int32
    | TUInt32 -> Ply KnownVT.uint32
    | TInt64 -> Ply KnownVT.int64
    | TUInt64 -> Ply KnownVT.uint64
    | TInt128 -> Ply KnownVT.int128
    | TUInt128 -> Ply KnownVT.uint128
    | TInt -> Ply KnownVT.int
    | TFloat -> Ply KnownVT.float
    | TChar -> Ply KnownVT.char
    | TString -> Ply KnownVT.string
    | TUuid -> Ply KnownVT.uuid
    | TDateTime -> Ply KnownVT.dateTime
    | TBlob -> Ply KnownVT.blob

    | TVariable name ->
      Ply(
        match TST.tryFind name tst with
        | ValueSome vt -> vt
        | ValueNone -> ValueType.Unknown
      )

    | TCustomType({ originalName = names; resolved = Error nre }, _) ->
      raiseUntargetedRTE (RuntimeError.ParseTimeNameResolution(names, nre))

    // Only these can be an alias, or hold something that has to be resolved, so only these pay for
    // the builder. `unwrapAlias` is a no-op for everything above.
    | TStream _
    | TTuple _
    | TList _
    | TDict _
    | TCustomType _
    | TFn _
    | TDB _ ->
      uply {
        match! unwrapAlias types typeRef with
        | TStream inner ->
          let! inner = toVT types tst inner
          return ValueType.Known(KTStream inner)

        | TTuple(first, second, theRest) ->
          let! first = toVT types tst first
          let! second = toVT types tst second
          let! theRest = theRest |> Ply.List.mapSequentially (toVT types tst)
          return KTTuple(first, second, theRest) |> ValueType.Known

        | TList inner ->
          let! inner = toVT types tst inner
          return ValueType.Known(KTList inner)

        | TDict inner ->
          let! inner = toVT types tst inner
          return ValueType.Known(KTDict inner)

        | TCustomType({ resolved = Ok typeName }, typeArgs) ->
          let! typeArgs = typeArgs |> Ply.List.mapSequentially (toVT types tst)
          return KTCustomType(typeName, typeArgs) |> ValueType.Known

        | TCustomType({ originalName = names; resolved = Error nre }, _) ->
          return
            raiseUntargetedRTE (RuntimeError.ParseTimeNameResolution(names, nre))

        | TFn(args, result) ->
          let! args = args |> Ply.NEList.mapSequentially (toVT types tst)
          let! result = toVT types tst result
          return KTFn(args, result) |> ValueType.Known

        | TDB inner ->
          let! inner = toVT types tst inner
          return ValueType.Known(KTDB inner)

        // An alias can unwrap to any of the cases handled without the builder above; those are
        // cheap, so recursing back into `toVT` costs nothing and keeps one copy of each rule.
        | unwrapped -> return! toVT types tst unwrapped
      }


  /// Convert a KnownType back to a TypeReference
  let rec fromKnownType (kt : KnownType) : TypeReference =
    let fromVT (vt : ValueType) : TypeReference =
      match vt with
      | ValueType.Unknown -> TVariable "_"
      | ValueType.Known kt -> fromKnownType kt

    match kt with
    | KTUnit -> TUnit
    | KTBool -> TBool
    | KTInt8 -> TInt8
    | KTUInt8 -> TUInt8
    | KTInt16 -> TInt16
    | KTUInt16 -> TUInt16
    | KTInt32 -> TInt32
    | KTUInt32 -> TUInt32
    | KTInt64 -> TInt64
    | KTUInt64 -> TUInt64
    | KTInt128 -> TInt128
    | KTUInt128 -> TUInt128
    | KTInt -> TInt
    | KTFloat -> TFloat
    | KTChar -> TChar
    | KTString -> TString
    | KTUuid -> TUuid
    | KTDateTime -> TDateTime
    | KTBlob -> TBlob
    | KTStream inner -> TStream(fromVT inner)
    | KTList inner -> TList(fromVT inner)
    | KTDict inner -> TDict(fromVT inner)
    | KTTuple(first, second, rest) ->
      TTuple(fromVT first, fromVT second, List.map fromVT rest)
    | KTCustomType(typeName, typeArgs) ->
      TCustomType(NameResolution.ok typeName, List.map fromVT typeArgs)
    | KTFn(args, ret) -> TFn(NEList.map fromVT args, fromVT ret)
    | KTDB inner -> TDB(fromVT inner)


  /// Resolve type variables in a TypeReference using the TypeSymbolTable.
  /// If a variable is not found or resolves to Unknown, it is kept as TVariable.
  let rec resolveTypeVariables
    (tst : TypeSymbolTable)
    (typ : TypeReference)
    : TypeReference =
    let r = resolveTypeVariables tst
    match typ with
    | TUnit
    | TBool
    | TInt8
    | TUInt8
    | TInt16
    | TUInt16
    | TInt32
    | TUInt32
    | TInt64
    | TUInt64
    | TInt128
    | TUInt128
    | TInt
    | TFloat
    | TChar
    | TString
    | TUuid
    | TDateTime
    | TBlob -> typ

    | TVariable name ->
      match TST.tryFind name tst |> ValueOption.toOption with
      | Some(ValueType.Known kt) -> fromKnownType kt
      | Some ValueType.Unknown
      | None -> typ // Keep as TVariable if not resolved

    | TList inner -> TList(r inner)
    | TStream inner -> TStream(r inner)
    | TDict inner -> TDict(r inner)
    | TTuple(first, second, rest) -> TTuple(r first, r second, List.map r rest)
    | TCustomType(typeName, typeArgs) -> TCustomType(typeName, List.map r typeArgs)
    | TFn(args, ret) -> TFn(NEList.map r args, r ret)
    | TDB inner -> TDB(r inner)



let consoleReporter : ExceptionReporter =
  fun _state _vm (metadata : Metadata) (exn : exn) ->
    uply { printException "runtime-error" metadata exn }

let consoleNotifier : Notifier =
  fun _state _vm msg tags ->
    uply { print $"A notification happened in the runtime:\n  {msg}\n  {tags}\n\n" }
