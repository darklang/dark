module Builtins.Pure.Libs.Json

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module DarkDateTime = LibExecution.DarkDateTime
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module PackageRefs = LibExecution.PackageRefs
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


// parsing
let parseJson (s : string) : JsonElement =
  let options =
    new JsonDocumentOptions(
      CommentHandling = JsonCommentHandling.Skip,
      MaxDepth = System.Int32.MaxValue // infinite
    )

  JsonDocument.Parse(s, options).RootElement


// serialization
let writeJson (f : Utf8JsonWriter -> unit) : string =
  let options =
    new JsonWriterOptions(
      // TODO: `true` here would make it hard to write tests...
      Indented = false,
      SkipValidation = true,
      Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

  let stream = new System.IO.MemoryStream()
  let w = new Utf8JsonWriter(stream, options)
  f w
  w.Flush()

  UTF8.ofBytesUnsafe (stream.ToArray())

type Utf8JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEndObject()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEndArray()



module JsonPath =
  module Part =
    type Part =
      | Root
      | Index of int
      | Field of string

    let typeName () =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Json.ParseError.JsonPath.part ())

    let toDT (part : Part) : Dval =
      let typeName = typeName ()
      let (caseName, fields) =
        match part with
        | Root -> "Root", []
        | Index i -> "Index", [ Dval.dint64 (int64 i) ]
        | Field s -> "Field", [ DString s ]
      DEnum(typeName, typeName, [], caseName, fields)


  type JsonPath = List<Part.Part>

  let toDT (path : JsonPath) : Dval =
    let partType = VT.customType (Part.typeName ()) []
    path |> List.map Part.toDT |> (fun parts -> DList(partType, Items(parts)))



/// The stdlib's range-checked integers: `Net.Port`, `Http.Status`, `Cli.Posix.Pid` and
/// `Cli.Posix.ExitCode`. Each is a record of one field, and JSON writes and reads it as the
/// bare number, so a port is `8080`, not `{"__port":8080}`. Parsing refuses a number outside
/// the type's range, the same range its `fromInt64` checks.
module RangedInt =
  [<Struct>]
  type Range = { field : string; what : string; min : int64; max : int64 }

  /// The F# side of each type's `fromInt64`; `tests/stdlib/json.dark` checks they agree.
  let find (typeName : FQTypeName.FQTypeName) : ValueOption<Range> =
    match typeName with
    | FQTypeName.Package(Hash h) ->
      if h = PackageRefs.Type.Stdlib.Net.port () then
        ValueSome { field = "__port"; what = "port"; min = 0L; max = 65535L }
      else if h = PackageRefs.Type.Stdlib.Http.status () then
        ValueSome
          { field = "__status"; what = "HTTP status"; min = 100L; max = 599L }
      else if h = PackageRefs.Type.Stdlib.Cli.Posix.pid () then
        ValueSome
          { field = "__pid"; what = "process ID"; min = 1L; max = 2147483647L }
      else if h = PackageRefs.Type.Stdlib.Cli.Posix.exitCode () then
        ValueSome { field = "__exitCode"; what = "exit code"; min = 0L; max = 255L }
      else
        ValueNone

  /// The number inside a record of one of these types.
  let valueOf
    (typeName : FQTypeName.FQTypeName)
    (fields : RecordFields)
    : ValueOption<int64> =
    if RecordFields.count fields <> 1 then
      ValueNone
    else
      match find typeName with
      | ValueNone -> ValueNone
      | ValueSome range ->
        match RecordFields.tryFind range.field fields with
        | Some(DInt64 n) -> ValueSome n
        | _ -> ValueNone

  let make (typeName : FQTypeName.FQTypeName) (range : Range) (n : int64) : Dval =
    DRecord(typeName, typeName, [], RecordFields.ofList [ range.field, DInt64 n ])


/// The value `serialize` found with no JSON form.
exception CannotSerialize of Dval

/// The resolved name, not the written one: a value built through an alias carries the alias first.
let private isOptionEnum (dv : Dval) : bool =
  match dv with
  | DEnum(_, resolvedTypeName, _, _, _) ->
    resolvedTypeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.option ())
  | _ -> false

let private isNoneValue (dv : Dval) : bool =
  match dv with
  | DEnum(_, _, _, "None", []) -> isOptionEnum dv
  | _ -> false

let rec serialize (w : Utf8JsonWriter) (dv : Dval) : unit =
  let r = serialize w
  match dv with
  // basic types
  | DUnit -> w.WriteNullValue()

  | DBool b -> w.WriteBooleanValue b

  | DInt8 i -> w.WriteNumberValue i
  | DUInt8 i -> w.WriteNumberValue i
  | DInt16 i -> w.WriteNumberValue i
  | DUInt16 i -> w.WriteNumberValue i
  | DInt32 i -> w.WriteNumberValue i
  | DUInt32 i -> w.WriteNumberValue i
  | DInt64 i -> w.WriteNumberValue i
  | DUInt64 i -> w.WriteNumberValue i
  | DInt128 i -> w.WriteRawValue(i.ToString())
  | DUInt128 i -> w.WriteRawValue(i.ToString())
  | DBigInt i -> w.WriteRawValue(string (DarkBigInt.toBigInt i))

  | DFloat64 f ->
    if System.Double.IsNaN f then w.WriteStringValue "NaN"
    else if System.Double.IsNegativeInfinity f then w.WriteStringValue "-Infinity"
    else if System.Double.IsPositiveInfinity f then w.WriteStringValue "Infinity"
    else w.WriteRawValue(floatToShortestString f)

  | DFloat32 f ->
    if System.Single.IsNaN f then w.WriteStringValue "NaN"
    else if System.Single.IsNegativeInfinity f then w.WriteStringValue "-Infinity"
    else if System.Single.IsPositiveInfinity f then w.WriteStringValue "Infinity"
    else w.WriteRawValue(float32ToShortestString f)

  // at its scale, so `1.10m` writes `1.10`
  | DDecimal d ->
    w.WriteRawValue(d.ToString(System.Globalization.CultureInfo.InvariantCulture))

  | DChar c -> w.WriteStringValue c
  | DStr s -> w.WriteStringValue s.Value

  | DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

  | DUuid uuid -> w.WriteStringValue(string uuid)

  // Nested types
  | DTuple(d1, d2, rest) -> w.writeArray (fun () -> List.iter r (d1 :: d2 :: rest))

  | DList(_, Items(items)) -> w.writeArray (fun () -> List.iter r items)

  | DDict(_, _, fields) ->
    // `iter` rather than `toList |> List.iter`: the latter builds a tuple and a cons per
    // field, for a list that is walked once and dropped. Both visit in ascending key order.
    w.writeObject (fun () ->
      fields
      |> DictMap.iter (fun k v ->
        match k with
        | DString k ->
          w.WritePropertyName k
          r v
        | _ -> raise (CannotSerialize dv)))

  // Enums and Records
  //
  // `Option` is written the way other systems write it: `Some v` is `v`, `None` is `null`,
  // and a `None` record field is left out. One layer is all this carries: `Some None`,
  // `Some ()` and `None` are all `null`, and parse back as `None`.
  | DEnum(_, _, _, "Some", [ inner ]) when isOptionEnum dv -> r inner
  | DEnum(_, _, _, "None", []) when isOptionEnum dv -> w.WriteNullValue()

  | DEnum(_, _, _, caseName, fields) ->
    w.writeObject (fun () ->
      w.WritePropertyName caseName
      w.writeArray (fun () -> fields |> List.iter r))

  | DRecord(_, typeName, _, fields) ->
    match RangedInt.valueOf typeName fields with
    | ValueSome n -> w.WriteNumberValue n
    | ValueNone ->
      // See `DDict` above.
      w.writeObject (fun () ->
        fields
        |> RecordFields.iter (fun fieldName dval ->
          if not (isNoneValue dval) then
            w.WritePropertyName fieldName
            r dval))

  // Not supported
  | DApplicable _
  | DBytes _
  | DStream _
  | DPromise _ -> raise (CannotSerialize dv)

module ParseError =
  module RT2DT = LibExecution.RuntimeTypesToDarkTypes
  let typeName () =
    FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Json.ParseError.parseError ())

  type ParseError =
    /// The json string can't be parsed as the given type.
    | CantMatchWithType of TypeReference * string * JsonPath.JsonPath
    | EnumExtraField of rawJson : string * JsonPath.JsonPath
    | EnumMissingField of TypeReference * index : int * JsonPath.JsonPath
    /// Provided caseName doesn't match any caseName the enum has
    | EnumInvalidCasename of TypeReference * caseName : string * JsonPath.JsonPath
    /// More than one label in a record expecting one
    | EnumTooManyCases of
      TypeReference *
      caseNames : List<string> *
      JsonPath.JsonPath
    | RecordMissingField of fieldName : string * JsonPath.JsonPath
    | RecordDuplicateField of fieldName : string * JsonPath.JsonPath
    /// A number outside the range of a type such as `Net.Port`
    | OutOfRange of
      what : string *
      value : int64 *
      min : int64 *
      max : int64 *
      JsonPath.JsonPath
    /// The text isn't JSON. Where it broke, both counted from 1; the column
    /// counts characters, not bytes
    | NotJson of line : int64 * column : int64
    /// A type with no JSON form: a function, bytes, a stream, or a type variable
    | UnsupportedType of TypeReference

  exception JsonException of ParseError

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | NotJson(line, column) -> "NotJson", [ Dval.dint64 line; Dval.dint64 column ]

      | UnsupportedType typ -> "UnsupportedType", [ RT2DT.TypeReference.toDT typ ]

      | CantMatchWithType(typ, json, errorPath) ->
        "CantMatchWithType",
        [ RT2DT.TypeReference.toDT typ
          DString(String.normalizeOutside json)
          JsonPath.toDT errorPath ]

      | EnumExtraField(json, errorPath) ->
        "EnumExtraField",
        [ DString(String.normalizeOutside json); JsonPath.toDT errorPath ]

      | EnumMissingField(typ, fieldCount, errorPath) ->
        "EnumMissingField",
        [ RT2DT.TypeReference.toDT typ
          Dval.dint64 (int64 fieldCount)
          JsonPath.toDT errorPath ]

      | EnumInvalidCasename(typ, caseName, errorPath) ->
        "EnumInvalidCasename",
        [ RT2DT.TypeReference.toDT typ; DString caseName; JsonPath.toDT errorPath ]

      | EnumTooManyCases(typ, cases, errorPath) ->
        "EnumTooManyCases",
        [ RT2DT.TypeReference.toDT typ
          cases |> List.map DString |> Dval.list KTString
          JsonPath.toDT errorPath ]

      | RecordMissingField(fieldName, errorPath) ->
        "RecordMissingField", [ DString fieldName; JsonPath.toDT errorPath ]

      | RecordDuplicateField(fieldName, errorPath) ->
        "RecordDuplicateField", [ DString fieldName; JsonPath.toDT errorPath ]

      | OutOfRange(what, value, min, max, errorPath) ->
        let rangeType =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Math.outOfRange ())
        let range =
          DRecord(
            rangeType,
            rangeType,
            [],
            RecordFields.ofList
              [ "what", DString what
                "value", DInt64 value
                "min", DInt64 min
                "max", DInt64 max ]
          )
        "OutOfRange", [ range; JsonPath.toDT errorPath ]

    let typeName = typeName ()
    DEnum(typeName, typeName, [], caseName, fields)

/// Where System.Text.Json stopped, as a `NotJson`. It reports a line counted
/// from 0 and a column in UTF-8 bytes; this walks to that line and counts
/// characters instead, so `é` or an emoji counts once. Only runs on failure,
/// so a successful parse pays nothing for it.
let private notJsonAt
  (s : string)
  (line : System.Nullable<int64>)
  (bytesInLine : System.Nullable<int64>)
  : ParseError.ParseError =
  let line = if line.HasValue then line.Value else 0L
  let bytesInLine = if bytesInLine.HasValue then bytesInLine.Value else 0L
  // IndexOf is vectorised: on a 15 MB document broken on its last line, a
  // char-by-char loop here added a sixth to the failed parse
  let rec lineStart (i : int) (seen : int64) : int =
    if seen < line && i < s.Length then
      let next = s.IndexOf('\n', i)
      lineStart (if next < 0 then s.Length else next + 1) (seen + 1L)
    else
      i
  let rec columnAt (i : int) (bytes : int64) (column : int64) : int64 =
    if bytes < bytesInLine && i < s.Length && s[i] <> '\n' then
      let c = s[i]
      if System.Char.IsHighSurrogate c && i + 1 < s.Length then
        columnAt (i + 2) (bytes + 4L) (column + 1L)
      else
        let width =
          if c < '\u0080' then 1L
          elif c < '\u0800' then 2L
          else 3L
        columnAt (i + 1) (bytes + width) (column + 1L)
    else
      column
  ParseError.NotJson(line + 1L, columnAt (lineStart 0 0L) 0L 1L)

let raiseError (e : ParseError.ParseError) : 'a = raise (ParseError.JsonException e)

let raiseCantMatchWithType
  (typ : TypeReference)
  (j : JsonElement)
  (path : JsonPath.JsonPath)
  : 'a =
  ParseError.CantMatchWithType(typ, j.GetRawText(), path) |> raiseError


let private maxJsonIntDigits = 1_000_000L


/// Parses a JSON number literal to an exact `bigint`, without going through a
/// double (which would round values past 2^53). Accepts integer-valued decimal
/// and exponent forms — `1.0`, `1.2E2`, `1200E-1`, `9007199254740993.0` — and
/// returns None for genuinely fractional values (`1.5`) or malformed input.
///
/// `BigInteger.TryParse` with `NumberStyles.Float` does the exact format parsing
/// (no double, integral-via-exponent accepted, true fractions rejected); we only
/// add a size bound up front, since it would otherwise allocate without limit for
/// a literal like `1e2000000000`.
let private parseExactJsonInteger (raw : string) : bigint option =
  let unsigned = raw.TrimStart('-', '+')

  // split the significand from the exponent
  let mantissa, exp =
    match unsigned.IndexOfAny [| 'e'; 'E' |] with
    | -1 -> unsigned, 0L
    | i ->
      match System.Int32.TryParse(unsigned.Substring(i + 1)) with
      | true, e -> unsigned.Substring(0, i), int64 e
      | false, _ -> unsigned, System.Int64.MaxValue // unparseable/huge exp → reject

  let dotIndex = mantissa.IndexOf '.'
  // integer-part length, and total significand digit count (excluding the point)
  let intLen = int64 (if dotIndex = -1 then mantissa.Length else dotIndex)
  let significandDigits = int64 mantissa.Length - (if dotIndex = -1 then 0L else 1L)

  // Two bounds, each catching a different blow-up, before BigInteger allocates:
  //  - significandDigits: the input itself (e.g. a 2M-digit fraction that
  //    TryParse would parse into a transient bigint before rejecting)
  //  - intLen + max exp 0: the expanded result (e.g. `1e2000000000`)
  if
    significandDigits > maxJsonIntDigits || intLen + max exp 0L > maxJsonIntDigits
  then
    None
  else
    match
      System.Numerics.BigInteger.TryParse(
        raw,
        System.Globalization.NumberStyles.Float,
        System.Globalization.CultureInfo.InvariantCulture
      )
    with
    | true, v -> Some v
    | false, _ -> None


/// Parses a JSON number literal to a `decimal` only when that is exact. `Decimal.TryParse`
/// rounds past 28-29 significant digits and flushes tiny values to zero without saying so, so
/// the result is checked: the literal times 10^scale has to be the same integer as the decimal's
/// own unscaled value.
let private parseExactJsonDecimal (raw : string) : decimal option =
  match
    System.Decimal.TryParse(
      raw,
      System.Globalization.NumberStyles.Float,
      System.Globalization.CultureInfo.InvariantCulture
    )
  with
  | false, _ -> None
  | true, d ->
    let scale = int d.Scale
    let mantissa, exp =
      match raw.IndexOfAny [| 'e'; 'E' |] with
      | -1 -> raw, 0L
      | i ->
        match System.Int64.TryParse(raw.Substring(i + 1)) with
        | true, e -> raw.Substring(0, i), e
        | false, _ -> raw, System.Int64.MinValue
    if exp = System.Int64.MinValue then
      None
    else
      let unscaled = System.Decimal.Truncate(d * pown 10m scale)
      match parseExactJsonInteger $"{mantissa}e{exp + int64 scale}" with
      | Some n when n = bigint unscaled -> Some d
      | _ -> None


/// A JSON number whose value is a whole number, read exactly. Whoever wrote a field we
/// read as an int may have printed it in float form (`1.0`, `1.2E2`), and going through a
/// double would round it past 2^53. The plain form is by far the commonest, so it is tried
/// first, without the string; `integer` below does the same without the `bigint`.
let private jsonInteger (j : JsonElement) : bigint option =
  match j.TryGetInt64() with
  | true, n -> Some(bigint n)
  | false, _ -> parseExactJsonInteger (j.GetRawText())

/// One of the fixed-width integer types: its range, and how to make one from a number in
/// it. `lo` and `hi` are the range clamped to Int64, which is all a plain-form number can be,
/// so that form is checked and built without a `bigint`.
type private IntegerType =
  { min : bigint
    max : bigint
    lo : int64
    hi : int64
    ofInt64 : int64 -> Dval
    ofBigint : bigint -> Dval }

/// A type whose whole range fits in Int64.
let private narrow
  (lo : int64)
  (hi : int64)
  (ofInt64 : int64 -> Dval)
  : IntegerType =
  { min = bigint lo
    max = bigint hi
    lo = lo
    hi = hi
    ofInt64 = ofInt64
    ofBigint = fun n -> ofInt64 (int64 n) }

/// A type whose range goes past Int64's.
let private wide
  (min : bigint)
  (max : bigint)
  (ofBigint : bigint -> Dval)
  : IntegerType =
  { min = min
    max = max
    lo =
      if min > bigint System.Int64.MinValue then int64 min else System.Int64.MinValue
    hi = System.Int64.MaxValue
    ofInt64 = fun n -> ofBigint (bigint n)
    ofBigint = ofBigint }

let private int8Type =
  narrow (int64 System.SByte.MinValue) (int64 System.SByte.MaxValue) (fun n ->
    Dval.int8 (int8 n))
let private uint8Type =
  narrow 0L (int64 System.Byte.MaxValue) (fun n -> Dval.uint8 (uint8 n))
let private int16Type =
  narrow (int64 System.Int16.MinValue) (int64 System.Int16.MaxValue) (fun n ->
    DInt16(int16 n))
let private uint16Type =
  narrow 0L (int64 System.UInt16.MaxValue) (fun n -> DUInt16(uint16 n))
let private int32Type =
  narrow (int64 System.Int32.MinValue) (int64 System.Int32.MaxValue) (fun n ->
    DInt32(int32 n))
let private uint32Type =
  narrow 0L (int64 System.UInt32.MaxValue) (fun n -> DUInt32(uint32 n))
let private int64Type = narrow System.Int64.MinValue System.Int64.MaxValue DInt64
let private uint64Type =
  wide 0I (bigint System.UInt64.MaxValue) (fun n -> DUInt64(uint64 n))
let private int128Type =
  wide
    (System.Numerics.BigInteger.op_Implicit System.Int128.MinValue)
    (System.Numerics.BigInteger.op_Implicit System.Int128.MaxValue)
    (fun n -> DInt128(System.Numerics.BigInteger.op_Explicit n))
let private uint128Type =
  wide 0I (System.Numerics.BigInteger.op_Implicit System.UInt128.MaxValue) (fun n ->
    DUInt128(System.Numerics.BigInteger.op_Explicit n))

/// A JSON number read as an integer type.
let private integer
  (t : IntegerType)
  (typ : TypeReference)
  (path : JsonPath.JsonPath)
  (j : JsonElement)
  : Ply<Dval> =
  match j.TryGetInt64() with
  | true, n when n >= t.lo && n <= t.hi -> Ply(t.ofInt64 n)
  | true, _ -> raiseCantMatchWithType typ j path
  | false, _ ->
    match parseExactJsonInteger (j.GetRawText()) with
    | Some n when n >= t.min && n <= t.max -> Ply(t.ofBigint n)
    | _ -> raiseCantMatchWithType typ j path


/// What a record field of type `Option` parses from when the JSON leaves it out or is `null`.
let private noneJson : JsonElement = JsonDocument.Parse("{\"None\":[]}").RootElement

/// `{"Some":[..]}` or `{"None":[]}`, the tagged form, which still parses.
let private isTaggedOption (j : JsonElement) : bool =
  j.ValueKind = JsonValueKind.Object
  && (match j.EnumerateObject() |> Seq.map (fun p -> p.Name) |> Seq.toList with
      | [ "Some" ]
      | [ "None" ] -> true
      | _ -> false)


let parse
  (threadID : ThreadID)
  (types : Types)
  (typ : TypeReference)
  (str : string)
  : Ply<Result<Dval, ParseError.ParseError>> =

  let tst = TST.empty // TODO consider passing this in.. somehow?

  /// What <param typ> holds when it is `Option`, directly or through aliases.
  let rec optionOf (typ : TypeReference) : Ply<TypeReference option> =
    uply {
      match typ with
      | TCustomType({ resolved = Ok(FQTypeName.Package(Hash h)) }, [ inner ]) when
        h = PackageRefs.Type.Stdlib.option ()
        ->
        return Some inner
      | TCustomType({ resolved = Ok typeName }, typeArgs) ->
        match! Types.find types typeName with
        | Some({ definition = TypeDeclaration.Alias alias } as decl) ->
          return! optionOf (Types.substitute decl.typeParams typeArgs alias)
        | _ -> return None
      | _ -> return None
    }

  let rec convert
    (typ : TypeReference)
    (pathSoFar : JsonPath.JsonPath)
    (j : JsonElement)
    : Ply<Dval> =

    match typ, j.ValueKind with
    // basic types
    | TUnit, JsonValueKind.Null -> DUnit |> Ply

    | TBool, JsonValueKind.True -> DBool true |> Ply
    | TBool, JsonValueKind.False -> DBool false |> Ply

    | TInt8, JsonValueKind.Number -> integer int8Type typ pathSoFar j
    | TUInt8, JsonValueKind.Number -> integer uint8Type typ pathSoFar j
    | TInt16, JsonValueKind.Number -> integer int16Type typ pathSoFar j
    | TUInt16, JsonValueKind.Number -> integer uint16Type typ pathSoFar j
    | TInt32, JsonValueKind.Number -> integer int32Type typ pathSoFar j
    | TUInt32, JsonValueKind.Number -> integer uint32Type typ pathSoFar j
    | TInt64, JsonValueKind.Number -> integer int64Type typ pathSoFar j
    | TUInt64, JsonValueKind.Number -> integer uint64Type typ pathSoFar j
    | TInt128, JsonValueKind.Number -> integer int128Type typ pathSoFar j
    | TUInt128, JsonValueKind.Number -> integer uint128Type typ pathSoFar j
    | TBigInt, JsonValueKind.Number ->
      match jsonInteger j with
      | Some n -> Dval.bigInt n |> Ply
      | None -> raiseCantMatchWithType TBigInt j pathSoFar |> Ply

    | TFloat64, JsonValueKind.Number -> j.GetDouble() |> DFloat64 |> Ply
    | TFloat64, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat64 System.Double.NaN
      | "Infinity" -> DFloat64 System.Double.PositiveInfinity
      | "-Infinity" -> DFloat64 System.Double.NegativeInfinity
      | _ -> raiseCantMatchWithType TFloat64 j pathSoFar
      |> Ply

    | TFloat32, JsonValueKind.Number -> j.GetSingle() |> DFloat32 |> Ply
    | TFloat32, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat32 System.Single.NaN
      | "Infinity" -> DFloat32 System.Single.PositiveInfinity
      | "-Infinity" -> DFloat32 System.Single.NegativeInfinity
      | _ -> raiseCantMatchWithType TFloat32 j pathSoFar
      |> Ply

    | TDecimal, JsonValueKind.Number ->
      match parseExactJsonDecimal (j.GetRawText()) with
      | Some d -> Ply(DDecimal d)
      | None -> raiseCantMatchWithType TDecimal j pathSoFar |> Ply

    | TChar, JsonValueKind.String ->
      match String.toEgcChar (j.GetString()) with
      | Some c -> Ply(Dval.char c)
      | None -> raiseCantMatchWithType TChar j pathSoFar


    | TString, JsonValueKind.String ->
      // A lone escaped surrogate is valid JSON, but no string can hold it.
      try
        DString(String.normalizeOutside (j.GetString())) |> Ply
      with :? System.InvalidOperationException ->
        raiseCantMatchWithType TString j pathSoFar |> Ply

    | TUuid, JsonValueKind.String ->
      try
        DUuid(System.Guid(j.GetString())) |> Ply
      with _ ->
        raiseCantMatchWithType TUuid j pathSoFar

    | TDateTime, JsonValueKind.String ->
      try
        j.GetString()
        |> NodaTime.Instant.ofIsoString
        |> DarkDateTime.fromInstant
        |> DDateTime
        |> Ply
      with _ ->
        raiseCantMatchWithType TDateTime j pathSoFar


    // Nested types

    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray()
      |> Seq.mapi (fun i v -> convert nested (JsonPath.Part.Index i :: pathSoFar) v)
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (TypeChecker.DvalCreator.list threadID VT.unknownTODO)

    | TTuple(t1, t2, rest), JsonValueKind.Array ->
      let values = j.EnumerateArray() |> Seq.toList
      let types = t1 :: t2 :: rest
      if values.Length <> types.Length then raiseCantMatchWithType typ j pathSoFar

      List.zip types values
      |> List.mapi (fun i (t, v) -> convert t (JsonPath.Part.Index i :: pathSoFar) v)
      |> Ply.List.flatten
      |> Ply.map (fun mapped ->
        match mapped with
        | (d1 :: d2 :: rest) -> DTuple(d1, d2, rest)
        | _ ->
          Exception.raiseInternal "Shouldn't be possible due to length check" [])

    | TDict(TString, tDict), JsonValueKind.Object ->
      j.EnumerateObject()
      |> Seq.map (fun jp ->
        uply {
          let! converted =
            convert tDict (JsonPath.Part.Field jp.Name :: pathSoFar) jp.Value
          return (DString(String.normalizeOutside jp.Name), converted)
        })
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (TypeChecker.DvalCreator.dict threadID VT.string VT.unknownTODO)

    | TCustomType({ resolved = Ok(FQTypeName.Package(Hash h)) }, [ inner ]), _ when
      h = PackageRefs.Type.Stdlib.option () && not (isTaggedOption j)
      ->
      optionValue typ inner pathSoFar j

    | TCustomType({ resolved = Ok typeName }, _), jsonValueKind when
      (RangedInt.find typeName).IsSome
      ->
      let range = (RangedInt.find typeName).Value
      let number =
        if jsonValueKind <> JsonValueKind.Number then
          ValueNone
        else
          match jsonInteger j with
          | Some n when n >= int64Type.min && n <= int64Type.max ->
            ValueSome(int64 n)
          | _ -> ValueNone
      match number with
      | ValueNone -> raiseCantMatchWithType typ j pathSoFar
      | ValueSome n when n < range.min || n > range.max ->
        raiseError (
          ParseError.OutOfRange(range.what, n, range.min, range.max, pathSoFar)
        )
      | ValueSome n -> RangedInt.make typeName range n |> Ply

    | TCustomType({ resolved = Ok typeName }, typeArgs), jsonValueKind ->
      uply {
        let! typeArgsVT =
          typeArgs |> Ply.List.mapSequentially (TypeReference.toVT types tst)

        match! Types.find types typeName with
        | None ->
          return
            Exception.raiseInternal "Couldn't find type" [ "typeName", typeName ]

        | Some decl ->
          match decl.definition with
          | TypeDeclaration.Alias alias ->
            let aliasType = Types.substitute decl.typeParams typeArgs alias
            return! convert aliasType pathSoFar j

          | TypeDeclaration.Enum cases ->
            if jsonValueKind <> JsonValueKind.Object then
              do! raiseCantMatchWithType typ j pathSoFar

            let enumerated =
              j.EnumerateObject()
              |> Seq.map (fun jp -> (jp.Name, jp.Value))
              |> Seq.toList

            match enumerated with
            | [ (caseName, j) ] ->
              let matchingCase =
                match cases |> NEList.find (fun c -> c.name = caseName) with
                | Some c -> c
                | None ->
                  raiseError (
                    ParseError.EnumInvalidCasename(typ, caseName, pathSoFar)
                  )

              let casePath = JsonPath.Part.Field caseName :: pathSoFar

              // A case's fields come as an array; anything else, such as
              // `{"Some":"x"}`, can't match the enum
              if j.ValueKind <> JsonValueKind.Array then
                raiseCantMatchWithType typ j casePath

              let j = j.EnumerateArray() |> Seq.toList

              // If the field count is off, process whatever fields make sense
              // and then error afterwards
              let expectedFieldCount = List.length matchingCase.fields
              let actualFieldCount = List.length j
              let maxFields = min expectedFieldCount actualFieldCount
              let shortExpectedFields = List.take maxFields matchingCase.fields
              let shortActualFields = List.take maxFields j

              let! fields =
                List.zip shortExpectedFields shortActualFields
                |> List.mapWithIndex (fun i (typ, j) ->
                  let path = JsonPath.Part.Index i :: casePath
                  let typ = Types.substitute decl.typeParams typeArgs typ
                  convert typ path j)
                |> Ply.List.flatten

              if expectedFieldCount > actualFieldCount then
                let index = actualFieldCount // one higher than greatest index
                let expectedType =
                  List.getAt index matchingCase.fields
                  |> Exception.unwrapOptionInternal
                    "Can't find expected field"
                    [ "index", index
                      "expectedFields", matchingCase.fields
                      "actualFields", j ]
                return
                  raiseError (
                    ParseError.EnumMissingField(expectedType, index, casePath)
                  )
              else if expectedFieldCount < actualFieldCount then
                let index = expectedFieldCount // one higher than greatest index
                let fieldJson =
                  List.getAt index j
                  |> Exception.unwrapOptionInternal
                    "Can't find actual field"
                    [ "index", index
                      "expectedFields", matchingCase.fields
                      "actualFields", j ]
                let path = JsonPath.Part.Index index :: casePath
                return
                  raiseError (
                    ParseError.EnumExtraField(fieldJson.GetRawText(), path)
                  )
              else
                let! enum =
                  TypeChecker.DvalCreator.enum
                    types
                    threadID
                    tst
                    typeName
                    typeArgsVT
                    caseName
                    fields
                return enum


            | [] -> return raiseCantMatchWithType typ j pathSoFar
            | cases ->
              let caseNames = List.map Tuple2.first cases
              return
                raiseError (ParseError.EnumTooManyCases(typ, caseNames, pathSoFar))

          | TypeDeclaration.Record fields ->
            if jsonValueKind <> JsonValueKind.Object then
              do! raiseCantMatchWithType typ j pathSoFar

            let enumerated = j.EnumerateObject() |> Seq.toList

            // Extra fields are allowed, and so is leaving out an `Option` field, which
            // parses as `None`. Any other field has to be there, so go by the
            // definition's fields and look each one up in the JSON.
            let! fields =
              fields
              |> NEList.toList
              |> List.map (fun def ->
                uply {
                  let typ = Types.substitute decl.typeParams typeArgs def.typ
                  let path = JsonPath.Part.Field def.name :: pathSoFar
                  let! optionArg = optionOf typ
                  let! converted =
                    match
                      enumerated |> List.filter (fun v -> v.Name = def.name),
                      optionArg
                    with
                    | [], Some _ -> convert typ path noneJson
                    | [], None ->
                      raiseError (
                        ParseError.RecordMissingField(def.name, pathSoFar)
                      )
                    | [ field ], _ -> convert typ path field.Value
                    | _ ->
                      raiseError (
                        ParseError.RecordDuplicateField(def.name, pathSoFar)
                      )
                  return (def.name, converted)
                })
              |> Ply.List.flatten

            let! record =
              TypeChecker.DvalCreator.record
                types
                threadID
                tst
                typeName
                typeArgsVT
                (fields |> List.map (fun (n, v) -> struct (n, v)) |> Array.ofList)
            return record
      }


    // Explicitly not supported
    | TVariable _, _
    | TFn _, _
    | TBytes, _
    | TStream _, _ -> raiseError (ParseError.UnsupportedType typ)


    // exhaust TypeReferences
    | TUnit, _
    | TBool, _
    | TInt8, _
    | TUInt8, _
    | TInt16, _
    | TUInt16, _
    | TInt32, _
    | TUInt32, _
    | TInt64, _
    | TUInt64, _
    | TInt128, _
    | TUInt128, _
    | TBigInt, _
    | TFloat64, _
    | TFloat32, _
    | TDecimal, _
    | TChar, _
    | TString, _
    | TUuid, _
    | TDateTime, _
    | TTuple _, _
    | TList _, _
    | TDict _, _
    | TCustomType _, _ -> raiseCantMatchWithType typ j pathSoFar

  /// An `Option` not in the tagged form: `null` is `None`, and any other value is the `Some`.
  and optionValue
    (typ : TypeReference)
    (inner : TypeReference)
    (path : JsonPath.JsonPath)
    (j : JsonElement)
    : Ply<Dval> =
    if j.ValueKind = JsonValueKind.Null then
      convert typ path noneJson
    else
      uply {
        let! value = convert inner path j
        let! innerVT = TypeReference.toVT types tst inner
        return!
          TypeChecker.DvalCreator.enum
            types
            threadID
            tst
            (Dval.optionType ())
            [ innerVT ]
            "Some"
            [ value ]
      }

  let parsed =
    try
      Ok(parseJson str)
    with
    | :? System.Text.Json.JsonException as ex ->
      Error(notJsonAt str ex.LineNumber ex.BytePositionInLine)
    | _ -> Error(ParseError.NotJson(1L, 1L))

  match parsed with
  | Error err -> Error err |> Ply
  | Ok parsed ->
    uply {
      try
        let! converted = convert typ [ JsonPath.Part.Root ] parsed
        return Ok converted
      with ParseError.JsonException ex ->
        return Error ex
    }


let fns () : List<BuiltInFn> =
  [ { name = fn "jsonSerialize" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "arg" (TVariable "a") "" ]
      returnType = TString
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | _, vm, [ _typeToSerializeAs ], [| arg |] ->
          try
            Ply(DString(writeJson (fun w -> serialize w arg)))
          with CannotSerialize dv ->
            RTE.Jsons.CannotSerializeValue dv |> RTE.Json |> raiseRTE vm.threadID
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "jsonCheckedSerialize" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "arg" (TVariable "a") "" ]
      returnType =
        TypeReference.result
          TString
          (TCustomType(NR.ok (RT2DT.Dval.typeName ()), []))
      description =
        "Serializes a Dark value to a JSON string, or the first value found that has no JSON form."
      fn =
        (function
        | _, _, [ _typeToSerializeAs ], [| arg |] ->
          let errType = RT2DT.Dval.knownType ()
          try
            Ply(
              Dval.resultOk
                KTString
                errType
                (DString(writeJson (fun w -> serialize w arg)))
            )
          with CannotSerialize dv ->
            Ply(Dval.resultError KTString errType (RT2DT.Dval.toDT dv))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "jsonParse" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "json" TString "" ]
      returnType =
        TypeReference.result
          (TVariable "a")
          (TCustomType(NR.ok (ParseError.typeName ()), []))
      description =
        "Parses a JSON string <param json> as a Dark value, matching the type <typeParam a>"
      fn =
        (function
        | exeState, vm, [ typeArg ], [| DString arg |] ->
          let threadID = vm.threadID

          let okType = VT.unknownTODO // "a"
          let errType = KTCustomType(ParseError.typeName (), []) |> VT.known

          // Applied in full in each branch rather than partially applied into `resultOk` and
          // `resultError` up front. Both closures were built on every call and exactly one was
          // ever invoked.
          uply {
            match! parse threadID exeState.types typeArg arg with
            | Ok v ->
              return TypeChecker.DvalCreator.Result.ok threadID okType errType v
            | Error e ->
              return
                TypeChecker.DvalCreator.Result.error
                  threadID
                  okType
                  errType
                  (ParseError.toDT e)
          }
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
