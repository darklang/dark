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


let rec serialize (threadID : ThreadID) (w : Utf8JsonWriter) (dv : Dval) : unit =
  let r = serialize threadID w
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

  | DFloat f ->
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
  | DString s -> w.WriteStringValue s

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
        | _ -> (RTE.Jsons.CannotSerializeValue dv) |> RTE.Json |> raiseRTE threadID))

  // Enums and Records
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
          w.WritePropertyName fieldName
          r dval))

  // Not supported
  | DApplicable _
  | DBytes _
  | DStream _
  | DPromise _ ->
    (RTE.Jsons.CannotSerializeValue dv) |> RTE.Json |> raiseRTE threadID

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

  exception JsonException of ParseError

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | NotJson(line, column) -> "NotJson", [ Dval.dint64 line; Dval.dint64 column ]

      | CantMatchWithType(typ, json, errorPath) ->
        "CantMatchWithType",
        [ RT2DT.TypeReference.toDT typ; DString json; JsonPath.toDT errorPath ]

      | EnumExtraField(json, errorPath) ->
        "EnumExtraField", [ DString json; JsonPath.toDT errorPath ]

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
/// and exponent forms — `1.0`, `1.2E2`, `1200E-1`, `9007199254740993.0` — the
/// same forms the fixed-width int parsers accept, but exactly. Returns None for
/// genuinely fractional values (`1.5`) or malformed input.
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


/// A number written in float form (`1.0`, `1E+0`) whose value is a whole number
/// in [min, max]. JSON numbers are floats, and whoever wrote a field we read as an
/// int may have printed it that way.
let private integralDouble
  (j : JsonElement)
  (min : float)
  (max : float)
  : voption<float> =
  match j.TryGetDouble() with
  | true, d when d <= max && d >= min && System.Double.IsInteger d -> ValueSome d
  | _ -> ValueNone

/// What a record field of type `Option` parses from when the JSON leaves it out.
let private noneJson : JsonElement = JsonDocument.Parse("{\"None\":[]}").RootElement


let parse
  (threadID : ThreadID)
  (types : Types)
  (typ : TypeReference)
  (str : string)
  : Ply<Result<Dval, ParseError.ParseError>> =

  let tst = TST.empty // TODO consider passing this in.. somehow?

  /// Whether <param typ> is `Option`, directly or through aliases.
  let rec isOption (typ : TypeReference) : Ply<bool> =
    uply {
      match typ with
      | TCustomType({ resolved = Ok(FQTypeName.Package(Hash h)) }, _) when
        h = PackageRefs.Type.Stdlib.option ()
        ->
        return true
      | TCustomType({ resolved = Ok typeName }, typeArgs) ->
        match! Types.find types typeName with
        | Some({ definition = TypeDeclaration.Alias alias } as decl) ->
          return! isOption (Types.substitute decl.typeParams typeArgs alias)
        | _ -> return false
      | _ -> return false
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

    | TInt64, JsonValueKind.Number ->
      // dotnet will wrap 9223372036854775808 to be -9223372036854775808 instead, we
      // don't want that and will error instead
      match j.TryGetUInt64() with
      | true, ui64 when ui64 <= uint64 System.Int64.MaxValue ->
        DInt64(int64 ui64) |> Ply
      | true, _ -> raiseCantMatchWithType TInt64 j pathSoFar |> Ply
      | false, _ ->
        match j.TryGetInt64() with
        | true, i64 -> DInt64 i64 |> Ply
        | false, _ ->
          match
            integralDouble
              j
              (float System.Int64.MinValue)
              (float System.Int64.MaxValue)
          with
          | ValueSome d -> int64 d |> DInt64 |> Ply
          | ValueNone -> raiseCantMatchWithType TInt64 j pathSoFar |> Ply

    | TUInt64, JsonValueKind.Number ->
      // Parse decimal/exponent forms exactly. Double rounds UInt64.MaxValue
      // up to 2^64, accepting overflow and losing low bits near the boundary.
      match parseExactJsonInteger (j.GetRawText()) with
      | Some n when n >= 0I && n <= bigint System.UInt64.MaxValue ->
        DUInt64(uint64 n) |> Ply
      | _ -> raiseCantMatchWithType TUInt64 j pathSoFar |> Ply

    | TInt8, JsonValueKind.Number ->
      match j.TryGetUInt64() with
      | true, ui64 when
        ui64 >= uint64 System.SByte.MinValue && ui64 <= uint64 System.SByte.MaxValue
        ->
        DInt8(int8 ui64) |> Ply
      | _ ->
        match j.TryGetInt64() with
        | true, i64 when
          i64 >= int System.SByte.MinValue && i64 <= int System.SByte.MaxValue
          ->
          DInt8(int8 i64) |> Ply
        | _ ->
          match
            integralDouble
              j
              (float System.SByte.MinValue)
              (float System.SByte.MaxValue)
          with
          | ValueSome d -> int8 d |> DInt8 |> Ply
          | ValueNone -> raiseCantMatchWithType TInt8 j pathSoFar |> Ply

    | TUInt8, JsonValueKind.Number ->
      match j.TryGetUInt64() with
      | true, ui64 when ui64 <= uint64 System.Byte.MaxValue ->
        DUInt8(uint8 ui64) |> Ply
      | _ ->
        match j.TryGetInt64() with
        | true, i64 when
          i64 >= int System.Byte.MinValue && i64 <= int System.Byte.MaxValue
          ->
          DUInt8(uint8 i64) |> Ply
        | _ ->
          match
            integralDouble
              j
              (float System.Byte.MinValue)
              (float System.Byte.MaxValue)
          with
          | ValueSome d -> uint8 d |> DUInt8 |> Ply
          | ValueNone -> raiseCantMatchWithType TUInt8 j pathSoFar |> Ply

    | TInt16, JsonValueKind.Number ->
      match j.TryGetInt16() with
      | true, i16 -> DInt16 i16 |> Ply
      | false, _ ->
        match
          integralDouble
            j
            (float System.Int16.MinValue)
            (float System.Int16.MaxValue)
        with
        | ValueSome d -> int16 d |> DInt16 |> Ply
        | ValueNone -> raiseCantMatchWithType TInt16 j pathSoFar |> Ply

    | TUInt16, JsonValueKind.Number ->
      match j.TryGetUInt16() with
      | true, ui16 -> DUInt16 ui16 |> Ply
      | false, _ ->
        match
          integralDouble
            j
            (float System.UInt16.MinValue)
            (float System.UInt16.MaxValue)
        with
        | ValueSome d -> uint16 d |> DUInt16 |> Ply
        | ValueNone -> raiseCantMatchWithType TUInt16 j pathSoFar |> Ply

    | TInt32, JsonValueKind.Number ->
      match j.TryGetInt32() with
      | true, i32 -> DInt32 i32 |> Ply
      | false, _ ->
        match
          integralDouble
            j
            (float System.Int32.MinValue)
            (float System.Int32.MaxValue)
        with
        | ValueSome d -> int32 d |> DInt32 |> Ply
        | ValueNone -> raiseCantMatchWithType TInt32 j pathSoFar |> Ply

    | TUInt32, JsonValueKind.Number ->
      match j.TryGetUInt32() with
      | true, ui32 -> DUInt32 ui32 |> Ply
      | false, _ ->
        match
          integralDouble
            j
            (float System.UInt32.MinValue)
            (float System.UInt32.MaxValue)
        with
        | ValueSome d -> uint32 d |> DUInt32 |> Ply
        | ValueNone -> raiseCantMatchWithType TUInt32 j pathSoFar |> Ply


    | TInt128, JsonValueKind.Number ->
      match System.Int128.TryParse(j.GetRawText()) with
      | true, i128 -> DInt128 i128 |> Ply
      | false, _ ->
        match j.TryGetDouble() with
        | true, d when System.Double.IsInteger d ->
          try
            System.Int128.Parse(
              d.ToString("F0", System.Globalization.CultureInfo.InvariantCulture)
            )
            |> DInt128
            |> Ply
          with :? System.OverflowException ->
            raiseCantMatchWithType TInt128 j pathSoFar |> Ply
        | _ -> raiseCantMatchWithType TInt128 j pathSoFar |> Ply

    | TUInt128, JsonValueKind.Number ->
      match System.UInt128.TryParse(j.GetRawText()) with
      | true, ui128 -> DUInt128 ui128 |> Ply
      | false, _ ->
        match j.TryGetDouble() with
        | true, d when System.Double.IsInteger d ->
          try
            System.UInt128.Parse(
              d.ToString("F0", System.Globalization.CultureInfo.InvariantCulture)
            )
            |> DUInt128
            |> Ply
          with :? System.OverflowException ->
            raiseCantMatchWithType TUInt128 j pathSoFar |> Ply
        | _ -> raiseCantMatchWithType TUInt128 j pathSoFar |> Ply

    | TBigInt, JsonValueKind.Number ->
      // Accept integer-valued decimal/exponent forms (like the fixed-width
      // types do), but parse them EXACTLY — never through a double, which would
      // round values past 2^53 (e.g. `9007199254740993.0`).
      match parseExactJsonInteger (j.GetRawText()) with
      | Some i -> Dval.bigInt i |> Ply
      | None -> raiseCantMatchWithType TBigInt j pathSoFar |> Ply

    | TFloat, JsonValueKind.Number -> j.GetDouble() |> DFloat |> Ply
    | TFloat, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat System.Double.NaN
      | "Infinity" -> DFloat System.Double.PositiveInfinity
      | "-Infinity" -> DFloat System.Double.NegativeInfinity
      | _ -> raiseCantMatchWithType TFloat j pathSoFar
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
      | Some c -> Ply(DChar c)
      | None -> raiseCantMatchWithType TChar j pathSoFar


    | TString, JsonValueKind.String ->
      // A lone escaped surrogate is valid JSON, but no string can hold it.
      try
        DString(j.GetString()) |> Ply
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
          return (DString jp.Name, converted)
        })
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (TypeChecker.DvalCreator.dict threadID VT.string VT.unknownTODO)

    | TCustomType({ resolved = Ok typeName }, _), jsonValueKind when
      (RangedInt.find typeName).IsSome
      ->
      let range = (RangedInt.find typeName).Value
      let number =
        if jsonValueKind <> JsonValueKind.Number then
          ValueNone
        else
          match j.TryGetInt64() with
          | true, n -> ValueSome n
          | false, _ ->
            integralDouble
              j
              (float System.Int64.MinValue)
              (float System.Int64.MaxValue)
            |> ValueOption.map int64
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

              let j = j.EnumerateArray() |> Seq.toList


              let casePath = JsonPath.Part.Field caseName :: pathSoFar

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
                  let! correspondingValue =
                    uply {
                      match
                        enumerated |> List.filter (fun v -> v.Name = def.name)
                      with
                      | [] ->
                        match! isOption typ with
                        | true -> return noneJson
                        | false ->
                          return
                            raiseError (
                              ParseError.RecordMissingField(def.name, pathSoFar)
                            )
                      | [ matchingFieldDef ] -> return matchingFieldDef.Value
                      | _ ->
                        return
                          raiseError (
                            ParseError.RecordDuplicateField(def.name, pathSoFar)
                          )
                    }

                  let! converted =
                    convert
                      typ
                      (JsonPath.Part.Field def.name :: pathSoFar)
                      correspondingValue
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
    | TStream _, _ ->
      (RTE.Jsons.UnsupportedType typ) |> RTE.Json |> raiseRTE threadID


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
    | TFloat, _
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
          Ply(DString(writeJson (fun w -> serialize vm.threadID w arg)))
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
