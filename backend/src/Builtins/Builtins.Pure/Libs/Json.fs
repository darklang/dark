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
module Interpreter = LibExecution.Interpreter
module Traits = LibExecution.Traits


// parsing
let parseJson (s : string) : JsonElement =
  let options =
    new JsonDocumentOptions(
      CommentHandling = JsonCommentHandling.Skip,
      MaxDepth = System.Int32.MaxValue // infinite
    )

  JsonDocument.Parse(s, options).RootElement


// serialization
let writeJsonBytes (f : Utf8JsonWriter -> unit) : byte[] =
  let options =
    new JsonWriterOptions(
      Indented = false,
      SkipValidation = true,
      Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
      MaxDepth = System.Int32.MaxValue // infinite
    )

  let stream = new System.IO.MemoryStream()
  let w = new Utf8JsonWriter(stream, options)
  f w
  w.Flush()
  stream.ToArray()

let writeJson (f : Utf8JsonWriter -> unit) : string =
  UTF8.ofBytesUnsafe (writeJsonBytes f)

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

  /// The F# side of each type's `fromInt64`; `tests/stdlib/json/userTypes.dark` checks they agree.
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


/// `Stdlib.AltJson.Json`: any JSON value. Asked for by name in `parse<'a>`, it takes whatever is
/// there, so a record or dict can hold a field of mixed or unknown shape; `serialize` writes it back
/// as that JSON.
module AnyJson =
  let isIt (h : string) : bool = h = PackageRefs.Type.Stdlib.AltJson.json ()

  let typeName () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.AltJson.json ())

  let rec ofElement (j : JsonElement) : Dval =
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    let typeName = typeName ()
    let make caseName fields = DEnum(typeName, typeName, [], caseName, fields)
    match j.ValueKind with
    | JsonValueKind.Null -> make "Null" []
    | JsonValueKind.True -> make "Bool" [ DBool true ]
    | JsonValueKind.False -> make "Bool" [ DBool false ]
    | JsonValueKind.Number ->
      match j.TryGetInt64() with
      | true, n -> make "Integer" [ DInt64 n ]
      | false, _ -> make "Float" [ DFloat64(j.GetDouble()) ]
    | JsonValueKind.String ->
      make "String" [ DString(String.normalizeOutside (j.GetString())) ]
    | JsonValueKind.Array ->
      let items = j.EnumerateArray() |> Seq.map ofElement |> Seq.toList
      make "Array" [ DList(VT.customType typeName [], Items(items)) ]
    | _ ->
      let fields =
        j.EnumerateObject()
        |> Seq.map (fun p ->
          DTuple(DString(String.normalizeOutside p.Name), ofElement p.Value, []))
        |> Seq.toList
      make
        "Object"
        [ DList(VT.tuple VT.string (VT.customType typeName []) [], Items(fields)) ]


/// The value `serialize` found with no JSON form.
exception CannotSerialize of Dval

/// <param typ> with aliases followed.
let rec unalias (types : Types) (typ : TypeReference) : TypeReference =
  match typ with
  | TCustomType({ resolved = Ok typeName }, typeArgs) ->
    match Types.find types typeName with
    | Some({ definition = TypeDeclaration.Alias alias } as decl) ->
      unalias types (Types.substitute decl.typeParams typeArgs alias)
    | _ -> typ
  | _ -> typ

/// What <param typ> holds when it is `Option`, directly or through aliases.
let optionOf (types : Types) (typ : TypeReference) : TypeReference option =
  match unalias types typ with
  | TCustomType({ resolved = Ok(FQTypeName.Package(Hash h)) }, [ inner ]) when
    h = PackageRefs.Type.Stdlib.option ()
    ->
    Some inner
  | _ -> None

/// Whether a value of <param typ> can be written as `null`: `Unit`, or an `Option` that
/// writes `None` as `null`. An `Option` of such a type is written in the tagged form,
/// `{"Some":[..]}`, so that `Some(None)` and `None` stay apart.
let rec writesNull (types : Types) (typ : TypeReference) : bool =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  match unalias types typ with
  | TUnit -> true
  | typ ->
    match optionOf types typ with
    | Some inner -> not (writesNull types inner)
    | None -> false

/// <param typ>, or the value's own type where <param typ> is a type variable.
let private declared
  (types : Types)
  (typ : TypeReference)
  (dv : Dval)
  : TypeReference =
  match typ with
  | TVariable _ ->
    match LibExecution.RuntimeTypes.Dval.toValueType dv with
    | ValueType.Known kt -> TypeReference.fromKnownType kt
    | ValueType.Unknown -> typ
  | _ -> unalias types typ

let private unknownType = TVariable "_"

/// The declared types of an enum case's fields, or unknown ones.
let private caseFieldTypes
  (types : Types)
  (typ : TypeReference)
  (caseName : string)
  (fields : List<Dval>)
  : List<TypeReference> =
  let declaredFields =
    match typ with
    | TCustomType({ resolved = Ok typeName }, typeArgs) ->
      match Types.find types typeName with
      | Some({ definition = TypeDeclaration.Enum cases } as decl) ->
        match cases |> NEList.find (fun c -> c.name = caseName) with
        | Some c when List.length c.fields = List.length fields ->
          Some(c.fields |> List.map (Types.substitute decl.typeParams typeArgs))
        | _ -> None
      | _ -> None
    | _ -> None
  declaredFields
  |> Option.defaultWith (fun () -> fields |> List.map (fun _ -> unknownType))

/// The declared type of a record's field, or an unknown one.
let private recordFieldType
  (types : Types)
  (typ : TypeReference)
  : string -> TypeReference =
  match typ with
  | TCustomType({ resolved = Ok typeName }, typeArgs) ->
    match Types.find types typeName with
    | Some({ definition = TypeDeclaration.Record defs } as decl) ->
      fun name ->
        match defs |> NEList.find (fun d -> d.name = name) with
        | Some d -> Types.substitute decl.typeParams typeArgs d.typ
        | None -> unknownType
    | _ -> fun _ -> unknownType
  | _ -> fun _ -> unknownType

/// A record's field names in the order its type declares them, or None when the type is not
/// known; JSON objects are written in that order, as a reader of the type expects.
let private recordFieldOrder
  (types : Types)
  (typ : TypeReference)
  : Option<List<string>> =
  match typ with
  | TCustomType({ resolved = Ok typeName }, _) ->
    match Types.find types typeName with
    | Some({ definition = TypeDeclaration.Record defs }) ->
      defs |> NEList.toList |> List.map (fun d -> d.name) |> Some
    | _ -> None
  | _ -> None

/// A value whose type has a `ToJson` impl. `serialize` writes the one byte `0` for it, at
/// <c>at</c>, and what the impl returns replaces that.
type Hole = { at : int64; value : Dval; impl : Hash }

type Writing =
  {
    types : Types
    noneAsNull : bool
    /// The `ToJson` impl for a value, when its type has one.
    toJson : Dval -> Hash voption
    holes : ResizeArray<Hole>
  }

let private writeFloat64 (w : Utf8JsonWriter) (f : float) : unit =
  if System.Double.IsNaN f then w.WriteStringValue "NaN"
  else if System.Double.IsNegativeInfinity f then w.WriteStringValue "-Infinity"
  else if System.Double.IsPositiveInfinity f then w.WriteStringValue "Infinity"
  else w.WriteRawValue(floatToShortestString f)

/// Writes <param dv> as JSON, going by its declared type <param typ> where an `Option` is
/// written. An `Option` writes `None` as `null` and `Some(v)` as `v`, except where `v` itself
/// can write `null`: see <see cref="writesNull"/>. A `None` record field is left out, or with
/// <param noneAsNull> written as `null`; a field whose `Some` can be `null` leaves `None` out
/// either way, and writes `Some(v)` as `v`.
let rec serialize
  (ctx : Writing)
  (w : Utf8JsonWriter)
  (typ : TypeReference)
  (dv : Dval)
  : unit =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  let typ = declared ctx.types typ dv
  let impl =
    match dv with
    | DRecord _
    | DEnum _ when (optionOf ctx.types typ).IsNone -> ctx.toJson dv
    | _ -> ValueNone
  match impl with
  | ValueSome impl ->
    // the placeholder is the last byte once flushed, after any `,` before it
    w.WriteRawValue "0"
    w.Flush()
    ctx.holes.Add { at = w.BytesCommitted - 1L; value = dv; impl = impl }
  | ValueNone -> writeValue ctx w typ dv

and private writeValue
  (ctx : Writing)
  (w : Utf8JsonWriter)
  (typ : TypeReference)
  (dv : Dval)
  : unit =
  let r = serialize ctx w
  let types = ctx.types
  let noneAsNull = ctx.noneAsNull
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

  | DFloat64 f -> writeFloat64 w f

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
  | DTuple(d1, d2, rest) ->
    let items = d1 :: d2 :: rest
    let itemTypes =
      match typ with
      | TTuple(t1, t2, ts) when List.length ts = List.length rest -> t1 :: t2 :: ts
      | _ -> items |> List.map (fun _ -> unknownType)
    w.writeArray (fun () -> List.iter2 r itemTypes items)

  | DList(_, Items(items)) ->
    let itemType =
      match typ with
      | TList t -> t
      | _ -> unknownType
    w.writeArray (fun () -> List.iter (r itemType) items)

  | DDict(_, _, fields) ->
    let valueType =
      match typ with
      | TDict(_, t) -> t
      | _ -> unknownType
    // `iter` rather than `toList |> List.iter`: the latter builds a tuple and a cons per
    // field, for a list that is walked once and dropped. Both visit in ascending key order.
    w.writeObject (fun () ->
      fields
      |> DictMap.iter (fun k v ->
        match k with
        | DString k ->
          w.WritePropertyName k
          r valueType v
        | _ -> raise (CannotSerialize dv)))

  | DEnum(FQTypeName.Package(Hash h), _, _, caseName, fields) when AnyJson.isIt h ->
    match caseName, fields with
    | "Null", [] -> w.WriteNullValue()
    | "Bool", [ b ] -> r TBool b
    | "Integer", [ n ] -> r TInt64 n
    | "Float", [ f ] -> r TFloat64 f
    | "String", [ str ] -> r TString str
    | "Array", [ DList(_, Items(items)) ] ->
      w.writeArray (fun () -> List.iter (r typ) items)
    | "Object", [ DList(_, Items(pairs)) ] ->
      w.writeObject (fun () ->
        pairs
        |> List.iter (fun pair ->
          match pair with
          | DTuple(DString k, v, []) ->
            w.WritePropertyName k
            r typ v
          | _ -> raise (CannotSerialize dv)))
    | _ -> raise (CannotSerialize dv)

  // Enums and Records
  | DEnum(_, _, _, caseName, fields) ->
    let tagged (fieldTypes : List<TypeReference>) =
      w.writeObject (fun () ->
        w.WritePropertyName caseName
        w.writeArray (fun () -> List.iter2 r fieldTypes fields))
    match optionOf types typ, fields with
    | Some inner, _ when writesNull types inner ->
      tagged (fields |> List.map (fun _ -> inner))
    | Some inner, [ v ] -> r inner v
    | Some _, _ -> w.WriteNullValue()
    | None, _ -> tagged (caseFieldTypes types typ caseName fields)

  | DRecord(_, typeName, _, fields) ->
    match RangedInt.valueOf typeName fields with
    | ValueSome n -> w.WriteNumberValue n
    | ValueNone ->
      let fieldType = recordFieldType types typ
      // See `DDict` above.
      let field fieldName dval =
        let t = declared types (fieldType fieldName) dval
        match optionOf types t, dval with
        | Some inner, DEnum(_, _, _, "None", _) when
          not noneAsNull || writesNull types inner
          ->
          ()
        | Some inner, DEnum(_, _, _, "Some", [ v ]) ->
          w.WritePropertyName fieldName
          r inner v
        | _ ->
          w.WritePropertyName fieldName
          r t dval
      w.writeObject (fun () ->
        match recordFieldOrder types typ with
        | Some names when List.length names = RecordFields.count fields ->
          names
          |> List.iter (fun name ->
            match RecordFields.tryFind name fields with
            | Some dval -> field name dval
            | None -> raise (CannotSerialize dv))
        | _ -> fields |> RecordFields.iter field)

  // Not supported
  | DApplicable _
  | DBytes _
  | DStream _
  | DPromise _ -> raise (CannotSerialize dv)

/// Writes an `AltJson.Json` tree, as a `ToJson` impl returns it.
let rec writeTree (w : Utf8JsonWriter) (tree : Dval) : unit =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
  match tree with
  | DEnum(_, _, _, "Null", []) -> w.WriteNullValue()
  | DEnum(_, _, _, "Bool", [ DBool b ]) -> w.WriteBooleanValue b
  | DEnum(_, _, _, "Integer", [ DInt64 i ]) -> w.WriteNumberValue i
  | DEnum(_, _, _, "Float", [ DFloat64 f ]) -> writeFloat64 w f
  | DEnum(_, _, _, "String", [ DString s ]) -> w.WriteStringValue s
  | DEnum(_, _, _, "Array", [ DList(_, Items items) ]) ->
    w.writeArray (fun () -> items |> List.iter (writeTree w))
  | DEnum(_, _, _, "Object", [ DList(_, Items fields) ]) ->
    w.writeObject (fun () ->
      fields
      |> List.iter (fun field ->
        match field with
        | DTuple(DString k, v, []) ->
          w.WritePropertyName k
          writeTree w v
        | _ -> Exception.raiseInternal "not an AltJson field" [ "field", field ]))
  | _ -> Exception.raiseInternal "not an AltJson tree" [ "tree", tree ]

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
    /// The type's `FromJson` impl refused the JSON, saying why
    | Rejected of message : string * JsonPath.JsonPath

  exception JsonException of ParseError

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | NotJson(line, column) -> "NotJson", [ Dval.dint64 line; Dval.dint64 column ]

      | UnsupportedType typ -> "UnsupportedType", [ RT2DT.TypeReference.toDT typ ]

      | Rejected(message, errorPath) ->
        "Rejected", [ DString message; JsonPath.toDT errorPath ]

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
  : Dval =
  match j.TryGetInt64() with
  | true, n when n >= t.lo && n <= t.hi -> (t.ofInt64 n)
  | true, _ -> raiseCantMatchWithType typ j path
  | false, _ ->
    match parseExactJsonInteger (j.GetRawText()) with
    | Some n when n >= t.min && n <= t.max -> (t.ofBigint n)
    | _ -> raiseCantMatchWithType typ j path


/// `{"Some":[..]}` or `{"None":[..]}`, the tagged form of an `Option`.
let private isTaggedOption (j : JsonElement) : bool =
  j.ValueKind = JsonValueKind.Object
  && (match j.EnumerateObject() |> Seq.map (fun p -> p.Name) |> Seq.toList with
      | [ "Some" ]
      | [ "None" ] -> true
      | _ -> false)


/// JSON at a type with a `FromJson` impl, waiting for the impl's answer.
type FromJsonHole = { impl : Hash; json : JsonElement }

let holeKey (hole : FromJsonHole) : string * string =
  let (Hash impl) = hole.impl
  (impl, hole.json.GetRawText())

exception NeedsFromJson of FromJsonHole

type Reading =
  {
    /// The `FromJson` impl for a type, when it has one.
    fromJson : TypeReference -> Hash voption
    /// What each impl answered, by `holeKey`.
    answers : Map<string * string, Result<Dval, string>>
    /// While collecting, every hole without an answer goes here, and nothing is built.
    collecting : ResizeArray<FromJsonHole> option
  }

type Parsed =
  | Parsed of Result<Dval, ParseError.ParseError>
  /// The impls to ask before a parse can finish.
  | Waiting of List<FromJsonHole>

let parse
  (threadID : ThreadID)
  (types : Types)
  (reading : Reading)
  (typ : TypeReference)
  (root : JsonElement)
  : Parsed =

  let tst = TST.empty // TODO consider passing this in.. somehow?

  /// While collecting, a placeholder: only the holes are wanted.
  let build (f : unit -> Dval) : Dval =
    if reading.collecting.IsSome then DUnit else f ()

  let rec convert
    (typ : TypeReference)
    (pathSoFar : JsonPath.JsonPath)
    (j : JsonElement)
    : Dval =
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    match typ, j.ValueKind with
    // basic types
    | TUnit, JsonValueKind.Null -> DUnit
    | TBool, JsonValueKind.True -> DBool true
    | TBool, JsonValueKind.False -> DBool false
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
      | Some n -> Dval.bigInt n
      | None -> raiseCantMatchWithType TBigInt j pathSoFar
    | TFloat64, JsonValueKind.Number -> j.GetDouble() |> DFloat64
    | TFloat64, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat64 System.Double.NaN
      | "Infinity" -> DFloat64 System.Double.PositiveInfinity
      | "-Infinity" -> DFloat64 System.Double.NegativeInfinity
      | _ -> raiseCantMatchWithType TFloat64 j pathSoFar
    | TFloat32, JsonValueKind.Number -> j.GetSingle() |> DFloat32
    | TFloat32, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat32 System.Single.NaN
      | "Infinity" -> DFloat32 System.Single.PositiveInfinity
      | "-Infinity" -> DFloat32 System.Single.NegativeInfinity
      | _ -> raiseCantMatchWithType TFloat32 j pathSoFar
    | TDecimal, JsonValueKind.Number ->
      match parseExactJsonDecimal (j.GetRawText()) with
      | Some d -> (DDecimal d)
      | None -> raiseCantMatchWithType TDecimal j pathSoFar
    | TChar, JsonValueKind.String ->
      match String.toEgcChar (j.GetString()) with
      | Some c -> (Dval.char c)
      | None -> raiseCantMatchWithType TChar j pathSoFar


    | TString, JsonValueKind.String ->
      // A lone escaped surrogate is valid JSON, but no string can hold it.
      try
        DString(String.normalizeOutside (j.GetString()))
      with :? System.InvalidOperationException ->
        raiseCantMatchWithType TString j pathSoFar
    | TUuid, JsonValueKind.String ->
      try
        DUuid(System.Guid(j.GetString()))
      with _ ->
        raiseCantMatchWithType TUuid j pathSoFar

    | TDateTime, JsonValueKind.String ->
      try
        j.GetString()
        |> NodaTime.Instant.ofIsoString
        |> DarkDateTime.fromInstant
        |> DDateTime
      with _ ->
        raiseCantMatchWithType TDateTime j pathSoFar


    // Nested types

    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray()
      |> Seq.mapi (fun i v -> convert nested (JsonPath.Part.Index i :: pathSoFar) v)
      |> Seq.toList
      |> (fun items ->
        build (fun () -> TypeChecker.DvalCreator.list threadID VT.unknownTODO items))

    | TTuple(t1, t2, rest), JsonValueKind.Array ->
      let values = j.EnumerateArray() |> Seq.toList
      let types = t1 :: t2 :: rest
      if values.Length <> types.Length then raiseCantMatchWithType typ j pathSoFar

      List.zip types values
      |> List.mapi (fun i (t, v) -> convert t (JsonPath.Part.Index i :: pathSoFar) v)
      |> (fun mapped ->
        match mapped with
        | (d1 :: d2 :: rest) -> DTuple(d1, d2, rest)
        | _ ->
          Exception.raiseInternal "Shouldn't be possible due to length check" [])

    | TDict(TString, tDict), JsonValueKind.Object ->
      j.EnumerateObject()
      |> Seq.map (fun jp ->
        let converted =
          convert tDict (JsonPath.Part.Field jp.Name :: pathSoFar) jp.Value
        (DString(String.normalizeOutside jp.Name), converted))
      |> Seq.toList
      |> (fun entries ->
        build (fun () ->
          TypeChecker.DvalCreator.dict threadID VT.string VT.unknownTODO entries))

    | TCustomType({ resolved = Ok _ }, _), _ when (reading.fromJson typ).IsSome ->
      let hole = { impl = (reading.fromJson typ).Value; json = j }
      match Map.tryFind (holeKey hole) reading.answers, reading.collecting with
      | Some(Ok v), _ -> v
      | Some(Error message), _ ->
        raiseError (ParseError.Rejected(message, pathSoFar))
      | None, Some holes ->
        holes.Add hole
        DUnit
      | None, None -> raise (NeedsFromJson hole)

    | TCustomType({ resolved = Ok(FQTypeName.Package(Hash h)) }, []), _ when
      AnyJson.isIt h
      ->
      // a lone escaped surrogate is valid JSON, but no string can hold it
      try
        AnyJson.ofElement j
      with :? System.InvalidOperationException ->
        raiseCantMatchWithType typ j pathSoFar

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
      | ValueSome n -> RangedInt.make typeName range n
    | TCustomType({ resolved = Ok _ }, _), _ ->
      match optionOf types typ with
      | Some inner -> optionValue typ inner pathSoFar j
      | None -> customType typ pathSoFar j

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

  /// A custom type, with an `Option` read in the tagged form.
  and customType
    (typ : TypeReference)
    (pathSoFar : JsonPath.JsonPath)
    (j : JsonElement)
    : Dval =
    match typ with
    | TCustomType({ resolved = Ok typeName }, typeArgs) ->
      let jsonValueKind = j.ValueKind
      let typeArgsVT = typeArgs |> List.map (TypeReference.toVT types tst)

      match Types.find types typeName with
      | None -> Exception.raiseInternal "Couldn't find type" [ "typeName", typeName ]

      | Some decl ->
        match decl.definition with
        | TypeDeclaration.Alias alias ->
          let aliasType = Types.substitute decl.typeParams typeArgs alias
          convert aliasType pathSoFar j

        | TypeDeclaration.Enum cases ->
          if jsonValueKind <> JsonValueKind.Object then
            raiseCantMatchWithType typ j pathSoFar

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
                raiseError (ParseError.EnumInvalidCasename(typ, caseName, pathSoFar))

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

            let fields =
              List.zip shortExpectedFields shortActualFields
              |> List.mapWithIndex (fun i (typ, j) ->
                let path = JsonPath.Part.Index i :: casePath
                let typ = Types.substitute decl.typeParams typeArgs typ
                convert typ path j)

            if expectedFieldCount > actualFieldCount then
              let index = actualFieldCount // one higher than greatest index
              let expectedType =
                List.getAt index matchingCase.fields
                |> Exception.unwrapOptionInternal
                  "Can't find expected field"
                  [ "index", index
                    "expectedFields", matchingCase.fields
                    "actualFields", j ]
              raiseError (ParseError.EnumMissingField(expectedType, index, casePath))
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
              raiseError (ParseError.EnumExtraField(fieldJson.GetRawText(), path))
            else
              build (fun () ->
                TypeChecker.DvalCreator.enum
                  types
                  threadID
                  tst
                  typeName
                  typeArgsVT
                  caseName
                  fields)


          | [] -> raiseCantMatchWithType typ j pathSoFar
          | cases ->
            let caseNames = List.map Tuple2.first cases
            raiseError (ParseError.EnumTooManyCases(typ, caseNames, pathSoFar))

        | TypeDeclaration.Record fields ->
          if jsonValueKind <> JsonValueKind.Object then
            raiseCantMatchWithType typ j pathSoFar

          let enumerated = j.EnumerateObject() |> Seq.toList

          // Extra fields are allowed, and so is leaving out an `Option` field, which
          // parses as `None`. Any other field has to be there, so go by the
          // definition's fields and look each one up in the JSON.
          let fields =
            fields
            |> NEList.toList
            |> List.map (fun def ->
              let typ = Types.substitute decl.typeParams typeArgs def.typ
              let path = JsonPath.Part.Field def.name :: pathSoFar
              let optionArg = optionOf types typ
              let converted =
                match
                  enumerated |> List.filter (fun v -> v.Name = def.name), optionArg
                with
                | [], Some inner -> option inner "None" []
                | [], None ->
                  raiseError (ParseError.RecordMissingField(def.name, pathSoFar))
                | [ field ], Some inner -> optionField typ inner path field.Value
                | [ field ], None -> convert typ path field.Value
                | _ ->
                  raiseError (ParseError.RecordDuplicateField(def.name, pathSoFar))
              (def.name, converted))

          build (fun () ->
            TypeChecker.DvalCreator.record
              types
              threadID
              tst
              typeName
              typeArgsVT
              (fields |> List.map (fun (n, v) -> struct (n, v)) |> Array.ofList))
    | _ -> raiseCantMatchWithType typ j pathSoFar

  and option
    (inner : TypeReference)
    (caseName : string)
    (fields : List<Dval>)
    : Dval =
    build (fun () ->
      TypeChecker.DvalCreator.enum
        types
        threadID
        tst
        (Dval.optionType ())
        [ TypeReference.toVT types tst inner ]
        caseName
        fields)

  /// An `Option` that isn't a record field: `null` is `None` and any other value is the
  /// `Some`. Where the inner type can write `null` itself, only the tagged form is read.
  and optionValue
    (typ : TypeReference)
    (inner : TypeReference)
    (path : JsonPath.JsonPath)
    (j : JsonElement)
    : Dval =
    if writesNull types inner then customType typ path j
    else if j.ValueKind = JsonValueKind.Null then option inner "None" []
    else someOrTagged typ inner path j

  /// A present `Option` record field: `null` is `None`, or `Some` of the inner type's `null`
  /// where it has one, and any other value is the `Some`.
  and optionField
    (typ : TypeReference)
    (inner : TypeReference)
    (path : JsonPath.JsonPath)
    (j : JsonElement)
    : Dval =
    if j.ValueKind <> JsonValueKind.Null then someOrTagged typ inner path j
    else if writesNull types inner then option inner "Some" [ convert inner path j ]
    else option inner "None" []

  /// The value as the `Some`, or else the tagged form. The `Some` is tried first, so a
  /// record with a field named `Some` reads as that record.
  and someOrTagged
    (typ : TypeReference)
    (inner : TypeReference)
    (path : JsonPath.JsonPath)
    (j : JsonElement)
    : Dval =
    try
      option inner "Some" [ convert inner path j ]
    with ParseError.JsonException _ when isTaggedOption j ->
      customType typ path j

  try
    let converted = convert typ [ JsonPath.Part.Root ] root
    match reading.collecting with
    | Some holes -> Waiting(List.ofSeq holes)
    | None -> Parsed(Ok converted)
  with
  | ParseError.JsonException ex -> Parsed(Error ex)
  | NeedsFromJson hole -> Waiting [ hole ]


let parseText (str : string) : Result<JsonElement, ParseError.ParseError> =
  try
    Ok(parseJson str)
  with
  | :? System.Text.Json.JsonException as ex ->
    Error(notJsonAt str ex.LineNumber ex.BytePositionInLine)
  | _ -> Error(ParseError.NotJson(1L, 1L))


/// What a type's impl of <param traitHash> has as <param methodName>, or `ValueNone` when no
/// type on the branch implements the trait at all.
let private implFinder
  (exeState : ExecutionState)
  (traitHash : string)
  (methodName : string)
  : (KnownType -> Hash voption) voption =
  let trait_ = Hash traitHash
  match exeState.fns.implCandidates exeState.branchId trait_ with
  | [] -> ValueNone
  | candidates ->
    ValueSome(fun self ->
      match self with
      | KTCustomType _ ->
        match Traits.select candidates self with
        | Traits.Selected c ->
          match Map.tryFind methodName c.methods with
          | Some fnHash -> ValueSome fnHash
          | None -> ValueNone
        | Traits.NoImpl -> ValueNone
        | Traits.Ambiguous cs ->
          RTE.Trait(
            RTE.Traits.DispatchAmbiguous(
              FQTraitName.Package trait_,
              VT.known self,
              cs |> List.map (fun c -> c.source)
            )
          )
          |> raiseRTE
      | _ -> ValueNone)

/// Calls the impl method <param impl> on <param arg>, and hands its answer to <param next>.
let private applyImpl
  (vm : VMState)
  (impl : Hash)
  (arg : Dval)
  (next : Dval -> Dval)
  : Dval =
  let fn =
    AppNamedFn
      { name = FQFnName.Package impl
        typeSymbolTable = TST.empty
        typeArgs = []
        approvalName = None
        access = None
        argsSoFar = []
        boundImpls = [] }
  Interpreter.requestApply vm fn arg ValueNone (fun answer ->
    Interpreter.withValue vm answer next)

/// <param arg> as JSON, handed to <param next> once each `ToJson` impl it reaches has answered.
let private serializeThen
  (exeState : ExecutionState)
  (vm : VMState)
  (noneAsNull : bool)
  (typ : TypeReference)
  (arg : Dval)
  (next : string -> Dval)
  : Dval =
  let toJson =
    match implFinder exeState (PackageRefs.Trait.Stdlib.Json.toJson ()) "toJson" with
    | ValueNone -> fun _ -> ValueNone
    | ValueSome find ->
      fun dv ->
        match LibExecution.RuntimeTypes.Dval.toValueType dv with
        | ValueType.Known self -> find self
        | ValueType.Unknown -> ValueNone
  let ctx =
    { types = exeState.types
      noneAsNull = noneAsNull
      toJson = toJson
      holes = ResizeArray() }
  let bytes = writeJsonBytes (fun w -> serialize ctx w typ arg)
  let rec fill (holes : List<Hole>) (from : int64) (parts : List<string>) : Dval =
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    match holes with
    | [] when from = 0L -> next (UTF8.ofBytesUnsafe bytes)
    | [] ->
      let last = UTF8.ofBytesUnsafe bytes[int from ..]
      next (String.concat "" (List.rev (last :: parts)))
    | hole :: rest ->
      applyImpl vm hole.impl hole.value (fun tree ->
        let before = UTF8.ofBytesUnsafe bytes[int from .. int hole.at - 1]
        let written = writeJson (fun w -> writeTree w tree)
        fill rest (hole.at + 1L) (written :: before :: parts))
  fill (List.ofSeq ctx.holes) 0L []

/// <param root> read as <param typ>, handed to <param next> once each `FromJson` impl it
/// reaches has answered. A read that reaches an impl not yet asked starts over with its
/// answer; a pass that only collects the rest first asks them all at once.
let private parseThen
  (exeState : ExecutionState)
  (vm : VMState)
  (typ : TypeReference)
  (root : JsonElement)
  (next : Result<Dval, ParseError.ParseError> -> Dval)
  : Dval =
  let fromJson =
    match
      implFinder exeState (PackageRefs.Trait.Stdlib.Json.fromJson ()) "fromJson"
    with
    | ValueNone -> fun _ -> ValueNone
    | ValueSome find ->
      fun typ ->
        match TypeReference.toVT exeState.types TST.empty typ with
        | ValueType.Known self -> find self
        | ValueType.Unknown -> ValueNone
  let read answers collecting =
    parse
      vm.threadID
      exeState.types
      { fromJson = fromJson; answers = answers; collecting = collecting }
      typ
      root
  let rec attempt answers =
    match read answers None with
    | Parsed result -> next result
    | Waiting first ->
      let collected =
        match read answers (Some(ResizeArray())) with
        | Waiting holes -> holes
        | Parsed _ -> []
      let fresh =
        first @ collected
        |> List.distinctBy holeKey
        |> List.filter (fun h -> not (Map.containsKey (holeKey h) answers))
      ask fresh answers
  and ask holes answers =
    System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()
    match holes with
    | [] -> attempt answers
    | hole :: rest ->
      applyImpl vm hole.impl (AnyJson.ofElement hole.json) (fun answer ->
        let answer =
          match answer with
          | DEnum(_, _, _, "Ok", [ v ]) -> Ok v
          | DEnum(_, _, _, "Error", [ DString message ]) -> Error message
          | other ->
            Exception.raiseInternal
              "FromJson answered a non-Result"
              [ "answer", other ]
        ask rest (Map.add (holeKey hole) answer answers))
  attempt Map.empty


let private optionsType () : TypeReference =
  TCustomType(
    NR.ok (FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Json.options ())),
    []
  )

/// Whether a `Json.Options` says to write a `None` record field as `null`.
let private noneAsNull (options : Dval) : bool =
  match options with
  | DRecord(_, _, _, fields) ->
    match RecordFields.tryFind "noneFields" fields with
    | Some(DEnum(_, _, _, "Null", [])) -> true
    | _ -> false
  | _ -> false


let fns () : List<BuiltInFn> =
  [ { name = fn "jsonSerialize" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "options" (optionsType ()) ""
          Param.make "arg" (TVariable "a") "" ]
      returnType = TString
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | exeState, vm, [ typ ], [| options; arg |] ->
          let noneAsNull = noneAsNull options
          try
            serializeThen exeState vm noneAsNull typ arg DString
          with CannotSerialize dv ->
            RTE.Jsons.CannotSerializeValue dv |> RTE.Json |> raiseRTE
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = set [ LibExecution.Effects.Effect.Raise ]
      deprecated = NotDeprecated }


    { name = fn "jsonCheckedSerialize" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "options" (optionsType ()) ""
          Param.make "arg" (TVariable "a") "" ]
      returnType =
        TypeReference.result
          TString
          (TCustomType(NR.ok (RT2DT.Dval.typeName ()), []))
      description =
        "Serializes a Dark value to a JSON string, or the first value found that has no JSON form."
      fn =
        (function
        | exeState, vm, [ typ ], [| options; arg |] ->
          let noneAsNull = noneAsNull options
          let errType = RT2DT.Dval.knownType ()
          try
            serializeThen exeState vm noneAsNull typ arg (fun json ->
              Dval.resultOk KTString errType (DString json))
          with CannotSerialize dv ->
            (Dval.resultError KTString errType (RT2DT.Dval.toDT dv))
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
          let okType = VT.unknownTODO // "a"
          let errType = KTCustomType(ParseError.typeName (), []) |> VT.known

          // Applied in full in each branch rather than partially applied into `resultOk` and
          // `resultError` up front. Both closures were built on every call and exactly one was
          // ever invoked.
          let finish result =
            match result with
            | Ok v -> TypeChecker.DvalCreator.Result.ok okType errType v
            | Error e ->
              TypeChecker.DvalCreator.Result.error okType errType (ParseError.toDT e)
          match parseText arg with
          | Error e -> finish (Error e)
          | Ok root -> parseThen exeState vm typeArg root finish
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
