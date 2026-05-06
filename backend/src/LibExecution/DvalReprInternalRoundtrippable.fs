/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in `darklang serve`,
/// in HttpClient, etc.
module LibExecution.DvalReprInternalRoundtrippable

open Prelude

// Note: we intentionally don't `open` RT here, so that we don't accidentally
// confuse the types defined here and the types defined in RT.
module RT = RuntimeTypes


module FormatV0 =
  // In the past, we used bespoke serialization formats, that were terrifying to
  // change. Going forward, if we want to use a format that we want to save and
  // reload, but don't need to search for, let's just use the simplest possible
  // format, using standard serializers.

  // We create our own format here because:
  // 1. we don't want to serialize some things, such as lambdas. Our own type allows
  //    us to be careful
  // 2. This needs to be backwards compatible, but we don't want to constrain how we
  //    change RT.Dval.

  module FQTypeName =
    type Package = RT.Hash

    type FQTypeName = Package of Package

    let toRT (t : FQTypeName) : RT.FQTypeName.FQTypeName =
      match t with
      | Package h -> RT.FQTypeName.Package h

    let fromRT (t : RT.FQTypeName.FQTypeName) : FQTypeName =
      match t with
      | RT.FQTypeName.Package h -> FQTypeName.Package h



  module rec ValueType =
    module KnownType =
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

        | KTFloat

        | KTChar
        | KTString

        | KTUuid

        | KTDateTime

        | KTBlob

        | KTStream of ValueType

        | KTTuple of ValueType * ValueType * List<ValueType>
        | KTList of ValueType
        | KTDict of ValueType

        | KTCustomType of FQTypeName.FQTypeName * typeArgs : List<ValueType>

        | KTFn of NEList<ValueType> * ValueType

        | KTDB of ValueType

      let rec toRT (kt : KnownType) : RT.KnownType =
        match kt with
        | KTUnit -> RT.KTUnit
        | KTBool -> RT.KTBool
        | KTInt64 -> RT.KTInt64
        | KTUInt64 -> RT.KTUInt64
        | KTInt8 -> RT.KTInt8
        | KTUInt8 -> RT.KTUInt8
        | KTInt16 -> RT.KTInt16
        | KTUInt16 -> RT.KTUInt16
        | KTInt32 -> RT.KTInt32
        | KTUInt32 -> RT.KTUInt32
        | KTInt128 -> RT.KTInt128
        | KTUInt128 -> RT.KTUInt128
        | KTFloat -> RT.KTFloat
        | KTChar -> RT.KTChar
        | KTString -> RT.KTString
        | KTUuid -> RT.KTUuid
        | KTDateTime -> RT.KTDateTime
        | KTBlob -> RT.KTBlob
        | KTStream vt -> RT.KTStream(ValueType.toRT vt)

        | KTList vt -> RT.KTList(ValueType.toRT vt)
        | KTTuple(vt1, vt2, vts) ->
          RT.KTTuple(
            ValueType.toRT vt1,
            ValueType.toRT vt2,
            List.map ValueType.toRT vts
          )
        | KTDict vt -> RT.KTDict(ValueType.toRT vt)

        | KTFn(argTypes, returnType) ->
          RT.KTFn(NEList.map ValueType.toRT argTypes, ValueType.toRT returnType)

        | KTCustomType(typeName, typeArgs) ->
          RT.KTCustomType(FQTypeName.toRT typeName, List.map ValueType.toRT typeArgs)

        | KTDB vt -> RT.KTDB(ValueType.toRT vt)

      let rec fromRT (kt : RT.KnownType) : KnownType =
        match kt with
        | RT.KTUnit -> KTUnit
        | RT.KTBool -> KTBool
        | RT.KTInt64 -> KTInt64
        | RT.KTUInt64 -> KTUInt64
        | RT.KTInt8 -> KTInt8
        | RT.KTUInt8 -> KTUInt8
        | RT.KTInt16 -> KTInt16
        | RT.KTUInt16 -> KTUInt16
        | RT.KTInt32 -> KTInt32
        | RT.KTUInt32 -> KTUInt32
        | RT.KTInt128 -> KTInt128
        | RT.KTUInt128 -> KTUInt128
        | RT.KTFloat -> KTFloat
        | RT.KTChar -> KTChar
        | RT.KTString -> KTString
        | RT.KTUuid -> KTUuid
        | RT.KTDateTime -> KTDateTime
        | RT.KTBlob -> KTBlob
        | RT.KTStream vt -> KTStream(ValueType.fromRT vt)

        | RT.KTList vt -> KTList(ValueType.fromRT vt)
        | RT.KTTuple(vt1, vt2, vts) ->
          KTTuple(
            ValueType.fromRT vt1,
            ValueType.fromRT vt2,
            List.map ValueType.fromRT vts
          )
        | RT.KTDict vt -> KTDict(ValueType.fromRT vt)

        | RT.KTFn(argTypes, returnType) ->
          KTFn(NEList.map ValueType.fromRT argTypes, ValueType.fromRT returnType)

        | RT.KTCustomType(typeName, typeArgs) ->
          KTCustomType(
            FQTypeName.fromRT typeName,
            List.map ValueType.fromRT typeArgs
          )

        | RT.KTDB vt -> KTDB(ValueType.fromRT vt)

    [<RequireQualifiedAccess>]
    type ValueType =
      | Unknown
      | Known of KnownType.KnownType

    let toRT (vt : ValueType) : RT.ValueType =
      match vt with
      | ValueType.Unknown -> RT.ValueType.Unknown
      | ValueType.Known kt -> RT.ValueType.Known(ValueType.KnownType.toRT kt)

    let fromRT (vt : RT.ValueType) : ValueType =
      match vt with
      | RT.ValueType.Unknown -> ValueType.ValueType.Unknown
      | RT.ValueType.Known kt ->
        ValueType.ValueType.Known(ValueType.KnownType.fromRT kt)

  let valueTypeTODO = ValueType.ValueType.Unknown


  type DvalMap = Map<string, Dval>

  and Dval =
    | DUnit
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
    | DFloat of double
    | DChar of string
    | DString of string
    | DDateTime of NodaTime.LocalDateTime
    | DUuid of System.Guid
    | DTuple of Dval * Dval * List<Dval>
    | DList of ValueType.ValueType * List<Dval>
    | DDict of ValueType.ValueType * DvalMap
    | DRecord of
      runtimeTypeName : FQTypeName.FQTypeName *
      sourceTypeName : FQTypeName.FQTypeName *
      typeArgs : List<ValueType.ValueType> *
      fields : DvalMap
    | DEnum of
      runtimeTypeName : FQTypeName.FQTypeName *
      sourceTypeName : FQTypeName.FQTypeName *
      typeArgs : List<ValueType.ValueType> *
      caseName : string *
      fields : List<Dval>
    | DLambda // See docs/dblock-serialization.md
    | DDB of string
    | DBlobPersistent of hash : string * length : int64
    | DBlobEphemeral of id : System.Guid
    // DStream is not persistable — this tag is a stub that exists only
    // so the exhaustiveness check holds; `toRT` raises on it.
    | DStreamStub


  let rec toRT (dv : Dval) : RT.Dval =
    match dv with
    | DUnit -> RT.DUnit

    | DBool b -> RT.DBool b

    | DInt8 i -> RT.DInt8 i
    | DUInt8 i -> RT.DUInt8 i
    | DInt16 i -> RT.DInt16 i
    | DUInt16 i -> RT.DUInt16 i
    | DInt32 i -> RT.DInt32 i
    | DUInt32 i -> RT.DUInt32 i
    | DInt64 i -> RT.DInt64 i
    | DUInt64 i -> RT.DUInt64 i
    | DInt128 i -> RT.DInt128 i
    | DUInt128 i -> RT.DUInt128 i

    | DFloat f -> RT.DFloat f

    | DChar c -> RT.DChar c
    | DString s -> RT.DString s

    | DDateTime d -> RT.DDateTime d

    | DUuid uuid -> RT.DUuid uuid

    | DTuple(first, second, theRest) ->
      RT.DTuple(toRT first, toRT second, List.map toRT theRest)

    | DList(typ, l) -> RT.DList(ValueType.toRT typ, List.map toRT l)

    | DDict(typ, entries) -> RT.DDict(ValueType.toRT typ, Map.map toRT entries)


    | DRecord(typeName, original, typeArgs, o) ->
      RT.DRecord(
        FQTypeName.toRT typeName,
        FQTypeName.toRT original,
        List.map ValueType.toRT typeArgs,
        Map.map toRT o
      )

    | DEnum(typeName, original, typeArgs, caseName, fields) ->
      RT.DEnum(
        FQTypeName.toRT typeName,
        FQTypeName.toRT original,
        List.map ValueType.toRT typeArgs,
        caseName,
        List.map toRT fields
      )

    | DLambda ->
      RT.DApplicable(
        RT.AppLambda
          { exprId = gid ()
            closedRegisters = []
            argsSoFar = []
            typeSymbolTable = Map.empty }
      )

    | DDB name -> RT.DDB name

    | DBlobPersistent(hash, length) -> RT.DBlob(RT.Persistent(hash, length))
    | DBlobEphemeral id -> RT.DBlob(RT.Ephemeral id)
    | DStreamStub ->
      Exception.raiseInternal
        "DStream is not persistable — can't deserialize a stub to a live stream"
        []



  let rec fromRT (dv : RT.Dval) : Dval =
    match dv with
    | RT.DUnit -> DUnit

    | RT.DBool b -> DBool b

    | RT.DInt8 i -> DInt8 i
    | RT.DUInt8 i -> DUInt8 i
    | RT.DInt16 i -> DInt16 i
    | RT.DUInt16 i -> DUInt16 i
    | RT.DInt32 i -> DInt32 i
    | RT.DUInt32 i -> DUInt32 i
    | RT.DInt64 i -> DInt64 i
    | RT.DUInt64 i -> DUInt64 i
    | RT.DInt128 i -> DInt128 i
    | RT.DUInt128 i -> DUInt128 i

    | RT.DFloat f -> DFloat f

    | RT.DChar c -> DChar c
    | RT.DString s -> DString s

    | RT.DDateTime d -> DDateTime d

    | RT.DUuid uuid -> DUuid uuid

    | RT.DTuple(first, second, theRest) ->
      DTuple(fromRT first, fromRT second, List.map fromRT theRest)
    | RT.DList(typ, l) -> DList(ValueType.fromRT typ, List.map fromRT l)
    | RT.DDict(typ, entries) -> DDict(ValueType.fromRT typ, Map.map fromRT entries)

    | RT.DRecord(typeName, original, typeArgs, o) ->
      DRecord(
        FQTypeName.fromRT typeName,
        FQTypeName.fromRT original,
        List.map ValueType.fromRT typeArgs,
        Map.map fromRT o
      )

    | RT.DEnum(typeName, original, typeArgs, caseName, fields) ->
      DEnum(
        FQTypeName.fromRT typeName,
        FQTypeName.fromRT original,
        List.map ValueType.fromRT typeArgs,
        caseName,
        List.map fromRT fields
      )

    | RT.DApplicable _ -> DLambda

    | RT.DDB name -> DDB name

    | RT.DBlob(RT.Persistent(hash, length)) -> DBlobPersistent(hash, length)
    | RT.DBlob(RT.Ephemeral id) -> DBlobEphemeral id
    | RT.DStream _ ->
      // Streams aren't persistable by design. For the rt_dval column
      // that's strictly a no-op target (a stream can't live past its
      // VM anyway), so we emit a stub that round-trips to an error on
      // read-back rather than raising at capture time. This lets the
      // trace pipeline (which captures every intermediate dval) pass
      // a stream through without aborting the eval.
      DStreamStub


  // -- Hand-rolled Utf8Json for FormatV0.Dval --
  //
  // Replaces `Json.Vanilla.serialize`/`deserialize<T>`, which routes
  // through F#'s reflective union-converter and breaks under AOT
  // trimming (the case-name lookup on FormatV0.Dval throws when case
  // metadata is pruned).
  //
  // Wire format: each union value is a JSON array `[tag, ...args]`,
  // and nullary cases are just `"tag"` strings. Not wire-compatible
  // with the prior FSharp.SystemTextJson ExternalTag output — trace
  // data captured under the prior format won't deserialize. Trace
  // data is per-machine local; lose-and-regrow.

  open System.Text.Json

  // Writer helpers — wrap `[tag, ...]` so individual case branches
  // become a single line each.
  let inline private wTagged0 (w : Utf8JsonWriter) (tag : string) : unit =
    w.WriteStringValue(tag)

  let inline private wArr (w : Utf8JsonWriter) (body : unit -> unit) : unit =
    w.WriteStartArray()
    body ()
    w.WriteEndArray()

  let inline private wObj (w : Utf8JsonWriter) (body : unit -> unit) : unit =
    w.WriteStartObject()
    body ()
    w.WriteEndObject()

  let inline private wTagStr
    (w : Utf8JsonWriter)
    (tag : string)
    (s : string)
    : unit =
    wArr w (fun () ->
      w.WriteStringValue(tag)
      w.WriteStringValue(s))

  let inline private wTagBool (w : Utf8JsonWriter) (tag : string) (b : bool) : unit =
    wArr w (fun () ->
      w.WriteStringValue(tag)
      w.WriteBooleanValue(b))

  let inline private wTagNum (w : Utf8JsonWriter) (tag : string) (n : int) : unit =
    wArr w (fun () ->
      w.WriteStringValue(tag)
      w.WriteNumberValue(n))

  // Reader helpers
  let inline private rNext (r : byref<Utf8JsonReader>) : unit =
    if not (r.Read()) then Exception.raiseInternal "Unexpected end of JSON" []

  let private rExpectStartArray (r : byref<Utf8JsonReader>) : unit =
    if r.TokenType <> JsonTokenType.StartArray then
      Exception.raiseInternal "Expected JSON [" [ "tokenType", r.TokenType ]

  let private rExpectEndArray (r : byref<Utf8JsonReader>) : unit =
    if r.TokenType <> JsonTokenType.EndArray then
      Exception.raiseInternal "Expected JSON ]" [ "tokenType", r.TokenType ]

  let private rTag (r : byref<Utf8JsonReader>) : string * bool =
    // Returns (tag, isNullary). Nullary is `"Tag"`; compound is `[Tag, ...]`.
    if r.TokenType = JsonTokenType.String then
      r.GetString(), true
    else
      rExpectStartArray &r
      rNext &r
      if r.TokenType <> JsonTokenType.String then
        Exception.raiseInternal "Expected tag" [ "tokenType", r.TokenType ]
      r.GetString(), false

  let private rString (r : byref<Utf8JsonReader>) : string =
    rNext &r
    if r.TokenType <> JsonTokenType.String then
      Exception.raiseInternal "Expected JSON string" [ "tokenType", r.TokenType ]
    r.GetString()

  let private rBool (r : byref<Utf8JsonReader>) : bool =
    rNext &r
    match r.TokenType with
    | JsonTokenType.True -> true
    | JsonTokenType.False -> false
    | t -> Exception.raiseInternal "Expected JSON bool" [ "tokenType", t ]

  // -- FQTypeName / Hash --

  let private wHash (w : Utf8JsonWriter) (RT.Hash s : RT.Hash) : unit =
    w.WriteStringValue(s)

  let private rHash (r : byref<Utf8JsonReader>) : RT.Hash = rString &r |> RT.Hash

  let private wFQTypeName (w : Utf8JsonWriter) (n : FQTypeName.FQTypeName) : unit =
    match n with
    | FQTypeName.Package h ->
      wArr w (fun () ->
        w.WriteStringValue("Package")
        wHash w h)

  // -- ValueType + KnownType (mutually recursive) --

  let rec private wKnownType
    (w : Utf8JsonWriter)
    (kt : ValueType.KnownType.KnownType)
    : unit =
    let nullary = wTagged0 w
    match kt with
    | ValueType.KnownType.KTUnit -> nullary "KTUnit"
    | ValueType.KnownType.KTBool -> nullary "KTBool"
    | ValueType.KnownType.KTInt8 -> nullary "KTInt8"
    | ValueType.KnownType.KTUInt8 -> nullary "KTUInt8"
    | ValueType.KnownType.KTInt16 -> nullary "KTInt16"
    | ValueType.KnownType.KTUInt16 -> nullary "KTUInt16"
    | ValueType.KnownType.KTInt32 -> nullary "KTInt32"
    | ValueType.KnownType.KTUInt32 -> nullary "KTUInt32"
    | ValueType.KnownType.KTInt64 -> nullary "KTInt64"
    | ValueType.KnownType.KTUInt64 -> nullary "KTUInt64"
    | ValueType.KnownType.KTInt128 -> nullary "KTInt128"
    | ValueType.KnownType.KTUInt128 -> nullary "KTUInt128"
    | ValueType.KnownType.KTFloat -> nullary "KTFloat"
    | ValueType.KnownType.KTChar -> nullary "KTChar"
    | ValueType.KnownType.KTString -> nullary "KTString"
    | ValueType.KnownType.KTUuid -> nullary "KTUuid"
    | ValueType.KnownType.KTDateTime -> nullary "KTDateTime"
    | ValueType.KnownType.KTBlob -> nullary "KTBlob"
    | ValueType.KnownType.KTStream vt ->
      wArr w (fun () ->
        w.WriteStringValue("KTStream")
        wValueType w vt)
    | ValueType.KnownType.KTList vt ->
      wArr w (fun () ->
        w.WriteStringValue("KTList")
        wValueType w vt)
    | ValueType.KnownType.KTTuple(a, b, rest) ->
      wArr w (fun () ->
        w.WriteStringValue("KTTuple")
        wValueType w a
        wValueType w b
        wArr w (fun () ->
          for vt in rest do
            wValueType w vt))
    | ValueType.KnownType.KTDict vt ->
      wArr w (fun () ->
        w.WriteStringValue("KTDict")
        wValueType w vt)
    | ValueType.KnownType.KTCustomType(typeName, typeArgs) ->
      wArr w (fun () ->
        w.WriteStringValue("KTCustomType")
        wFQTypeName w typeName
        wArr w (fun () ->
          for vt in typeArgs do
            wValueType w vt))
    | ValueType.KnownType.KTFn(argTypes, retType) ->
      wArr w (fun () ->
        w.WriteStringValue("KTFn")
        // NEList → [head, ...tail]
        wArr w (fun () ->
          wValueType w argTypes.head
          for vt in argTypes.tail do
            wValueType w vt)
        wValueType w retType)
    | ValueType.KnownType.KTDB vt ->
      wArr w (fun () ->
        w.WriteStringValue("KTDB")
        wValueType w vt)

  and private wValueType (w : Utf8JsonWriter) (vt : ValueType.ValueType) : unit =
    match vt with
    | ValueType.ValueType.Unknown -> w.WriteStringValue("Unknown")
    | ValueType.ValueType.Known kt ->
      wArr w (fun () ->
        w.WriteStringValue("Known")
        wKnownType w kt)

  let rec private rKnownType
    (r : byref<Utf8JsonReader>)
    : ValueType.KnownType.KnownType =
    // Advance from the outer "Known" tag to the actual KT* element.
    rNext &r
    let tag, _ = rTag &r
    match tag with
    | "KTUnit" -> ValueType.KnownType.KTUnit
    | "KTBool" -> ValueType.KnownType.KTBool
    | "KTInt8" -> ValueType.KnownType.KTInt8
    | "KTUInt8" -> ValueType.KnownType.KTUInt8
    | "KTInt16" -> ValueType.KnownType.KTInt16
    | "KTUInt16" -> ValueType.KnownType.KTUInt16
    | "KTInt32" -> ValueType.KnownType.KTInt32
    | "KTUInt32" -> ValueType.KnownType.KTUInt32
    | "KTInt64" -> ValueType.KnownType.KTInt64
    | "KTUInt64" -> ValueType.KnownType.KTUInt64
    | "KTInt128" -> ValueType.KnownType.KTInt128
    | "KTUInt128" -> ValueType.KnownType.KTUInt128
    | "KTFloat" -> ValueType.KnownType.KTFloat
    | "KTChar" -> ValueType.KnownType.KTChar
    | "KTString" -> ValueType.KnownType.KTString
    | "KTUuid" -> ValueType.KnownType.KTUuid
    | "KTDateTime" -> ValueType.KnownType.KTDateTime
    | "KTBlob" -> ValueType.KnownType.KTBlob
    | "KTStream" ->
      let vt = rValueType &r
      rNext &r
      rExpectEndArray &r
      ValueType.KnownType.KTStream vt
    | "KTList" ->
      let vt = rValueType &r
      rNext &r
      rExpectEndArray &r
      ValueType.KnownType.KTList vt
    | "KTTuple" ->
      let a = rValueType &r
      let b = rValueType &r
      rNext &r
      rExpectStartArray &r
      let rest = ResizeArray<ValueType.ValueType>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        rest.Add(rValueTypeFromCurrent &r)
        rNext &r
      rNext &r
      rExpectEndArray &r
      ValueType.KnownType.KTTuple(a, b, List.ofSeq rest)
    | "KTDict" ->
      let vt = rValueType &r
      rNext &r
      rExpectEndArray &r
      ValueType.KnownType.KTDict vt
    | "KTCustomType" ->
      rNext &r
      let typeName = rFQTypeNameFromCurrent &r
      rNext &r
      rExpectStartArray &r
      let typeArgs = ResizeArray<ValueType.ValueType>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        typeArgs.Add(rValueTypeFromCurrent &r)
        rNext &r
      rNext &r
      rExpectEndArray &r
      ValueType.KnownType.KTCustomType(typeName, List.ofSeq typeArgs)
    | "KTFn" ->
      rNext &r
      rExpectStartArray &r
      let argTypes = ResizeArray<ValueType.ValueType>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        argTypes.Add(rValueTypeFromCurrent &r)
        rNext &r
      let retType = rValueType &r
      rNext &r
      rExpectEndArray &r
      let argList = List.ofSeq argTypes
      match argList with
      | [] -> Exception.raiseInternal "KTFn: empty arg NEList" []
      | head :: tail -> ValueType.KnownType.KTFn(NEList.ofList head tail, retType)
    | "KTDB" ->
      let vt = rValueType &r
      rNext &r
      rExpectEndArray &r
      ValueType.KnownType.KTDB vt
    | t -> Exception.raiseInternal "Unknown KnownType tag" [ "tag", t ]

  and private rValueType (r : byref<Utf8JsonReader>) : ValueType.ValueType =
    rNext &r
    rValueTypeFromCurrent &r

  and private rValueTypeFromCurrent
    (r : byref<Utf8JsonReader>)
    : ValueType.ValueType =
    let tag, isNullary = rTag &r
    match tag with
    | "Unknown" ->
      if not isNullary then Exception.raiseInternal "Expected nullary 'Unknown'" []
      ValueType.ValueType.Unknown
    | "Known" ->
      let kt = rKnownType &r
      rNext &r
      rExpectEndArray &r
      ValueType.ValueType.Known kt
    | t -> Exception.raiseInternal "Unknown ValueType tag" [ "tag", t ]

  and private rFQTypeNameFromCurrent
    (r : byref<Utf8JsonReader>)
    : FQTypeName.FQTypeName =
    let tag, _ = rTag &r
    match tag with
    | "Package" ->
      let h = rHash &r
      rNext &r
      rExpectEndArray &r
      FQTypeName.Package h
    | t -> Exception.raiseInternal "Unknown FQTypeName tag" [ "tag", t ]

  // -- Dval --

  let rec private wDval (w : Utf8JsonWriter) (dv : Dval) : unit =
    match dv with
    | DUnit -> wTagged0 w "DUnit"
    | DLambda -> wTagged0 w "DLambda"
    | DStreamStub -> wTagged0 w "DStreamStub"
    | DBool b -> wTagBool w "DBool" b
    | DInt8 i -> wTagNum w "DInt8" (int i)
    | DUInt8 i -> wTagNum w "DUInt8" (int i)
    | DInt16 i -> wTagNum w "DInt16" (int i)
    | DUInt16 i -> wTagNum w "DUInt16" (int i)
    | DInt32 i ->
      wArr w (fun () ->
        w.WriteStringValue("DInt32")
        w.WriteNumberValue(i))
    | DUInt32 i ->
      wArr w (fun () ->
        w.WriteStringValue("DUInt32")
        w.WriteNumberValue(i))
    // 64+-bit ints encoded as strings to dodge JSON's 53-bit precision limit.
    | DInt64 i -> wTagStr w "DInt64" (string i)
    | DUInt64 i -> wTagStr w "DUInt64" (string i)
    | DInt128 i -> wTagStr w "DInt128" (string i)
    | DUInt128 i -> wTagStr w "DUInt128" (string i)
    | DFloat f ->
      wArr w (fun () ->
        w.WriteStringValue("DFloat")
        // JSON has no native NaN/Infinity; emit as strings.
        if System.Double.IsNaN f then w.WriteStringValue("NaN")
        elif System.Double.IsPositiveInfinity f then w.WriteStringValue("+Infinity")
        elif System.Double.IsNegativeInfinity f then w.WriteStringValue("-Infinity")
        else w.WriteNumberValue(f))
    | DChar c -> wTagStr w "DChar" c
    | DString s -> wTagStr w "DString" s
    | DDateTime d ->
      let zoned =
        NodaTime.ZonedDateTime(d, NodaTime.DateTimeZone.Utc, NodaTime.Offset.Zero)
      wTagStr w "DDateTime" (zoned.ToInstant().toIsoString ())
    | DUuid g -> wTagStr w "DUuid" (g.ToString())
    | DDB name -> wTagStr w "DDB" name
    | DBlobEphemeral g -> wTagStr w "DBlobEphemeral" (g.ToString())
    | DBlobPersistent(hash, length) ->
      wArr w (fun () ->
        w.WriteStringValue("DBlobPersistent")
        w.WriteStringValue(hash)
        w.WriteStringValue(string length))
    | DTuple(a, b, rest) ->
      wArr w (fun () ->
        w.WriteStringValue("DTuple")
        wDval w a
        wDval w b
        wArr w (fun () ->
          for d in rest do
            wDval w d))
    | DList(typ, items) ->
      wArr w (fun () ->
        w.WriteStringValue("DList")
        wValueType w typ
        wArr w (fun () ->
          for d in items do
            wDval w d))
    | DDict(typ, entries) ->
      wArr w (fun () ->
        w.WriteStringValue("DDict")
        wValueType w typ
        wObj w (fun () ->
          for KeyValue(k, v) in entries do
            w.WritePropertyName(k)
            wDval w v))
    | DRecord(rtt, st, ta, fields) ->
      wArr w (fun () ->
        w.WriteStringValue("DRecord")
        wFQTypeName w rtt
        wFQTypeName w st
        wArr w (fun () ->
          for vt in ta do
            wValueType w vt)
        wObj w (fun () ->
          for KeyValue(k, v) in fields do
            w.WritePropertyName(k)
            wDval w v))
    | DEnum(rtt, st, ta, caseName, fields) ->
      wArr w (fun () ->
        w.WriteStringValue("DEnum")
        wFQTypeName w rtt
        wFQTypeName w st
        wArr w (fun () ->
          for vt in ta do
            wValueType w vt)
        w.WriteStringValue(caseName)
        wArr w (fun () ->
          for d in fields do
            wDval w d))


  let rec private rDvalMap (r : byref<Utf8JsonReader>) : DvalMap =
    rNext &r
    if r.TokenType <> JsonTokenType.StartObject then
      Exception.raiseInternal
        "Expected JSON {} for DvalMap"
        [ "tokenType", r.TokenType ]
    let entries = ResizeArray<string * Dval>()
    rNext &r
    while r.TokenType <> JsonTokenType.EndObject do
      if r.TokenType <> JsonTokenType.PropertyName then
        Exception.raiseInternal
          "Expected DvalMap property name"
          [ "tokenType", r.TokenType ]
      let key = r.GetString()
      let value = rDval &r
      entries.Add(key, value)
      rNext &r
    Map.ofSeq entries

  and private rDval (r : byref<Utf8JsonReader>) : Dval =
    rNext &r
    rDvalFromCurrent &r

  and private rDvalFromCurrent (r : byref<Utf8JsonReader>) : Dval =
    let tag, isNullary = rTag &r
    match tag with
    | "DUnit" -> DUnit
    | "DLambda" -> DLambda
    | "DStreamStub" -> DStreamStub
    | "DBool" ->
      let b = rBool &r
      rNext &r
      rExpectEndArray &r
      DBool b
    | "DInt8" ->
      rNext &r
      let i = r.GetSByte()
      rNext &r
      rExpectEndArray &r
      DInt8 i
    | "DUInt8" ->
      rNext &r
      let i = r.GetByte()
      rNext &r
      rExpectEndArray &r
      DUInt8 i
    | "DInt16" ->
      rNext &r
      let i = r.GetInt16()
      rNext &r
      rExpectEndArray &r
      DInt16 i
    | "DUInt16" ->
      rNext &r
      let i = r.GetUInt16()
      rNext &r
      rExpectEndArray &r
      DUInt16 i
    | "DInt32" ->
      rNext &r
      let i = r.GetInt32()
      rNext &r
      rExpectEndArray &r
      DInt32 i
    | "DUInt32" ->
      rNext &r
      let i = r.GetUInt32()
      rNext &r
      rExpectEndArray &r
      DUInt32 i
    | "DInt64" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DInt64(System.Int64.Parse s)
    | "DUInt64" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DUInt64(System.UInt64.Parse s)
    | "DInt128" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DInt128(System.Int128.Parse s)
    | "DUInt128" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DUInt128(System.UInt128.Parse s)
    | "DFloat" ->
      rNext &r
      let f =
        match r.TokenType with
        | JsonTokenType.Number -> r.GetDouble()
        | JsonTokenType.String ->
          match r.GetString() with
          | "NaN" -> System.Double.NaN
          | "+Infinity" -> System.Double.PositiveInfinity
          | "-Infinity" -> System.Double.NegativeInfinity
          | s -> System.Double.Parse s
        | t ->
          Exception.raiseInternal "Expected number for DFloat" [ "tokenType", t ]
      rNext &r
      rExpectEndArray &r
      DFloat f
    | "DChar" ->
      let c = rString &r
      rNext &r
      rExpectEndArray &r
      DChar c
    | "DString" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DString s
    | "DDateTime" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DDateTime((NodaTime.Instant.ofIsoString s).toUtcLocalTimeZone ())
    | "DUuid" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DUuid(System.Guid.Parse s)
    | "DDB" ->
      let name = rString &r
      rNext &r
      rExpectEndArray &r
      DDB name
    | "DBlobEphemeral" ->
      let s = rString &r
      rNext &r
      rExpectEndArray &r
      DBlobEphemeral(System.Guid.Parse s)
    | "DBlobPersistent" ->
      let hash = rString &r
      let lengthStr = rString &r
      rNext &r
      rExpectEndArray &r
      DBlobPersistent(hash, System.Int64.Parse lengthStr)
    | "DTuple" ->
      let a = rDval &r
      let b = rDval &r
      rNext &r
      rExpectStartArray &r
      let rest = ResizeArray<Dval>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        rest.Add(rDvalFromCurrent &r)
        rNext &r
      rNext &r
      rExpectEndArray &r
      DTuple(a, b, List.ofSeq rest)
    | "DList" ->
      let typ = rValueType &r
      rNext &r
      rExpectStartArray &r
      let items = ResizeArray<Dval>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        items.Add(rDvalFromCurrent &r)
        rNext &r
      rNext &r
      rExpectEndArray &r
      DList(typ, List.ofSeq items)
    | "DDict" ->
      let typ = rValueType &r
      let entries = rDvalMap &r
      rNext &r
      rExpectEndArray &r
      DDict(typ, entries)
    | "DRecord" ->
      rNext &r
      let rtt = rFQTypeNameFromCurrent &r
      rNext &r
      let st = rFQTypeNameFromCurrent &r
      rNext &r
      rExpectStartArray &r
      let typeArgs = ResizeArray<ValueType.ValueType>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        typeArgs.Add(rValueTypeFromCurrent &r)
        rNext &r
      let fields = rDvalMap &r
      rNext &r
      rExpectEndArray &r
      DRecord(rtt, st, List.ofSeq typeArgs, fields)
    | "DEnum" ->
      rNext &r
      let rtt = rFQTypeNameFromCurrent &r
      rNext &r
      let st = rFQTypeNameFromCurrent &r
      rNext &r
      rExpectStartArray &r
      let typeArgs = ResizeArray<ValueType.ValueType>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        typeArgs.Add(rValueTypeFromCurrent &r)
        rNext &r
      let caseName = rString &r
      rNext &r
      rExpectStartArray &r
      let fields = ResizeArray<Dval>()
      rNext &r
      while r.TokenType <> JsonTokenType.EndArray do
        fields.Add(rDvalFromCurrent &r)
        rNext &r
      rNext &r
      rExpectEndArray &r
      DEnum(rtt, st, List.ofSeq typeArgs, caseName, List.ofSeq fields)
    | t ->
      let kind = if isNullary then "nullary" else "compound"
      Exception.raiseInternal $"Unknown {kind} Dval tag" [ "tag", t ]

  // -- public entry points --

  let serialize (dv : Dval) : string =
    use stream = new System.IO.MemoryStream()
    use writer = new Utf8JsonWriter(stream)
    wDval writer dv
    writer.Flush()
    System.Text.Encoding.UTF8.GetString(stream.ToArray())

  let deserialize (json : string) : Dval =
    let bytes = System.Text.Encoding.UTF8.GetBytes(json)
    let mutable reader = Utf8JsonReader(System.ReadOnlySpan(bytes))
    rDval &reader

  /// Serialize a flat list of Dvals as a JSON array — one stream pass for
  /// hashing without intermediate string allocation.
  let hashList (items : List<Dval>) : byte[] =
    use stream = new System.IO.MemoryStream()
    use writer = new Utf8JsonWriter(stream)
    wArr writer (fun () ->
      for d in items do
        wDval writer d)
    writer.Flush()
    stream.ToArray()


let toJsonV0 (dv : RT.Dval) : string = dv |> FormatV0.fromRT |> FormatV0.serialize

let parseJsonV0 (json : string) : RT.Dval =
  json |> FormatV0.deserialize |> FormatV0.toRT

let toHashV2 (dvals : list<RT.Dval>) : string =
  dvals
  |> List.map FormatV0.fromRT
  |> FormatV0.hashList
  |> System.IO.Hashing.XxHash64.Hash // fastest in .NET, does not need to be secure
  |> Base64.urlEncodeToString



module Test =
  let rec isRoundtrippableDval (dval : RT.Dval) : bool =
    match dval with
    | RT.DUnit
    | RT.DBool _
    | RT.DInt8 _
    | RT.DUInt8 _
    | RT.DInt16 _
    | RT.DUInt16 _
    | RT.DInt32 _
    | RT.DUInt32 _
    | RT.DInt64 _
    | RT.DUInt64 _
    | RT.DInt128 _
    | RT.DUInt128 _
    | RT.DFloat _
    | RT.DChar _
    | RT.DString _
    | RT.DUuid _
    | RT.DDateTime _
    | RT.DBlob _ -> true

    | RT.DStream _ -> false

    | RT.DTuple(v1, v2, rest) -> List.all isRoundtrippableDval (v1 :: v2 :: rest)

    | RT.DList(_, dvals) -> List.all isRoundtrippableDval dvals

    | RT.DDict(_, map) -> map |> Map.values |> List.all isRoundtrippableDval

    | RT.DRecord(_, _, _, map) -> map |> Map.values |> List.all isRoundtrippableDval

    | RT.DEnum(_typeName, _, _typeArgsDEnumTODO, _caseName, fields) ->
      List.all isRoundtrippableDval fields

    | RT.DApplicable _ -> false // not supported

    | RT.DDB _ -> true
