module Json

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let isBaseType (t : System.Type) =
  t = typeof<int>
  || t = typeof<bool>
  || t = typeof<string>
  || t = typeof<float>
  || t = typeof<int64>
  || t = typeof<uint64>
  || t = typeof<int8>
  || t = typeof<uint8>
  || t = typeof<int16>
  || t = typeof<uint16>
  || t = typeof<int32>
  || t = typeof<uint32>
  || t = typeof<System.Int128>
  || t = typeof<System.UInt128>
  || t = typeof<NodaTime.Instant>
  || t = typeof<System.Guid>

module Vanilla =

  open System.Text.Json
  open System.Text.Json.Serialization
  open NodaTime.Serialization.SystemTextJson

  type Int64Converter() =
    // We serialize int64s as valid JSON numbers for as long as we're allowed, and
    // then we switch to strings. Since the deserialization is type-directed, we
    // always know we're looking to convert them to int64s, so if we see a string
    // we know exactly what it means
    inherit JsonConverter<int64>()

    override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
      if reader.TokenType = JsonTokenType.String then
        let str = reader.GetString()
        System.Convert.ToInt64 str
      else
        reader.GetInt64()

    override _.Write(writer : Utf8JsonWriter, value : int64, _options) =
      // largest int64 for which all smaller ints can be represented in a double
      // without losing precision
      if value > 9007199254740992L then
        writer.WriteStringValue(value.ToString())
      else if value < -9007199254740992L then
        writer.WriteStringValue(value.ToString())
      else
        writer.WriteNumberValue(value)

  type UInt64Converter() =
    // We serialize uint64s as valid JSON numbers for as long as we're allowed, and
    // then we switch to strings. Since the deserialization is type-directed, we
    // always know we're looking to convert them to uint64s, so if we see a string
    // we know exactly what it means
    inherit JsonConverter<uint64>()

    override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
      if reader.TokenType = JsonTokenType.String then
        let str = reader.GetString()
        System.Convert.ToUInt64 str
      else
        reader.GetUInt64()

    override _.Write(writer : Utf8JsonWriter, value : uint64, _options) =
      // largest uint64 for which all smaller ints can be represented in a double
      // without losing precision
      if value > 9007199254740992UL then
        writer.WriteStringValue(value.ToString())
      else
        writer.WriteNumberValue(value)


  type Int128Converter() =
    // We serialize int128s as strings because they're too big to be represented as numbers in JSON.
    // Since the deserialization is type-directed, we always know we're looking
    // to convert them to int128s, so if we see a string we know exactly what it means
    inherit JsonConverter<System.Int128>()

    override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
      let str = reader.GetString()
      System.Int128.Parse(str)

    override _.Write(writer : Utf8JsonWriter, value : System.Int128, _options) =
      writer.WriteStringValue(value.ToString())

  type UInt128Converter() =
    // We serialize uint128s as strings because they're too big to be represented as numbers in JSON.
    // Since the deserialization is type-directed, we always know we're looking to convert them to uint128s,
    // so if we see a string we know exactly what it means
    inherit JsonConverter<System.UInt128>()

    override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
      let str = reader.GetString()
      System.UInt128.Parse(str)

    override _.Write(writer : Utf8JsonWriter, value : System.UInt128, _options) =
      writer.WriteStringValue(value.ToString())


  type NEListValueConverter<'TValue>() =
    inherit JsonConverter<NEList<'TValue>>()

    override this.Read
      (
        reader : byref<Utf8JsonReader>,
        _typeToConvert : System.Type,
        options : JsonSerializerOptions
      ) =
      let list =
        JsonSerializer.Deserialize<'TValue seq>(&reader, options) |> Seq.toList
      match list with
      | [] -> Exception.raiseInternal "Empty list" []
      | head :: tail -> NEList.ofList head tail


    override this.Write
      (
        writer : Utf8JsonWriter,
        value : NEList<'TValue>,
        options : JsonSerializerOptions
      ) =
      let value = NEList.toList value
      JsonSerializer.Serialize(writer, (List.toSeq value), options)


  type NEListConverter() =
    inherit JsonConverterFactory()
    override this.CanConvert(typeToConvert : System.Type) : bool =
      typeToConvert.IsGenericType
      && List.contains
        (typeToConvert.GetGenericTypeDefinition())
        [ typedefof<NEList<_>>
          typedefof<System.Collections.Generic.IReadOnlyCollection<_>> ]

    override this.CreateConverter
      (
        typeToConvert : System.Type,
        _options : JsonSerializerOptions
      ) : JsonConverter =
      let typArgs = typeToConvert.GetGenericArguments()
      let converterType = typedefof<NEListValueConverter<_>>.MakeGenericType(typArgs)
      System.Activator.CreateInstance(converterType) :?> JsonConverter


  type LocalDateTimeConverter() =
    inherit JsonConverter<NodaTime.LocalDateTime>()

    override _.Read(reader : byref<Utf8JsonReader>, _typ, _options) =
      let rawToken = reader.GetString()
      (NodaTime.Instant.ofIsoString rawToken).toUtcLocalTimeZone ()

    override _.Write
      (
        writer : Utf8JsonWriter,
        value : NodaTime.LocalDateTime,
        _options
      ) =
      let value =
        NodaTime
          .ZonedDateTime(value, NodaTime.DateTimeZone.Utc, NodaTime.Offset.Zero)
          .ToInstant()
          .toIsoString ()
      writer.WriteStringValue(value)

  type RawBytesConverter() =
    inherit JsonConverter<byte array>()
    override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
      reader.GetString() |> Base64.fromUrlEncoded |> Base64.decode

    override _.Write(writer : Utf8JsonWriter, value : byte[], _) =
      value |> Base64.urlEncodeToString |> writer.WriteStringValue


  // This is used for "normal" JSON conversion, such as converting Pos into
  // json.
  let getDefaultOptions () =
    let fsharpConverter =
      JsonFSharpConverter(unionEncoding = (JsonUnionEncoding.ExternalTag))
    let options = JsonSerializerOptions()
    options.MaxDepth <- System.Int32.MaxValue // infinite
    options.NumberHandling <- JsonNumberHandling.AllowNamedFloatingPointLiterals
    options.Converters.Add(NodaConverters.InstantConverter)
    options.Converters.Add(LocalDateTimeConverter())
    options.Converters.Add(UInt64Converter())
    options.Converters.Add(Int64Converter())
    options.Converters.Add(Int128Converter())
    options.Converters.Add(UInt128Converter())
    options.Converters.Add(RawBytesConverter())
    options.Converters.Add(NEListConverter())
    options.Converters.Add(fsharpConverter)

    options

  let _options = getDefaultOptions ()

  let _prettyOptions =
    let options = getDefaultOptions ()
    options.WriteIndented <- true
    options

  let _deserializeWithCommentsOptions =
    let options = getDefaultOptions ()
    options.ReadCommentHandling <- JsonCommentHandling.Skip
    options

  let registerConverter (c : JsonConverter<'a>) =
    // insert in the front as the formatter will use the first converter that
    // supports the type, not the best one
    _options.Converters.Insert(0, c)
    _prettyOptions.Converters.Insert(0, c)
    _deserializeWithCommentsOptions.Converters.Insert(0, c)

  // ----------------------
  // Tracking serializers
  // ----------------------

  // Serializers are easy to be unsure about. We use the Vanilla serializer a lot
  // for random types, and we would like to be sure that we don't accidentally
  // change the format of those types by adding new converters or whatever. Our
  // solution this is:

  // 1) track all types that are serialized and also how they are serialized
  // 2) store sample input/output for each type
  // 3) During testing, assert stored sample output has not changed
  // 4) warn when serializing using a type that is not allowed/tested serializer.

  // We track the types by explicitly calling the `allow<type>` function.  This
  // adds the type to a dictionary: at test-time we check all members of the
  // dictionary are accounted for. At runtime we warn if we're attempting to serialize
  // a type that hasn't been allowed. During development, we upgrade that warning
  // to an exception.
  //
  // See also Serializers.Tests.fs and [/docs/serialization.md]

  let mutable allowedTypes = Dictionary.T<string, string>()

  let rec isSerializable (t : System.Type) : bool =
    if isBaseType t then true else allowedTypes.ContainsKey(string t)

  let allow<'a> (reason : string) : unit =
    try
      let key = string typeof<'a>
      let reason =
        if allowedTypes.ContainsKey key then
          $"{reason}+{allowedTypes[key]}"
        else
          reason
      allowedTypes[key] <- reason
      ()
    with _ ->
      System.Console.Write("error allowing Vanilla type")


  let assertSerializable (t : System.Type) : unit =
    if not (isSerializable t) then
      Exception.sendRollbarError
        "Invalid serialization call to type not allowed: add `Json.Vanilla.allow<type>()` to allow it to be serialized"
        [ "type", string t ]


  let serialize (data : 'a) : string =
    assertSerializable typeof<'a>
    JsonSerializer.Serialize(data, _options)

  let serializeToStream (stream : System.IO.Stream, data : 'a) : Task<unit> =
    task {
      assertSerializable typeof<'a>
      return! JsonSerializer.SerializeAsync(stream, data, _options)
    }

  let deserialize<'a> (json : string) : 'a =
    assertSerializable typeof<'a>
    JsonSerializer.Deserialize<'a>(json, _options)

  let deserializeWithComments<'a> (json : string) : 'a =
    assertSerializable typeof<'a>
    JsonSerializer.Deserialize<'a>(json, _deserializeWithCommentsOptions)

  let prettySerialize (data : 'a) : string =
    assertSerializable typeof<'a>
    JsonSerializer.Serialize(data, _prettyOptions)
