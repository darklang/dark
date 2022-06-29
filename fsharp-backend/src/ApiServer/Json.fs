/// Json compatibility with the client, used by the middleware
module ApiServer.Json

// This provides a JSON encoding that exactly matches the encoding used in the
// client, but uses System.Text.Json. The intent is to be significantly faster
// than old ocamlCompatible one. It doesn't differ from Prelude.Json.Vanilla
// very much, and I suspect it could be switched to such.

open Prelude

open System.Text.Json
open System.Text.Json.Serialization
open NodaTime.Serialization.SystemTextJson

/// OCamlCompatible converter ported to STJ
type FloatConverter() =
  // CLEANUP: check if we need the float converter
  inherit JsonConverter<float>()

  override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
    reader.GetDouble()

  override _.Write(writer : Utf8JsonWriter, value : float, _) =
    let maxPreciselyRepresentedIntValue : float = float (1L <<< 49)
    if System.Math.Abs(value % 1.0) < System.Double.Epsilon
       && value < maxPreciselyRepresentedIntValue
       && value > (-maxPreciselyRepresentedIntValue) then
      let asString = string value
      writer.WriteRawValue $"{value}.0"
    else
      writer.WriteRawValue(string value)

type LocalDateTimeConverter() =
  // CLEANUP: check if the default format is actually OK
  // To match the client we use the format: 1906-11-23T23:01:23Z
  // The default format is 1906-11-23T23:01:23.428
  inherit JsonConverter<NodaTime.LocalDateTime>()

  override _.Read(reader : byref<Utf8JsonReader>, _type, _options) =
    NodaConverters.LocalDateTimeConverter.Read(&reader, _type, _options)

  override _.Write
    (
      writer : Utf8JsonWriter,
      value : NodaTime.LocalDateTime,
      _
    ) : unit =
    let value =
      NodaTime
        .ZonedDateTime(value, NodaTime.DateTimeZone.Utc, NodaTime.Offset.Zero)
        .ToInstant()
        .toIsoString ()
    writer.WriteStringValue value


let options =
  let fsharpConverter =
    JsonFSharpConverter(
      unionEncoding =
        (JsonUnionEncoding.InternalTag ||| JsonUnionEncoding.UnwrapOption)
    )
  let options = JsonSerializerOptions()
  options.MaxDepth <- System.Int32.MaxValue // infinite
  options.Converters.Add(FloatConverter())
  options.Converters.Add(LocalDateTimeConverter())
  options.Converters.Add(NodaConverters.InstantConverter)
  options.Converters.Add(Prelude.Json.Vanilla.TLIDConverter())
  options.Converters.Add(Prelude.Json.Vanilla.PasswordConverter())
  options.Converters.Add(Prelude.Json.Vanilla.RawBytesConverter())
  options.Converters.Add(fsharpConverter)
  options


// SERIALIZER_DEF STJ ApiServer.Json.serialize
// Plan: remove and replace with `dark-editor` canvas
let serialize (data : 'a) : string = JsonSerializer.Serialize(data, options)

// SERIALIZER_DEF STJ ApiServer.Json.serialize
// Plan: remove and replace with `dark-editor` canvas
let deserialize<'a> (json : string) : 'a =
  JsonSerializer.Deserialize<'a>(json, options)
