module ClientTypes.Converters

module STJ =
  open System.Text.Json
  open System.Text.Json.Serialization

  type WorkerStateConverter() =
    inherit JsonConverter<Worker.WorkerState>()

    override _.Read(reader : byref<Utf8JsonReader>, _type, _options) : Worker.WorkerState =
      reader.GetString() |> Worker.parse

    override _.Write(writer : Utf8JsonWriter, value : Worker.WorkerState, _options) =
      writer.WriteStringValue(string value)
