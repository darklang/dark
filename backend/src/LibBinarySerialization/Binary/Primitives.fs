/// Low-level binary read/write primitives
module LibBinarySerialization.Binary.Primitives

open System
open System.IO
open System.Text
open BinaryFormat

/// High-performance binary writer with custom primitives
module Writer =

  /// Write a variable-length integer (varint encoding)
  let writeVarint (writer : BinaryWriter) (value : int) =
    let mutable remaining = uint32 value
    while remaining >= 128u do
      writer.Write(byte (remaining ||| 128u))
      remaining <- remaining >>> 7
    writer.Write(byte remaining)

  /// Write a string with length prefix
  let writeString (writer : BinaryWriter) (value : string) =
    if isNull value then
      writeVarint writer 0
    else
      let bytes = Encoding.UTF8.GetBytes(value)
      writeVarint writer bytes.Length
      writer.Write(bytes)

  /// Write an optional value
  let writeOption
    (writer : BinaryWriter)
    (writeValue : BinaryWriter -> 'T -> unit)
    (value : 'T option)
    =
    match value with
    | None -> writer.Write(0uy)
    | Some v ->
      writer.Write(1uy)
      writeValue writer v

  /// Write a list with count prefix
  let writeList
    (writer : BinaryWriter)
    (writeItem : BinaryWriter -> 'T -> unit)
    (items : 'T list)
    =
    writeVarint writer items.Length
    items |> List.iter (writeItem writer)

  /// Write an array with count prefix
  let writeArray
    (writer : BinaryWriter)
    (writeItem : BinaryWriter -> 'T -> unit)
    (items : 'T array)
    =
    writeVarint writer items.Length
    items |> Array.iter (writeItem writer)

  /// Write a Guid as 16 bytes
  let writeGuid (writer : BinaryWriter) (guid : Guid) =
    writer.Write(guid.ToByteArray())

  /// Write a UInt64 value
  let writeUInt64 (writer : BinaryWriter) (value : uint64) = writer.Write(value)

  /// Write binary header
  let writeHeader (writer : BinaryWriter) (header : BinaryHeader) =
    writer.Write(header.Magic)
    writer.Write(header.Version)
    writer.Write(header.DataLength)
    writer.Write(byte header.TypeId)
    writer.Write(byte header.Flags)
    writer.Write(header.Reserved)

/// High-performance binary reader with custom primitives
module Reader =

  /// Read a variable-length integer (varint encoding)
  let readVarint (reader : BinaryReader) : int =
    let mutable result = 0u
    let mutable shift = 0
    let mutable continueReading = true

    while continueReading do
      if shift >= 32 then
        raise (BinaryFormatException(CorruptedData "Varint too long"))

      let b = reader.ReadByte()
      result <- result ||| ((uint32 (b &&& Varint.ValueMask)) <<< shift)
      shift <- shift + 7
      continueReading <- (b &&& Varint.ContinuationBit) <> 0uy

    int result

  /// Read a string with length prefix
  let readString (reader : BinaryReader) : string =
    let length = readVarint reader
    if length = 0 then
      ""
    else
      let bytes = reader.ReadBytes(length)
      if bytes.Length <> length then
        raise (BinaryFormatException(UnexpectedEndOfStream))
      Encoding.UTF8.GetString(bytes)

  /// Read an optional value
  let readOption
    (reader : BinaryReader)
    (readValue : BinaryReader -> 'T)
    : 'T option =
    match reader.ReadByte() with
    | 0uy -> None
    | 1uy -> Some(readValue reader)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid option tag: {b}"))

  /// Read a list with count prefix
  let readList (reader : BinaryReader) (readItem : BinaryReader -> 'T) : 'T list =
    let count = readVarint reader
    [| for _ in 1..count -> readItem reader |] |> Array.toList

  /// Read an array with count prefix
  let readArray (reader : BinaryReader) (readItem : BinaryReader -> 'T) : 'T array =
    let count = readVarint reader
    [| for _ in 1..count -> readItem reader |]

  /// Read a Guid from 16 bytes
  let readGuid (reader : BinaryReader) : Guid =
    let bytes = reader.ReadBytes(16)
    if bytes.Length <> 16 then raise (BinaryFormatException(UnexpectedEndOfStream))
    Guid(bytes)

  /// Read a UInt64 value
  let readUInt64 (reader : BinaryReader) : uint64 = reader.ReadUInt64()

  /// Read binary header
  let readHeader (reader : BinaryReader) : BinaryHeader =
    let magic = reader.ReadUInt32()
    Validation.validateMagicHeader magic

    let version = reader.ReadUInt32()
    Validation.validateVersion version

    let dataLength = reader.ReadUInt32()
    let typeId = reader.ReadByte()
    Validation.validateTypeId typeId

    let flags = reader.ReadByte()
    let reserved = reader.ReadUInt16()

    { Magic = magic
      Version = version
      DataLength = dataLength
      TypeId = LanguagePrimitives.EnumOfValue<byte, TypeId> typeId
      Flags = LanguagePrimitives.EnumOfValue<byte, FormatFlags> flags
      Reserved = reserved }

/// High-level serialization helpers
module Serialization =

  /// Serialize a value to byte array with header
  let serializeWithHeader
    (typeId : TypeId)
    (writePayload : BinaryWriter -> unit)
    : byte[] =
    use payloadStream = new MemoryStream()
    use payloadWriter = new BinaryWriter(payloadStream)

    // Write payload first to get length
    writePayload payloadWriter
    payloadWriter.Flush()
    let payloadBytes = payloadStream.ToArray()

    // Now write header + payload
    use finalStream = new MemoryStream()
    use finalWriter = new BinaryWriter(finalStream)

    let header =
      { Magic = MagicHeader
        Version = CurrentVersion
        DataLength = uint32 payloadBytes.Length
        TypeId = typeId
        Flags = FormatFlags.None
        Reserved = 0us }

    Writer.writeHeader finalWriter header
    finalWriter.Write(payloadBytes)
    finalWriter.Flush()

    finalStream.ToArray()

  /// Deserialize a value from byte array with header validation
  let deserializeWithHeader (readPayload : BinaryReader -> 'T) (data : byte[]) : 'T =
    use stream = new MemoryStream(data)
    use reader = new BinaryReader(stream)

    let header = Reader.readHeader reader

    // Validate remaining data length
    let remainingBytes = data.Length - 16 // header is 16 bytes
    if uint32 remainingBytes <> header.DataLength then
      Validation.validateDataLength header.DataLength (uint32 remainingBytes)

    readPayload reader
