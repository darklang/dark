/// Low-level binary read/write primitives
module LibBinarySerialization.Serializers.Common

open System
open System.IO
open System.Text
open System.Numerics
open Prelude

open LibBinarySerialization.BinaryFormat


module Header =
  let write (w : BinaryWriter) (header : BinaryHeader) =
    w.Write header.Version
    w.Write header.DataLength

  let read (reader : BinaryReader) : BinaryHeader =
    let version = reader.ReadUInt32()
    Validation.validateVersion version

    let dataLength = reader.ReadUInt32()

    { Version = version; DataLength = dataLength }


/// Variable-length integer (varint encoding)
module Varint =
  let write (w : BinaryWriter) (value : int) =
    let mutable remaining = uint32 value
    while remaining >= 128u do
      w.Write(byte (remaining ||| 128u))
      remaining <- remaining >>> 7
    w.Write(byte remaining)


  let read (r : BinaryReader) : int =
    let mutable result = 0u
    let mutable shift = 0
    let mutable continueReading = true

    while continueReading do
      if shift >= 32 then
        raise (BinaryFormatException(CorruptedData "Varint too long"))

      let b = r.ReadByte()
      result <- result ||| ((uint32 (b &&& Varint.ValueMask)) <<< shift)
      shift <- shift + 7
      continueReading <- (b &&& Varint.ContinuationBit) <> 0uy

    int result


/// Write/read a string with length prefix
module String =
  let write (w : BinaryWriter) (value : string) =
    if isNull value then
      Varint.write w 0
    else
      let bytes = Encoding.UTF8.GetBytes(value)
      Varint.write w bytes.Length
      w.Write bytes

  let read (r : BinaryReader) : string =
    let length = Varint.read r
    if length = 0 then
      ""
    else
      let bytes = r.ReadBytes(length)
      if bytes.Length <> length then
        raise (BinaryFormatException(UnexpectedEndOfStream))
      Encoding.UTF8.GetString bytes


module Hash =
  let write (w : BinaryWriter) (value : Hash) =
    let (Hash hashStr) = value
    String.write w hashStr

  let read (r : BinaryReader) : Hash =
    let hashStr = String.read r
    Hash hashStr


module Bool =
  let write (w : BinaryWriter) (value : bool) = w.Write(value)
  let read (r : BinaryReader) : bool = r.ReadBoolean()

module Int16 =
  let write (w : BinaryWriter) (value : int16) = w.Write(value)
  let read (r : BinaryReader) : int16 = r.ReadInt16()

module UInt16 =
  let writeUInt16 (w : BinaryWriter) (value : uint16) = w.Write(value)
  let readUInt16 (r : BinaryReader) : uint16 = r.ReadUInt16()

module Int32 =
  let writeInt32 (w : BinaryWriter) (value : int32) = w.Write(value)
  let readInt32 (r : BinaryReader) : int32 = r.ReadInt32()

module UInt32 =
  let writeUInt32 (w : BinaryWriter) (value : uint32) = w.Write(value)
  let readUInt32 (r : BinaryReader) : uint32 = r.ReadUInt32()

module Int64 =
  let writeInt64 (w : BinaryWriter) (value : int64) = w.Write(value)
  let readInt64 (r : BinaryReader) : int64 = r.ReadInt64()

module UInt64 =
  let writeUInt64 (w : BinaryWriter) (value : uint64) = w.Write(value)
  let readUInt64 (r : BinaryReader) : uint64 = r.ReadUInt64()

module Float =
  let write (w : BinaryWriter) (value : float) =
    if System.Double.IsNaN(value) then
      w.Write(0uy) // NaN tag
    elif System.Double.IsPositiveInfinity(value) then
      w.Write(1uy) // +Infinity tag
    elif System.Double.IsNegativeInfinity(value) then
      w.Write(2uy) // -Infinity tag
    else
      w.Write(3uy) // Regular float tag
      w.Write(value)

  let read (r : BinaryReader) : float =
    match r.ReadByte() with
    | 0uy -> System.Double.NaN
    | 1uy -> System.Double.PositiveInfinity
    | 2uy -> System.Double.NegativeInfinity
    | 3uy -> r.ReadDouble()
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid float tag: {b}"))

module DateTime =
  let write (w : BinaryWriter) (value : NodaTime.Instant) =
    w.Write(value.ToUnixTimeTicks())

  let read (r : BinaryReader) : NodaTime.Instant =
    NodaTime.Instant.FromUnixTimeTicks(r.ReadInt64())

// Read/write from/to string for now - simpler than byte manipulation
// TODO - what, why?
module Int128 =
  let read (r : BinaryReader) : System.Int128 =
    let str = String.read r
    System.Int128.Parse(str)

  let write (w : BinaryWriter) (value : System.Int128) =
    let str = string value
    String.write w str

// As string for now - simpler than byte manipulation
// TODO: what?
module UInt128 =
  let write (w : BinaryWriter) (value : System.UInt128) =
    let str = string value
    String.write w str

  let read (r : BinaryReader) : System.UInt128 =
    let str = String.read r
    System.UInt128.Parse str



module Guid =
  let write (w : BinaryWriter) (guid : Guid) = w.Write(guid.ToByteArray())

  let read (r : BinaryReader) : Guid =
    let bytes = r.ReadBytes 16
    if bytes.Length <> 16 then raise (BinaryFormatException UnexpectedEndOfStream)
    Guid(bytes)


module Option =
  let write
    (w : BinaryWriter)
    (writeValue : BinaryWriter -> 'T -> unit)
    (value : 'T option)
    =
    match value with
    | None -> w.Write(0uy)
    | Some v ->
      w.Write(1uy)
      writeValue w v

  let read (reader : BinaryReader) (readValue : BinaryReader -> 'T) : 'T option =
    match reader.ReadByte() with
    | 0uy -> None
    | 1uy -> Some(readValue reader)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid option tag: {b}"))


/// Read/write a list -- with count prefix
module List =
  let write
    (w : BinaryWriter)
    (writeItem : BinaryWriter -> 'T -> unit)
    (items : 'T list)
    =
    Varint.write w items.Length
    items |> List.iter (writeItem w)

  let read (reader : BinaryReader) (readItem : BinaryReader -> 'T) : 'T list =
    let count = Varint.read reader
    [| for _ in 1..count -> readItem reader |] |> Array.toList


/// Read/write an array -- with count prefix
module Array =
  let writeArray
    (w : BinaryWriter)
    (writeItem : BinaryWriter -> 'T -> unit)
    (items : 'T array)
    =
    Varint.write w items.Length
    items |> Array.iter (writeItem w)

  let readArray (reader : BinaryReader) (readItem : BinaryReader -> 'T) : 'T array =
    let count = Varint.read reader
    [| for _ in 1..count -> readItem reader |]



/// Read/write non-empty lists
module Map =
  let write
    (writeKey : BinaryWriter -> 'K -> unit)
    (writeValue : BinaryWriter -> 'V -> unit)
    (w : BinaryWriter)
    (map : Map<'K, 'V>)
    =
    let entries = Map.toList map
    Varint.write w entries.Length
    entries
    |> List.iter (fun (k, v) ->
      writeKey w k
      writeValue w v)

  let read
    (readKey : BinaryReader -> 'K)
    (readValue : BinaryReader -> 'V)
    (r : BinaryReader)
    : Map<'K, 'V> =
    let count = Varint.read r
    [| for _ in 1..count ->
         let k = readKey r
         let v = readValue r
         (k, v) |]
    |> Map


module NEList =
  let write
    (writeItem : BinaryWriter -> 'T -> unit)
    (w : BinaryWriter)
    (nel : NEList<'T>)
    =
    writeItem w nel.head
    List.write w writeItem nel.tail

  let read (readItem : BinaryReader -> 'T) (r : BinaryReader) : NEList<'T> =
    let head = readItem r
    let tail = List.read r readItem
    { head = head; tail = tail }


module DarkDateTime =
  let write (w : BinaryWriter) (dt : LibExecution.DarkDateTime.T) =
    let instant = LibExecution.DarkDateTime.toInstant dt
    DateTime.write w instant

  let read (r : BinaryReader) : LibExecution.DarkDateTime.T =
    let instant = DateTime.read r
    instant.InUtc().LocalDateTime
