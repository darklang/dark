/// Low-level binary read/write primitives
module LibSerialization.Binary.Serializers.Common

open System
open System.IO
open System.Text
open System.Numerics
open Prelude

open LibSerialization.Binary.BaseFormat


/// Helper for serializer files to raise format errors without opening BaseFormat
let raiseFormatError msg = raise (BinaryFormatException(CorruptedData msg))


/// This thread's reusable buffers, so a write or read doesn't allocate a byte array of its own.
///
/// The cold first run serializes and hashes every item it folds, and byte arrays were a third of
/// what it allocated: a `MemoryStream` grown by doubling and copied out twice per blob, plus one
/// array per string, hash and Guid. Everything that borrows here is synchronous and never keeps
/// the buffer past its own call.
type Scratch() =
  /// A buffer kept for reuse is at most this big; a bigger one is made for that call alone.
  static let keepAtMost = 64 * 1024

  [<System.ThreadStatic; DefaultValue>]
  static val mutable private bytes : byte[]

  [<System.ThreadStatic; DefaultValue>]
  static val mutable private stream : MemoryStream

  [<System.ThreadStatic; DefaultValue>]
  static val mutable private writer : BinaryWriter

  [<System.ThreadStatic; DefaultValue>]
  static val mutable private streamInUse : bool

  /// At least `size` bytes, valid until this thread's next call here. Leaf use only: nothing that
  /// holds it may call into another serializer before it's done with it.
  static member Bytes(size : int) : byte[] =
    let b = Scratch.bytes
    if not (isNull b) && b.Length >= size then
      b
    elif size > keepAtMost then
      Array.zeroCreate size
    else
      let fresh =
        Array.zeroCreate (
          max
            256
            (int (System.Numerics.BitOperations.RoundUpToPowerOf2(uint32 size)))
        )
      Scratch.bytes <- fresh
      fresh

  /// Runs `f` with this thread's stream, empty, and a writer on it. A writer that serializes
  /// something else on the way gets a fresh pair for that, so the outer bytes are never touched.
  static member WithStream(f : MemoryStream -> BinaryWriter -> 'a) : 'a =
    if Scratch.streamInUse then
      use ms = new MemoryStream()
      use w = new BinaryWriter(ms)
      f ms w
    else
      if isNull Scratch.stream then
        Scratch.stream <- new MemoryStream(4096)
        Scratch.writer <- new BinaryWriter(Scratch.stream)
      let ms = Scratch.stream
      ms.SetLength 0L
      ms.Position <- 0L
      Scratch.streamInUse <- true
      try
        f ms Scratch.writer
      finally
        Scratch.streamInUse <- false
        if ms.Capacity > 16 * keepAtMost then
          ms.SetLength 0L
          ms.Capacity <- 4096

  /// Fills `buffer[0..count)` from the reader; how many bytes it got, short only at the end.
  static member ReadInto(r : BinaryReader, buffer : byte[], count : int) : int =
    let mutable got = 0
    let mutable more = true
    while more && got < count do
      let n = r.Read(buffer, got, count - got)
      if n = 0 then more <- false else got <- got + n
    got


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
      let buffer = Scratch.Bytes(Encoding.UTF8.GetMaxByteCount value.Length)
      let length = Encoding.UTF8.GetBytes(value, 0, value.Length, buffer, 0)
      Varint.write w length
      w.Write(buffer, 0, length)

  let read (r : BinaryReader) : string =
    let length = Varint.read r
    if length = 0 then
      ""
    elif length < 0 then
      // A corrupt length: let `ReadBytes` refuse it as it always has.
      r.ReadBytes(length) |> ignore<byte[]>
      ""
    else
      let buffer = Scratch.Bytes length
      if Scratch.ReadInto(r, buffer, length) <> length then
        raise (BinaryFormatException(UnexpectedEndOfStream))
      Encoding.UTF8.GetString(buffer, 0, length)


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

/// The raw binary32 bits, so every value (NaN payloads, -0) round-trips.
module Float32 =
  let write (w : BinaryWriter) (value : float32) =
    w.Write(System.BitConverter.SingleToUInt32Bits value)

  let read (r : BinaryReader) : float32 =
    System.BitConverter.UInt32BitsToSingle(r.ReadUInt32())

/// The four 32-bit words of `Decimal.GetBits`: the 96-bit magnitude, then sign and scale. Exact,
/// scale included, so `1.10m` reads back as 1.10.
module Decimal =
  let write (w : BinaryWriter) (value : decimal) =
    for part in System.Decimal.GetBits value do
      w.Write(part)

  let read (r : BinaryReader) : decimal =
    let parts = Array.init 4 (fun _ -> r.ReadInt32())
    try
      System.Decimal(parts)
    with :? System.ArgumentException ->
      raise (BinaryFormatException(CorruptedData "Invalid decimal bits"))

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
  let write (w : BinaryWriter) (guid : Guid) =
    let buffer = Scratch.Bytes 16
    guid.TryWriteBytes(Span<byte>(buffer, 0, 16)) |> ignore<bool>
    w.Write(buffer, 0, 16)

  let read (r : BinaryReader) : Guid =
    let buffer = Scratch.Bytes 16
    if Scratch.ReadInto(r, buffer, 16) <> 16 then
      raise (BinaryFormatException UnexpectedEndOfStream)
    Guid(ReadOnlySpan<byte>(buffer, 0, 16))


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
