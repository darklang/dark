/// Conversion to/from Dark values to binary formats
module LibBinarySerialization.BinarySerialization

open System
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Buffers

open Prelude

module PT = LibExecution.ProgramTypes
module ST = SerializedTypes
module PT2ST = ProgramTypesToSerializedTypes

open MessagePack
open MessagePack.Resolvers
open MessagePack.FSharp
open MessagePack.Formatters


module Int128Utils =

  let ToBytes (value : System.Int128) =
    if value = System.Int128.Zero then
      Array.zeroCreate 16
    else
      let low = uint64 value
      let high = uint64 (value >>> 64)
      let lowBytes = BitConverter.GetBytes(low)
      let highBytes = BitConverter.GetBytes(high)

      Array.concat [ lowBytes; highBytes ]

  let FromBytes (bytes : byte[]) : System.Int128 =
    if bytes.Length <> 16 then
      raise (ArgumentException "Byte array must be exactly 16 bytes long.")

    let lowBytes = bytes[0..7]
    let highBytes = bytes[8..15]

    let low = BitConverter.ToUInt64(lowBytes, 0)
    let high = BitConverter.ToUInt64(highBytes, 0)

    System.Int128(uint64 high, uint64 low)


module UInt128Utils =

  let ToBytes (value : System.UInt128) =
    if value = System.UInt128.Zero then
      Array.zeroCreate 16
    else
      let low = uint64 value
      let high = uint64 (value >>> 64)
      let lowBytes = BitConverter.GetBytes(low)
      let highBytes = BitConverter.GetBytes(high)

      Array.concat [ lowBytes; highBytes ]

  let FromBytes (bytes : byte[]) : System.UInt128 =
    if bytes.Length <> 16 then
      raise (ArgumentException "Byte array must be exactly 16 bytes long.")

    let lowBytes = bytes[0..7]
    let highBytes = bytes[8..15]

    let low = BitConverter.ToUInt64(lowBytes, 0)
    let high = BitConverter.ToUInt64(highBytes, 0)

    System.UInt128(uint64 high, uint64 low)


type Int128Formatter() =
  interface IMessagePackFormatter<System.Int128> with
    member this.Serialize
      (
        writer : byref<MessagePackWriter>,
        value : System.Int128,
        _options : MessagePackSerializerOptions
      ) =
      let bytes = Int128Utils.ToBytes(value)
      writer.Write(bytes)

    member this.Deserialize
      (
        reader : byref<MessagePackReader>,
        _options : MessagePackSerializerOptions
      ) : System.Int128 =
      let nullableSequence = reader.ReadBytes()
      if nullableSequence.HasValue then
        let sequence = nullableSequence.Value
        let array = Array.zeroCreate<byte> (16)
        sequence.CopyTo(array)
        Int128Utils.FromBytes(array)
      else
        raise (InvalidOperationException("Invalid binary format for System.Int128"))

type UInt128Formatter() =
  interface IMessagePackFormatter<System.UInt128> with
    member this.Serialize
      (
        writer : byref<MessagePackWriter>,
        value : System.UInt128,
        _options : MessagePackSerializerOptions
      ) =
      let bytes = UInt128Utils.ToBytes(value)
      writer.Write(bytes)

    member this.Deserialize
      (
        reader : byref<MessagePackReader>,
        _options : MessagePackSerializerOptions
      ) : System.UInt128 =
      let nullableSequence = reader.ReadBytes()
      if nullableSequence.HasValue then
        let sequence = nullableSequence.Value
        let array = Array.zeroCreate<byte> (16)
        sequence.CopyTo(array)
        UInt128Utils.FromBytes(array)
      else
        raise (InvalidOperationException("Invalid binary format for System.UInt128"))

// Serializers sometimes throw at runtime if the setup is not right. We do not
// currently know of a way to statically ensure these run. As a result, we don't
// expose the generic serialization functions, only functions for specific types that
// are tested (that is, they have unit tests!) and are known to work.

let resolver =
  Resolvers.CompositeResolver.Create(
    [| Int128Formatter() :> IMessagePackFormatter
       UInt128Formatter() :> IMessagePackFormatter |],
    [| FSharpResolver.Instance; StandardResolver.Instance |]
  )

let optionsWithoutZip = MessagePackSerializerOptions.Standard.WithResolver(resolver)

let optionsWithZip =
  MessagePack.MessagePackSerializerOptions.Standard
    .WithResolver(resolver)
    .WithCompression(MessagePack.MessagePackCompression.Lz4BlockArray)

let wrapSerializationException (id : string) (f : unit -> 'a) : 'a =
  try
    f ()
  with e ->
    Exception.callExceptionCallback e
    raise (
      Exception.InternalException(
        "error deserializing toplevel",
        [ "id", id
          "suggestion", "maybe annotation are missing in SerializationTypes" ],
        e
      )
    )

let serializeExpr (tlid : tlid) (e : PT.Expr) : byte[] =
  wrapSerializationException (string tlid) (fun () ->
    let serializableValue = PT2ST.Expr.toST e
    MessagePack.MessagePackSerializer.Serialize(
      serializableValue,
      optionsWithoutZip
    ))

let deserializeExpr (tlid : tlid) (data : byte[]) : PT.Expr =
  wrapSerializationException (string tlid) (fun () ->
    MessagePack.MessagePackSerializer.Deserialize<ST.Expr>(data, optionsWithoutZip)
    |> PT2ST.Expr.toPT)

let serializeToplevel (tl : PT.Toplevel.T) : byte[] =
  wrapSerializationException (PT.Toplevel.toTLID tl |> string) (fun () ->
    let v = PT2ST.Toplevel.toST tl
    MessagePack.MessagePackSerializer.Serialize(v, optionsWithoutZip))

let deserializeToplevel (tlid : tlid) (data : byte[]) : PT.Toplevel.T =
  wrapSerializationException (string tlid) (fun () ->
    MessagePack.MessagePackSerializer.Deserialize(data, optionsWithoutZip)
    |> PT2ST.Toplevel.toPT)

let serializeToplevels (msg : string) (tls : List<PT.Toplevel.T>) : byte[] =
  wrapSerializationException msg (fun () ->
    let v = List.map PT2ST.Toplevel.toST tls
    MessagePack.MessagePackSerializer.Serialize(v, optionsWithoutZip))

let deserializeToplevels (msg : string) (data : byte[]) : List<PT.Toplevel.T> =
  wrapSerializationException msg (fun () ->
    MessagePack.MessagePackSerializer.Deserialize(data, optionsWithoutZip)
    |> List.map PT2ST.Toplevel.toPT)


let serializePackageFn (fn : PT.PackageFn.T) : byte[] =
  wrapSerializationException (string fn.id) (fun () ->
    let v = PT2ST.PackageFn.toST fn
    MessagePack.MessagePackSerializer.Serialize(v, optionsWithoutZip))

let serializePackageType (pt : PT.PackageType.T) : byte[] =
  wrapSerializationException (string pt.id) (fun () ->
    let v = PT2ST.PackageType.toST pt
    MessagePack.MessagePackSerializer.Serialize(v, optionsWithoutZip))

let serializePackageConstant (constant : PT.PackageConstant.T) : byte[] =
  wrapSerializationException (string constant.id) (fun () ->
    let v = PT2ST.PackageConstant.toST constant
    MessagePack.MessagePackSerializer.Serialize(v, optionsWithoutZip))

let deserializePackageType (uuid : System.Guid) (data : byte[]) : PT.PackageType.T =
  wrapSerializationException (string uuid) (fun () ->
    MessagePack.MessagePackSerializer.Deserialize(data, optionsWithoutZip)
    |> PT2ST.PackageType.toPT)

let deserializePackageConstant
  (uuid : System.Guid)
  (data : byte[])
  : PT.PackageConstant.T =
  wrapSerializationException (string uuid) (fun () ->
    MessagePack.MessagePackSerializer.Deserialize(data, optionsWithoutZip)
    |> PT2ST.PackageConstant.toPT)

let deserializePackageFn (uuid : System.Guid) (data : byte[]) : PT.PackageFn.T =
  wrapSerializationException (string uuid) (fun () ->
    MessagePack.MessagePackSerializer.Deserialize(data, optionsWithoutZip)
    |> PT2ST.PackageFn.toPT)


module Test =
  let serializeToplevelsToJson (tls : List<PT.Toplevel.T>) : string =
    wrapSerializationException "test" (fun () ->
      let v = List.map PT2ST.Toplevel.toST tls
      let jsonString =
        MessagePack.MessagePackSerializer.SerializeToJson(v, optionsWithZip)
      let jsonDocument = System.Text.Json.JsonDocument.Parse(jsonString)
      let options = System.Text.Json.JsonSerializerOptions(WriteIndented = true)
      System.Text.Json.JsonSerializer.Serialize(jsonDocument, options))
