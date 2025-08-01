/// Custom binary serialization for Dark values
module LibBinarySerialization.BinarySerialization

open System
open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers


let wrap (id : string) (f : unit -> 'a) : 'a =
  try
    f ()
  with e ->
    Exception.callExceptionCallback e

    Exception.InternalException(
      "error serializing/deserializing with custom binary format",
      [ "id", id ],
      e
    )
    |> raise


/// Create an optimized serializer function with embedded error handling
/// Takes any type of ID and converts to string only when needed
let makeSerializer<'T, 'ID>
  (writer : BinaryWriter -> 'T -> unit)
  : 'ID -> 'T -> byte[] =
  fun id value ->
    wrap (string id) (fun () ->
      // First, write payload to get length for header
      use payloadStream = new MemoryStream()
      use payloadWriter = new BinaryWriter(payloadStream)

      writer payloadWriter value
      payloadWriter.Flush()
      let payloadBytes = payloadStream.ToArray()

      // Now write header + payload
      use finalStream = new MemoryStream()
      use finalWriter = new BinaryWriter(finalStream)

      let header =
        { Version = CurrentVersion; DataLength = uint32 payloadBytes.Length }

      // Write header
      Header.write finalWriter header
      finalWriter.Write(payloadBytes)
      finalWriter.Flush()

      finalStream.ToArray())


/// Create an optimized deserializer function with embedded error handling
let makeDeserializer<'T, 'ID> (reader : BinaryReader -> 'T) : 'ID -> byte[] -> 'T =
  fun id data ->
    wrap (string id) (fun () ->
      use stream = new MemoryStream(data)
      use r = new BinaryReader(stream)

      // Read header
      let header = Header.read r

      // Validate remaining data length
      let remainingBytes = data.Length - 8 // header is 8 bytes (2 × uint32)
      if uint32 remainingBytes <> header.DataLength then
        Validation.validateDataLength header.DataLength (uint32 remainingBytes)

      reader r)


module PT =
  module PackageType =
    let serialize id value = makeSerializer PT.PackageType.write id value
    let deserialize id data = makeDeserializer PT.PackageType.read id data

  module PackageConstant =
    let serialize id value = makeSerializer PT.PackageConstant.write id value
    let deserialize id data = makeDeserializer PT.PackageConstant.read id data

  module PackageFn =
    let serialize id value = makeSerializer PT.PackageFn.write id value
    let deserialize id data = makeDeserializer PT.PackageFn.read id data

  module Toplevel =
    let serialize id value = makeSerializer PT.Toplevel.write id value
    let deserialize id data = makeDeserializer PT.Toplevel.read id data


module RT =
  // TODO upstream, it might be better to serialize a slightly lower type,
  // since we'll always have the corresponding ID in any context we use this
  // (just for type and constants?)
  module PackageType =
    let serialize id value = makeSerializer RT.PackageType.write id value
    let deserialize id data = makeDeserializer RT.PackageType.read id data

  module Dval =
    let serialize id value = makeSerializer RT.Dval.write id value
    let deserialize id data = makeDeserializer RT.Dval.read id data

  module Instructions =
    let serialize id value = makeSerializer RT.Instructions.write id value
    let deserialize id data = makeDeserializer RT.Instructions.read id data

  module PackageConstant =
    let serialize id value = makeSerializer RT.PackageConstant.write id value
    let deserialize id data = makeDeserializer RT.PackageConstant.read id data

  module PackageFn =
    let serialize id value = makeSerializer RT.PackageFn.write id value
    let deserialize id data = makeDeserializer RT.PackageFn.read id data
