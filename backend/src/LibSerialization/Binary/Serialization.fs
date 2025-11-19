/// Custom binary serialization for Dark values
module LibSerialization.Binary.Serialization

open System
open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibSerialization.Binary
open BaseFormat
open Serializers.Common


let wrap (f : unit -> 'a) : 'a =
  try
    f ()
  with e ->
    Exception.callExceptionCallback e

    Exception.InternalException(
      "error serializing/deserializing with custom binary format",
      [],
      e
    )
    |> raise


/// Create an optimized serializer function with embedded error handling
let makeSerializer<'T> (writer : BinaryWriter -> 'T -> unit) : 'T -> byte[] =
  fun value ->
    wrap (fun () ->
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
let makeDeserializer<'T> (reader : BinaryReader -> 'T) : byte[] -> 'T =
  fun data ->
    wrap (fun () ->
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
  open Serializers.PT.Common

  module PackageLocation =
    let serialize value = makeSerializer PackageLocation.write value

    let deserialize data = makeDeserializer PackageLocation.read data

  module PackageType =
    let serialize value = makeSerializer Serializers.PT.PackageType.write value
    let deserialize data = makeDeserializer Serializers.PT.PackageType.read data

  module PackageValue =
    let serialize value = makeSerializer Serializers.PT.PackageValue.write value
    let deserialize data = makeDeserializer Serializers.PT.PackageValue.read data

  module PackageFn =
    let serialize value = makeSerializer Serializers.PT.PackageFn.write value
    let deserialize data = makeDeserializer Serializers.PT.PackageFn.read data

  module PackageOp =
    let serialize value = makeSerializer Serializers.PT.PackageOp.write value
    let deserialize data = makeDeserializer Serializers.PT.PackageOp.read data

  module Toplevel =
    let serialize value = makeSerializer Serializers.PT.Toplevel.write value
    let deserialize data = makeDeserializer Serializers.PT.Toplevel.read data


module RT =
  // TODO upstream, it might be better to serialize a slightly lower type,
  // since we'll always have the corresponding ID in any context we use this
  // (just for type and constants?)
  module PackageType =
    let serialize value = makeSerializer Serializers.RT.PackageType.write value
    let deserialize data = makeDeserializer Serializers.RT.PackageType.read data

  module Dval =
    let serialize value = makeSerializer Serializers.RT.Dval.write value
    let deserialize data = makeDeserializer Serializers.RT.Dval.read data

  module Instructions =
    let serialize value = makeSerializer Serializers.RT.Instructions.write value
    let deserialize data = makeDeserializer Serializers.RT.Instructions.read data

  module PackageValue =
    let serialize value = makeSerializer Serializers.RT.PackageValue.write value
    let deserialize data = makeDeserializer Serializers.RT.PackageValue.read data

  module PackageFn =
    let serialize value = makeSerializer Serializers.RT.PackageFn.write value
    let deserialize data = makeDeserializer Serializers.RT.PackageFn.read data
