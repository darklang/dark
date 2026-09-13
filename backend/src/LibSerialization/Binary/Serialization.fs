/// Custom binary serialization for Dark values
module LibSerialization.Binary.Serialization

open System
open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

open LibSerialization.Binary.BaseFormat
open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers


// Stringifying the id is deferred to the error path: `string id` on a generic 'ID can route
// through F#'s reflection-based ToString for union types (e.g. `type Hash = Hash of
// string`), which is broken under AOT trimming.
let wrap (idF : unit -> string) (f : unit -> 'a) : 'a =
  try
    f ()
  with e ->
    Exception.callExceptionCallback e

    let id =
      try
        idF ()
      with idEx ->
        $"<id stringification failed: {idEx.GetType().Name}>"

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
    wrap (fun () -> string id) (fun () ->
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
        { Version = currentVersion; DataLength = uint32 payloadBytes.Length }

      Header.write finalWriter header
      finalWriter.Write(payloadBytes)
      finalWriter.Flush()

      finalStream.ToArray())


/// Version-dispatched deserializer: the reader receives the blob's format version, so a type whose
/// on-disk layout has changed can branch (`match version with 1u -> readV1 r | 2u -> readV2 r`). Keep
/// every historical readVN alongside one current writer; that is what lets a new binary decode an OLD
/// blob.
///
/// No reader dispatches on the version yet, because `currentVersion` is still 1: v1 is the first
/// format whose blobs outlive the binary that wrote them, since before it every store was rebuilt
/// from `.dark` on each build. This exists so the first layout change has somewhere to go.
let makeDeserializerV<'T, 'ID>
  (reader : uint32 -> BinaryReader -> 'T)
  : 'ID -> byte[] -> 'T =
  fun id data ->
    wrap (fun () -> string id) (fun () ->
      use stream = new MemoryStream(data)
      use r = new BinaryReader(stream)

      let header = Header.read r

      // Validate remaining data length
      let remainingBytes = data.Length - 8 // header is 8 bytes (2 x uint32)
      if uint32 remainingBytes <> header.DataLength then
        Validation.validateDataLength header.DataLength (uint32 remainingBytes)

      reader header.Version r)


/// Deserializer for a type with one on-disk layout: the reader never sees the blob's version.
/// Everything uses this; `makeDeserializerV` is for the first type that needs to decode two.
let makeDeserializer<'T, 'ID> (reader : BinaryReader -> 'T) : 'ID -> byte[] -> 'T =
  makeDeserializerV (fun _version r -> reader r)


module PT =
  module Hash =
    let serialize id value =
      makeSerializer
        LibSerialization.Binary.Serializers.PT.Common.Hash.write
        id
        value

    let deserialize id data =
      makeDeserializer
        LibSerialization.Binary.Serializers.PT.Common.Hash.read
        id
        data

  module PackageLocation =
    let serialize id value =
      makeSerializer
        LibSerialization.Binary.Serializers.PT.Common.PackageLocation.write
        id
        value

    let deserialize id data =
      makeDeserializer
        LibSerialization.Binary.Serializers.PT.Common.PackageLocation.read
        id
        data

  module PackageType =
    let serialize id value = makeSerializer PT.PackageType.write id value
    let deserialize id data = makeDeserializer PT.PackageType.read id data

  module PackageValue =
    let serialize id value = makeSerializer PT.PackageValue.write id value
    let deserialize id data = makeDeserializer PT.PackageValue.read id data

  module PackageFn =
    let serialize id value = makeSerializer PT.PackageFn.write id value
    let deserialize id data = makeDeserializer PT.PackageFn.read id data

  module PackageOp =
    let serialize id value = makeSerializer PT.PackageOp.write id value
    let deserialize id data = makeDeserializer PT.PackageOp.read id data

    /// The op, or None when THIS BUILD cannot read it. Nearly every reader wants this
    /// one rather than `deserialize`: a synced store's own log legitimately holds ops
    /// a peer authored on a newer format -- stored and left unapplied so a later
    /// build can apply them -- so local readers meet unreadable ops routinely.
    ///
    /// One decoder returns an Option, every reader tolerates, and no writer deletes
    /// what it could not decode: `Inserts.wholeMainDeletes` excludes them BY ID,
    /// never by whether they parse.
    let tryDeserialize (id : System.Guid) (data : byte[]) : Option<PT.PackageOp> =
      try
        Some(deserialize id data)
      with _ ->
        None

    /// Does this op BIND A NAME, judged from its tag alone and without decoding it?
    ///
    /// The point is what it avoids. A sync import decodes every incoming op to work out which names
    /// moved, but only `SetName` and `Decision` bind anything; `AddFn` and friends carry content and
    /// answer "no name". They are also the expensive ones, being whole ASTs, so decoding them to
    /// discover they say nothing was most of the cost of planning an import.
    ///
    /// `Unbind` (12) is deliberately not counted: it binds nothing, so the incoming side of the conflict
    /// detector has no binding to compare against yours. A peer removing a name you edited is settled by
    /// the fold's LWW rather than raised as a conflict. Open, and known.
    ///
    /// Reads the header for its length rather than assuming one, so a header change cannot silently
    /// shift which byte is read. An unreadable or truncated blob answers `false`: the caller learns
    /// nothing from it either way, and the one decoder is where a bad blob gets reported.
    let bindsAName (data : byte[]) : bool =
      try
        use stream = new MemoryStream(data)
        use reader = new BinaryReader(stream)
        let _header = Header.read reader

        // Tags from `PT.PackageOp.write`: 3 = SetName, 11 = Decision. Both are also the cheap ops to
        // decode, so nothing is gained by narrowing further.
        match reader.ReadByte() with
        | 3uy
        | 11uy -> true
        | _ -> false
      with _ ->
        false

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

  module PackageValue =
    let serialize id value = makeSerializer RT.PackageValue.write id value
    let deserialize id data = makeDeserializer RT.PackageValue.read id data

  module PackageFn =
    let serialize id value = makeSerializer RT.PackageFn.write id value
    let deserialize id data = makeDeserializer RT.PackageFn.read id data

  module ValueType =
    let serialize (vt : LibExecution.RuntimeTypes.ValueType) : byte[] =
      use stream = new MemoryStream()
      use w = new BinaryWriter(stream)
      RT.ValueType.write w vt
      w.Flush()
      stream.ToArray()

    let deserialize (data : byte[]) : LibExecution.RuntimeTypes.ValueType =
      use stream = new MemoryStream(data)
      use r = new BinaryReader(stream)
      RT.ValueType.read r
