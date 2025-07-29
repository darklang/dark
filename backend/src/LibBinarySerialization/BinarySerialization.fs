/// Custom binary serialization for Dark values
module LibBinarySerialization.BinarySerialization

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude

module PT = LibExecution.ProgramTypes
module ST = SerializedTypes
module PT2ST = ProgramTypesToSerializedTypes
module SExpr = LibBinarySerialization.Serializers.Expr
module SPackageType = LibBinarySerialization.Serializers.PackageType
module SPackageConstant = LibBinarySerialization.Serializers.PackageConstant
module SPackageFn = LibBinarySerialization.Serializers.PackageFn
module SToplevel = LibBinarySerialization.Serializers.Toplevel

let wrapSerializationException (id : string) (f : unit -> 'a) : 'a =
  try
    f ()
  with e ->
    Exception.callExceptionCallback e
    raise (
      Exception.InternalException(
        "error serializing with custom binary format",
        [ "id", id; "suggestion", "check custom binary serializer implementation" ],
        e
      )
    )

module Expr =
  let serialize (tlid : tlid) (e : PT.Expr) : byte[] =
    wrapSerializationException (string tlid) (fun () ->
      let serializableValue = PT2ST.Expr.toST e
      Primitives.Serialization.serializeWithHeader
        BinaryFormat.TypeId.Expr
        (fun writer -> SExpr.writeExpr writer serializableValue))

  let deserialize (tlid : tlid) (data : byte[]) : PT.Expr =
    wrapSerializationException (string tlid) (fun () ->
      let deserializedST =
        Primitives.Serialization.deserializeWithHeader SExpr.readExpr data
      PT2ST.Expr.toPT deserializedST)

module PackageType =
  let serialize (pt : PT.PackageType.PackageType) : byte[] =
    wrapSerializationException (string pt.id) (fun () ->
      let serializableValue = PT2ST.PackageType.toST pt
      Primitives.Serialization.serializeWithHeader
        BinaryFormat.TypeId.PackageType
        (fun writer -> SPackageType.write writer serializableValue))

  let deserialize (uuid : System.Guid) (data : byte[]) : PT.PackageType.PackageType =
    wrapSerializationException (string uuid) (fun () ->
      let deserializedST =
        Primitives.Serialization.deserializeWithHeader SPackageType.read data
      PT2ST.PackageType.toPT deserializedST)

module PackageConstant =
  let serialize (constant : PT.PackageConstant.PackageConstant) : byte[] =
    wrapSerializationException (string constant.id) (fun () ->
      let serializableValue = PT2ST.PackageConstant.toST constant
      Primitives.Serialization.serializeWithHeader
        BinaryFormat.TypeId.PackageConstant
        (fun writer -> SPackageConstant.write writer serializableValue))

  let deserialize
    (uuid : System.Guid)
    (data : byte[])
    : PT.PackageConstant.PackageConstant =
    wrapSerializationException (string uuid) (fun () ->
      let deserializedST =
        Primitives.Serialization.deserializeWithHeader SPackageConstant.read data
      PT2ST.PackageConstant.toPT deserializedST)

module PackageFn =
  let serialize (fn : PT.PackageFn.PackageFn) : byte[] =
    wrapSerializationException (string fn.id) (fun () ->
      let serializableValue = PT2ST.PackageFn.toST fn
      Primitives.Serialization.serializeWithHeader
        BinaryFormat.TypeId.PackageFn
        (fun writer -> SPackageFn.write writer serializableValue))

  let deserialize (uuid : System.Guid) (data : byte[]) : PT.PackageFn.PackageFn =
    wrapSerializationException (string uuid) (fun () ->
      let deserializedST =
        Primitives.Serialization.deserializeWithHeader SPackageFn.read data
      PT2ST.PackageFn.toPT deserializedST)

module Toplevel =
  let serialize (tl : PT.Toplevel.T) : byte[] =
    wrapSerializationException (string (PT.Toplevel.toTLID tl)) (fun () ->
      let serializableValue = PT2ST.Toplevel.toST tl
      Primitives.Serialization.serializeWithHeader
        BinaryFormat.TypeId.Toplevel
        (fun writer -> SToplevel.write writer serializableValue))

  let deserialize (tlid : tlid) (data : byte[]) : PT.Toplevel.T =
    wrapSerializationException (string tlid) (fun () ->
      let deserializedST =
        Primitives.Serialization.deserializeWithHeader SToplevel.read data
      PT2ST.Toplevel.toPT deserializedST)

module Toplevels =
  let serialize (msg : string) (tls : List<PT.Toplevel.T>) : byte[] =
    wrapSerializationException msg (fun () ->
      let serializableValues = List.map PT2ST.Toplevel.toST tls
      Primitives.Serialization.serializeWithHeader
        BinaryFormat.TypeId.Toplevels
        (fun writer ->
          Primitives.Writer.writeString writer msg
          Primitives.Writer.writeList writer SToplevel.write serializableValues))

  let deserialize (msg : string) (data : byte[]) : List<PT.Toplevel.T> =
    wrapSerializationException msg (fun () ->
      use stream = new System.IO.MemoryStream(data)
      use reader = new System.IO.BinaryReader(stream)
      let _header = Primitives.Reader.readHeader reader
      let _msg = Primitives.Reader.readString reader
      let deserializedSTs = Primitives.Reader.readList reader SToplevel.read
      List.map PT2ST.Toplevel.toPT deserializedSTs)
