module LibBinarySerialization.Serializers.PT.PackageOp

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.PT.Common


let write (w : BinaryWriter) (op : PackageOp) : unit =
  match op with
  | PackageOp.AddType typ ->
    w.Write(0uy)
    LibBinarySerialization.Serializers.PT.PackageType.write w typ
  | PackageOp.AddValue value ->
    w.Write(1uy)
    LibBinarySerialization.Serializers.PT.PackageValue.write w value
  | PackageOp.AddFn fn ->
    w.Write(2uy)
    LibBinarySerialization.Serializers.PT.PackageFn.write w fn
  | PackageOp.SetTypeName(id, location) ->
    w.Write(3uy)
    FQTypeName.Package.write w id
    PackageLocation.write w location
  | PackageOp.SetValueName(id, location) ->
    w.Write(4uy)
    FQValueName.Package.write w id
    PackageLocation.write w location
  | PackageOp.SetFnName(id, location) ->
    w.Write(5uy)
    FQFnName.Package.write w id
    PackageLocation.write w location
  | PackageOp.PropagateUpdate(propagationId,
                              sourceLocation,
                              sourceItemKind,
                              fromSourceUUIDs,
                              toSourceUUID,
                              repoints) ->
    w.Write(6uy)
    Guid.write w propagationId
    PackageLocation.write w sourceLocation
    String.write w (sourceItemKind.toString ())
    List.write w (fun w id -> Guid.write w id) fromSourceUUIDs
    Guid.write w toSourceUUID
    List.write
      w
      (fun w (r : PropagateRepoint) ->
        PackageLocation.write w r.location
        String.write w (r.itemKind.toString ())
        Guid.write w r.fromUUID
        Guid.write w r.toUUID)
      repoints
  | PackageOp.RevertPropagation(revertId,
                                revertedPropagationIds,
                                sourceLocation,
                                sourceItemKind,
                                restoredSourceUUID,
                                revertedRepoints) ->
    w.Write(7uy)
    Guid.write w revertId
    List.write w (fun w id -> Guid.write w id) revertedPropagationIds
    PackageLocation.write w sourceLocation
    String.write w (sourceItemKind.toString ())
    Guid.write w restoredSourceUUID
    List.write
      w
      (fun w (r : PropagateRepoint) ->
        PackageLocation.write w r.location
        String.write w (r.itemKind.toString ())
        Guid.write w r.fromUUID
        Guid.write w r.toUUID)
      revertedRepoints

let read (r : BinaryReader) : PackageOp =
  match r.ReadByte() with
  | 0uy ->
    let typ = LibBinarySerialization.Serializers.PT.PackageType.read r
    PackageOp.AddType typ
  | 1uy ->
    let value = LibBinarySerialization.Serializers.PT.PackageValue.read r
    PackageOp.AddValue value
  | 2uy ->
    let fn = LibBinarySerialization.Serializers.PT.PackageFn.read r
    PackageOp.AddFn fn
  | 3uy ->
    let id = FQTypeName.Package.read r
    let location = PackageLocation.read r
    PackageOp.SetTypeName(id, location)
  | 4uy ->
    let id = FQValueName.Package.read r
    let location = PackageLocation.read r
    PackageOp.SetValueName(id, location)
  | 5uy ->
    let id = FQFnName.Package.read r
    let location = PackageLocation.read r
    PackageOp.SetFnName(id, location)
  | 6uy ->
    let propagationId = Guid.read r
    let sourceLocation = PackageLocation.read r
    let sourceItemKind = String.read r |> ItemKind.fromString
    let fromSourceUUIDs = List.read r (fun r -> Guid.read r)
    let toSourceUUID = Guid.read r
    let repoints =
      List.read r (fun r ->
        { location = PackageLocation.read r
          itemKind = String.read r |> ItemKind.fromString
          fromUUID = Guid.read r
          toUUID = Guid.read r })
    PackageOp.PropagateUpdate(
      propagationId,
      sourceLocation,
      sourceItemKind,
      fromSourceUUIDs,
      toSourceUUID,
      repoints
    )
  | 7uy ->
    let revertId = Guid.read r
    let revertedPropagationIds = List.read r (fun r -> Guid.read r)
    let sourceLocation = PackageLocation.read r
    let sourceItemKind = String.read r |> ItemKind.fromString
    let restoredSourceUUID = Guid.read r
    let revertedRepoints =
      List.read r (fun r ->
        { location = PackageLocation.read r
          itemKind = String.read r |> ItemKind.fromString
          fromUUID = Guid.read r
          toUUID = Guid.read r })
    PackageOp.RevertPropagation(
      revertId,
      revertedPropagationIds,
      sourceLocation,
      sourceItemKind,
      restoredSourceUUID,
      revertedRepoints
    )
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid PackageOp tag: {b}"))


let serialize (id : uuid) (op : PackageOp) : byte array =
  use memoryStream = new MemoryStream()
  use binaryWriter = new BinaryWriter(memoryStream)
  Guid.write binaryWriter id
  write binaryWriter op
  memoryStream.ToArray()

let deserialize (id : uuid) (bytes : byte array) : PackageOp =
  use memoryStream = new MemoryStream(bytes)
  use binaryReader = new BinaryReader(memoryStream)
  let readId = Guid.read binaryReader
  if readId <> id then
    raise (
      BinaryFormatException(
        CorruptedData $"PackageOp id mismatch: expected {id}, got {readId}"
      )
    )
  read binaryReader
