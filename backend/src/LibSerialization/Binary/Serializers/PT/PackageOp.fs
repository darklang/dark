module LibSerialization.Binary.Serializers.PT.PackageOp

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common


let write (w : BinaryWriter) (op : PackageOp) : unit =
  match op with
  | PackageOp.AddType typ ->
    w.Write(0uy)
    LibSerialization.Binary.Serializers.PT.PackageType.write w typ
  | PackageOp.AddValue value ->
    w.Write(1uy)
    LibSerialization.Binary.Serializers.PT.PackageValue.write w value
  | PackageOp.AddFn fn ->
    w.Write(2uy)
    LibSerialization.Binary.Serializers.PT.PackageFn.write w fn
  | PackageOp.SetTypeName(hash, location) ->
    w.Write(3uy)
    FQTypeName.Package.write w hash
    PackageLocation.write w location
  | PackageOp.SetValueName(hash, location) ->
    w.Write(4uy)
    FQValueName.Package.write w hash
    PackageLocation.write w location
  | PackageOp.SetFnName(hash, location) ->
    w.Write(5uy)
    FQFnName.Package.write w hash
    PackageLocation.write w location
  | PackageOp.PropagateUpdate(propagationId,
                              sourceLocation,
                              sourceItemKind,
                              fromSourceHashes,
                              toSourceHash,
                              repoints) ->
    w.Write(6uy)
    Guid.write w propagationId
    PackageLocation.write w sourceLocation
    String.write w (sourceItemKind.toString ())
    List.write w ContentHash.write fromSourceHashes
    ContentHash.write w toSourceHash
    List.write
      w
      (fun w (r : PropagateRepoint) ->
        PackageLocation.write w r.location
        String.write w (r.itemKind.toString ())
        ContentHash.write w r.fromHash
        ContentHash.write w r.toHash)
      repoints
  | PackageOp.RevertPropagation(revertId,
                                revertedPropagationIds,
                                sourceLocation,
                                sourceItemKind,
                                restoredSourceHash,
                                revertedRepoints) ->
    w.Write(7uy)
    Guid.write w revertId
    List.write w (fun w id -> Guid.write w id) revertedPropagationIds
    PackageLocation.write w sourceLocation
    String.write w (sourceItemKind.toString ())
    ContentHash.write w restoredSourceHash
    List.write
      w
      (fun w (r : PropagateRepoint) ->
        PackageLocation.write w r.location
        String.write w (r.itemKind.toString ())
        ContentHash.write w r.fromHash
        ContentHash.write w r.toHash)
      revertedRepoints

let read (r : BinaryReader) : PackageOp =
  match r.ReadByte() with
  | 0uy ->
    let typ = LibSerialization.Binary.Serializers.PT.PackageType.read r
    PackageOp.AddType typ
  | 1uy ->
    let value = LibSerialization.Binary.Serializers.PT.PackageValue.read r
    PackageOp.AddValue value
  | 2uy ->
    let fn = LibSerialization.Binary.Serializers.PT.PackageFn.read r
    PackageOp.AddFn fn
  | 3uy ->
    let hash = FQTypeName.Package.read r
    let location = PackageLocation.read r
    PackageOp.SetTypeName(hash, location)
  | 4uy ->
    let hash = FQValueName.Package.read r
    let location = PackageLocation.read r
    PackageOp.SetValueName(hash, location)
  | 5uy ->
    let hash = FQFnName.Package.read r
    let location = PackageLocation.read r
    PackageOp.SetFnName(hash, location)
  | 6uy ->
    let propagationId = Guid.read r
    let sourceLocation = PackageLocation.read r
    let sourceItemKind = String.read r |> ItemKind.fromString
    let fromSourceHashes = List.read r ContentHash.read
    let toSourceHash = ContentHash.read r
    let repoints =
      List.read r (fun r ->
        { location = PackageLocation.read r
          itemKind = String.read r |> ItemKind.fromString
          fromHash = ContentHash.read r
          toHash = ContentHash.read r })
    PackageOp.PropagateUpdate(
      propagationId,
      sourceLocation,
      sourceItemKind,
      fromSourceHashes,
      toSourceHash,
      repoints
    )
  | 7uy ->
    let revertId = Guid.read r
    let revertedPropagationIds = List.read r (fun r -> Guid.read r)
    let sourceLocation = PackageLocation.read r
    let sourceItemKind = String.read r |> ItemKind.fromString
    let restoredSourceHash = ContentHash.read r
    let revertedRepoints =
      List.read r (fun r ->
        { location = PackageLocation.read r
          itemKind = String.read r |> ItemKind.fromString
          fromHash = ContentHash.read r
          toHash = ContentHash.read r })
    PackageOp.RevertPropagation(
      revertId,
      revertedPropagationIds,
      sourceLocation,
      sourceItemKind,
      restoredSourceHash,
      revertedRepoints
    )
  | b -> raiseFormatError $"Invalid PackageOp tag: {b}"


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
    raiseFormatError $"PackageOp id mismatch: expected {id}, got {readId}"
  read binaryReader
