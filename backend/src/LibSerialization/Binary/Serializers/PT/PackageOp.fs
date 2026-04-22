module LibSerialization.Binary.Serializers.PT.PackageOp

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common


// -- Reference --

module Reference =
  let write (w : BinaryWriter) (r : Reference) : unit =
    match r with
    | PackageType h ->
      w.Write(0uy)
      Hash.write w h
    | PackageValue h ->
      w.Write(1uy)
      Hash.write w h
    | PackageFn h ->
      w.Write(2uy)
      Hash.write w h

  let read (r : BinaryReader) : Reference =
    match r.ReadByte() with
    | 0uy -> PackageType(Hash.read r)
    | 1uy -> PackageValue(Hash.read r)
    | 2uy -> PackageFn(Hash.read r)
    | b -> raiseFormatError $"Invalid Reference tag: {b}"


// -- DeprecationKind --

module DeprecationKind =
  let write (w : BinaryWriter) (k : DeprecationKind) : unit =
    match k with
    | SupersededBy ref ->
      w.Write(0uy)
      Reference.write w ref
    | Harmful -> w.Write(1uy)
    | Obsolete -> w.Write(2uy)

  let read (r : BinaryReader) : DeprecationKind =
    match r.ReadByte() with
    | 0uy -> SupersededBy(Reference.read r)
    | 1uy -> Harmful
    | 2uy -> Obsolete
    | b -> raiseFormatError $"Invalid DeprecationKind tag: {b}"


// -- PropagateRepoint --

module PropagateRepoint =
  let write (w : BinaryWriter) (r : PropagateRepoint) : unit =
    PackageLocation.write w r.location
    Reference.write w r.fromRef
    Reference.write w r.toRef

  let read (r : BinaryReader) : PropagateRepoint =
    { location = PackageLocation.read r
      fromRef = Reference.read r
      toRef = Reference.read r }


// -- PackageOp --

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
  | PackageOp.SetName(location, target) ->
    w.Write(3uy)
    PackageLocation.write w location
    Reference.write w target
  | PackageOp.Deprecate(target, kind, message) ->
    w.Write(4uy)
    Reference.write w target
    DeprecationKind.write w kind
    String.write w message
  | PackageOp.Undeprecate target ->
    w.Write(5uy)
    Reference.write w target
  | PackageOp.PropagateUpdate(propagationId,
                              sourceLocation,
                              fromRefs,
                              toRef,
                              repoints) ->
    w.Write(6uy)
    Guid.write w propagationId
    PackageLocation.write w sourceLocation
    List.write w Reference.write fromRefs
    Reference.write w toRef
    List.write w PropagateRepoint.write repoints
  | PackageOp.RevertPropagation(revertId,
                                revertedPropagationIds,
                                sourceLocation,
                                restoredSourceRef,
                                revertedRepoints) ->
    w.Write(7uy)
    Guid.write w revertId
    List.write w Guid.write revertedPropagationIds
    PackageLocation.write w sourceLocation
    Reference.write w restoredSourceRef
    List.write w PropagateRepoint.write revertedRepoints


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
    let location = PackageLocation.read r
    let target = Reference.read r
    PackageOp.SetName(location, target)
  | 4uy ->
    let target = Reference.read r
    let kind = DeprecationKind.read r
    let message = String.read r
    PackageOp.Deprecate(target, kind, message)
  | 5uy ->
    let target = Reference.read r
    PackageOp.Undeprecate target
  | 6uy ->
    let propagationId = Guid.read r
    let sourceLocation = PackageLocation.read r
    let fromRefs = List.read r Reference.read
    let toRef = Reference.read r
    let repoints = List.read r PropagateRepoint.read
    PackageOp.PropagateUpdate(
      propagationId,
      sourceLocation,
      fromRefs,
      toRef,
      repoints
    )
  | 7uy ->
    let revertId = Guid.read r
    let revertedPropagationIds = List.read r Guid.read
    let sourceLocation = PackageLocation.read r
    let restoredSourceRef = Reference.read r
    let revertedRepoints = List.read r PropagateRepoint.read
    PackageOp.RevertPropagation(
      revertId,
      revertedPropagationIds,
      sourceLocation,
      restoredSourceRef,
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
