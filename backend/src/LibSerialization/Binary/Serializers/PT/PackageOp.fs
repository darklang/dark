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


// -- BranchEventKind --

module BranchEventKind =
  let write (w : BinaryWriter) (k : BranchEventKind) : unit =
    match k with
    | Merged -> w.Write(0uy)
    | Archived -> w.Write(1uy)

  let read (r : BinaryReader) : BranchEventKind =
    match r.ReadByte() with
    | 0uy -> Merged
    | 1uy -> Archived
    | b -> raiseFormatError $"Invalid BranchEventKind tag: {b}"


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
  | PackageOp.SetName(location, target, previous) ->
    w.Write(3uy)
    PackageLocation.write w location
    Reference.write w target
    // A presence byte, not a sentinel hash: "no predecessor known" and "replaced the empty hash" are
    // different facts and only one of them is representable as a string.
    match previous with
    | None -> w.Write(0uy)
    | Some(Hash h) ->
      w.Write(1uy)
      String.write w h
  | PackageOp.Deprecate(target, kind, message) ->
    w.Write(4uy)
    Reference.write w target
    DeprecationKind.write w kind
    String.write w message
  | PackageOp.Undeprecate target ->
    w.Write(5uy)
    Reference.write w target
  // 8, not the freed 6/7. Recycling a tag makes an old blob decode as a DIFFERENT op
  // rather than failing, and silently decoding as something else is the worst thing
  // a format can do. Cheap to avoid: tags are arbitrary and there is no shortage of
  // them.
  | PackageOp.Resolve(decisionId, location, target) ->
    w.Write(8uy)
    String.write w decisionId
    PackageLocation.write w location
    Reference.write w target
  | PackageOp.Decide(kind, location, value, reason, decidedAt) ->
    w.Write(9uy)
    String.write w kind
    PackageLocation.write w location
    String.write w value
    String.write w reason
    String.write w decidedAt
  // A new TAG, which is why this costs nothing: every existing op's bytes are
  // untouched, so every existing op id (its content hash) is untouched, and an older
  // store simply never contains a 10.
  | PackageOp.BranchEvent(branchId, event, at) ->
    w.Write(10uy)
    String.write w branchId
    BranchEventKind.write w event
    String.write w at

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
    let previous =
      match r.ReadByte() with
      | 0uy -> None
      | 1uy -> Some(Hash(String.read r))
      | b -> raiseFormatError $"Invalid SetName previous tag: {b}"
    PackageOp.SetName(location, target, previous)
  | 4uy ->
    let target = Reference.read r
    let kind = DeprecationKind.read r
    let message = String.read r
    PackageOp.Deprecate(target, kind, message)
  | 5uy ->
    let target = Reference.read r
    PackageOp.Undeprecate target
  | 8uy ->
    let decisionId = String.read r
    let location = PackageLocation.read r
    let target = Reference.read r
    PackageOp.Resolve(decisionId, location, target)
  | 9uy ->
    let kind = String.read r
    let location = PackageLocation.read r
    let value = String.read r
    let reason = String.read r
    let decidedAt = String.read r
    PackageOp.Decide(kind, location, value, reason, decidedAt)
  | 10uy ->
    let branchId = String.read r
    let event = BranchEventKind.read r
    let at = String.read r
    PackageOp.BranchEvent(branchId, event, at)
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
