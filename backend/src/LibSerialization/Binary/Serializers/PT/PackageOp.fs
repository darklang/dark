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


// -- DecisionKind --

module PropagationPolicy =
  let write (w : BinaryWriter) (p : PropagationPolicy) : unit =
    match p with
    | Pin -> w.Write(0uy)
    | Follow -> w.Write(1uy)
    | Unset -> w.Write(2uy)

  let read (r : BinaryReader) : PropagationPolicy =
    match r.ReadByte() with
    | 0uy -> Pin
    | 1uy -> Follow
    | 2uy -> Unset
    | b -> raiseFormatError $"Invalid PropagationPolicy tag: {b}"

module DecisionKind =
  let write (w : BinaryWriter) (k : DecisionKind) : unit =
    match k with
    | Override target ->
      w.Write(0uy)
      Reference.write w target
    | Ack findingId ->
      w.Write(1uy)
      String.write w findingId
    | Propagation policy ->
      w.Write(2uy)
      PropagationPolicy.write w policy

  let read (r : BinaryReader) : DecisionKind =
    match r.ReadByte() with
    | 0uy -> Override(Reference.read r)
    | 1uy -> Ack(String.read r)
    | 2uy -> Propagation(PropagationPolicy.read r)
    | b -> raiseFormatError $"Invalid DecisionKind tag: {b}"


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
  // 11, not the freed 6/7/8/9. Recycling a tag makes an old blob decode as a DIFFERENT
  // op rather than failing, and silently decoding as something else is the worst thing
  // a format can do. Cheap to avoid: tags are arbitrary and there is no shortage of
  // them.
  | PackageOp.Decision(id, location, reason, kind) ->
    w.Write(11uy)
    String.write w id
    PackageLocation.write w location
    String.write w reason
    DecisionKind.write w kind
  | PackageOp.BranchEvent(branchId, event, at) ->
    w.Write(10uy)
    Guid.write w branchId.Guid
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
  | 10uy ->
    let branchId = LibExecution.Branching.BranchId.Id(Guid.read r)
    let event = BranchEventKind.read r
    let at = String.read r
    PackageOp.BranchEvent(branchId, event, at)
  | 11uy ->
    let id = String.read r
    let location = PackageLocation.read r
    let reason = String.read r
    let kind = DecisionKind.read r
    PackageOp.Decision(id, location, reason, kind)
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
