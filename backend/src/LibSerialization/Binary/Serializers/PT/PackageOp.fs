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
    | PackageTrait h ->
      w.Write(3uy)
      Hash.write w h
    | PackageTraitImpl h ->
      w.Write(4uy)
      Hash.write w h
    | PackageTest h ->
      w.Write(5uy)
      Hash.write w h

  let read (r : BinaryReader) : Reference =
    match r.ReadByte() with
    | 0uy -> PackageType(Hash.read r)
    | 1uy -> PackageValue(Hash.read r)
    | 2uy -> PackageFn(Hash.read r)
    | 3uy -> PackageTrait(Hash.read r)
    | 4uy -> PackageTraitImpl(Hash.read r)
    | 5uy -> PackageTest(Hash.read r)
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


// -- DecisionKind --

module DecisionKind =
  let write (w : BinaryWriter) (k : DecisionKind) : unit =
    match k with
    | Override target ->
      w.Write(0uy)
      Reference.write w target
    | Restore(revision, target, names) ->
      w.Write(2uy)
      Guid.write w revision
      Reference.write w target
      List.write
        w
        (fun w (reference, location) ->
          Reference.write w reference
          match location with
          | None -> w.Write(0uy)
          | Some location ->
            w.Write(1uy)
            PackageLocation.write w location)
        names
    | Ack findingId ->
      w.Write(1uy)
      String.write w findingId

  let read (r : BinaryReader) : DecisionKind =
    match r.ReadByte() with
    | 0uy -> Override(Reference.read r)
    | 1uy -> Ack(String.read r)
    | 2uy ->
      let revision = Guid.read r
      let target = Reference.read r
      let names =
        List.read r (fun r ->
          let reference = Reference.read r
          let location =
            match r.ReadByte() with
            | 0uy -> None
            | 1uy -> Some(PackageLocation.read r)
            | b -> raiseFormatError $"Invalid reference location tag: {b}"
          reference, location)
      Restore(revision, target, names)
    | b -> raiseFormatError $"Invalid DecisionKind tag: {b}"



// -- DocPart --

module DocPart =
  let write (w : BinaryWriter) (part : DocPart) : unit =
    match part with
    | DocPart.WholeItem -> w.Write(0uy)
    | DocPart.RecordField name ->
      w.Write(1uy)
      String.write w name
    | DocPart.EnumCase name ->
      w.Write(2uy)
      String.write w name
    | DocPart.Parameter index ->
      w.Write(3uy)
      w.Write(index)

  let read (r : BinaryReader) : DocPart =
    match r.ReadByte() with
    | 0uy -> DocPart.WholeItem
    | 1uy -> DocPart.RecordField(String.read r)
    | 2uy -> DocPart.EnumCase(String.read r)
    | 3uy -> DocPart.Parameter(r.ReadInt32())
    | b -> raiseFormatError $"Invalid DocPart tag: {b}"


// -- PackageOp --

/// Read a `restating` stamp written at the END of the payload, or None when the payload ends here.
///
/// The one place a field is not self-delimiting, and it is deliberate. A `PackageOp` is only ever
/// serialized standalone -- `Serialization.makeSerializer` writes a header carrying the payload's
/// real length and nothing follows it -- so "is there another byte" is a question this reader can
/// ask. Do not copy the trick into a field that could ever be followed by another op.
///
/// What it buys: an EARLIER binary meeting a stamped op reads the target, the kind and the message,
/// stops, ignores the trailing bytes and APPLIES the deprecation with a fresh `origin_ts`, so it
/// agrees with a newer binary about what is deprecated. A new tag would have decoded as nothing at
/// all and left the item reading as undeprecated until the old binary caught up, and the direction
/// of that failure is "not deprecated", which for `Harmful` is the unsafe one.
///
/// What it costs: tags 4 and 5 are no longer self-delimiting, and an earlier binary silently decodes
/// the blob as an op that is not quite the one written. That is normally the worst thing a format
/// can do, and it is tolerable HERE for one reason only: the dropped field is the stamp, which is
/// part of the op's identity and is never read by anything that acts on the op.
let private readTrailingStamp (r : BinaryReader) : Option<string> =
  if r.BaseStream.Position >= r.BaseStream.Length then
    None
  else
    match r.ReadByte() with
    | 0uy -> None
    | 1uy -> Some(String.read r)
    | b -> raiseFormatError $"Invalid trailing restating tag: {b}"

/// The counterpart: nothing at all when there is no stamp, so every stamp-free blob is byte for
/// byte what earlier builds wrote and every op id already in a store is stable.
let private writeTrailingStamp (w : BinaryWriter) (stamp : Option<string>) : unit =
  match stamp with
  | None -> ()
  | Some stamp ->
    w.Write(1uy)
    String.write w stamp


let private writeSnapshot (w : BinaryWriter) (snapshot : ReferenceNamesSnapshot) =
  let { location = location; target = target; names = names; restating = restating } =
    snapshot
  PackageLocation.write w location
  Reference.write w target
  w.Write(List.length names)
  for (reference, location) in names do
    Reference.write w reference
    match location with
    | None -> w.Write(0uy)
    | Some loc ->
      w.Write(1uy)
      PackageLocation.write w loc
  // Unlike the legacy deprecation tags, this payload is self-delimiting:
  // snapshots can be nested inside an atomic Rename.
  match restating with
  | None -> w.Write(0uy)
  | Some stamp ->
    w.Write(1uy)
    String.write w stamp

let private readSnapshot (r : BinaryReader) : ReferenceNamesSnapshot =
  let location = PackageLocation.read r
  let target = Reference.read r
  let count = r.ReadInt32()
  if count < 0 then raiseFormatError "Invalid reference names count"
  let names =
    List.init count (fun _ ->
      let reference = Reference.read r
      let location =
        match r.ReadByte() with
        | 0uy -> None
        | 1uy -> Some(PackageLocation.read r)
        | b -> raiseFormatError $"Invalid reference location tag: {b}"
      reference, location)
  let restating =
    match r.ReadByte() with
    | 0uy -> None
    | 1uy -> Some(String.read r)
    | b -> raiseFormatError $"Invalid reference names restating tag: {b}"
  { location = location; target = target; names = names; restating = restating }



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
  | PackageOp.AddTrait t ->
    w.Write(15uy)
    LibSerialization.Binary.Serializers.PT.Trait.write w t
  | PackageOp.AddTraitImpl i ->
    w.Write(16uy)
    LibSerialization.Binary.Serializers.PT.Trait.TraitImpl.write w i
  | PackageOp.AddTest test ->
    // New op kinds never recycle retired tags; 15 and 16 are traits'.
    w.Write(17uy)
    LibSerialization.Binary.Serializers.PT.PackageTest.write w test
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
  | PackageOp.Unbind(location, previous) ->
    w.Write(12uy)
    PackageLocation.write w location
    match previous with
    | None -> w.Write(0uy)
    | Some(Hash h) ->
      w.Write(1uy)
      String.write w h
  // The stamp rides on the EXISTING tags as a trailing field rather than taking new ones, so an
  // older binary applies a restated deprecation correctly instead of storing it unapplied. See
  // `readTrailingStamp` for what that trades away.
  | PackageOp.Deprecate(target, kind, message, restating) ->
    w.Write(4uy)
    Reference.write w target
    DeprecationKind.write w kind
    String.write w message
    writeTrailingStamp w restating
  | PackageOp.Undeprecate(target, restating) ->
    w.Write(5uy)
    Reference.write w target
    writeTrailingStamp w restating
  | PackageOp.Rename(oldLocation, newLocation, target, references, stamp) ->
    w.Write(19uy)
    PackageLocation.write w oldLocation
    PackageLocation.write w newLocation
    Reference.write w target
    w.Write(List.length references)
    for reference in references do
      w.Write(18uy)
      writeSnapshot w reference
    String.write w stamp
  | PackageOp.SetReferenceNames(location, target, names, restating) ->
    w.Write(18uy)
    writeSnapshot
      w
      { location = location; target = target; names = names; restating = restating }
  | PackageOp.UpdateDoc(location, part, text, previous, restating) ->
    w.Write(14uy)
    PackageLocation.write w location
    DocPart.write w part
    String.write w text
    (match previous with
     | None -> w.Write(0uy)
     | Some(Hash h) ->
       w.Write(1uy)
       String.write w h)
    match restating with
    | None -> w.Write(0uy)
    | Some stamp ->
      w.Write(1uy)
      String.write w stamp
  // 11, not one of the retired 6-10 (nor 13, which was `Describe` before docs learned to name a
  // predecessor). A retired tag is never recycled: an old blob would then decode as a DIFFERENT op
  // rather than failing, and silently decoding as something else is the worst thing a format can
  // do. Cheap to avoid -- tags are arbitrary and there is no shortage of them.
  | PackageOp.Decision(id, location, reason, kind) ->
    w.Write(11uy)
    String.write w id
    PackageLocation.write w location
    String.write w reason
    DecisionKind.write w kind

let read (version : uint32) (r : BinaryReader) : PackageOp =
  match r.ReadByte() with
  | 0uy ->
    let typ = LibSerialization.Binary.Serializers.PT.PackageType.read version r
    PackageOp.AddType typ
  | 1uy ->
    let value = LibSerialization.Binary.Serializers.PT.PackageValue.read version r
    PackageOp.AddValue value
  | 2uy ->
    let fn = LibSerialization.Binary.Serializers.PT.PackageFn.read version r
    PackageOp.AddFn fn
  | 15uy -> PackageOp.AddTrait(LibSerialization.Binary.Serializers.PT.Trait.read r)
  | 16uy ->
    PackageOp.AddTraitImpl(
      LibSerialization.Binary.Serializers.PT.Trait.TraitImpl.read version r
    )
  | 17uy ->
    let test = LibSerialization.Binary.Serializers.PT.PackageTest.read version r
    PackageOp.AddTest test
  | 3uy ->
    let location = PackageLocation.read r
    let target = Reference.read r
    let previous =
      match r.ReadByte() with
      | 0uy -> None
      | 1uy -> Some(Hash(String.read r))
      | b -> raiseFormatError $"Invalid SetName previous tag: {b}"
    PackageOp.SetName(location, target, previous)
  | 12uy ->
    let location = PackageLocation.read r
    let previous =
      match r.ReadByte() with
      | 0uy -> None
      | 1uy -> Some(Hash(String.read r))
      | b -> raiseFormatError $"Invalid Unbind previous tag: {b}"
    PackageOp.Unbind(location, previous)
  | 4uy ->
    let target = Reference.read r
    let kind = DeprecationKind.read r
    let message = String.read r
    PackageOp.Deprecate(target, kind, message, readTrailingStamp r)
  | 5uy ->
    let target = Reference.read r
    PackageOp.Undeprecate(target, readTrailingStamp r)
  | 19uy ->
    let oldLocation = PackageLocation.read r
    let newLocation = PackageLocation.read r
    let target = Reference.read r
    let count = r.ReadInt32()
    if count < 0 then raiseFormatError "Invalid rename reference count"
    // Retain the existing tag-18 payload inside tag 19 for byte-for-byte compatibility.
    let references =
      List.init count (fun _ ->
        if r.ReadByte() <> 18uy then
          raiseFormatError "A rename may contain only reference-name snapshots"
        readSnapshot r)
    PackageOp.Rename(oldLocation, newLocation, target, references, String.read r)
  | 18uy ->
    let snapshot = readSnapshot r
    PackageOp.SetReferenceNames(
      snapshot.location,
      snapshot.target,
      snapshot.names,
      snapshot.restating
    )
  | 14uy ->
    let location = PackageLocation.read r
    let part = DocPart.read r
    let text = String.read r
    let previous =
      match r.ReadByte() with
      | 0uy -> None
      | 1uy -> Some(Hash(String.read r))
      | b -> raiseFormatError $"Invalid UpdateDoc previous tag: {b}"
    let restating =
      match r.ReadByte() with
      | 0uy -> None
      | 1uy -> Some(String.read r)
      | b -> raiseFormatError $"Invalid UpdateDoc restating tag: {b}"
    PackageOp.UpdateDoc(location, part, text, previous, restating)
  | 11uy ->
    let id = String.read r
    let location = PackageLocation.read r
    let reason = String.read r
    let kind = DecisionKind.read r
    PackageOp.Decision(id, location, reason, kind)
  | b -> raiseFormatError $"Invalid PackageOp tag: {b}"


let serialize (id : uuid) (op : PackageOp) : byte array =
  Scratch.WithStream(fun memoryStream binaryWriter ->
    Guid.write binaryWriter id
    write binaryWriter op
    memoryStream.ToArray())

let deserialize (id : uuid) (bytes : byte array) : PackageOp =
  use memoryStream = new MemoryStream(bytes)
  use binaryReader = new BinaryReader(memoryStream)
  let readId = Guid.read binaryReader
  if readId <> id then
    raiseFormatError $"PackageOp id mismatch: expected {id}, got {readId}"
  // Headerless form carries no version; it is only ever written by this build.
  read LibSerialization.Binary.BaseFormat.CurrentVersion binaryReader
