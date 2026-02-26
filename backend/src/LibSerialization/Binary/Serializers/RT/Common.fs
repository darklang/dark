module LibSerialization.Binary.Serializers.RT.Common

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibSerialization.Binary.Serializers.Common

module ContentHash =
  let write (w : BinaryWriter) (ContentHash h : ContentHash) = String.write w h
  let read (r : BinaryReader) : ContentHash = ContentHash(String.read r)


module NameResolutionError =
  let rec write (w : BinaryWriter) (error : NameResolutionError) =
    match error with
    | NotFound -> w.Write 0uy
    | InvalidName -> w.Write 1uy

  and read (r : BinaryReader) : NameResolutionError =
    match r.ReadByte() with
    | 0uy -> NotFound
    | 1uy -> InvalidName
    | b -> raiseFormatError $"Invalid NameResolutionError tag: {b}"


module NameResolution =
  let write
    (writeInner : BinaryWriter -> 'inner -> unit)
    (w : BinaryWriter)
    (nr : NameResolution<'inner>)
    : unit =
    List.write w String.write nr.originalName
    match nr.resolved with
    | Ok name ->
      w.Write 0uy
      writeInner w name
    | Error error ->
      w.Write 1uy
      NameResolutionError.write w error

  let read
    (readInner : BinaryReader -> 'inner)
    (r : BinaryReader)
    : NameResolution<'inner> =
    let originalName = List.read r String.read
    let resolved =
      match r.ReadByte() with
      | 0uy -> Ok(readInner r)
      | 1uy ->
        let error = NameResolutionError.read r
        Error(error)
      | b -> raiseFormatError $"Invalid NameResolution tag: {b}"
    { originalName = originalName; resolved = resolved }


module FQTypeName =
  let write (w : BinaryWriter) (n : FQTypeName.FQTypeName) : unit =
    match n with
    | FQTypeName.Package h -> ContentHash.write w h

  let read (r : BinaryReader) : FQTypeName.FQTypeName =
    let h = ContentHash.read r
    FQTypeName.Package h


module FQFnName =
  let write (w : BinaryWriter) (n : FQFnName.FQFnName) : unit =
    match n with
    | FQFnName.Builtin b ->
      w.Write 0uy
      String.write w b.name
      w.Write b.version
    | FQFnName.Package h ->
      w.Write 1uy
      ContentHash.write w h

  let read (r : BinaryReader) : FQFnName.FQFnName =
    match r.ReadByte() with
    | 0uy ->
      let name = String.read r
      let version = r.ReadInt32()
      FQFnName.Builtin { name = name; version = version }
    | 1uy ->
      let h = ContentHash.read r
      FQFnName.Package h
    | b -> raiseFormatError $"Invalid FQFnName tag: {b}"


module FQValueName =
  let write (w : BinaryWriter) (n : FQValueName.FQValueName) : unit =
    match n with
    | FQValueName.Builtin builtin ->
      w.Write 0uy
      String.write w builtin.name
      w.Write builtin.version
    | FQValueName.Package h ->
      w.Write 1uy
      ContentHash.write w h

  let read (r : BinaryReader) : FQValueName.FQValueName =
    match r.ReadByte() with
    | 0uy ->
      let name = String.read r
      let version = r.ReadInt32()
      FQValueName.Builtin { name = name; version = version }
    | 1uy ->
      let h = ContentHash.read r
      FQValueName.Package h
    | b -> raiseFormatError $"Invalid FQValueName tag: {b}"


module Sign =
  let write (w : BinaryWriter) (sign : Sign) : unit =
    match sign with
    | Positive -> w.Write 0uy
    | Negative -> w.Write 1uy

  let read (r : BinaryReader) : Sign =
    match r.ReadByte() with
    | 0uy -> Positive
    | 1uy -> Negative
    | b -> raiseFormatError $"Invalid Sign tag: {b}"
