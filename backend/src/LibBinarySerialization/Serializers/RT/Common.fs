module LibBinarySerialization.Serializers.RT.Common

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common

module NameResolutionError =
  let rec write (w : BinaryWriter) (error : NameResolutionError) =
    match error with
    | NotFound names ->
      w.Write 0uy
      List.write w String.write names
    | InvalidName names ->
      w.Write 1uy
      List.write w String.write names

  and read (r : BinaryReader) : NameResolutionError =
    match r.ReadByte() with
    | 0uy ->
      let names = List.read r String.read
      NotFound names
    | 1uy ->
      let names = List.read r String.read
      InvalidName names
    | b ->
      raise (
        BinaryFormatException(CorruptedData $"Invalid NameResolutionError tag: {b}")
      )


module NameResolution =
  let write
    (writeInner : BinaryWriter -> 'inner -> unit)
    (w : BinaryWriter)
    (nr : NameResolution<'inner>)
    : unit =
    match nr with
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
    match r.ReadByte() with
    | 0uy -> Ok(readInner r)
    | 1uy ->
      let error = NameResolutionError.read r
      Error(error)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid NameResolution tag: {b}"))


module FQTypeName =
  let write (w : BinaryWriter) (n : FQTypeName.FQTypeName) : unit =
    match n with
    | FQTypeName.Package id -> Hash.write w id

  let read (r : BinaryReader) : FQTypeName.FQTypeName =
    let id = Hash.read r
    FQTypeName.Package id


module FQFnName =
  let write (w : BinaryWriter) (n : FQFnName.FQFnName) : unit =
    match n with
    | FQFnName.Builtin b ->
      w.Write 0uy
      String.write w b.name
      w.Write b.version
    | FQFnName.Package id ->
      w.Write 1uy
      Hash.write w id

  let read (r : BinaryReader) : FQFnName.FQFnName =
    match r.ReadByte() with
    | 0uy ->
      let name = String.read r
      let version = r.ReadInt32()
      FQFnName.Builtin { name = name; version = version }
    | 1uy ->
      let id = Hash.read r
      FQFnName.Package id
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid FQFnName tag: {b}"))


module FQValueName =
  let write (w : BinaryWriter) (n : FQValueName.FQValueName) : unit =
    match n with
    | FQValueName.Builtin builtin ->
      w.Write 0uy
      String.write w builtin.name
      w.Write builtin.version
    | FQValueName.Package id ->
      w.Write 1uy
      Hash.write w id

  let read (r : BinaryReader) : FQValueName.FQValueName =
    match r.ReadByte() with
    | 0uy ->
      let name = String.read r
      let version = r.ReadInt32()
      FQValueName.Builtin { name = name; version = version }
    | 1uy ->
      let id = Hash.read r
      FQValueName.Package id
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid FQValueName tag: {b}"))


module Sign =
  let write (w : BinaryWriter) (sign : Sign) : unit =
    match sign with
    | Positive -> w.Write 0uy
    | Negative -> w.Write 1uy

  let read (r : BinaryReader) : Sign =
    match r.ReadByte() with
    | 0uy -> Positive
    | 1uy -> Negative
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Sign tag: {b}"))
