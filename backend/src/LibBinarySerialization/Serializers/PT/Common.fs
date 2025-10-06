module LibBinarySerialization.Serializers.PT.Common

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common


module Sign =
  let write (w : BinaryWriter) (sign : Sign) =
    match sign with
    | Positive -> w.Write 0uy
    | Negative -> w.Write 1uy

  let read (r : BinaryReader) : Sign =
    match r.ReadByte() with
    | 0uy -> Positive
    | 1uy -> Negative
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Sign tag: {b}"))


module NameResolutionError =
  let write (w : BinaryWriter) (error : NameResolutionError) =
    match error with
    | NotFound items ->
      w.Write(0uy)
      List.write w String.write items
    | InvalidName items ->
      w.Write(1uy)
      List.write w String.write items

  let read (r : BinaryReader) : NameResolutionError =
    match r.ReadByte() with
    | 0uy -> NotFound(List.read r String.read)
    | 1uy -> InvalidName(List.read r String.read)
    | b ->
      raise (
        BinaryFormatException(CorruptedData $"Invalid NameResolutionError tag: {b}")
      )


module NameResolution =
  let write
    (writeValue : BinaryWriter -> 'a -> unit)
    (w : BinaryWriter)
    (result : NameResolution<'a>)
    =
    match result with
    | Ok value ->
      w.Write(0uy)
      writeValue w value
    | Error error ->
      w.Write(1uy)
      NameResolutionError.write w error

  let read (readValue : BinaryReader -> 'a) (r : BinaryReader) : NameResolution<'a> =
    match r.ReadByte() with
    | 0uy -> Ok(readValue r)
    | 1uy -> Error(NameResolutionError.read r)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid NameResolution tag: {b}"))


module FQTypeName =
  module Package =
    let write (w : BinaryWriter) (p : FQTypeName.Package) = Hash.write w p

    let read (r : BinaryReader) : FQTypeName.Package = Hash.read r


  let write (w : BinaryWriter) (name : FQTypeName.FQTypeName) =
    match name with
    | FQTypeName.Package p ->
      w.Write(0uy)
      Package.write w p

  let read (r : BinaryReader) : FQTypeName.FQTypeName =
    match r.ReadByte() with
    | 0uy -> FQTypeName.Package(Package.read r)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid FQTypeName tag: {b}"))


module FQValueName =
  module Builtin =
    let write (w : BinaryWriter) (b : FQValueName.Builtin) =
      String.write w b.name
      w.Write(b.version : int)

    let read (r : BinaryReader) : FQValueName.Builtin =
      let name = String.read r
      let version = r.ReadInt32()
      { name = name; version = version }

  module Package =
    let write (w : BinaryWriter) (p : FQValueName.Package) = Hash.write w p

    let read (r : BinaryReader) : FQValueName.Package = Hash.read r


  let write (w : BinaryWriter) (n : FQValueName.FQValueName) =
    match n with
    | FQValueName.Builtin builtin ->
      w.Write(0uy)
      Builtin.write w builtin
    | FQValueName.Package p ->
      w.Write(1uy)
      Package.write w p

  let read (r : BinaryReader) : FQValueName.FQValueName =
    match r.ReadByte() with
    | 0uy -> FQValueName.Builtin(Builtin.read r)
    | 1uy -> FQValueName.Package(Package.read r)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid FQValueName tag: {b}"))


module FQFnName =
  module Builtin =
    let write (w : BinaryWriter) (b : FQFnName.Builtin) =
      String.write w b.name
      w.Write(b.version : int)

    let read (r : BinaryReader) : FQFnName.Builtin =
      let name = String.read r
      let version = r.ReadInt32()
      { name = name; version = version }

  module Package =
    let write (w : BinaryWriter) (p : FQFnName.Package) = Hash.write w p

    let read (r : BinaryReader) : FQFnName.Package = Hash.read r


  let write (w : BinaryWriter) (n : FQFnName.FQFnName) =
    match n with
    | FQFnName.Builtin b ->
      w.Write(0uy)
      Builtin.write w b
    | FQFnName.Package p ->
      w.Write(1uy)
      Package.write w p

  let read (r : BinaryReader) : FQFnName.FQFnName =
    match r.ReadByte() with
    | 0uy -> FQFnName.Builtin(Builtin.read r)
    | 1uy -> FQFnName.Package(Package.read r)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid FQFnName tag: {b}"))


module Deprecation =
  let write
    (w : BinaryWriter)
    (writeNameFn : BinaryWriter -> 'name -> unit)
    (d : Deprecation<'name>)
    : unit =
    match d with
    | NotDeprecated -> w.Write(0uy)
    | RenamedTo name ->
      w.Write(1uy)
      writeNameFn w name
    | ReplacedBy name ->
      w.Write(2uy)
      writeNameFn w name
    | DeprecatedBecause reason ->
      w.Write(3uy)
      String.write w reason

  let read
    (r : BinaryReader)
    (readNameFn : BinaryReader -> 'name)
    : Deprecation<'name> =
    match r.ReadByte() with
    | 0uy -> NotDeprecated
    | 1uy -> RenamedTo(readNameFn r)
    | 2uy -> ReplacedBy(readNameFn r)
    | 3uy -> DeprecatedBecause(String.read r)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid Deprecation tag: {b}"))
