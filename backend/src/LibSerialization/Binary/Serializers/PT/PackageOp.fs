module LibSerialization.Binary.Serializers.PT.PackageOp

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary
open BaseFormat
open Serializers.Common
open Serializers.PT.Common


let write (w : BinaryWriter) (op : PackageOp) : unit =
  match op with
  | AddType typ ->
    w.Write 0uy // tag
    PackageType.write w typ
  | AddValue value ->
    w.Write 1uy // tag
    PackageValue.write w value
  | AddFn fn ->
    w.Write 2uy // tag
    PackageFn.write w fn
  | SetTypeName(id, location) ->
    w.Write 3uy // tag
    FQTypeName.Package.write w id
    PackageLocation.write w location
  | SetValueName(id, location) ->
    w.Write 4uy // tag
    FQValueName.Package.write w id
    PackageLocation.write w location
  | SetFnName(id, location) ->
    w.Write 5uy // tag
    FQFnName.Package.write w id
    PackageLocation.write w location

let read (r : BinaryReader) : PackageOp =
  match r.ReadByte() with
  | 0uy ->
    let typ = PackageType.read r
    AddType typ
  | 1uy ->
    let value = PackageValue.read r
    AddValue value
  | 2uy ->
    let fn = PackageFn.read r
    AddFn fn
  | 3uy ->
    let id = FQTypeName.Package.read r
    let location = PackageLocation.read r
    SetTypeName(id, location)
  | 4uy ->
    let id = FQValueName.Package.read r
    let location = PackageLocation.read r
    SetValueName(id, location)
  | 5uy ->
    let id = FQFnName.Package.read r
    let location = PackageLocation.read r
    SetFnName(id, location)
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
