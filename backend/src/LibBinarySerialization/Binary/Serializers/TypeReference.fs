/// TypeReference serialization for custom binary format
module LibBinarySerialization.Binary.Serializers.TypeReference

open System.IO
open Prelude
open LibBinarySerialization.Binary.BinaryFormat
open LibBinarySerialization.Binary.Primitives
open LibBinarySerialization.SerializedTypes

let rec writeTypeReference (writer: BinaryWriter) (typeRef: TypeReference) : unit =
  match typeRef with
  | TInt64 -> writer.Write(0uy)
  | TUInt64 -> writer.Write(1uy)
  | TInt8 -> writer.Write(2uy)
  | TUInt8 -> writer.Write(3uy)
  | TInt16 -> writer.Write(4uy)
  | TUInt16 -> writer.Write(5uy)
  | TInt32 -> writer.Write(6uy)
  | TUInt32 -> writer.Write(7uy)
  | TInt128 -> writer.Write(8uy)
  | TUInt128 -> writer.Write(9uy)
  | TFloat -> writer.Write(10uy)
  | TBool -> writer.Write(11uy)
  | TUnit -> writer.Write(12uy)
  | TString -> writer.Write(13uy)
  | TList typeRef ->
      writer.Write(14uy)
      writeTypeReference writer typeRef
  | TDict typeRef ->
      writer.Write(15uy)
      writeTypeReference writer typeRef
  | TDB typeRef ->
      writer.Write(16uy)
      writeTypeReference writer typeRef
  | TDateTime -> writer.Write(17uy)
  | TChar -> writer.Write(18uy)
  | TUuid -> writer.Write(19uy)
  | TCustomType (typeName, typeArgs) ->
      writer.Write(20uy)
      writeNameResolutionFQTypeName writer typeName
      Writer.writeList writer writeTypeReference typeArgs
  | TVariable name ->
      writer.Write(21uy)
      Writer.writeString writer name
  | TFn (paramTypes, returnType) ->
      writer.Write(22uy)
      writeNEListTypeReference writer paramTypes
      writeTypeReference writer returnType
  | TTuple (first, second, rest) ->
      writer.Write(23uy)
      writeTypeReference writer first
      writeTypeReference writer second
      Writer.writeList writer writeTypeReference rest

and writeNEListTypeReference (writer: BinaryWriter) (types: NEList<TypeReference>) : unit =
  writeTypeReference writer types.head
  Writer.writeList writer writeTypeReference types.tail

and writeNameResolutionFQTypeName (writer: BinaryWriter) (nameRes: NameResolution<FQTypeName.FQTypeName>) : unit =
  match nameRes with
  | Ok (FQTypeName.Package uuid) ->
      writer.Write(0uy)
      Writer.writeGuid writer uuid
  | Error (NameResolutionError.NotFound names) ->
      writer.Write(1uy)
      Writer.writeList writer Writer.writeString names
  | Error (NameResolutionError.InvalidName names) ->
      writer.Write(2uy)
      Writer.writeList writer Writer.writeString names

let rec readTypeReference (reader: BinaryReader) : TypeReference =
  match reader.ReadByte() with
  | 0uy -> TInt64
  | 1uy -> TUInt64
  | 2uy -> TInt8
  | 3uy -> TUInt8
  | 4uy -> TInt16
  | 5uy -> TUInt16
  | 6uy -> TInt32
  | 7uy -> TUInt32
  | 8uy -> TInt128
  | 9uy -> TUInt128
  | 10uy -> TFloat
  | 11uy -> TBool
  | 12uy -> TUnit
  | 13uy -> TString
  | 14uy -> 
      let typeRef = readTypeReference reader
      TList typeRef
  | 15uy ->
      let typeRef = readTypeReference reader
      TDict typeRef
  | 16uy ->
      let typeRef = readTypeReference reader
      TDB typeRef
  | 17uy -> TDateTime
  | 18uy -> TChar
  | 19uy -> TUuid
  | 20uy ->
      let typeName = readNameResolutionFQTypeName reader
      let typeArgs = Reader.readList reader readTypeReference
      TCustomType (typeName, typeArgs)
  | 21uy ->
      let name = Reader.readString reader
      TVariable name
  | 22uy ->
      let paramTypes = readNEListTypeReference reader
      let returnType = readTypeReference reader
      TFn (paramTypes, returnType)
  | 23uy ->
      let first = readTypeReference reader
      let second = readTypeReference reader
      let rest = Reader.readList reader readTypeReference
      TTuple (first, second, rest)
  | b -> 
      raise (BinaryFormatException(CorruptedData $"Invalid TypeReference tag: {b}"))

and readNameResolutionFQTypeName (reader: BinaryReader) : NameResolution<FQTypeName.FQTypeName> =
  match reader.ReadByte() with
  | 0uy ->
      let uuid = Reader.readGuid reader
      Ok (FQTypeName.Package uuid)
  | 1uy ->
      let names = Reader.readList reader Reader.readString
      Error (NameResolutionError.NotFound names)
  | 2uy ->
      let names = Reader.readList reader Reader.readString
      Error (NameResolutionError.InvalidName names)
  | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid NameResolution tag: {b}"))

and readNEListTypeReference (reader: BinaryReader) : NEList<TypeReference> =
  let head = readTypeReference reader
  let tail = Reader.readList reader readTypeReference
  { head = head; tail = tail }