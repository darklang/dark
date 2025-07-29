/// PackageType serialization for custom binary format
module LibBinarySerialization.Binary.Serializers.PackageType

open System
open System.IO
open Prelude
open LibBinarySerialization.Binary.BinaryFormat
open LibBinarySerialization.Binary.Primitives
open LibBinarySerialization.SerializedTypes
open TypeReference
open Common

module Name =
  let write (writer : BinaryWriter) (name : PackageType.Name) : unit =
    Writer.writeString writer name.owner
    Writer.writeList writer Writer.writeString name.modules
    Writer.writeString writer name.name

  let read (reader : BinaryReader) : PackageType.Name =
    let owner = Reader.readString reader
    let modules = Reader.readList reader Reader.readString
    let name = Reader.readString reader
    { owner = owner; modules = modules; name = name }

module Deprecation =
  let write
    (writer : BinaryWriter)
    (writeNameFn : BinaryWriter -> 'name -> unit)
    (deprecation : Deprecation<'name>)
    : unit =
    match deprecation with
    | NotDeprecated -> writer.Write(0uy)
    | RenamedTo name ->
      writer.Write(1uy)
      writeNameFn writer name
    | ReplacedBy name ->
      writer.Write(2uy)
      writeNameFn writer name
    | DeprecatedBecause reason ->
      writer.Write(3uy)
      Writer.writeString writer reason

  let read
    (reader : BinaryReader)
    (readNameFn : BinaryReader -> 'name)
    : Deprecation<'name> =
    match reader.ReadByte() with
    | 0uy -> NotDeprecated
    | 1uy -> RenamedTo(readNameFn reader)
    | 2uy -> ReplacedBy(readNameFn reader)
    | 3uy -> DeprecatedBecause(Reader.readString reader)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid Deprecation tag: {b}"))

module TypeDeclaration =
  let rec write (writer : BinaryWriter) (decl : TypeDeclaration.T) : unit =
    Writer.writeList writer Writer.writeString decl.typeParams
    writeDefinition writer decl.definition

  and writeDefinition
    (writer : BinaryWriter)
    (def : TypeDeclaration.Definition)
    : unit =
    match def with
    | TypeDeclaration.Alias typeRef ->
      writer.Write(0uy)
      writeTypeReference writer typeRef
    | TypeDeclaration.Record fields ->
      writer.Write(1uy)
      writeNEListRecordField writer fields
    | TypeDeclaration.Enum cases ->
      writer.Write(2uy)
      writeNEListEnumCase writer cases

  and writeRecordField
    (writer : BinaryWriter)
    (field : TypeDeclaration.RecordField)
    : unit =
    Writer.writeString writer field.name
    writeTypeReference writer field.typ
    Writer.writeString writer field.description

  and writeNEListRecordField
    (writer : BinaryWriter)
    (fields : NEList<TypeDeclaration.RecordField>)
    : unit =
    writeRecordField writer fields.head
    Writer.writeList writer writeRecordField fields.tail

  and writeEnumCase
    (writer : BinaryWriter)
    (case : TypeDeclaration.EnumCase)
    : unit =
    Writer.writeString writer case.name
    Writer.writeList writer writeEnumField case.fields
    Writer.writeString writer case.description

  and writeEnumField
    (writer : BinaryWriter)
    (field : TypeDeclaration.EnumField)
    : unit =
    writeTypeReference writer field.typ
    Writer.writeOption writer Writer.writeString field.label
    Writer.writeString writer field.description

  and writeNEListEnumCase
    (writer : BinaryWriter)
    (cases : NEList<TypeDeclaration.EnumCase>)
    : unit =
    writeEnumCase writer cases.head
    Writer.writeList writer writeEnumCase cases.tail

  let rec read (reader : BinaryReader) : TypeDeclaration.T =
    let typeParams = Reader.readList reader Reader.readString
    let definition = readDefinition reader
    { typeParams = typeParams; definition = definition }

  and readDefinition (reader : BinaryReader) : TypeDeclaration.Definition =
    match reader.ReadByte() with
    | 0uy -> TypeDeclaration.Alias(readTypeReference reader)
    | 1uy -> TypeDeclaration.Record(readNEListRecordField reader)
    | 2uy -> TypeDeclaration.Enum(readNEListEnumCase reader)
    | b ->
      raise (
        BinaryFormatException(
          CorruptedData $"Invalid TypeDeclaration.Definition tag: {b}"
        )
      )

  and readRecordField (reader : BinaryReader) : TypeDeclaration.RecordField =
    let name = Reader.readString reader
    let typ = readTypeReference reader
    let description = Reader.readString reader
    { name = name; typ = typ; description = description }

  and readNEListRecordField
    (reader : BinaryReader)
    : NEList<TypeDeclaration.RecordField> =
    let head = readRecordField reader
    let tail = Reader.readList reader readRecordField
    { head = head; tail = tail }

  and readEnumCase (reader : BinaryReader) : TypeDeclaration.EnumCase =
    let name = Reader.readString reader
    let fields = Reader.readList reader readEnumField
    let description = Reader.readString reader
    { name = name; fields = fields; description = description }

  and readEnumField (reader : BinaryReader) : TypeDeclaration.EnumField =
    let typ = readTypeReference reader
    let label = Reader.readOption reader Reader.readString
    let description = Reader.readString reader
    { typ = typ; label = label; description = description }

  and readNEListEnumCase (reader : BinaryReader) : NEList<TypeDeclaration.EnumCase> =
    let head = readEnumCase reader
    let tail = Reader.readList reader readEnumCase
    { head = head; tail = tail }

let writeFQTypeName (writer : BinaryWriter) (name : FQTypeName.FQTypeName) : unit =
  FQTypeName.write writer name

let readFQTypeName (reader : BinaryReader) : FQTypeName.FQTypeName =
  FQTypeName.read reader

let write (writer : BinaryWriter) (packageType : PackageType.PackageType) : unit =
  Writer.writeGuid writer packageType.id
  Name.write writer packageType.name
  TypeDeclaration.write writer packageType.declaration
  Writer.writeString writer packageType.description
  Deprecation.write writer writeFQTypeName packageType.deprecated

let read (reader : BinaryReader) : PackageType.PackageType =
  let id = Reader.readGuid reader
  let name = Name.read reader
  let declaration = TypeDeclaration.read reader
  let description = Reader.readString reader
  let deprecated = Deprecation.read reader readFQTypeName
  { id = id
    name = name
    declaration = declaration
    description = description
    deprecated = deprecated }
