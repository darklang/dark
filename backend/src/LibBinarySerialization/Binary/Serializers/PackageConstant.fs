/// PackageConstant serialization for custom binary format
module LibBinarySerialization.Binary.Serializers.PackageConstant

open System
open System.IO
open Prelude
open LibBinarySerialization.Binary.BinaryFormat
open LibBinarySerialization.Binary.Primitives
open LibBinarySerialization.SerializedTypes
open PackageType
open TypeReference
open Common

module Name =
  let write (writer: BinaryWriter) (name: PackageConstant.Name) : unit =
    Writer.writeString writer name.owner
    Writer.writeList writer Writer.writeString name.modules
    Writer.writeString writer name.name

  let read (reader: BinaryReader) : PackageConstant.Name =
    let owner = Reader.readString reader
    let modules = Reader.readList reader Reader.readString
    let name = Reader.readString reader
    { owner = owner; modules = modules; name = name }

module Const =
  let rec write (writer: BinaryWriter) (c: Const) : unit =
    match c with
    | CInt64 value ->
        writer.Write(0uy)
        writer.Write(value)
    | CUInt64 value ->
        writer.Write(1uy)
        writer.Write(value)
    | CInt8 value ->
        writer.Write(2uy)
        writer.Write(value)
    | CUInt8 value ->
        writer.Write(3uy)
        writer.Write(value)
    | CInt16 value ->
        writer.Write(4uy)
        writer.Write(value)
    | CUInt16 value ->
        writer.Write(5uy)
        writer.Write(value)
    | CInt32 value ->
        writer.Write(6uy)
        writer.Write(value)
    | CUInt32 value ->
        writer.Write(7uy)
        writer.Write(value)
    | CInt128 value ->
        writer.Write(8uy)
        Writer.writeString writer (value.ToString())
    | CUInt128 value ->
        writer.Write(9uy)
        Writer.writeString writer (value.ToString())
    | CBool value ->
        writer.Write(10uy)
        writer.Write(value)
    | CString value ->
        writer.Write(11uy)
        Writer.writeString writer value
    | CChar value ->
        writer.Write(12uy)
        Writer.writeString writer value
    | CFloat (sign, whole, fractional) ->
        writer.Write(13uy)
        writeSign writer sign
        Writer.writeString writer whole
        Writer.writeString writer fractional
    | CUnit ->
        writer.Write(14uy)
    | CTuple (first, second, rest) ->
        writer.Write(15uy)
        write writer first
        write writer second
        Writer.writeList writer write rest
    | CEnum (typeName, caseName, fields) ->
        writer.Write(16uy)
        writeNameResolutionFQTypeName writer typeName
        Writer.writeString writer caseName
        Writer.writeList writer write fields
    | CList consts ->
        writer.Write(17uy)
        Writer.writeList writer write consts
    | CDict pairs ->
        writer.Write(18uy)
        Writer.writeList writer (fun w (key, value) ->
          Writer.writeString w key
          write w value) pairs

  and writeSign (writer: BinaryWriter) (sign: Sign) : unit =
    match sign with
    | Positive -> writer.Write(0uy)
    | Negative -> writer.Write(1uy)

  and writeNameResolutionFQTypeName (writer: BinaryWriter) (nameRes: NameResolution<FQTypeName.FQTypeName>) : unit =
    TypeReference.writeNameResolutionFQTypeName writer nameRes

  let rec read (reader: BinaryReader) : Const =
    match reader.ReadByte() with
    | 0uy -> CInt64 (reader.ReadInt64())
    | 1uy -> CUInt64 (reader.ReadUInt64())
    | 2uy -> CInt8 (reader.ReadSByte())
    | 3uy -> CUInt8 (reader.ReadByte())
    | 4uy -> CInt16 (reader.ReadInt16())
    | 5uy -> CUInt16 (reader.ReadUInt16())
    | 6uy -> CInt32 (reader.ReadInt32())
    | 7uy -> CUInt32 (reader.ReadUInt32())
    | 8uy -> 
        let str = Reader.readString reader
        CInt128 (bigint.Parse(str))
    | 9uy ->
        let str = Reader.readString reader
        CUInt128 (bigint.Parse(str))
    | 10uy -> CBool (reader.ReadBoolean())
    | 11uy -> CString (Reader.readString reader)
    | 12uy -> CChar (Reader.readString reader)
    | 13uy ->
        let sign = readSign reader
        let whole = Reader.readString reader
        let fractional = Reader.readString reader
        CFloat (sign, whole, fractional)
    | 14uy -> CUnit
    | 15uy ->
        let first = read reader
        let second = read reader
        let rest = Reader.readList reader read
        CTuple (first, second, rest)
    | 16uy ->
        let typeName = readNameResolutionFQTypeName reader
        let caseName = Reader.readString reader
        let fields = Reader.readList reader read
        CEnum (typeName, caseName, fields)
    | 17uy ->
        let consts = Reader.readList reader read
        CList consts
    | 18uy ->
        let pairs = Reader.readList reader (fun r ->
          let key = Reader.readString r
          let value = read r
          (key, value))
        CDict pairs
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Const tag: {b}"))

  and readSign (reader: BinaryReader) : Sign =
    match reader.ReadByte() with
    | 0uy -> Positive
    | 1uy -> Negative
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Sign tag: {b}"))

  and readNameResolutionFQTypeName (reader: BinaryReader) : NameResolution<FQTypeName.FQTypeName> =
    TypeReference.readNameResolutionFQTypeName reader

let writeFQConstantName (writer: BinaryWriter) (name: FQConstantName.FQConstantName) : unit =
  FQConstantName.write writer name

let readFQConstantName (reader: BinaryReader) : FQConstantName.FQConstantName =
  FQConstantName.read reader

let write (writer: BinaryWriter) (packageConstant: PackageConstant.PackageConstant) : unit =
  Writer.writeGuid writer packageConstant.id
  Name.write writer packageConstant.name
  Const.write writer packageConstant.body
  Writer.writeString writer packageConstant.description
  PackageType.Deprecation.write writer writeFQConstantName packageConstant.deprecated

let read (reader: BinaryReader) : PackageConstant.PackageConstant =
  let id = Reader.readGuid reader
  let name = Name.read reader
  let body = Const.read reader
  let description = Reader.readString reader
  let deprecated = PackageType.Deprecation.read reader readFQConstantName
  {
    id = id
    name = name
    body = body
    description = description
    deprecated = deprecated
  }