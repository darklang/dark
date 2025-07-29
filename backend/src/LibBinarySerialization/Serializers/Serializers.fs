/// Type-specific binary serializers for SerializedTypes
module LibBinarySerialization.Serializers.Common

open System
open System.IO
open Prelude
open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Primitives
open LibBinarySerialization.SerializedTypes
open LibBinarySerialization.SerializedTypes.FQConstantName
open LibBinarySerialization.SerializedTypes.FQFnName
open LibBinarySerialization.SerializedTypes.FQTypeName

/// Serialize Sign enum
module Sign =
  let write (writer : BinaryWriter) (sign : Sign) =
    match sign with
    | Positive -> writer.Write(0uy)
    | Negative -> writer.Write(1uy)

  let read (reader : BinaryReader) : Sign =
    match reader.ReadByte() with
    | 0uy -> Positive
    | 1uy -> Negative
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Sign tag: {b}"))

/// Serialize NEList<'a>
module NEList =
  let write
    (writeItem : BinaryWriter -> 'a -> unit)
    (writer : BinaryWriter)
    (nel : NEList<'a>)
    =
    writeItem writer nel.head
    Writer.writeList writer writeItem nel.tail

  let read (readItem : BinaryReader -> 'a) (reader : BinaryReader) : NEList<'a> =
    let head = readItem reader
    let tail = Reader.readList reader readItem
    { head = head; tail = tail }

/// Serialize NameResolutionError
module NameResolutionError =
  let write (writer : BinaryWriter) (error : NameResolutionError) =
    match error with
    | NotFound items ->
      writer.Write(0uy)
      Writer.writeList writer Writer.writeString items
    | InvalidName items ->
      writer.Write(1uy)
      Writer.writeList writer Writer.writeString items

  let read (reader : BinaryReader) : NameResolutionError =
    match reader.ReadByte() with
    | 0uy -> NotFound(Reader.readList reader Reader.readString)
    | 1uy -> InvalidName(Reader.readList reader Reader.readString)
    | b ->
      raise (
        BinaryFormatException(CorruptedData $"Invalid NameResolutionError tag: {b}")
      )

/// Serialize NameResolution<'a>
module NameResolution =
  let write
    (writeValue : BinaryWriter -> 'a -> unit)
    (writer : BinaryWriter)
    (result : NameResolution<'a>)
    =
    match result with
    | Ok value ->
      writer.Write(0uy)
      writeValue writer value
    | Error error ->
      writer.Write(1uy)
      NameResolutionError.write writer error

  let read
    (readValue : BinaryReader -> 'a)
    (reader : BinaryReader)
    : NameResolution<'a> =
    match reader.ReadByte() with
    | 0uy -> Ok(readValue reader)
    | 1uy -> Error(NameResolutionError.read reader)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid NameResolution tag: {b}"))

/// Serialize FQTypeName types
module FQTypeName =
  module Package =
    let write (writer : BinaryWriter) (package : FQTypeName.Package) =
      Writer.writeGuid writer package

    let read (reader : BinaryReader) : FQTypeName.Package = Reader.readGuid reader

  let write (writer : BinaryWriter) (name : FQTypeName.FQTypeName) =
    match name with
    | FQTypeName.Package package ->
      writer.Write(0uy)
      Package.write writer package

  let read (reader : BinaryReader) : FQTypeName.FQTypeName =
    match reader.ReadByte() with
    | 0uy -> FQTypeName.Package(Package.read reader)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid FQTypeName tag: {b}"))

/// Serialize FQConstantName types
module FQConstantName =
  module Builtin =
    let write (writer : BinaryWriter) (builtin : FQConstantName.Builtin) =
      Writer.writeString writer builtin.name
      writer.Write(builtin.version : int)

    let read (reader : BinaryReader) : FQConstantName.Builtin =
      let name = Reader.readString reader
      let version = reader.ReadInt32()
      { name = name; version = version }

  module Package =
    let write (writer : BinaryWriter) (package : FQConstantName.Package) =
      Writer.writeGuid writer package

    let read (reader : BinaryReader) : FQConstantName.Package =
      Reader.readGuid reader

  let write (writer : BinaryWriter) (name : FQConstantName.FQConstantName) =
    match name with
    | FQConstantName.Builtin builtin ->
      writer.Write(0uy)
      Builtin.write writer builtin
    | FQConstantName.Package package ->
      writer.Write(1uy)
      Package.write writer package

  let read (reader : BinaryReader) : FQConstantName.FQConstantName =
    match reader.ReadByte() with
    | 0uy -> FQConstantName.Builtin(Builtin.read reader)
    | 1uy -> FQConstantName.Package(Package.read reader)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid FQConstantName tag: {b}"))

/// Serialize FQFnName types
module FQFnName =
  module Builtin =
    let write (writer : BinaryWriter) (builtin : FQFnName.Builtin) =
      Writer.writeString writer builtin.name
      writer.Write(builtin.version : int)

    let read (reader : BinaryReader) : FQFnName.Builtin =
      let name = Reader.readString reader
      let version = reader.ReadInt32()
      { name = name; version = version }

  module Package =
    let write (writer : BinaryWriter) (package : FQFnName.Package) =
      Writer.writeGuid writer package

    let read (reader : BinaryReader) : FQFnName.Package = Reader.readGuid reader

  let write (writer : BinaryWriter) (name : FQFnName.FQFnName) =
    match name with
    | FQFnName.Builtin builtin ->
      writer.Write(0uy)
      Builtin.write writer builtin
    | FQFnName.Package package ->
      writer.Write(1uy)
      Package.write writer package

  let read (reader : BinaryReader) : FQFnName.FQFnName =
    match reader.ReadByte() with
    | 0uy -> FQFnName.Builtin(Builtin.read reader)
    | 1uy -> FQFnName.Package(Package.read reader)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid FQFnName tag: {b}"))


/// Serialize InfixFnName enum
module InfixFnName =
  let write (writer : BinaryWriter) (name : InfixFnName) =
    let tag =
      match name with
      | ArithmeticPlus -> 0uy
      | ArithmeticMinus -> 1uy
      | ArithmeticMultiply -> 2uy
      | ArithmeticDivide -> 3uy
      | ArithmeticModulo -> 4uy
      | ArithmeticPower -> 5uy
      | ComparisonGreaterThan -> 6uy
      | ComparisonGreaterThanOrEqual -> 7uy
      | ComparisonLessThan -> 8uy
      | ComparisonLessThanOrEqual -> 9uy
      | ComparisonEquals -> 10uy
      | ComparisonNotEquals -> 11uy
      | StringConcat -> 12uy
    writer.Write(tag)

  let read (reader : BinaryReader) : InfixFnName =
    match reader.ReadByte() with
    | 0uy -> ArithmeticPlus
    | 1uy -> ArithmeticMinus
    | 2uy -> ArithmeticMultiply
    | 3uy -> ArithmeticDivide
    | 4uy -> ArithmeticModulo
    | 5uy -> ArithmeticPower
    | 6uy -> ComparisonGreaterThan
    | 7uy -> ComparisonGreaterThanOrEqual
    | 8uy -> ComparisonLessThan
    | 9uy -> ComparisonLessThanOrEqual
    | 10uy -> ComparisonEquals
    | 11uy -> ComparisonNotEquals
    | 12uy -> StringConcat
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid InfixFnName tag: {b}"))

/// Serialize BinaryOperation enum
module BinaryOperation =
  let write (writer : BinaryWriter) (op : BinaryOperation) =
    match op with
    | BinOpAnd -> writer.Write(0uy)
    | BinOpOr -> writer.Write(1uy)

  let read (reader : BinaryReader) : BinaryOperation =
    match reader.ReadByte() with
    | 0uy -> BinOpAnd
    | 1uy -> BinOpOr
    | b ->
      raise (
        BinaryFormatException(CorruptedData $"Invalid BinaryOperation tag: {b}")
      )

/// Serialize Infix
module Infix =
  let write (writer : BinaryWriter) (infix : Infix) =
    match infix with
    | InfixFnCall name ->
      writer.Write(0uy)
      InfixFnName.write writer name
    | BinOp op ->
      writer.Write(1uy)
      BinaryOperation.write writer op

  let read (reader : BinaryReader) : Infix =
    match reader.ReadByte() with
    | 0uy -> InfixFnCall(InfixFnName.read reader)
    | 1uy -> BinOp(BinaryOperation.read reader)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Infix tag: {b}"))

/// Helper for writing bigint (used for Int128/UInt128)
module BigInt =
  let write (writer : BinaryWriter) (value : bigint) =
    let bytes = value.ToByteArray()
    Writer.writeArray writer (fun w b -> w.Write(b : byte)) bytes

  let read (reader : BinaryReader) : bigint =
    let bytes = Reader.readArray reader (fun r -> r.ReadByte())
    bigint (bytes)

/// Serialize LetPattern
module LetPattern =
  let rec write (writer : BinaryWriter) (pattern : LetPattern) =
    match pattern with
    | LPVariable(id, name) ->
      writer.Write(0uy)
      Writer.writeUInt64 writer id
      Writer.writeString writer name
    | LPUnit id ->
      writer.Write(1uy)
      Writer.writeUInt64 writer id
    | LPTuple(id, first, second, rest) ->
      writer.Write(2uy)
      Writer.writeUInt64 writer id
      write writer first
      write writer second
      Writer.writeList writer write rest

  and read (reader : BinaryReader) : LetPattern =
    match reader.ReadByte() with
    | 0uy ->
      let id = Reader.readUInt64 reader
      let name = Reader.readString reader
      LPVariable(id, name)
    | 1uy ->
      let id = Reader.readUInt64 reader
      LPUnit id
    | 2uy ->
      let id = Reader.readUInt64 reader
      let first = read reader
      let second = read reader
      let rest = Reader.readList reader read
      LPTuple(id, first, second, rest)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid LetPattern tag: {b}"))

/// Serialize MatchPattern (mutually recursive)
module MatchPattern =
  let rec write (writer : BinaryWriter) (pattern : MatchPattern) =
    match pattern with
    | MPVariable(id, name) ->
      writer.Write(0uy)
      Writer.writeUInt64 writer id
      Writer.writeString writer name
    | MPEnum(id, caseName, fieldPats) ->
      writer.Write(1uy)
      Writer.writeUInt64 writer id
      Writer.writeString writer caseName
      Writer.writeList writer write fieldPats
    | MPInt64(id, value) ->
      writer.Write(2uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPUInt64(id, value) ->
      writer.Write(3uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPInt8(id, value) ->
      writer.Write(4uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPUInt8(id, value) ->
      writer.Write(5uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPInt16(id, value) ->
      writer.Write(6uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPUInt16(id, value) ->
      writer.Write(7uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPInt32(id, value) ->
      writer.Write(8uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPUInt32(id, value) ->
      writer.Write(9uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPInt128(id, value) ->
      writer.Write(10uy)
      Writer.writeUInt64 writer id
      BigInt.write writer value
    | MPUInt128(id, value) ->
      writer.Write(11uy)
      Writer.writeUInt64 writer id
      BigInt.write writer value
    | MPBool(id, value) ->
      writer.Write(12uy)
      Writer.writeUInt64 writer id
      writer.Write(value)
    | MPChar(id, value) ->
      writer.Write(13uy)
      Writer.writeUInt64 writer id
      Writer.writeString writer value
    | MPString(id, value) ->
      writer.Write(14uy)
      Writer.writeUInt64 writer id
      Writer.writeString writer value
    | MPFloat(id, sign, whole, fractional) ->
      writer.Write(15uy)
      Writer.writeUInt64 writer id
      Sign.write writer sign
      Writer.writeString writer whole
      Writer.writeString writer fractional
    | MPUnit id ->
      writer.Write(16uy)
      Writer.writeUInt64 writer id
    | MPTuple(id, first, second, rest) ->
      writer.Write(17uy)
      Writer.writeUInt64 writer id
      write writer first
      write writer second
      Writer.writeList writer write rest
    | MPList(id, patterns) ->
      writer.Write(18uy)
      Writer.writeUInt64 writer id
      Writer.writeList writer write patterns
    | MPListCons(id, head, tail) ->
      writer.Write(19uy)
      Writer.writeUInt64 writer id
      write writer head
      write writer tail
    | MPOr(id, patterns) ->
      writer.Write(20uy)
      Writer.writeUInt64 writer id
      NEList.write write writer patterns

  let rec read (reader : BinaryReader) : MatchPattern =
    match reader.ReadByte() with
    | 0uy ->
      let id = Reader.readUInt64 reader
      let name = Reader.readString reader
      MPVariable(id, name)
    | 1uy ->
      let id = Reader.readUInt64 reader
      let caseName = Reader.readString reader
      let fieldPats = Reader.readList reader read
      MPEnum(id, caseName, fieldPats)
    | 2uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadInt64()
      MPInt64(id, value)
    | 3uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadUInt64()
      MPUInt64(id, value)
    | 4uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadSByte()
      MPInt8(id, value)
    | 5uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadByte()
      MPUInt8(id, value)
    | 6uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadInt16()
      MPInt16(id, value)
    | 7uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadUInt16()
      MPUInt16(id, value)
    | 8uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadInt32()
      MPInt32(id, value)
    | 9uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadUInt32()
      MPUInt32(id, value)
    | 10uy ->
      let id = Reader.readUInt64 reader
      let value = BigInt.read reader
      MPInt128(id, value)
    | 11uy ->
      let id = Reader.readUInt64 reader
      let value = BigInt.read reader
      MPUInt128(id, value)
    | 12uy ->
      let id = Reader.readUInt64 reader
      let value = reader.ReadBoolean()
      MPBool(id, value)
    | 13uy ->
      let id = Reader.readUInt64 reader
      let value = Reader.readString reader
      MPChar(id, value)
    | 14uy ->
      let id = Reader.readUInt64 reader
      let value = Reader.readString reader
      MPString(id, value)
    | 15uy ->
      let id = Reader.readUInt64 reader
      let sign = Sign.read reader
      let whole = Reader.readString reader
      let fractional = Reader.readString reader
      MPFloat(id, sign, whole, fractional)
    | 16uy ->
      let id = Reader.readUInt64 reader
      MPUnit id
    | 17uy ->
      let id = Reader.readUInt64 reader
      let first = read reader
      let second = read reader
      let rest = Reader.readList reader read
      MPTuple(id, first, second, rest)
    | 18uy ->
      let id = Reader.readUInt64 reader
      let patterns = Reader.readList reader read
      MPList(id, patterns)
    | 19uy ->
      let id = Reader.readUInt64 reader
      let head = read reader
      let tail = read reader
      MPListCons(id, head, tail)
    | 20uy ->
      let id = Reader.readUInt64 reader
      let patterns = NEList.read read reader
      MPOr(id, patterns)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid MatchPattern tag: {b}"))
