module LibBinarySerialization.Serializers.RT.ValueType

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.RT.Common


let rec write (w : BinaryWriter) (v : ValueType) : unit =
  match v with
  | ValueType.Unknown -> w.Write 0uy
  | ValueType.Known kt ->
    w.Write 1uy
    writeKnownType w kt

and writeKnownType (w : BinaryWriter) (kt : KnownType) : unit =
  match kt with
  | KTUnit -> w.Write 0uy
  | KTBool -> w.Write 1uy
  | KTInt8 -> w.Write 2uy
  | KTUInt8 -> w.Write 3uy
  | KTInt16 -> w.Write 4uy
  | KTUInt16 -> w.Write 5uy
  | KTInt32 -> w.Write 6uy
  | KTUInt32 -> w.Write 7uy
  | KTInt64 -> w.Write 8uy
  | KTUInt64 -> w.Write 9uy
  | KTInt128 -> w.Write 10uy
  | KTUInt128 -> w.Write 11uy
  | KTFloat -> w.Write 12uy
  | KTChar -> w.Write 13uy
  | KTString -> w.Write 14uy
  | KTUuid -> w.Write 15uy
  | KTDateTime -> w.Write 16uy
  | KTList vt ->
    w.Write 17uy
    write w vt
  | KTTuple(first, second, rest) ->
    w.Write 18uy
    write w first
    write w second
    List.write w write rest
  | KTFn(argTypes, returnType) ->
    w.Write 19uy
    NEList.write write w argTypes
    write w returnType
  | KTDB vt ->
    w.Write 20uy
    write w vt
  | KTCustomType(typeName, typeArgs) ->
    w.Write 21uy
    FQTypeName.write w typeName
    List.write w write typeArgs
  | KTDict vt ->
    w.Write 22uy
    write w vt

let rec read (r : BinaryReader) : ValueType =
  match r.ReadByte() with
  | 0uy -> ValueType.Unknown
  | 1uy -> ValueType.Known(readKnownType r)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid ValueType tag: {b}"))

and readKnownType (r : BinaryReader) : KnownType =
  match r.ReadByte() with
  | 0uy -> KTUnit
  | 1uy -> KTBool
  | 2uy -> KTInt8
  | 3uy -> KTUInt8
  | 4uy -> KTInt16
  | 5uy -> KTUInt16
  | 6uy -> KTInt32
  | 7uy -> KTUInt32
  | 8uy -> KTInt64
  | 9uy -> KTUInt64
  | 10uy -> KTInt128
  | 11uy -> KTUInt128
  | 12uy -> KTFloat
  | 13uy -> KTChar
  | 14uy -> KTString
  | 15uy -> KTUuid
  | 16uy -> KTDateTime
  | 17uy -> KTList(read r)
  | 18uy ->
    let first = read r
    let second = read r
    let rest = List.read r read
    KTTuple(first, second, rest)
  | 19uy ->
    let argTypes = NEList.read read r
    let returnType = read r
    KTFn(argTypes, returnType)
  | 20uy -> KTDB(read r)
  | 21uy ->
    let typeName = FQTypeName.read r
    let typeArgs = List.read r read
    KTCustomType(typeName, typeArgs)
  | 22uy -> KTDict(read r)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid KnownType tag: {b}"))
