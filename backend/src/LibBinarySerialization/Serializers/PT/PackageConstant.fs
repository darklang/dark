module LibBinarySerialization.Serializers.PT.PackageConstant

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.PT.Common


module Name =
  let write (w : BinaryWriter) (name : PackageConstant.Name) : unit =
    String.write w name.owner
    List.write w String.write name.modules
    String.write w name.name

  let read (r : BinaryReader) : PackageConstant.Name =
    let owner = String.read r
    let modules = List.read r String.read
    let name = String.read r
    { owner = owner; modules = modules; name = name }


module Const =
  let rec write (w : BinaryWriter) (c : Const) : unit =
    match c with
    | CInt64 value ->
      w.Write 0uy
      w.Write value
    | CUInt64 value ->
      w.Write 1uy
      w.Write value
    | CInt8 value ->
      w.Write 2uy
      w.Write value
    | CUInt8 value ->
      w.Write 3uy
      w.Write value
    | CInt16 value ->
      w.Write 4uy
      w.Write value
    | CUInt16 value ->
      w.Write 5uy
      w.Write value
    | CInt32 value ->
      w.Write 6uy
      w.Write value
    | CUInt32 value ->
      w.Write 7uy
      w.Write value
    | CInt128 value ->
      w.Write 8uy
      String.write w (string value)
    | CUInt128 value ->
      w.Write 9uy
      String.write w (string value)
    | CBool value ->
      w.Write 10uy
      w.Write value
    | CString value ->
      w.Write 11uy
      String.write w value
    | CChar value ->
      w.Write 12uy
      String.write w value
    | CFloat(sign, whole, fractional) ->
      w.Write 13uy
      Sign.write w sign
      String.write w whole
      String.write w fractional
    | CUnit -> w.Write 14uy
    | CTuple(first, second, rest) ->
      w.Write 15uy
      write w first
      write w second
      List.write w write rest
    | CEnum(typeName, caseName, fields) ->
      w.Write 16uy
      NameResolution.write FQTypeName.write w typeName
      String.write w caseName
      List.write w write fields
    | CList consts ->
      w.Write 17uy
      List.write w write consts
    | CDict pairs ->
      w.Write 18uy
      List.write
        w
        (fun w (key, value) ->
          String.write w key
          write w value)
        pairs

  let rec read (r : BinaryReader) : Const =
    match r.ReadByte() with
    | 0uy -> CInt64(r.ReadInt64())
    | 1uy -> CUInt64(r.ReadUInt64())
    | 2uy -> CInt8(r.ReadSByte())
    | 3uy -> CUInt8(r.ReadByte())
    | 4uy -> CInt16(r.ReadInt16())
    | 5uy -> CUInt16(r.ReadUInt16())
    | 6uy -> CInt32(r.ReadInt32())
    | 7uy -> CUInt32(r.ReadUInt32())
    | 8uy ->
      let str = String.read r
      CInt128(System.Int128.Parse str)
    | 9uy ->
      let str = String.read r
      CUInt128(System.UInt128.Parse str)
    | 10uy -> CBool(r.ReadBoolean())
    | 11uy -> CString(String.read r)
    | 12uy -> CChar(String.read r)
    | 13uy ->
      let sign = Sign.read r
      let whole = String.read r
      let fractional = String.read r
      CFloat(sign, whole, fractional)
    | 14uy -> CUnit
    | 15uy ->
      let first = read r
      let second = read r
      let rest = List.read r read
      CTuple(first, second, rest)
    | 16uy ->
      let typeName = NameResolution.read FQTypeName.read r
      let caseName = String.read r
      let fields = List.read r read
      CEnum(typeName, caseName, fields)
    | 17uy ->
      let consts = List.read r read
      CList consts
    | 18uy ->
      let pairs =
        List.read r (fun r ->
          let key = String.read r
          let value = read r
          (key, value))
      CDict pairs
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Const tag: {b}"))


let write (w : BinaryWriter) (c : PackageConstant.PackageConstant) : unit =
  Guid.write w c.id
  Name.write w c.name
  Const.write w c.body
  String.write w c.description
  Deprecation.write w FQConstantName.write c.deprecated

let read (r : BinaryReader) : PackageConstant.PackageConstant =
  let id = Guid.read r
  let name = Name.read r
  let body = Const.read r
  let description = String.read r
  let deprecated = Deprecation.read r FQConstantName.read
  { id = id
    name = name
    body = body
    description = description
    deprecated = deprecated }
