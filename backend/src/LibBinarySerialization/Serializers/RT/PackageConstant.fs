module LibBinarySerialization.Serializers.RT.PackageConstant

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.RT.Common


module Const =
  let rec write (w : BinaryWriter) (c : Const) : unit =
    match c with
    | CUnit -> w.Write 0uy

    | CBool value ->
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

    | CInt64 value ->
      w.Write 8uy
      w.Write value

    | CUInt64 value ->
      w.Write 9uy
      w.Write value

    | CInt128 value ->
      w.Write 10uy
      String.write w (value.ToString())

    | CUInt128 value ->
      w.Write 11uy
      String.write w (value.ToString())

    | CFloat(sign, whole, fraction) ->
      w.Write 12uy
      match sign with
      | Positive -> w.Write 0uy
      | Negative -> w.Write 1uy
      String.write w whole
      String.write w fraction

    | CChar value ->
      w.Write 13uy
      String.write w value

    | CString value ->
      w.Write 14uy
      String.write w value

    | CList items ->
      w.Write 15uy
      List.write w write items

    | CTuple(first, second, rest) ->
      w.Write 16uy
      write w first
      write w second
      List.write w write rest

    | CDict pairs ->
      w.Write 17uy
      List.write
        w
        (fun w (key, value) ->
          String.write w key
          write w value)
        pairs

    | CEnum(nameResolution, caseName, fields) ->
      w.Write 18uy
      NameResolution.write FQTypeName.write w nameResolution
      String.write w caseName
      List.write w write fields


  let rec read (r : BinaryReader) : Const =
    match r.ReadByte() with
    | 0uy -> CUnit
    | 1uy -> CBool(r.ReadBoolean())
    | 2uy -> CInt8(r.ReadSByte())
    | 3uy -> CUInt8(r.ReadByte())
    | 4uy -> CInt16(r.ReadInt16())
    | 5uy -> CUInt16(r.ReadUInt16())
    | 6uy -> CInt32(r.ReadInt32())
    | 7uy -> CUInt32(r.ReadUInt32())
    | 8uy -> CInt64(r.ReadInt64())
    | 9uy -> CUInt64(r.ReadUInt64())
    | 10uy -> CInt128(System.Int128.Parse(String.read r))
    | 11uy -> CUInt128(System.UInt128.Parse(String.read r))
    | 12uy ->
      let sign =
        match r.ReadByte() with
        | 0uy -> Positive
        | 1uy -> Negative
        | b -> raise (BinaryFormatException(CorruptedData $"Invalid Sign tag: {b}"))
      let whole = String.read r
      let fraction = String.read r
      CFloat(sign, whole, fraction)

    | 13uy -> CChar(String.read r)
    | 14uy -> CString(String.read r)
    | 15uy -> CList(List.read r read)

    | 16uy ->
      let first = read r
      let second = read r
      let rest = List.read r read
      CTuple(first, second, rest)

    | 17uy ->
      let pairs =
        List.read r (fun r ->
          let key = String.read r
          let value = read r
          (key, value))
      CDict pairs

    | 18uy ->
      let nameResolution = NameResolution.read FQTypeName.read r
      let caseName = String.read r
      let fields = List.read r read
      CEnum(nameResolution, caseName, fields)

    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Const tag: {b}"))



let write (w : BinaryWriter) (c : PackageConstant.PackageConstant) : unit =
  Guid.write w c.id
  Const.write w c.body

let read (r : BinaryReader) : PackageConstant.PackageConstant =
  let id = Guid.read r
  let body = Const.read r
  { id = id; body = body }
