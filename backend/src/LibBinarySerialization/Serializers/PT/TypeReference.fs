module LibBinarySerialization.Serializers.PT.TypeReference

open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.PT.Common


let rec write (w : BinaryWriter) (t : TypeReference) : unit =
  match t with
  // CLEANUP reorder these
  | TInt64 -> w.Write 0uy
  | TUInt64 -> w.Write 1uy
  | TInt8 -> w.Write 2uy
  | TUInt8 -> w.Write 3uy
  | TInt16 -> w.Write 4uy
  | TUInt16 -> w.Write 5uy
  | TInt32 -> w.Write 6uy
  | TUInt32 -> w.Write 7uy
  | TInt128 -> w.Write 8uy
  | TUInt128 -> w.Write 9uy
  | TFloat -> w.Write 10uy
  | TBool -> w.Write 11uy
  | TUnit -> w.Write 12uy
  | TString -> w.Write 13uy
  | TList inner ->
    w.Write 14uy
    write w inner
  | TDict inner ->
    w.Write 15uy
    write w inner
  | TDB inner ->
    w.Write 16uy
    write w inner
  | TDateTime -> w.Write 17uy
  | TChar -> w.Write 18uy
  | TUuid -> w.Write 19uy
  | TCustomType(typeName, typeArgs) ->
    w.Write 20uy
    NameResolution.write FQTypeName.write w typeName
    List.write w write typeArgs
  | TVariable name ->
    w.Write 21uy
    String.write w name
  | TFn(paramTypes, returnType) ->
    w.Write 22uy
    NEList.write write w paramTypes
    write w returnType
  | TTuple(first, second, rest) ->
    w.Write 23uy
    write w first
    write w second
    List.write w write rest

let rec read (r : BinaryReader) : TypeReference =
  match r.ReadByte() with
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
  | 14uy -> TList(read r)
  | 15uy -> TDict(read r)
  | 16uy -> TDB(read r)
  | 17uy -> TDateTime
  | 18uy -> TChar
  | 19uy -> TUuid
  | 20uy ->
    let typeName = NameResolution.read FQTypeName.read r
    let typeArgs = List.read r read
    TCustomType(typeName, typeArgs)
  | 21uy ->
    let name = String.read r
    TVariable name
  | 22uy ->
    let paramTypes = NEList.read read r
    let returnType = read r
    TFn(paramTypes, returnType)
  | 23uy ->
    let first = read r
    let second = read r
    let rest = List.read r read
    TTuple(first, second, rest)
  | b ->
    raise (BinaryFormatException(CorruptedData $"Invalid TypeReference tag: {b}"))
