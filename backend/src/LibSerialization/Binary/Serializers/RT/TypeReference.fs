module LibSerialization.Binary.Serializers.RT.TypeReference

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibSerialization.Binary
open BaseFormat
open Serializers.Common
open Serializers.RT.Common


let rec write (w : BinaryWriter) (t : TypeReference) : unit =
  match t with
  | TUnit -> w.Write 0uy
  | TBool -> w.Write 1uy
  | TInt8 -> w.Write 2uy
  | TUInt8 -> w.Write 3uy
  | TInt16 -> w.Write 4uy
  | TUInt16 -> w.Write 5uy
  | TInt32 -> w.Write 6uy
  | TUInt32 -> w.Write 7uy
  | TInt64 -> w.Write 8uy
  | TUInt64 -> w.Write 9uy
  | TInt128 -> w.Write 10uy
  | TUInt128 -> w.Write 11uy
  | TFloat -> w.Write 12uy
  | TChar -> w.Write 13uy
  | TString -> w.Write 14uy
  | TUuid -> w.Write 15uy
  | TDateTime -> w.Write 16uy
  | TTuple(first, second, rest) ->
    w.Write 17uy
    write w first
    write w second
    List.write w write rest
  | TList inner ->
    w.Write 18uy
    write w inner
  | TDict inner ->
    w.Write 19uy
    write w inner
  | TFn(paramTypes, returnType) ->
    w.Write 20uy
    NEList.write write w paramTypes
    write w returnType
  | TCustomType(typeName, typeArgs) ->
    w.Write 21uy
    NameResolution.write FQTypeName.write w typeName
    List.write w write typeArgs
  | TVariable name ->
    w.Write 22uy
    String.write w name
  | TDB inner ->
    w.Write 23uy
    write w inner

let rec read (r : BinaryReader) : TypeReference =
  match r.ReadByte() with
  | 0uy -> TUnit
  | 1uy -> TBool
  | 2uy -> TInt8
  | 3uy -> TUInt8
  | 4uy -> TInt16
  | 5uy -> TUInt16
  | 6uy -> TInt32
  | 7uy -> TUInt32
  | 8uy -> TInt64
  | 9uy -> TUInt64
  | 10uy -> TInt128
  | 11uy -> TUInt128
  | 12uy -> TFloat
  | 13uy -> TChar
  | 14uy -> TString
  | 15uy -> TUuid
  | 16uy -> TDateTime
  | 17uy ->
    let first = read r
    let second = read r
    let rest = List.read r read
    TTuple(first, second, rest)
  | 18uy -> TList(read r)
  | 19uy -> TDict(read r)
  | 20uy ->
    let paramTypes = NEList.read read r
    let returnType = read r
    TFn(paramTypes, returnType)
  | 21uy ->
    let typeName = NameResolution.read FQTypeName.read r
    let typeArgs = List.read r read
    TCustomType(typeName, typeArgs)
  | 22uy -> TVariable(String.read r)
  | 23uy -> TDB(read r)
  | b ->
    raise (BinaryFormatException(CorruptedData $"Invalid TypeReference tag: {b}"))
