// TODO tidy this file
module LibBinarySerialization.Serializers.RT.Dval

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibBinarySerialization.BinaryFormat
open LibBinarySerialization.Serializers.Common
open LibBinarySerialization.Serializers.RT.Common


// Forward declarations for mutual recursion
let rec writeDval : BinaryWriter -> Dval -> unit = fun w dval -> writeDvalImpl w dval

and writeValueType : BinaryWriter -> ValueType -> unit =
  fun w vt -> writeValueTypeImpl w vt

and writeApplicable : BinaryWriter -> Applicable -> unit =
  fun w app -> writeApplicableImpl w app

and writeValueTypeImpl (w : BinaryWriter) (vt : ValueType) =
  match vt with
  | ValueType.Unknown -> w.Write 0uy
  | ValueType.Known kt ->
    w.Write 1uy
    writeKnownType w kt

and writeKnownType (w : BinaryWriter) (kt : KnownType) =
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
    writeValueType w vt
  | KTTuple(first, second, rest) ->
    w.Write 18uy
    writeValueType w first
    writeValueType w second
    List.write w writeValueType rest
  | KTFn(args, ret) ->
    w.Write 19uy
    NEList.write writeValueType w args
    writeValueType w ret
  | KTDB vt ->
    w.Write 20uy
    writeValueType w vt
  | KTCustomType(fqTypeName, typeArgs) ->
    w.Write 21uy
    FQTypeName.write w fqTypeName
    List.write w writeValueType typeArgs
  | KTDict vt ->
    w.Write 22uy
    writeValueType w vt

and writeApplicableImpl (w : BinaryWriter) (app : Applicable) =
  match app with
  | AppLambda lambda ->
    w.Write 0uy
    writeApplicableLambda w lambda
  | AppNamedFn namedFn ->
    w.Write 1uy
    writeApplicableNamedFn w namedFn

and writeApplicableLambda (w : BinaryWriter) (lambda : ApplicableLambda) =
  w.Write lambda.exprId
  List.write
    w
    (fun w ((reg : int), dval) ->
      w.Write reg
      writeDval w dval)
    lambda.closedRegisters
  writeTypeSymbolTable w lambda.typeSymbolTable
  List.write w writeDval lambda.argsSoFar

and writeApplicableNamedFn (w : BinaryWriter) (namedFn : ApplicableNamedFn) =
  FQFnName.write w namedFn.name
  writeTypeSymbolTable w namedFn.typeSymbolTable
  List.write w TypeReference.write namedFn.typeArgs
  List.write w writeDval namedFn.argsSoFar

and writeTypeSymbolTable (w : BinaryWriter) (tst : TypeSymbolTable) =
  Map.write String.write writeValueType w tst

and writeDvalImpl (w : BinaryWriter) (dval : Dval) =
  match dval with
  | DUnit -> w.Write 0uy
  | DBool value ->
    w.Write 1uy
    w.Write value
  | DInt8 value ->
    w.Write 2uy
    w.Write value
  | DUInt8 value ->
    w.Write 3uy
    w.Write value
  | DInt16 value ->
    w.Write 4uy
    w.Write value
  | DUInt16 value ->
    w.Write 5uy
    w.Write value
  | DInt32 value ->
    w.Write 6uy
    w.Write value
  | DUInt32 value ->
    w.Write 7uy
    w.Write value
  | DInt64 value ->
    w.Write 8uy
    w.Write value
  | DUInt64 value ->
    w.Write 9uy
    w.Write value
  | DInt128 value ->
    w.Write 10uy
    String.write w (string value)
  | DUInt128 value ->
    w.Write 11uy
    String.write w (string value)
  | DFloat value ->
    w.Write 12uy
    Float.write w value
  | DChar value ->
    w.Write 13uy
    String.write w value
  | DString value ->
    w.Write 14uy
    String.write w value
  | DUuid value ->
    w.Write 15uy
    Guid.write w value
  | DDateTime value ->
    w.Write 16uy
    DarkDateTime.write w value
  | DList(valueType, items) ->
    w.Write 17uy
    writeValueType w valueType
    List.write w writeDval items
  | DTuple(first, second, rest) ->
    w.Write 18uy
    writeDval w first
    writeDval w second
    List.write w writeDval rest
  | DDict(valueType, entries) ->
    w.Write 19uy
    writeValueType w valueType
    Map.write String.write writeDval w entries
  | DRecord(sourceTypeName, runtimeTypeName, typeArgs, fields) ->
    w.Write 20uy
    FQTypeName.write w sourceTypeName
    FQTypeName.write w runtimeTypeName
    List.write w writeValueType typeArgs
    Map.write String.write writeDval w fields
  | DEnum(sourceTypeName, runtimeTypeName, typeArgs, caseName, fields) ->
    w.Write 21uy
    FQTypeName.write w sourceTypeName
    FQTypeName.write w runtimeTypeName
    List.write w writeValueType typeArgs
    String.write w caseName
    List.write w writeDval fields
  | DApplicable applicable ->
    w.Write 22uy
    writeApplicable w applicable
  | DDB value ->
    w.Write 23uy
    String.write w value


// Read functions
let rec readDval : BinaryReader -> Dval = fun r -> readDvalImpl r

and readValueType : BinaryReader -> ValueType = fun r -> readValueTypeImpl r

and readApplicable : BinaryReader -> Applicable = fun r -> readApplicableImpl r

and readValueTypeImpl (r : BinaryReader) : ValueType =
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
  | 17uy -> KTList(readValueType r)
  | 18uy ->
    let first = readValueType r
    let second = readValueType r
    let rest = List.read r readValueType
    KTTuple(first, second, rest)
  | 19uy ->
    let args = NEList.read readValueType r
    let ret = readValueType r
    KTFn(args, ret)
  | 20uy -> KTDB(readValueType r)
  | 21uy ->
    let fqTypeName = FQTypeName.read r
    let typeArgs = List.read r readValueType
    KTCustomType(fqTypeName, typeArgs)
  | 22uy -> KTDict(readValueType r)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid KnownType tag: {b}"))

and readApplicableImpl (r : BinaryReader) : Applicable =
  match r.ReadByte() with
  | 0uy -> AppLambda(readApplicableLambda r)
  | 1uy -> AppNamedFn(readApplicableNamedFn r)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid Applicable tag: {b}"))

and readApplicableLambda (r : BinaryReader) : ApplicableLambda =
  let exprId = r.ReadUInt64()
  let closedRegisters =
    List.read r (fun r ->
      let reg = r.ReadInt32()
      let dval = readDval r
      (reg, dval))
  let typeSymbolTable = readTypeSymbolTable r
  let argsSoFar = List.read r readDval
  { exprId = exprId
    closedRegisters = closedRegisters
    typeSymbolTable = typeSymbolTable
    argsSoFar = argsSoFar }

and readApplicableNamedFn (r : BinaryReader) : ApplicableNamedFn =
  let name = FQFnName.read r
  let typeSymbolTable = readTypeSymbolTable r
  let typeArgs = List.read r TypeReference.read
  let argsSoFar = List.read r readDval
  { name = name
    typeSymbolTable = typeSymbolTable
    typeArgs = typeArgs
    argsSoFar = argsSoFar }

and readTypeSymbolTable (r : BinaryReader) : TypeSymbolTable =
  Map.read String.read readValueType r

and readDvalImpl (r : BinaryReader) : Dval =
  match r.ReadByte() with
  | 0uy -> DUnit
  | 1uy -> DBool(r.ReadBoolean())
  | 2uy -> DInt8(r.ReadSByte())
  | 3uy -> DUInt8(r.ReadByte())
  | 4uy -> DInt16(r.ReadInt16())
  | 5uy -> DUInt16(r.ReadUInt16())
  | 6uy -> DInt32(r.ReadInt32())
  | 7uy -> DUInt32(r.ReadUInt32())
  | 8uy -> DInt64(r.ReadInt64())
  | 9uy -> DUInt64(r.ReadUInt64())
  | 10uy -> DInt128(System.Int128.Parse(String.read r))
  | 11uy -> DUInt128(System.UInt128.Parse(String.read r))
  | 12uy -> DFloat(Float.read r)
  | 13uy -> DChar(String.read r)
  | 14uy -> DString(String.read r)
  | 15uy -> DUuid(Guid.read r)
  | 16uy -> DDateTime(DarkDateTime.read r)
  | 17uy ->
    let valueType = readValueType r
    let items = List.read r readDval
    DList(valueType, items)
  | 18uy ->
    let first = readDval r
    let second = readDval r
    let rest = List.read r readDval
    DTuple(first, second, rest)
  | 19uy ->
    let valueType = readValueType r
    let entries = Map.read String.read readDval r
    DDict(valueType, entries)
  | 20uy ->
    let sourceTypeName = FQTypeName.read r
    let runtimeTypeName = FQTypeName.read r
    let typeArgs = List.read r readValueType
    let fields = Map.read String.read readDval r
    DRecord(sourceTypeName, runtimeTypeName, typeArgs, fields)
  | 21uy ->
    let sourceTypeName = FQTypeName.read r
    let runtimeTypeName = FQTypeName.read r
    let typeArgs = List.read r readValueType
    let caseName = String.read r
    let fields = List.read r readDval
    DEnum(sourceTypeName, runtimeTypeName, typeArgs, caseName, fields)
  | 22uy -> DApplicable(readApplicable r)
  | 23uy -> DDB(String.read r)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid Dval tag: {b}"))


// Export modules for compatibility
module ValueType =
  let write = writeValueType
  let read = readValueType

module Applicable =
  let write = writeApplicable
  let read = readApplicable

// Main exports
let write = writeDval
let read = readDval
