/// Complete Expr serialization for custom binary format
module LibBinarySerialization.Binary.Serializers.Expr

open System
open System.IO
open Prelude
open LibBinarySerialization.Binary.BinaryFormat
open LibBinarySerialization.Binary.Primitives
open LibBinarySerialization.SerializedTypes
open TypeReference
open Common

// Mutually recursive functions for reading and writing expressions
let rec readExpr (reader: BinaryReader) : Expr =
  match reader.ReadByte() with
  | 0uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadInt64()
      EInt64 (id, value)
  | 1uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadUInt64()
      EUInt64 (id, value)
  | 2uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadSByte()
      EInt8 (id, value)
  | 3uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadByte()
      EUInt8 (id, value)
  | 4uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadInt16()
      EInt16 (id, value)
  | 5uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadUInt16()
      EUInt16 (id, value)
  | 6uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadInt32()
      EInt32 (id, value)
  | 7uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadUInt32()
      EUInt32 (id, value)
  | 8uy ->
      let id = reader.ReadUInt64()
      let value = Reader.readString reader |> bigint.Parse
      EInt128 (id, value)
  | 9uy ->
      let id = reader.ReadUInt64()
      let value = Reader.readString reader |> bigint.Parse
      EUInt128 (id, value)
  | 10uy ->
      let id = reader.ReadUInt64()
      let value = reader.ReadBoolean()
      EBool (id, value)
  | 11uy ->
      let id = reader.ReadUInt64()
      let segments = Reader.readList reader readStringSegment
      EString (id, segments)
  | 12uy ->
      let id = reader.ReadUInt64()
      let value = Reader.readString reader
      EChar (id, value)
  | 13uy ->
      let id = reader.ReadUInt64()
      let sign = Sign.read reader
      let whole = Reader.readString reader
      let fractional = Reader.readString reader
      EFloat (id, sign, whole, fractional)
  | 14uy ->
      let id = reader.ReadUInt64()
      EUnit id
  | 15uy ->
      let id = reader.ReadUInt64()
      let nameRes = NameResolution.read FQConstantName.read reader
      EConstant (id, nameRes)
  | 16uy ->
      let id = reader.ReadUInt64()
      let pattern = LetPattern.read reader
      let rhs = readExpr reader
      let body = readExpr reader
      ELet (id, pattern, rhs, body)
  | 17uy ->
      let id = reader.ReadUInt64()
      let cond = readExpr reader
      let thenExpr = readExpr reader
      let elseExpr = Reader.readOption reader readExpr
      EIf (id, cond, thenExpr, elseExpr)
  | 18uy ->
      let id = reader.ReadUInt64()
      let pats = readNEListLetPattern reader
      let body = readExpr reader
      ELambda (id, pats, body)
  | 19uy ->
      let id = reader.ReadUInt64()
      let expr = readExpr reader
      let field = Reader.readString reader
      ERecordFieldAccess (id, expr, field)
  | 20uy ->
      let id = reader.ReadUInt64()
      let name = Reader.readString reader
      EVariable (id, name)
  | 21uy ->
      let id = reader.ReadUInt64()
      let fn = readExpr reader
      let typeArgs = Reader.readList reader readTypeReference
      let args = readNEListExpr reader
      EApply (id, fn, typeArgs, args)
  | 22uy ->
      let id = reader.ReadUInt64()
      let exprs = Reader.readList reader readExpr
      EList (id, exprs)
  | 23uy ->
      let id = reader.ReadUInt64()
      let typeName = readNameResolutionFQTypeName reader
      let typeArgs = Reader.readList reader readTypeReference
      let fields = Reader.readList reader (fun r -> 
        let name = Reader.readString r
        let expr = readExpr r
        (name, expr))
      ERecord (id, typeName, typeArgs, fields)
  | 24uy ->
      let id = reader.ReadUInt64()
      let record = readExpr reader
      let updates = readNEListStringExpr reader
      ERecordUpdate (id, record, updates)
  | 25uy ->
      let id = reader.ReadUInt64()
      let expr = readExpr reader
      let pipes = Reader.readList reader readPipeExpr
      EPipe (id, expr, pipes)
  | 26uy ->
      let id = reader.ReadUInt64()
      let typeName = readNameResolutionFQTypeName reader
      let typeArgs = Reader.readList reader readTypeReference
      let caseName = Reader.readString reader
      let fields = Reader.readList reader readExpr
      EEnum (id, typeName, typeArgs, caseName, fields)
  | 27uy ->
      let id = reader.ReadUInt64()
      let expr = readExpr reader
      let cases = Reader.readList reader readMatchCase
      EMatch (id, expr, cases)
  | 28uy ->
      let id = reader.ReadUInt64()
      let first = readExpr reader
      let second = readExpr reader
      let rest = Reader.readList reader readExpr
      ETuple (id, first, second, rest)
  | 29uy ->
      let id = reader.ReadUInt64()
      let op = readInfix reader
      let left = readExpr reader
      let right = readExpr reader
      EInfix (id, op, left, right)
  | 30uy ->
      let id = reader.ReadUInt64()
      let pairs = Reader.readList reader (fun r ->
        let key = Reader.readString r
        let value = readExpr r
        (key, value))
      EDict (id, pairs)
  | 31uy ->
      let id = reader.ReadUInt64()
      let nameRes = readNameResolutionFQFnName reader
      EFnName (id, nameRes)
  | 32uy ->
      let id = reader.ReadUInt64()
      let first = readExpr reader
      let next = readExpr reader
      EStatement (id, first, next)
  | b -> 
      raise (BinaryFormatException(CorruptedData $"Invalid Expr tag: {b}"))

and readStringSegment (reader: BinaryReader) : StringSegment =
  match reader.ReadByte() with
  | 0uy -> StringText (Reader.readString reader)
  | 1uy -> StringInterpolation (readExpr reader)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid StringSegment tag: {b}"))

and readNEListLetPattern (reader: BinaryReader) : NEList<LetPattern> =
  NEList.read LetPattern.read reader

and readNEListExpr (reader: BinaryReader) : NEList<Expr> =
  NEList.read readExpr reader

and readNEListStringExpr (reader: BinaryReader) : NEList<string * Expr> =
  NEList.read (fun r ->
    let name = Reader.readString r
    let expr = readExpr r
    (name, expr)) reader

and readNameResolutionFQTypeName (reader: BinaryReader) : NameResolution<FQTypeName.FQTypeName> =
  TypeReference.readNameResolutionFQTypeName reader

and readNameResolutionFQFnName (reader: BinaryReader) : NameResolution<FQFnName.FQFnName> =
  NameResolution.read FQFnName.read reader

and readNameResolutionFQConstantName (reader: BinaryReader) : NameResolution<FQConstantName.FQConstantName> =
  NameResolution.read FQConstantName.read reader

and readPipeExpr (reader: BinaryReader) : PipeExpr =
  match reader.ReadByte() with
  | 0uy ->
    let id = reader.ReadUInt64()
    let name = Reader.readString reader
    let args = Reader.readList reader readExpr
    EPipeVariable (id, name, args)
  | 1uy ->
    let id = reader.ReadUInt64()
    let pats = readNEListLetPattern reader
    let body = readExpr reader
    EPipeLambda (id, pats, body)
  | 2uy ->
    let id = reader.ReadUInt64()
    let infix = readInfix reader
    let expr = readExpr reader
    EPipeInfix (id, infix, expr)
  | 3uy ->
    let id = reader.ReadUInt64()
    let fnName = readNameResolutionFQFnName reader
    let typeArgs = Reader.readList reader readTypeReference
    let args = Reader.readList reader readExpr
    EPipeFnCall (id, fnName, typeArgs, args)
  | 4uy ->
    let id = reader.ReadUInt64()
    let typeName = readNameResolutionFQTypeName reader
    let caseName = Reader.readString reader
    let fields = Reader.readList reader readExpr
    EPipeEnum (id, typeName, caseName, fields)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid PipeExpr tag: {b}"))

and readMatchCase (reader: BinaryReader) : MatchCase =
  let pat = MatchPattern.read reader
  let whenCondition = Reader.readOption reader readExpr
  let rhs = readExpr reader
  { pat = pat; whenCondition = whenCondition; rhs = rhs }

and readInfix (reader: BinaryReader) : Infix =
  Infix.read reader

and writeExpr (writer: BinaryWriter) (expr: Expr) : unit =
  match expr with
  | EInt64 (id, value) ->
      writer.Write(0uy)
      writer.Write(id)
      writer.Write(value)
  | EUInt64 (id, value) ->
      writer.Write(1uy)
      writer.Write(id)
      writer.Write(value)
  | EInt8 (id, value) ->
      writer.Write(2uy)
      writer.Write(id)
      writer.Write(value)
  | EUInt8 (id, value) ->
      writer.Write(3uy)
      writer.Write(id)
      writer.Write(value)
  | EInt16 (id, value) ->
      writer.Write(4uy)
      writer.Write(id)
      writer.Write(value)
  | EUInt16 (id, value) ->
      writer.Write(5uy)
      writer.Write(id)
      writer.Write(value)
  | EInt32 (id, value) ->
      writer.Write(6uy)
      writer.Write(id)
      writer.Write(value)
  | EUInt32 (id, value) ->
      writer.Write(7uy)
      writer.Write(id)
      writer.Write(value)
  | EInt128 (id, value) ->
      writer.Write(8uy)
      writer.Write(id)
      Writer.writeString writer (value.ToString())
  | EUInt128 (id, value) ->
      writer.Write(9uy)
      writer.Write(id)
      Writer.writeString writer (value.ToString())
  | EBool (id, value) ->
      writer.Write(10uy)
      writer.Write(id)
      writer.Write(value)
  | EString (id, segments) ->
      writer.Write(11uy)
      writer.Write(id)
      Writer.writeList writer writeStringSegment segments
  | EChar (id, value) ->
      writer.Write(12uy)
      writer.Write(id)
      Writer.writeString writer value
  | EFloat (id, sign, whole, fractional) ->
      writer.Write(13uy)
      writer.Write(id)
      writeSign writer sign
      Writer.writeString writer whole
      Writer.writeString writer fractional
  | EUnit id ->
      writer.Write(14uy)
      writer.Write(id)
  | EConstant (id, nameRes) ->
      writer.Write(15uy)
      writer.Write(id)
      writeNameResolutionFQConstantName writer nameRes
  | ELet (id, pattern, rhs, body) ->
      writer.Write(16uy)
      writer.Write(id)
      writeLetPattern writer pattern
      writeExpr writer rhs
      writeExpr writer body
  | EIf (id, cond, thenExpr, elseExpr) ->
      writer.Write(17uy)
      writer.Write(id)
      writeExpr writer cond
      writeExpr writer thenExpr
      Writer.writeOption writer writeExpr elseExpr
  | ELambda (id, pats, body) ->
      writer.Write(18uy)
      writer.Write(id)
      writeNEListLetPattern writer pats
      writeExpr writer body
  | ERecordFieldAccess (id, expr, field) ->
      writer.Write(19uy)
      writer.Write(id)
      writeExpr writer expr
      Writer.writeString writer field
  | EVariable (id, name) ->
      writer.Write(20uy)
      writer.Write(id)
      Writer.writeString writer name
  | EApply (id, fn, typeArgs, args) ->
      writer.Write(21uy)
      writer.Write(id)
      writeExpr writer fn
      Writer.writeList writer writeTypeReference typeArgs
      writeNEListExpr writer args
  | EList (id, exprs) ->
      writer.Write(22uy)
      writer.Write(id)
      Writer.writeList writer writeExpr exprs
  | ERecord (id, typeName, typeArgs, fields) ->
      writer.Write(23uy)
      writer.Write(id)
      writeNameResolutionFQTypeName writer typeName
      Writer.writeList writer writeTypeReference typeArgs
      Writer.writeList writer (fun w (name, expr) -> 
        Writer.writeString w name
        writeExpr w expr) fields
  | ERecordUpdate (id, record, updates) ->
      writer.Write(24uy)
      writer.Write(id)
      writeExpr writer record
      writeNEListStringExpr writer updates
  | EPipe (id, expr, pipes) ->
      writer.Write(25uy)
      writer.Write(id)
      writeExpr writer expr
      Writer.writeList writer writePipeExpr pipes
  | EEnum (id, typeName, typeArgs, caseName, fields) ->
      writer.Write(26uy)
      writer.Write(id)
      writeNameResolutionFQTypeName writer typeName
      Writer.writeList writer writeTypeReference typeArgs
      Writer.writeString writer caseName
      Writer.writeList writer writeExpr fields
  | EMatch (id, expr, cases) ->
      writer.Write(27uy)
      writer.Write(id)
      writeExpr writer expr
      Writer.writeList writer writeMatchCase cases
  | ETuple (id, first, second, rest) ->
      writer.Write(28uy)
      writer.Write(id)
      writeExpr writer first
      writeExpr writer second
      Writer.writeList writer writeExpr rest
  | EInfix (id, op, left, right) ->
      writer.Write(29uy)
      writer.Write(id)
      writeInfix writer op
      writeExpr writer left
      writeExpr writer right
  | EDict (id, pairs) ->
      writer.Write(30uy)
      writer.Write(id)
      Writer.writeList writer (fun w (key, value) ->
        Writer.writeString w key
        writeExpr w value) pairs
  | EFnName (id, nameRes) ->
      writer.Write(31uy)
      writer.Write(id)
      writeNameResolutionFQFnName writer nameRes
  | EStatement (id, first, next) ->
      writer.Write(32uy)
      writer.Write(id)
      writeExpr writer first
      writeExpr writer next

// Helper functions for nested types (placeholders for now)
and writeStringSegment (writer: BinaryWriter) (segment: StringSegment) : unit =
  match segment with
  | StringText text ->
      writer.Write(0uy)
      Writer.writeString writer text
  | StringInterpolation expr ->
      writer.Write(1uy)
      writeExpr writer expr

and writeSign (writer: BinaryWriter) (sign: Sign) : unit =
  match sign with
  | Positive -> writer.Write(0uy)
  | Negative -> writer.Write(1uy)

and writeNameResolutionFQConstantName (writer: BinaryWriter) (nameRes: NameResolution<FQConstantName.FQConstantName>) : unit =
  NameResolution.write FQConstantName.write writer nameRes

and writeLetPattern (writer: BinaryWriter) (pattern: LetPattern) : unit =
  LetPattern.write writer pattern

and writeNEListLetPattern (writer: BinaryWriter) (pats: NEList<LetPattern>) : unit =
  writeLetPattern writer pats.head
  Writer.writeList writer writeLetPattern pats.tail

// TypeReference is now implemented in SerializersTypeReference

and writeNEListExpr (writer: BinaryWriter) (exprs: NEList<Expr>) : unit =
  writeExpr writer exprs.head
  Writer.writeList writer writeExpr exprs.tail

and writeNameResolutionFQTypeName (writer: BinaryWriter) (nameRes: NameResolution<FQTypeName.FQTypeName>) : unit =
  TypeReference.writeNameResolutionFQTypeName writer nameRes

and writeNEListStringExpr (writer: BinaryWriter) (updates: NEList<string * Expr>) : unit =
  let (name, expr) = updates.head
  Writer.writeString writer name
  writeExpr writer expr
  Writer.writeList writer (fun w (n, e) ->
    Writer.writeString w n
    writeExpr w e) updates.tail

and writePipeExpr (writer: BinaryWriter) (pipeExpr: PipeExpr) : unit =
  match pipeExpr with
  | EPipeVariable (id, name, args) ->
      writer.Write(0uy)
      writer.Write(id)
      Writer.writeString writer name
      Writer.writeList writer writeExpr args
  | EPipeLambda (id, pats, body) ->
      writer.Write(1uy)
      writer.Write(id)
      writeNEListLetPattern writer pats
      writeExpr writer body
  | EPipeInfix (id, infix, expr) ->
      writer.Write(2uy)
      writer.Write(id)
      writeInfix writer infix
      writeExpr writer expr
  | EPipeFnCall (id, fnName, typeArgs, args) ->
      writer.Write(3uy)
      writer.Write(id)
      writeNameResolutionFQFnName writer fnName
      Writer.writeList writer writeTypeReference typeArgs
      Writer.writeList writer writeExpr args
  | EPipeEnum (id, typeName, caseName, fields) ->
      writer.Write(4uy)
      writer.Write(id)
      writeNameResolutionFQTypeName writer typeName
      Writer.writeString writer caseName
      Writer.writeList writer writeExpr fields

and writeMatchCase (writer: BinaryWriter) (case: MatchCase) : unit =
  writeMatchPattern writer case.pat
  Writer.writeOption writer writeExpr case.whenCondition
  writeExpr writer case.rhs

and writeMatchPattern (writer: BinaryWriter) (pattern: MatchPattern) : unit =
  MatchPattern.write writer pattern

and writeInfix (writer: BinaryWriter) (op: Infix) : unit =
  Infix.write writer op

and writeNameResolutionFQFnName (writer: BinaryWriter) (nameRes: NameResolution<FQFnName.FQFnName>) : unit =
  NameResolution.write FQFnName.write writer nameRes

