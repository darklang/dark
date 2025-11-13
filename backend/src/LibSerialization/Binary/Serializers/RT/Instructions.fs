// TODO tidy this file
module rec LibSerialization.Binary.Serializers.RT.Instructions

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibSerialization.Binary
open BaseFormat
open Serializers.Common
open Serializers.RT.Common


module LetPattern =
  let rec write (w : BinaryWriter) (pat : LetPattern) =
    match pat with
    | LPVariable extractTo ->
      w.Write 0uy
      w.Write(extractTo : int)
    | LPTuple(first, second, theRest) ->
      w.Write 1uy
      write w first
      write w second
      List.write w write theRest
    | LPUnit -> w.Write 2uy

  let rec read (r : BinaryReader) : LetPattern =
    match r.ReadByte() with
    | 0uy -> LPVariable(r.ReadInt32())
    | 1uy ->
      let first = read r
      let second = read r
      let theRest = List.read r read
      LPTuple(first, second, theRest)
    | 2uy -> LPUnit
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid LetPattern tag: {b}"))


module MatchPattern =
  let rec write (w : BinaryWriter) (pat : MatchPattern) =
    match pat with
    | MPUnit -> w.Write 0uy
    | MPBool value ->
      w.Write 1uy
      w.Write value
    | MPInt8 value ->
      w.Write 2uy
      w.Write value
    | MPUInt8 value ->
      w.Write 3uy
      w.Write value
    | MPInt16 value ->
      w.Write 4uy
      w.Write value
    | MPUInt16 value ->
      w.Write 5uy
      w.Write value
    | MPInt32 value ->
      w.Write 6uy
      w.Write value
    | MPUInt32 value ->
      w.Write 7uy
      w.Write value
    | MPInt64 value ->
      w.Write 8uy
      w.Write value
    | MPUInt64 value ->
      w.Write 9uy
      w.Write value
    | MPInt128 value ->
      w.Write 10uy
      String.write w (value.ToString())
    | MPUInt128 value ->
      w.Write 11uy
      String.write w (value.ToString())
    | MPFloat value ->
      w.Write 12uy
      Float.write w value
    | MPChar value ->
      w.Write 13uy
      String.write w value
    | MPString value ->
      w.Write 14uy
      String.write w value
    | MPList patterns ->
      w.Write 15uy
      List.write w write patterns
    | MPListCons(head, tail) ->
      w.Write 16uy
      write w head
      write w tail
    | MPTuple(first, second, theRest) ->
      w.Write 17uy
      write w first
      write w second
      List.write w write theRest
    | MPEnum(caseName, fields) ->
      w.Write 18uy
      String.write w caseName
      List.write w write fields
    | MPVariable reg ->
      w.Write 19uy
      w.Write reg
    | MPOr patterns ->
      w.Write 20uy
      NEList.write write w patterns

  let rec read (r : BinaryReader) : MatchPattern =
    match r.ReadByte() with
    | 0uy -> MPUnit
    | 1uy -> MPBool(r.ReadBoolean())
    | 2uy -> MPInt8(r.ReadSByte())
    | 3uy -> MPUInt8(r.ReadByte())
    | 4uy -> MPInt16(r.ReadInt16())
    | 5uy -> MPUInt16(r.ReadUInt16())
    | 6uy -> MPInt32(r.ReadInt32())
    | 7uy -> MPUInt32(r.ReadUInt32())
    | 8uy -> MPInt64(r.ReadInt64())
    | 9uy -> MPUInt64(r.ReadUInt64())
    | 10uy -> MPInt128(System.Int128.Parse(String.read r))
    | 11uy -> MPUInt128(System.UInt128.Parse(String.read r))
    | 12uy -> MPFloat(Float.read r)
    | 13uy -> MPChar(String.read r)
    | 14uy -> MPString(String.read r)
    | 15uy ->
      let patterns = List.read r read
      MPList(patterns)
    | 16uy ->
      let head = read r
      let tail = read r
      MPListCons(head, tail)
    | 17uy ->
      let first = read r
      let second = read r
      let theRest = List.read r read
      MPTuple(first, second, theRest)
    | 18uy ->
      let caseName = String.read r
      let fields = List.read r read
      MPEnum(caseName, fields)
    | 19uy ->
      let reg = r.ReadInt32()
      MPVariable(reg)
    | 20uy ->
      let patterns = NEList.read read r
      MPOr(patterns)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid MatchPattern tag: {b}"))


module LambdaImpl =
  let write (w : BinaryWriter) (l : LambdaImpl) =
    w.Write(l.exprId : uint64)
    NEList.write LetPattern.write w l.patterns
    List.write
      w
      (fun w (copyFrom, copyTo) ->
        w.Write(copyFrom : int)
        w.Write(copyTo : int))
      l.registersToCloseOver
    Instructions.write w l.instructions


  let read (r : BinaryReader) : LambdaImpl =
    let exprId = r.ReadUInt64()
    let patterns = NEList.read LetPattern.read r
    let registersToCloseOver =
      List.read r (fun r ->
        let copyFrom = r.ReadInt32()
        let copyTo = r.ReadInt32()
        (copyFrom, copyTo))

    let instructions = Instructions.read r
    { exprId = exprId
      patterns = patterns
      registersToCloseOver = registersToCloseOver
      instructions = instructions }


module StringSegment =
  let write (w : BinaryWriter) (seg : StringSegment) =
    match seg with
    | Text text ->
      w.Write 0uy
      String.write w text
    | Interpolated reg ->
      w.Write 1uy
      w.Write(reg : int)

  let read (r : BinaryReader) : StringSegment =
    match r.ReadByte() with
    | 0uy -> Text(String.read r)
    | 1uy -> Interpolated(r.ReadInt32())
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid StringSegment tag: {b}"))


module Instruction =
  let write (w : BinaryWriter) (i : Instruction) =
    match i with
    | LoadVal(loadTo, dval) ->
      w.Write 0uy
      w.Write(loadTo : int)
      Dval.write w dval
    | CopyVal(copyTo, copyFrom) ->
      w.Write 1uy
      w.Write(copyTo : int)
      w.Write(copyFrom : int)
    | Or(createTo, lhs, rhs) ->
      w.Write 2uy
      w.Write(createTo : int)
      w.Write(lhs : int)
      w.Write(rhs : int)
    | And(createTo, lhs, rhs) ->
      w.Write 3uy
      w.Write(createTo : int)
      w.Write(lhs : int)
      w.Write(rhs : int)
    | CreateString(createTo, segments) ->
      w.Write 4uy
      w.Write(createTo : int)
      List.write w StringSegment.write segments
    | CheckLetPatternAndExtractVars(valueReg, pat) ->
      w.Write 5uy
      w.Write(valueReg : int)
      LetPattern.write w pat
    | JumpByIfFalse(instrsToJump, conditionReg) ->
      w.Write 6uy
      w.Write(instrsToJump : int)
      w.Write(conditionReg : int)
    | JumpBy instrsToJump ->
      w.Write 7uy
      w.Write(instrsToJump : int)
    | CheckMatchPatternAndExtractVars(valueReg, pat, failJump) ->
      w.Write 8uy
      w.Write(valueReg : int)
      MatchPattern.write w pat
      w.Write(failJump : int)
    | MatchUnmatched(valueReg) ->
      w.Write 9uy
      w.Write(valueReg : int)
    | CreateTuple(createTo, first, second, theRest) ->
      w.Write 10uy
      w.Write(createTo : int)
      w.Write(first : int)
      w.Write(second : int)
      List.write w (fun w reg -> w.Write(reg : int)) theRest
    | CreateList(createTo, itemsToAdd) ->
      w.Write 11uy
      w.Write(createTo : int)
      List.write w (fun w reg -> w.Write(reg : int)) itemsToAdd
    | CreateDict(createTo, entries) ->
      w.Write 12uy
      w.Write(createTo : int)
      List.write
        w
        (fun w (key, reg) ->
          String.write w key
          w.Write(reg : int))
        entries
    | CreateRecord(createTo, typeName, typeArgs, fields) ->
      w.Write 13uy
      w.Write(createTo : int)
      FQTypeName.write w typeName
      List.write w TypeReference.write typeArgs
      List.write
        w
        (fun w (field, reg) ->
          String.write w field
          w.Write(reg : int))
        fields
    | CloneRecordWithUpdates(createTo, originalRecordReg, updates) ->
      w.Write 14uy
      w.Write(createTo : int)
      w.Write(originalRecordReg : int)
      List.write
        w
        (fun w (field, reg) ->
          String.write w field
          w.Write(reg : int))
        updates
    | GetRecordField(targetReg, recordReg, fieldName) ->
      w.Write 15uy
      w.Write(targetReg : int)
      w.Write(recordReg : int)
      String.write w fieldName
    | CreateEnum(createTo, typeName, typeArgs, caseName, fields) ->
      w.Write 16uy
      w.Write(createTo : int)
      FQTypeName.write w typeName
      List.write w TypeReference.write typeArgs
      String.write w caseName
      List.write w (fun w reg -> w.Write(reg : int)) fields
    | LoadValue(createTo, valueName) ->
      w.Write 17uy
      w.Write(createTo : int)
      FQValueName.write w valueName
    | CreateLambda(createTo, lambda) ->
      w.Write 18uy
      w.Write(createTo : int)
      LambdaImpl.write w lambda
    | Apply(createTo, thingToApply, typeArgs, args) ->
      w.Write 19uy
      w.Write(createTo : int)
      w.Write(thingToApply : int)
      List.write w TypeReference.write typeArgs
      NEList.write (fun w reg -> w.Write(reg : int)) w args
    | RaiseNRE error ->
      w.Write 20uy
      NameResolutionError.write w error
    | VarNotFound(targetRegIfSecretOrDB, name) ->
      w.Write 21uy
      w.Write(targetRegIfSecretOrDB : int)
      String.write w name
    | CheckIfFirstExprIsUnit reg ->
      w.Write 22uy
      w.Write(reg : int)

  let read (r : BinaryReader) : Instruction =
    match r.ReadByte() with
    | 0uy ->
      let loadTo = r.ReadInt32()
      let dval = Dval.read r
      LoadVal(loadTo, dval)
    | 1uy ->
      let copyTo = r.ReadInt32()
      let copyFrom = r.ReadInt32()
      CopyVal(copyTo, copyFrom)
    | 2uy ->
      let createTo = r.ReadInt32()
      let lhs = r.ReadInt32()
      let rhs = r.ReadInt32()
      Or(createTo, lhs, rhs)
    | 3uy ->
      let createTo = r.ReadInt32()
      let lhs = r.ReadInt32()
      let rhs = r.ReadInt32()
      And(createTo, lhs, rhs)
    | 4uy ->
      let createTo = r.ReadInt32()
      let segments = List.read r StringSegment.read
      CreateString(createTo, segments)
    | 5uy ->
      let valueReg = r.ReadInt32()
      let pat = LetPattern.read r
      CheckLetPatternAndExtractVars(valueReg, pat)
    | 6uy ->
      let instrsToJump = r.ReadInt32()
      let conditionReg = r.ReadInt32()
      JumpByIfFalse(instrsToJump, conditionReg)
    | 7uy ->
      let instrsToJump = r.ReadInt32()
      JumpBy(instrsToJump)
    | 8uy ->
      let valueReg = r.ReadInt32()
      let pat = MatchPattern.read r
      let failJump = r.ReadInt32()
      CheckMatchPatternAndExtractVars(valueReg, pat, failJump)
    | 9uy ->
      let valueReg = r.ReadInt32()
      MatchUnmatched(valueReg)
    | 10uy ->
      let createTo = r.ReadInt32()
      let first = r.ReadInt32()
      let second = r.ReadInt32()
      let theRest = List.read r (fun r -> r.ReadInt32())
      CreateTuple(createTo, first, second, theRest)
    | 11uy ->
      let createTo = r.ReadInt32()
      let itemsToAdd = List.read r (fun r -> r.ReadInt32())
      CreateList(createTo, itemsToAdd)
    | 12uy ->
      let createTo = r.ReadInt32()
      let entries =
        List.read r (fun r ->
          let key = String.read r
          let reg = r.ReadInt32()
          (key, reg))
      CreateDict(createTo, entries)
    | 13uy ->
      let createTo = r.ReadInt32()
      let typeName = FQTypeName.read r
      let typeArgs = List.read r TypeReference.read
      let fields =
        List.read r (fun r ->
          let field = String.read r
          let reg = r.ReadInt32()
          (field, reg))
      CreateRecord(createTo, typeName, typeArgs, fields)
    | 14uy ->
      let createTo = r.ReadInt32()
      let originalRecordReg = r.ReadInt32()
      let updates =
        List.read r (fun r ->
          let field = String.read r
          let reg = r.ReadInt32()
          (field, reg))
      CloneRecordWithUpdates(createTo, originalRecordReg, updates)
    | 15uy ->
      let targetReg = r.ReadInt32()
      let recordReg = r.ReadInt32()
      let fieldName = String.read r
      GetRecordField(targetReg, recordReg, fieldName)
    | 16uy ->
      let createTo = r.ReadInt32()
      let typeName = FQTypeName.read r
      let typeArgs = List.read r TypeReference.read
      let caseName = String.read r
      let fields = List.read r (fun r -> r.ReadInt32())
      CreateEnum(createTo, typeName, typeArgs, caseName, fields)
    | 17uy ->
      let createTo = r.ReadInt32()
      let valueName = FQValueName.read r
      LoadValue(createTo, valueName)
    | 18uy ->
      let createTo = r.ReadInt32()
      let lambda = LambdaImpl.read r
      CreateLambda(createTo, lambda)
    | 19uy ->
      let createTo = r.ReadInt32()
      let thingToApply = r.ReadInt32()
      let typeArgs = List.read r TypeReference.read
      let args = NEList.read (fun r -> r.ReadInt32()) r
      Apply(createTo, thingToApply, typeArgs, args)
    | 20uy ->
      let error = NameResolutionError.read r
      RaiseNRE(error)
    | 21uy ->
      let targetRegIfSecretOrDB = r.ReadInt32()
      let name = String.read r
      VarNotFound(targetRegIfSecretOrDB, name)
    | 22uy ->
      let reg = r.ReadInt32()
      CheckIfFirstExprIsUnit(reg)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid Instruction tag: {b}"))


let write (w : BinaryWriter) (instrs : Instructions) =
  Varint.write w instrs.registerCount
  List.write w Instruction.write instrs.instructions
  w.Write instrs.resultIn

let read (r : BinaryReader) : Instructions =
  let registerCount = Varint.read r
  let instructions = List.read r Instruction.read
  let resultIn = r.ReadInt32()
  { registerCount = registerCount; instructions = instructions; resultIn = resultIn }
