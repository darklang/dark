module rec LibSerialization.Binary.Serializers.PT.Expr

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary
open BaseFormat
open Serializers.Common
open Serializers.PT.Common


module InfixFnName =
  let write (w : BinaryWriter) (name : InfixFnName) =
    match name with
    | ArithmeticPlus -> w.Write 0uy
    | ArithmeticMinus -> w.Write 1uy
    | ArithmeticMultiply -> w.Write 2uy
    | ArithmeticDivide -> w.Write 3uy
    | ArithmeticModulo -> w.Write 4uy
    | ArithmeticPower -> w.Write 5uy
    | ComparisonGreaterThan -> w.Write 6uy
    | ComparisonGreaterThanOrEqual -> w.Write 7uy
    | ComparisonLessThan -> w.Write 8uy
    | ComparisonLessThanOrEqual -> w.Write 9uy
    | ComparisonEquals -> w.Write 10uy
    | ComparisonNotEquals -> w.Write 11uy
    | StringConcat -> w.Write 12uy

  let read (r : BinaryReader) : InfixFnName =
    match r.ReadByte() with
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


module BinaryOperation =
  let write (w : BinaryWriter) (op : BinaryOperation) =
    match op with
    | BinOpAnd -> w.Write 0uy
    | BinOpOr -> w.Write 1uy

  let read (r : BinaryReader) : BinaryOperation =
    match r.ReadByte() with
    | 0uy -> BinOpAnd
    | 1uy -> BinOpOr
    | b ->
      raise (
        BinaryFormatException(CorruptedData $"Invalid BinaryOperation tag: {b}")
      )


module Infix =
  let write (w : BinaryWriter) (infix : Infix) =
    match infix with
    | InfixFnCall name ->
      w.Write 0uy
      InfixFnName.write w name
    | BinOp op ->
      w.Write 1uy
      BinaryOperation.write w op

  let read (r : BinaryReader) : Infix =
    match r.ReadByte() with
    | 0uy -> InfixFnCall(InfixFnName.read r)
    | 1uy -> BinOp(BinaryOperation.read r)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Infix tag: {b}"))


module LetPattern =
  let rec write (w : BinaryWriter) (pattern : LetPattern) =
    match pattern with
    | LPVariable(id, name) ->
      w.Write 0uy
      w.Write id
      String.write w name
    | LPUnit id ->
      w.Write 1uy
      w.Write id
    | LPTuple(id, first, second, rest) ->
      w.Write 2uy
      w.Write id
      write w first
      write w second
      List.write w write rest

  let rec read (r : BinaryReader) : LetPattern =
    match r.ReadByte() with
    | 0uy ->
      let id = r.ReadUInt64()
      let name = String.read r
      LPVariable(id, name)
    | 1uy ->
      let id = r.ReadUInt64()
      LPUnit id
    | 2uy ->
      let id = r.ReadUInt64()
      let first = read r
      let second = read r
      let rest = List.read r read
      LPTuple(id, first, second, rest)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid LetPattern tag: {b}"))


module MatchPattern =
  let rec write (w : BinaryWriter) (pattern : MatchPattern) =
    match pattern with
    | MPVariable(id, name) ->
      w.Write 0uy
      w.Write id
      String.write w name
    | MPEnum(id, caseName, fieldPats) ->
      w.Write 1uy
      w.Write id
      String.write w caseName
      List.write w write fieldPats
    | MPInt64(id, value) ->
      w.Write 2uy
      w.Write id
      w.Write value
    | MPUInt64(id, value) ->
      w.Write 3uy
      w.Write id
      w.Write value
    | MPInt8(id, value) ->
      w.Write 4uy
      w.Write id
      w.Write value
    | MPUInt8(id, value) ->
      w.Write 5uy
      w.Write id
      w.Write value
    | MPInt16(id, value) ->
      w.Write 6uy
      w.Write id
      w.Write value
    | MPUInt16(id, value) ->
      w.Write 7uy
      w.Write id
      w.Write value
    | MPInt32(id, value) ->
      w.Write 8uy
      w.Write id
      w.Write value
    | MPUInt32(id, value) ->
      w.Write 9uy
      w.Write id
      w.Write value
    | MPInt128(id, value) ->
      w.Write 10uy
      w.Write id
      String.write w (string value)
    | MPUInt128(id, value) ->
      w.Write 11uy
      w.Write id
      String.write w (string value)
    | MPBool(id, value) ->
      w.Write 12uy
      w.Write id
      w.Write value
    | MPChar(id, value) ->
      w.Write 13uy
      w.Write id
      String.write w value
    | MPString(id, value) ->
      w.Write 14uy
      w.Write id
      String.write w value
    | MPFloat(id, sign, whole, fractional) ->
      w.Write 15uy
      w.Write id
      Sign.write w sign
      String.write w whole
      String.write w fractional
    | MPUnit id ->
      w.Write 16uy
      w.Write id
    | MPTuple(id, first, second, rest) ->
      w.Write 17uy
      w.Write id
      write w first
      write w second
      List.write w write rest
    | MPList(id, patterns) ->
      w.Write 18uy
      w.Write id
      List.write w write patterns
    | MPListCons(id, head, tail) ->
      w.Write 19uy
      w.Write id
      write w head
      write w tail
    | MPOr(id, patterns) ->
      w.Write 20uy
      w.Write id
      NEList.write write w patterns

  let rec read (r : BinaryReader) : MatchPattern =
    match r.ReadByte() with
    | 0uy ->
      let id = r.ReadUInt64()
      let name = String.read r
      MPVariable(id, name)
    | 1uy ->
      let id = r.ReadUInt64()
      let caseName = String.read r
      let fieldPats = List.read r read
      MPEnum(id, caseName, fieldPats)
    | 2uy ->
      let id = r.ReadUInt64()
      let value = r.ReadInt64()
      MPInt64(id, value)
    | 3uy ->
      let id = r.ReadUInt64()
      let value = r.ReadUInt64()
      MPUInt64(id, value)
    | 4uy ->
      let id = r.ReadUInt64()
      let value = r.ReadSByte()
      MPInt8(id, value)
    | 5uy ->
      let id = r.ReadUInt64()
      let value = r.ReadByte()
      MPUInt8(id, value)
    | 6uy ->
      let id = r.ReadUInt64()
      let value = r.ReadInt16()
      MPInt16(id, value)
    | 7uy ->
      let id = r.ReadUInt64()
      let value = r.ReadUInt16()
      MPUInt16(id, value)
    | 8uy ->
      let id = r.ReadUInt64()
      let value = r.ReadInt32()
      MPInt32(id, value)
    | 9uy ->
      let id = r.ReadUInt64()
      let value = r.ReadUInt32()
      MPUInt32(id, value)
    | 10uy ->
      let id = r.ReadUInt64()
      let value = String.read r |> System.Int128.Parse
      MPInt128(id, value)
    | 11uy ->
      let id = r.ReadUInt64()
      let value = String.read r |> System.UInt128.Parse
      MPUInt128(id, value)
    | 12uy ->
      let id = r.ReadUInt64()
      let value = r.ReadBoolean()
      MPBool(id, value)
    | 13uy ->
      let id = r.ReadUInt64()
      let value = String.read r
      MPChar(id, value)
    | 14uy ->
      let id = r.ReadUInt64()
      let value = String.read r
      MPString(id, value)
    | 15uy ->
      let id = r.ReadUInt64()
      let sign = Sign.read r
      let whole = String.read r
      let fractional = String.read r
      MPFloat(id, sign, whole, fractional)
    | 16uy ->
      let id = r.ReadUInt64()
      MPUnit id
    | 17uy ->
      let id = r.ReadUInt64()
      let first = read r
      let second = read r
      let rest = List.read r read
      MPTuple(id, first, second, rest)
    | 18uy ->
      let id = r.ReadUInt64()
      let patterns = List.read r read
      MPList(id, patterns)
    | 19uy ->
      let id = r.ReadUInt64()
      let head = read r
      let tail = read r
      MPListCons(id, head, tail)
    | 20uy ->
      let id = r.ReadUInt64()
      let patterns = NEList.read read r
      MPOr(id, patterns)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid MatchPattern tag: {b}"))


module StringSegment =
  let write (w : BinaryWriter) (segment : StringSegment) =
    match segment with
    | StringText text ->
      w.Write 0uy
      String.write w text
    | StringInterpolation expr ->
      w.Write 1uy
      Expr.write w expr

  let read (r : BinaryReader) : StringSegment =
    match r.ReadByte() with
    | 0uy -> StringText(String.read r)
    | 1uy -> StringInterpolation(Expr.read r)
    | b ->
      raise (BinaryFormatException(CorruptedData $"Invalid StringSegment tag: {b}"))


module MatchCase =
  let write (w : BinaryWriter) (case : MatchCase) =
    MatchPattern.write w case.pat
    Option.write w Expr.write case.whenCondition
    Expr.write w case.rhs

  let read (r : BinaryReader) : MatchCase =
    let pat = MatchPattern.read r
    let whenCondition = Option.read r Expr.read
    let rhs = Expr.read r
    { pat = pat; whenCondition = whenCondition; rhs = rhs }


module PipeExpr =
  let write (w : BinaryWriter) (pipeExpr : PipeExpr) =
    match pipeExpr with
    | EPipeVariable(id, name, args) ->
      w.Write 0uy
      w.Write id
      String.write w name
      List.write w Expr.write args
    | EPipeLambda(id, pats, body) ->
      w.Write 1uy
      w.Write id
      NEList.write LetPattern.write w pats
      Expr.write w body
    | EPipeInfix(id, infix, expr) ->
      w.Write 2uy
      w.Write id
      Infix.write w infix
      Expr.write w expr
    | EPipeFnCall(id, fnName, typeArgs, args) ->
      w.Write 3uy
      w.Write id
      NameResolution.write FQFnName.write w fnName
      List.write w TypeReference.write typeArgs
      List.write w Expr.write args
    | EPipeEnum(id, typeName, caseName, fields) ->
      w.Write 4uy
      w.Write id
      NameResolution.write FQTypeName.write w typeName
      String.write w caseName
      List.write w Expr.write fields

  let read (r : BinaryReader) : PipeExpr =
    match r.ReadByte() with
    | 0uy ->
      let id = r.ReadUInt64()
      let name = String.read r
      let args = List.read r Expr.read
      EPipeVariable(id, name, args)
    | 1uy ->
      let id = r.ReadUInt64()
      let pats = NEList.read LetPattern.read r
      let body = Expr.read r
      EPipeLambda(id, pats, body)
    | 2uy ->
      let id = r.ReadUInt64()
      let infix = Infix.read r
      let expr = Expr.read r
      EPipeInfix(id, infix, expr)
    | 3uy ->
      let id = r.ReadUInt64()
      let fnName = NameResolution.read FQFnName.read r
      let typeArgs = List.read r TypeReference.read
      let args = List.read r Expr.read
      EPipeFnCall(id, fnName, typeArgs, args)
    | 4uy ->
      let id = r.ReadUInt64()
      let typeName = NameResolution.read FQTypeName.read r
      let caseName = String.read r
      let fields = List.read r Expr.read
      EPipeEnum(id, typeName, caseName, fields)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid PipeExpr tag: {b}"))


module Expr =
  let rec write (w : BinaryWriter) (expr : Expr) =
    match expr with
    | EInt64(id, value) ->
      w.Write 0uy
      w.Write id
      w.Write value
    | EUInt64(id, value) ->
      w.Write 1uy
      w.Write id
      w.Write value
    | EInt8(id, value) ->
      w.Write 2uy
      w.Write id
      w.Write value
    | EUInt8(id, value) ->
      w.Write 3uy
      w.Write id
      w.Write value
    | EInt16(id, value) ->
      w.Write 4uy
      w.Write id
      w.Write value
    | EUInt16(id, value) ->
      w.Write 5uy
      w.Write id
      w.Write value
    | EInt32(id, value) ->
      w.Write 6uy
      w.Write id
      w.Write value
    | EUInt32(id, value) ->
      w.Write 7uy
      w.Write id
      w.Write value
    | EInt128(id, value) ->
      w.Write 8uy
      w.Write id
      String.write w (string value)
    | EUInt128(id, value) ->
      w.Write 9uy
      w.Write id
      String.write w (string value)
    | EBool(id, value) ->
      w.Write 10uy
      w.Write id
      w.Write value
    | EString(id, segments) ->
      w.Write 11uy
      w.Write id
      List.write w StringSegment.write segments
    | EChar(id, value) ->
      w.Write 12uy
      w.Write id
      String.write w value
    | EFloat(id, sign, whole, fractional) ->
      w.Write 13uy
      w.Write id
      Sign.write w sign
      String.write w whole
      String.write w fractional
    | EUnit id ->
      w.Write 14uy
      w.Write id
    | EValue(id, nameRes) ->
      w.Write 15uy
      w.Write id
      NameResolution.write FQValueName.write w nameRes
    | ELet(id, pattern, rhs, body) ->
      w.Write 16uy
      w.Write id
      LetPattern.write w pattern
      write w rhs
      write w body
    | EIf(id, cond, thenExpr, elseExpr) ->
      w.Write 17uy
      w.Write id
      write w cond
      write w thenExpr
      Option.write w write elseExpr
    | ELambda(id, pats, body) ->
      w.Write 18uy
      w.Write id
      NEList.write LetPattern.write w pats
      write w body
    | ERecordFieldAccess(id, expr, field) ->
      w.Write 19uy
      w.Write id
      write w expr
      String.write w field
    | EVariable(id, name) ->
      w.Write 20uy
      w.Write id
      String.write w name
    | EApply(id, fn, typeArgs, args) ->
      w.Write 21uy
      w.Write id
      write w fn
      List.write w TypeReference.write typeArgs
      NEList.write write w args
    | EList(id, exprs) ->
      w.Write 22uy
      w.Write id
      List.write w write exprs
    | ERecord(id, typeName, typeArgs, fields) ->
      w.Write 23uy
      w.Write id
      NameResolution.write FQTypeName.write w typeName
      List.write w TypeReference.write typeArgs
      List.write
        w
        (fun w (name, expr) ->
          String.write w name
          write w expr)
        fields
    | ERecordUpdate(id, record, updates) ->
      w.Write 24uy
      w.Write id
      write w record
      NEList.write
        (fun w (name, expr) ->
          String.write w name
          write w expr)
        w
        updates
    | EPipe(id, expr, pipes) ->
      w.Write 25uy
      w.Write id
      write w expr
      List.write w PipeExpr.write pipes
    | EEnum(id, typeName, typeArgs, caseName, fields) ->
      w.Write 26uy
      w.Write id
      NameResolution.write FQTypeName.write w typeName
      List.write w TypeReference.write typeArgs
      String.write w caseName
      List.write w write fields
    | EMatch(id, expr, cases) ->
      w.Write 27uy
      w.Write id
      write w expr
      List.write w MatchCase.write cases
    | ETuple(id, first, second, rest) ->
      w.Write 28uy
      w.Write id
      write w first
      write w second
      List.write w write rest
    | EInfix(id, op, left, right) ->
      w.Write 29uy
      w.Write id
      Infix.write w op
      write w left
      write w right
    | EDict(id, pairs) ->
      w.Write 30uy
      w.Write id
      List.write
        w
        (fun w (key, value) ->
          String.write w key
          write w value)
        pairs
    | EFnName(id, nameRes) ->
      w.Write 31uy
      w.Write id
      NameResolution.write FQFnName.write w nameRes
    | EStatement(id, first, next) ->
      w.Write 32uy
      w.Write id
      write w first
      write w next
    | ESelf id ->
      w.Write 33uy
      w.Write id
    | EArg(id, index) ->
      w.Write 34uy
      w.Write id
      w.Write index

  let rec read (r : BinaryReader) : Expr =
    match r.ReadByte() with
    | 0uy ->
      let id = r.ReadUInt64()
      let value = r.ReadInt64()
      EInt64(id, value)
    | 1uy ->
      let id = r.ReadUInt64()
      let value = r.ReadUInt64()
      EUInt64(id, value)
    | 2uy ->
      let id = r.ReadUInt64()
      let value = r.ReadSByte()
      EInt8(id, value)
    | 3uy ->
      let id = r.ReadUInt64()
      let value = r.ReadByte()
      EUInt8(id, value)
    | 4uy ->
      let id = r.ReadUInt64()
      let value = r.ReadInt16()
      EInt16(id, value)
    | 5uy ->
      let id = r.ReadUInt64()
      let value = r.ReadUInt16()
      EUInt16(id, value)
    | 6uy ->
      let id = r.ReadUInt64()
      let value = r.ReadInt32()
      EInt32(id, value)
    | 7uy ->
      let id = r.ReadUInt64()
      let value = r.ReadUInt32()
      EUInt32(id, value)
    | 8uy ->
      let id = r.ReadUInt64()
      let value = String.read r |> System.Int128.Parse
      EInt128(id, value)
    | 9uy ->
      let id = r.ReadUInt64()
      let value = String.read r |> System.UInt128.Parse
      EUInt128(id, value)
    | 10uy ->
      let id = r.ReadUInt64()
      let value = r.ReadBoolean()
      EBool(id, value)
    | 11uy ->
      let id = r.ReadUInt64()
      let segments = List.read r StringSegment.read
      EString(id, segments)
    | 12uy ->
      let id = r.ReadUInt64()
      let value = String.read r
      EChar(id, value)
    | 13uy ->
      let id = r.ReadUInt64()
      let sign = Sign.read r
      let whole = String.read r
      let fractional = String.read r
      EFloat(id, sign, whole, fractional)
    | 14uy ->
      let id = r.ReadUInt64()
      EUnit id
    | 15uy ->
      let id = r.ReadUInt64()
      let nameRes = NameResolution.read FQValueName.read r
      EValue(id, nameRes)
    | 16uy ->
      let id = r.ReadUInt64()
      let pattern = LetPattern.read r
      let rhs = read r
      let body = read r
      ELet(id, pattern, rhs, body)
    | 17uy ->
      let id = r.ReadUInt64()
      let cond = read r
      let thenExpr = read r
      let elseExpr = Option.read r read
      EIf(id, cond, thenExpr, elseExpr)
    | 18uy ->
      let id = r.ReadUInt64()
      let pats = NEList.read LetPattern.read r
      let body = read r
      ELambda(id, pats, body)
    | 19uy ->
      let id = r.ReadUInt64()
      let expr = read r
      let field = String.read r
      ERecordFieldAccess(id, expr, field)
    | 20uy ->
      let id = r.ReadUInt64()
      let name = String.read r
      EVariable(id, name)
    | 21uy ->
      let id = r.ReadUInt64()
      let fn = read r
      let typeArgs = List.read r TypeReference.read
      let args = NEList.read read r
      EApply(id, fn, typeArgs, args)
    | 22uy ->
      let id = r.ReadUInt64()
      let exprs = List.read r read
      EList(id, exprs)
    | 23uy ->
      let id = r.ReadUInt64()
      let typeName = NameResolution.read FQTypeName.read r
      let typeArgs = List.read r TypeReference.read
      let fields =
        List.read r (fun r ->
          let name = String.read r
          let expr = read r
          (name, expr))
      ERecord(id, typeName, typeArgs, fields)
    | 24uy ->
      let id = r.ReadUInt64()
      let record = read r
      let updates =
        NEList.read
          (fun r ->
            let name = String.read r
            let expr = read r
            (name, expr))
          r
      ERecordUpdate(id, record, updates)
    | 25uy ->
      let id = r.ReadUInt64()
      let expr = read r
      let pipes = List.read r PipeExpr.read
      EPipe(id, expr, pipes)
    | 26uy ->
      let id = r.ReadUInt64()
      let typeName = NameResolution.read FQTypeName.read r
      let typeArgs = List.read r TypeReference.read
      let caseName = String.read r
      let fields = List.read r read
      EEnum(id, typeName, typeArgs, caseName, fields)
    | 27uy ->
      let id = r.ReadUInt64()
      let expr = read r
      let cases = List.read r MatchCase.read
      EMatch(id, expr, cases)
    | 28uy ->
      let id = r.ReadUInt64()
      let first = read r
      let second = read r
      let rest = List.read r read
      ETuple(id, first, second, rest)
    | 29uy ->
      let id = r.ReadUInt64()
      let op = Infix.read r
      let left = read r
      let right = read r
      EInfix(id, op, left, right)
    | 30uy ->
      let id = r.ReadUInt64()
      let pairs =
        List.read r (fun r ->
          let key = String.read r
          let value = read r
          (key, value))
      EDict(id, pairs)
    | 31uy ->
      let id = r.ReadUInt64()
      let nameRes = NameResolution.read FQFnName.read r
      EFnName(id, nameRes)
    | 32uy ->
      let id = r.ReadUInt64()
      let first = read r
      let next = read r
      EStatement(id, first, next)
    | 33uy ->
      let id = r.ReadUInt64()
      ESelf id
    | 34uy ->
      let id = r.ReadUInt64()
      let index = r.ReadInt32()
      EArg(id, index)
    | b -> raise (BinaryFormatException(CorruptedData $"Invalid Expr tag: {b}"))
