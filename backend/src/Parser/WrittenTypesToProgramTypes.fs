/// Conversion functions from WrittenTypes to ProgramTypes
module Parser.WrittenTypesToProgramTypes

open Prelude
open Tablecloth

module WT = WrittenTypes
module PT = LibExecution.ProgramTypes

module InfixFnName =
  let toWT (name : PT.InfixFnName) : WT.InfixFnName =
    match name with
    | PT.ArithmeticPlus -> WT.ArithmeticPlus
    | PT.ArithmeticMinus -> WT.ArithmeticMinus
    | PT.ArithmeticMultiply -> WT.ArithmeticMultiply
    | PT.ArithmeticDivide -> WT.ArithmeticDivide
    | PT.ArithmeticModulo -> WT.ArithmeticModulo
    | PT.ArithmeticPower -> WT.ArithmeticPower
    | PT.ComparisonGreaterThan -> WT.ComparisonGreaterThan
    | PT.ComparisonGreaterThanOrEqual -> WT.ComparisonGreaterThanOrEqual
    | PT.ComparisonLessThan -> WT.ComparisonLessThan
    | PT.ComparisonLessThanOrEqual -> WT.ComparisonLessThanOrEqual
    | PT.ComparisonEquals -> WT.ComparisonEquals
    | PT.ComparisonNotEquals -> WT.ComparisonNotEquals
    | PT.StringConcat -> WT.StringConcat

  let toPT (name : WT.InfixFnName) : PT.InfixFnName =
    match name with
    | WT.ArithmeticPlus -> PT.ArithmeticPlus
    | WT.ArithmeticMinus -> PT.ArithmeticMinus
    | WT.ArithmeticMultiply -> PT.ArithmeticMultiply
    | WT.ArithmeticDivide -> PT.ArithmeticDivide
    | WT.ArithmeticModulo -> PT.ArithmeticModulo
    | WT.ArithmeticPower -> PT.ArithmeticPower
    | WT.ComparisonGreaterThan -> PT.ComparisonGreaterThan
    | WT.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
    | WT.ComparisonLessThan -> PT.ComparisonLessThan
    | WT.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
    | WT.ComparisonEquals -> PT.ComparisonEquals
    | WT.ComparisonNotEquals -> PT.ComparisonNotEquals
    | WT.StringConcat -> PT.StringConcat

module TypeReference =
  let rec toWT (t : PT.TypeReference) : WT.TypeReference =
    match t with
    | PT.TInt -> WT.TInt
    | PT.TFloat -> WT.TFloat
    | PT.TBool -> WT.TBool
    | PT.TUnit -> WT.TUnit
    | PT.TString -> WT.TString
    | PT.TList typ -> WT.TList(toWT typ)
    | PT.TTuple(first, second, theRest) ->
      WT.TTuple(toWT first, toWT second, List.map toWT theRest)
    | PT.TDict typ -> WT.TDict(toWT typ)
    | PT.TDB typ -> WT.TDB(toWT typ)
    | PT.TDateTime -> WT.TDateTime
    | PT.TChar -> WT.TChar
    | PT.TPassword -> WT.TPassword
    | PT.TUuid -> WT.TUuid
    | PT.TCustomType(t, typeArgs) -> WT.TCustomType(t, List.map toWT typeArgs)
    | PT.TBytes -> WT.TBytes
    | PT.TVariable(name) -> WT.TVariable(name)
    | PT.TFn(paramTypes, returnType) ->
      WT.TFn(List.map toWT paramTypes, toWT returnType)

  let rec toPT (t : WT.TypeReference) : PT.TypeReference =
    match t with
    | WT.TInt -> PT.TInt
    | WT.TFloat -> PT.TFloat
    | WT.TBool -> PT.TBool
    | WT.TUnit -> PT.TUnit
    | WT.TString -> PT.TString
    | WT.TList typ -> PT.TList(toPT typ)
    | WT.TTuple(firstType, secondType, otherTypes) ->
      PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
    | WT.TDict typ -> PT.TDict(toPT typ)
    | WT.TDB typ -> PT.TDB(toPT typ)
    | WT.TDateTime -> PT.TDateTime
    | WT.TChar -> PT.TChar
    | WT.TPassword -> PT.TPassword
    | WT.TUuid -> PT.TUuid
    | WT.TCustomType(t, typeArgs) -> PT.TCustomType(t, List.map toPT typeArgs)
    | WT.TBytes -> PT.TBytes
    | WT.TVariable(name) -> PT.TVariable(name)
    | WT.TFn(paramTypes, returnType) ->
      PT.TFn(List.map toPT paramTypes, toPT returnType)

module BinaryOperation =
  let toWT (op : PT.BinaryOperation) : WT.BinaryOperation =
    match op with
    | PT.BinOpAnd -> WT.BinOpAnd
    | PT.BinOpOr -> WT.BinOpOr

  let toPT (binop : WT.BinaryOperation) : PT.BinaryOperation =
    match binop with
    | WT.BinOpAnd -> PT.BinOpAnd
    | WT.BinOpOr -> PT.BinOpOr

module Infix =
  let toPT (infix : WT.Infix) : PT.Infix =
    match infix with
    | WT.InfixFnCall(fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
    | WT.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

module LetPattern =
  let rec toWT (p : PT.LetPattern) : WT.LetPattern =
    match p with
    | PT.LPVariable(id, str) -> WT.LPVariable(id, str)
    | PT.LPTuple(id, first, second, theRest) ->
      WT.LPTuple(id, toWT first, toWT second, List.map toWT theRest)

  let rec toPT (p : WT.LetPattern) : PT.LetPattern =
    match p with
    | WT.LPVariable(id, str) -> PT.LPVariable(id, str)
    | WT.LPTuple(id, first, second, theRest) ->
      PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)

module MatchPattern =
  let rec toWT (p : PT.MatchPattern) : WT.MatchPattern =
    match p with
    | PT.MPVariable(id, str) -> WT.MPVariable(id, str)
    | PT.MPEnum(id, caseName, fieldPats) ->
      WT.MPEnum(id, caseName, List.map toWT fieldPats)
    | PT.MPInt(id, i) -> WT.MPInt(id, i)
    | PT.MPBool(id, b) -> WT.MPBool(id, b)
    | PT.MPChar(id, c) -> WT.MPChar(id, c)
    | PT.MPString(id, s) -> WT.MPString(id, s)
    | PT.MPFloat(id, s, w, f) -> WT.MPFloat(id, s, w, f)
    | PT.MPUnit id -> WT.MPUnit id
    | PT.MPTuple(id, first, second, theRest) ->
      WT.MPTuple(id, toWT first, toWT second, List.map toWT theRest)
    | PT.MPList(id, pats) -> WT.MPList(id, List.map toWT pats)
    | PT.MPListCons(id, head, tail) -> WT.MPListCons(id, toWT head, toWT tail)


  let rec toPT (p : WT.MatchPattern) : PT.MatchPattern =
    match p with
    | WT.MPVariable(id, str) -> PT.MPVariable(id, str)
    | WT.MPEnum(id, caseName, fieldPats) ->
      PT.MPEnum(id, caseName, List.map toPT fieldPats)
    | WT.MPInt(id, i) -> PT.MPInt(id, i)
    | WT.MPBool(id, b) -> PT.MPBool(id, b)
    | WT.MPChar(id, c) -> PT.MPChar(id, c)
    | WT.MPString(id, s) -> PT.MPString(id, s)
    | WT.MPFloat(id, s, w, f) -> PT.MPFloat(id, s, w, f)
    | WT.MPUnit id -> PT.MPUnit id
    | WT.MPTuple(id, first, second, theRest) ->
      PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)
    | WT.MPList(id, pats) -> PT.MPList(id, List.map toPT pats)
    | WT.MPListCons(id, head, tail) -> PT.MPListCons(id, toPT head, toPT tail)


module Expr =
  let rec toWT (e : PT.Expr) : WT.Expr =
    match e with
    | PT.EChar(id, char) -> WT.EChar(id, char)
    | PT.EInt(id, num) -> WT.EInt(id, num)
    | PT.EString(id, segments) -> WT.EString(id, List.map stringSegmentToST segments)
    | PT.EFloat(id, sign, whole, fraction) -> WT.EFloat(id, sign, whole, fraction)
    | PT.EBool(id, b) -> WT.EBool(id, b)
    | PT.EUnit id -> WT.EUnit id
    | PT.EVariable(id, var) -> WT.EVariable(id, var)
    | PT.EFieldAccess(id, obj, fieldname) -> WT.EFieldAccess(id, toWT obj, fieldname)
    | PT.EApply(id, PT.FnTargetName name, typeArgs, args) ->
      WT.EApply(
        id,
        WT.FnTargetName(name),
        List.map TypeReference.toWT typeArgs,
        List.map toWT args
      )
    | PT.EApply(id, PT.FnTargetExpr name, typeArgs, args) ->
      WT.EApply(
        id,
        WT.FnTargetExpr(toWT name),
        List.map TypeReference.toWT typeArgs,
        List.map toWT args
      )
    | PT.EInfix(id, PT.InfixFnCall name, arg1, arg2) ->
      WT.EInfix(id, WT.InfixFnCall(InfixFnName.toWT name), toWT arg1, toWT arg2)
    | PT.EInfix(id, PT.BinOp(op), arg1, arg2) ->
      WT.EInfix(id, WT.BinOp(BinaryOperation.toWT (op)), toWT arg1, toWT arg2)
    | PT.ELambda(id, vars, body) -> WT.ELambda(id, vars, toWT body)
    | PT.ELet(id, pat, rhs, body) ->
      WT.ELet(id, LetPattern.toWT pat, toWT rhs, toWT body)
    | PT.EIf(id, cond, thenExpr, elseExpr) ->
      WT.EIf(id, toWT cond, toWT thenExpr, toWT elseExpr)
    | PT.EList(id, exprs) -> WT.EList(id, List.map toWT exprs)
    | PT.ETuple(id, first, second, theRest) ->
      WT.ETuple(id, toWT first, toWT second, List.map toWT theRest)
    | PT.ERecord(id, typeName, fields) ->
      WT.ERecord(id, typeName, List.map (Tuple2.mapSecond toWT) fields)
    | PT.ERecordUpdate(id, record, updates) ->
      WT.ERecordUpdate(
        id,
        toWT record,
        updates |> List.map (fun (name, expr) -> (name, toWT expr))
      )
    | PT.EPipe(pipeID, expr1, expr2, rest) ->
      WT.EPipe(pipeID, toWT expr1, pipeExprToST expr2, List.map pipeExprToST rest)
    | PT.EEnum(id, typeName, caseName, fields) ->
      WT.EEnum(id, typeName, caseName, List.map toWT fields)
    | PT.EMatch(id, mexpr, cases) ->
      WT.EMatch(
        id,
        toWT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toWT << Tuple2.mapSecond toWT) cases
      )
    | PT.EDict(id, fields) -> WT.EDict(id, List.map (Tuple2.mapSecond toWT) fields)

  and stringSegmentToST (segment : PT.StringSegment) : WT.StringSegment =
    match segment with
    | PT.StringText text -> WT.StringText text
    | PT.StringInterpolation expr -> WT.StringInterpolation(toWT expr)

  and pipeExprToST (pipeExpr : PT.PipeExpr) : WT.PipeExpr =
    match pipeExpr with
    | PT.EPipeVariable(id, name) -> WT.EPipeVariable(id, name)
    | PT.EPipeLambda(id, args, body) -> WT.EPipeLambda(id, args, toWT body)
    | PT.EPipeInfix(id, PT.InfixFnCall name, first) ->
      WT.EPipeInfix(id, WT.InfixFnCall(InfixFnName.toWT name), toWT first)
    | PT.EPipeInfix(id, PT.BinOp(op), first) ->
      WT.EPipeInfix(id, WT.BinOp(BinaryOperation.toWT (op)), toWT first)
    | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
      WT.EPipeFnCall(
        id,
        fnName,
        List.map TypeReference.toWT typeArgs,
        List.map toWT args
      )
    | PT.EPipeEnum(id, typeName, caseName, fields) ->
      WT.EPipeEnum(id, typeName, caseName, List.map toWT fields)

  let rec toPT (e : WT.Expr) : PT.Expr =
    match e with
    | WT.EChar(id, char) -> PT.EChar(id, char)
    | WT.EInt(id, num) -> PT.EInt(id, num)
    | WT.EString(id, segment) -> PT.EString(id, List.map stringSegmentToPT segment)
    | WT.EFloat(id, sign, whole, fraction) -> PT.EFloat(id, sign, whole, fraction)
    | WT.EBool(id, b) -> PT.EBool(id, b)
    | WT.EUnit id -> PT.EUnit id
    | WT.EVariable(id, var) -> PT.EVariable(id, var)
    | WT.EFieldAccess(id, obj, fieldname) -> PT.EFieldAccess(id, toPT obj, fieldname)
    | WT.EApply(id, WT.FnTargetName name, typeArgs, args) ->
      PT.EApply(
        id,
        PT.FnTargetName(name),
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | WT.EApply(id, WT.FnTargetExpr name, typeArgs, args) ->
      PT.EApply(
        id,
        PT.FnTargetExpr(toPT name),
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | WT.ELambda(id, vars, body) -> PT.ELambda(id, vars, toPT body)
    | WT.ELet(id, pat, rhs, body) ->
      PT.ELet(id, LetPattern.toPT pat, toPT rhs, toPT body)
    | WT.EIf(id, cond, thenExpr, elseExpr) ->
      PT.EIf(id, toPT cond, toPT thenExpr, toPT elseExpr)
    | WT.EList(id, exprs) -> PT.EList(id, List.map toPT exprs)
    | WT.ETuple(id, first, second, theRest) ->
      PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
    | WT.ERecord(id, typeName, fields) ->
      PT.ERecord(id, typeName, List.map (Tuple2.mapSecond toPT) fields)
    | WT.ERecordUpdate(id, record, updates) ->
      PT.ERecordUpdate(
        id,
        toPT record,
        updates |> List.map (fun (name, expr) -> (name, toPT expr))
      )
    | WT.EPipe(pipeID, expr1, expr2, rest) ->
      PT.EPipe(pipeID, toPT expr1, pipeExprToPT expr2, List.map pipeExprToPT rest)
    | WT.EEnum(id, typeName, caseName, exprs) ->
      PT.EEnum(id, typeName, caseName, List.map toPT exprs)
    | WT.EMatch(id, mexpr, pairs) ->
      PT.EMatch(
        id,
        toPT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toPT << Tuple2.mapSecond toPT) pairs
      )
    | WT.EInfix(id, infix, arg1, arg2) ->
      PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)
    | WT.EDict(id, pairs) -> PT.EDict(id, List.map (Tuple2.mapSecond toPT) pairs)

  and stringSegmentToPT (segment : WT.StringSegment) : PT.StringSegment =
    match segment with
    | WT.StringText text -> PT.StringText text
    | WT.StringInterpolation expr -> PT.StringInterpolation(toPT expr)

  and pipeExprToPT (pipeExpr : WT.PipeExpr) : PT.PipeExpr =
    match pipeExpr with
    | WT.EPipeVariable(id, name) -> PT.EPipeVariable(id, name)
    | WT.EPipeLambda(id, args, body) -> PT.EPipeLambda(id, args, toPT body)
    | WT.EPipeInfix(id, infix, first) ->
      PT.EPipeInfix(id, Infix.toPT infix, toPT first)
    | WT.EPipeFnCall(id, fnName, typeArgs, args) ->
      PT.EPipeFnCall(
        id,
        fnName,
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | WT.EPipeEnum(id, typeName, caseName, fields) ->
      PT.EPipeEnum(id, typeName, caseName, List.map toPT fields)

module Deprecation =
  type Deprecation<'name> =
    | NotDeprecated
    | RenamedTo of 'name
    | ReplacedBy of 'name
    | DeprecatedBecause of string

  let toWT
    (f : 'name1 -> 'name2)
    (d : PT.Deprecation<'name1>)
    : WT.Deprecation<'name2> =
    match d with
    | PT.DeprecatedBecause str -> WT.DeprecatedBecause str
    | PT.RenamedTo name -> WT.RenamedTo(f name)
    | PT.ReplacedBy name -> WT.ReplacedBy(f name)
    | PT.NotDeprecated -> WT.NotDeprecated

  let toPT
    (f : 'name1 -> 'name2)
    (d : WT.Deprecation<'name1>)
    : PT.Deprecation<'name2> =
    match d with
    | WT.NotDeprecated -> PT.NotDeprecated
    | WT.RenamedTo name -> PT.RenamedTo(f name)
    | WT.ReplacedBy name -> PT.ReplacedBy(f name)
    | WT.DeprecatedBecause reason -> PT.DeprecatedBecause reason



module TypeDeclaration =
  module RecordField =
    let toWT (f : PT.TypeDeclaration.RecordField) : WT.TypeDeclaration.RecordField =
      { name = f.name; typ = TypeReference.toWT f.typ; description = f.description }

    let toPT (f : WT.TypeDeclaration.RecordField) : PT.TypeDeclaration.RecordField =
      { name = f.name; typ = TypeReference.toPT f.typ; description = f.description }

  module EnumField =
    let toWT (f : PT.TypeDeclaration.EnumField) : WT.TypeDeclaration.EnumField =
      { typ = TypeReference.toWT f.typ
        label = f.label
        description = f.description }

    let toPT (f : WT.TypeDeclaration.EnumField) : PT.TypeDeclaration.EnumField =
      { typ = TypeReference.toPT f.typ
        label = f.label
        description = f.description }

  module EnumCase =
    let toWT (c : PT.TypeDeclaration.EnumCase) : WT.TypeDeclaration.EnumCase =
      { name = c.name
        fields = List.map EnumField.toWT c.fields
        description = c.description }

    let toPT (c : WT.TypeDeclaration.EnumCase) : PT.TypeDeclaration.EnumCase =
      { name = c.name
        fields = List.map EnumField.toPT c.fields
        description = c.description }

  module Definition =
    let toWT (d : PT.TypeDeclaration.Definition) : WT.TypeDeclaration.Definition =
      match d with
      | PT.TypeDeclaration.Alias typ ->
        WT.TypeDeclaration.Alias(TypeReference.toWT typ)
      | PT.TypeDeclaration.Record(firstField, additionalFields) ->
        WT.TypeDeclaration.Record(
          RecordField.toWT firstField,
          List.map RecordField.toWT additionalFields
        )
      | PT.TypeDeclaration.Enum(firstCase, additionalCases) ->
        WT.TypeDeclaration.Enum(
          EnumCase.toWT firstCase,
          List.map EnumCase.toWT additionalCases
        )

    let toPT (d : WT.TypeDeclaration.Definition) : PT.TypeDeclaration.Definition =
      match d with
      | WT.TypeDeclaration.Alias typ ->
        PT.TypeDeclaration.Alias(TypeReference.toPT typ)
      | WT.TypeDeclaration.Record(firstField, additionalFields) ->
        PT.TypeDeclaration.Record(
          RecordField.toPT firstField,
          List.map RecordField.toPT additionalFields
        )
      | WT.TypeDeclaration.Enum(firstCase, additionalCases) ->
        PT.TypeDeclaration.Enum(
          EnumCase.toPT firstCase,
          List.map EnumCase.toPT additionalCases
        )

  let toWT (d : PT.TypeDeclaration.T) : WT.TypeDeclaration.T =
    { typeParams = d.typeParams; definition = Definition.toWT d.definition }

  let toPT (d : WT.TypeDeclaration.T) : PT.TypeDeclaration.T =
    { typeParams = d.typeParams; definition = Definition.toPT d.definition }


module Handler =
  module CronInterval =
    let toWT (ci : PT.Handler.CronInterval) : WT.Handler.CronInterval =
      match ci with
      | PT.Handler.EveryDay -> WT.Handler.EveryDay
      | PT.Handler.EveryWeek -> WT.Handler.EveryWeek
      | PT.Handler.EveryFortnight -> WT.Handler.EveryFortnight
      | PT.Handler.EveryHour -> WT.Handler.EveryHour
      | PT.Handler.Every12Hours -> WT.Handler.Every12Hours
      | PT.Handler.EveryMinute -> WT.Handler.EveryMinute

    let toPT (ci : WT.Handler.CronInterval) : PT.Handler.CronInterval =
      match ci with
      | WT.Handler.EveryDay -> PT.Handler.EveryDay
      | WT.Handler.EveryWeek -> PT.Handler.EveryWeek
      | WT.Handler.EveryFortnight -> PT.Handler.EveryFortnight
      | WT.Handler.EveryHour -> PT.Handler.EveryHour
      | WT.Handler.Every12Hours -> PT.Handler.Every12Hours
      | WT.Handler.EveryMinute -> PT.Handler.EveryMinute

  module Spec =
    let toWT (s : PT.Handler.Spec) : WT.Handler.Spec =
      match s with
      | PT.Handler.HTTP(route, method) -> WT.Handler.HTTP(route, method)
      | PT.Handler.Worker name -> WT.Handler.Worker name
      | PT.Handler.Cron(name, interval) ->
        WT.Handler.Cron(name, CronInterval.toWT interval)
      | PT.Handler.REPL name -> WT.Handler.REPL name

    let toPT (s : WT.Handler.Spec) : PT.Handler.Spec =
      match s with
      | WT.Handler.HTTP(route, method) -> PT.Handler.HTTP(route, method)
      | WT.Handler.Worker name -> PT.Handler.Worker name
      | WT.Handler.Cron(name, interval) ->
        PT.Handler.Cron(name, CronInterval.toPT interval)
      | WT.Handler.REPL name -> PT.Handler.REPL name

  let toWT (h : PT.Handler.T) : WT.Handler.T =
    { tlid = h.tlid; ast = Expr.toWT h.ast; spec = Spec.toWT h.spec }

  let toPT (h : WT.Handler.T) : PT.Handler.T =
    { tlid = h.tlid; ast = Expr.toPT h.ast; spec = Spec.toPT h.spec }

module DB =
  let toWT (db : PT.DB.T) : WT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toWT db.typ }

  let toPT (db : WT.DB.T) : PT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toPT db.typ }

module UserType =
  let toWT (t : PT.UserType.T) : WT.UserType.T =
    { tlid = t.tlid
      name = t.name
      declaration = TypeDeclaration.toWT t.declaration
      description = t.description
      deprecated = Deprecation.toWT identity t.deprecated }

  let toPT (t : WT.UserType.T) : PT.UserType.T =
    { tlid = t.tlid
      name = t.name
      declaration = TypeDeclaration.toPT t.declaration
      description = t.description
      deprecated = Deprecation.toPT identity t.deprecated }


module UserFunction =
  module Parameter =
    let toWT (p : PT.UserFunction.Parameter) : WT.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toWT p.typ; description = p.description }

    let toPT (p : WT.UserFunction.Parameter) : PT.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toPT p.typ; description = p.description }

  let toWT (f : PT.UserFunction.T) : WT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toWT f.parameters
      returnType = TypeReference.toWT f.returnType
      description = f.description
      deprecated = Deprecation.toWT identity f.deprecated
      body = Expr.toWT f.body }

  let toPT (f : WT.UserFunction.T) : PT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toPT f.parameters
      returnType = TypeReference.toPT f.returnType
      description = f.description
      deprecated = Deprecation.toPT identity f.deprecated
      body = Expr.toPT f.body }

module Toplevel =
  let toWT (tl : PT.Toplevel.T) : WT.Toplevel.T =
    match tl with
    | PT.Toplevel.TLHandler h -> WT.Toplevel.TLHandler(Handler.toWT h)
    | PT.Toplevel.TLDB db -> WT.Toplevel.TLDB(DB.toWT db)
    | PT.Toplevel.TLFunction f -> WT.Toplevel.TLFunction(UserFunction.toWT f)
    | PT.Toplevel.TLType ut -> WT.Toplevel.TLType(UserType.toWT ut)

  let toPT (tl : WT.Toplevel.T) : PT.Toplevel.T =
    match tl with
    | WT.Toplevel.TLHandler h -> PT.Toplevel.TLHandler(Handler.toPT h)
    | WT.Toplevel.TLDB db -> PT.Toplevel.TLDB(DB.toPT db)
    | WT.Toplevel.TLFunction f -> PT.Toplevel.TLFunction(UserFunction.toPT f)
    | WT.Toplevel.TLType ut -> PT.Toplevel.TLType(UserType.toPT ut)

module PackageFn =
  module Parameter =
    let toWT (p : PT.PackageFn.Parameter) : WT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toWT p.typ; description = p.description }

    let toPT (p : WT.PackageFn.Parameter) : PT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toPT p.typ; description = p.description }

  let toWT (fn : PT.PackageFn.T) : WT.PackageFn.T =
    { name = fn.name
      parameters = List.map Parameter.toWT fn.parameters
      returnType = TypeReference.toWT fn.returnType
      description = fn.description
      deprecated = Deprecation.toWT identity fn.deprecated
      body = Expr.toWT fn.body
      typeParams = fn.typeParams
      id = fn.id
      tlid = fn.tlid }

  let toPT (fn : WT.PackageFn.T) : PT.PackageFn.T =
    { name = fn.name
      parameters = List.map Parameter.toPT fn.parameters
      returnType = TypeReference.toPT fn.returnType
      description = fn.description
      deprecated = Deprecation.toPT identity fn.deprecated
      body = Expr.toPT fn.body
      typeParams = fn.typeParams
      id = fn.id
      tlid = fn.tlid }

module PackageType =
  let toWT (pt : PT.PackageType.T) : WT.PackageType.T =
    { name = pt.name
      description = pt.description
      declaration = TypeDeclaration.toWT pt.declaration
      deprecated = Deprecation.toWT identity pt.deprecated
      id = pt.id
      tlid = pt.tlid }

  let toPT (pt : WT.PackageType.T) : PT.PackageType.T =
    { name = pt.name
      description = pt.description
      declaration = TypeDeclaration.toPT pt.declaration
      deprecated = Deprecation.toPT identity pt.deprecated
      id = pt.id
      tlid = pt.tlid }
