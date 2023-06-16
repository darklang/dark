/// Conversion functions from ProgramTypes to SerializedTypes and back
module LibBinarySerialization.ProgramTypesToSerializedTypes

open Prelude
open Tablecloth

// Used for conversion functions
module ST = SerializedTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

module FQTypeName =
  module StdlibTypeName =
    let toST (s : PT.FQTypeName.StdlibTypeName) : ST.FQTypeName.StdlibTypeName =
      { modules = s.modules; typ = s.typ; version = s.version }

    let toPT (s : ST.FQTypeName.StdlibTypeName) : PT.FQTypeName.StdlibTypeName =
      { modules = s.modules; typ = s.typ; version = s.version }

  module UserTypeName =
    let toST (u : PT.FQTypeName.UserTypeName) : ST.FQTypeName.UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }

    let toPT (u : ST.FQTypeName.UserTypeName) : PT.FQTypeName.UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }

  module PackageTypeName =
    let toST (p : PT.FQTypeName.PackageTypeName) : ST.FQTypeName.PackageTypeName =
      { owner = p.owner
        modules = { head = p.modules.Head; tail = p.modules.Tail }
        typ = p.typ
        version = p.version }

    let toPT (p : ST.FQTypeName.PackageTypeName) : PT.FQTypeName.PackageTypeName =
      { owner = p.owner
        modules = { Head = p.modules.head; Tail = p.modules.tail }
        typ = p.typ
        version = p.version }


  let toST (t : PT.FQTypeName.T) : ST.FQTypeName.T =
    match t with
    | PT.FQTypeName.Stdlib s -> ST.FQTypeName.Stdlib(StdlibTypeName.toST s)
    | PT.FQTypeName.User u -> ST.FQTypeName.User(UserTypeName.toST u)
    | PT.FQTypeName.Package p -> ST.FQTypeName.Package(PackageTypeName.toST p)

  let toPT (t : ST.FQTypeName.T) : PT.FQTypeName.T =
    match t with
    | ST.FQTypeName.Stdlib s -> PT.FQTypeName.Stdlib(StdlibTypeName.toPT s)
    | ST.FQTypeName.User u -> PT.FQTypeName.User(UserTypeName.toPT u)
    | ST.FQTypeName.Package p -> PT.FQTypeName.Package(PackageTypeName.toPT p)

module FQFnName =
  module PackageFnName =
    let toST (name : PT.FQFnName.PackageFnName) : ST.FQFnName.PackageFnName =
      { owner = name.owner
        modules = { head = name.modules.Head; tail = name.modules.Tail }
        function_ = name.function_
        version = name.version }

    let toPT (name : ST.FQFnName.PackageFnName) : PT.FQFnName.PackageFnName =
      { owner = name.owner
        modules = { Head = name.modules.head; Tail = name.modules.tail }
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    let toST (name : PT.FQFnName.StdlibFnName) : ST.FQFnName.StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

    let toPT (name : ST.FQFnName.StdlibFnName) : PT.FQFnName.StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

  module UserFnName =
    let toST (name : PT.FQFnName.UserFnName) : ST.FQFnName.UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

    let toPT (name : ST.FQFnName.UserFnName) : PT.FQFnName.UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

  let toST (fqfn : PT.FQFnName.T) : ST.FQFnName.T =
    match fqfn with
    | PT.FQFnName.User u -> ST.FQFnName.User(UserFnName.toST u)
    | PT.FQFnName.Stdlib fn -> ST.FQFnName.Stdlib(StdlibFnName.toST fn)
    | PT.FQFnName.Package p -> ST.FQFnName.Package(PackageFnName.toST p)

  let toPT (fqfn : ST.FQFnName.T) : PT.FQFnName.T =
    match fqfn with
    | ST.FQFnName.Stdlib fn -> PT.FQFnName.Stdlib(StdlibFnName.toPT fn)
    | ST.FQFnName.User fn -> PT.FQFnName.User(UserFnName.toPT fn)
    | ST.FQFnName.Package p -> PT.FQFnName.Package(PackageFnName.toPT p)

module InfixFnName =
  let toST (name : PT.InfixFnName) : ST.InfixFnName =
    match name with
    | PT.ArithmeticPlus -> ST.ArithmeticPlus
    | PT.ArithmeticMinus -> ST.ArithmeticMinus
    | PT.ArithmeticMultiply -> ST.ArithmeticMultiply
    | PT.ArithmeticDivide -> ST.ArithmeticDivide
    | PT.ArithmeticModulo -> ST.ArithmeticModulo
    | PT.ArithmeticPower -> ST.ArithmeticPower
    | PT.ComparisonGreaterThan -> ST.ComparisonGreaterThan
    | PT.ComparisonGreaterThanOrEqual -> ST.ComparisonGreaterThanOrEqual
    | PT.ComparisonLessThan -> ST.ComparisonLessThan
    | PT.ComparisonLessThanOrEqual -> ST.ComparisonLessThanOrEqual
    | PT.ComparisonEquals -> ST.ComparisonEquals
    | PT.ComparisonNotEquals -> ST.ComparisonNotEquals
    | PT.StringConcat -> ST.StringConcat

  let toPT (name : ST.InfixFnName) : PT.InfixFnName =
    match name with
    | ST.ArithmeticPlus -> PT.ArithmeticPlus
    | ST.ArithmeticMinus -> PT.ArithmeticMinus
    | ST.ArithmeticMultiply -> PT.ArithmeticMultiply
    | ST.ArithmeticDivide -> PT.ArithmeticDivide
    | ST.ArithmeticModulo -> PT.ArithmeticModulo
    | ST.ArithmeticPower -> PT.ArithmeticPower
    | ST.ComparisonGreaterThan -> PT.ComparisonGreaterThan
    | ST.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
    | ST.ComparisonLessThan -> PT.ComparisonLessThan
    | ST.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
    | ST.ComparisonEquals -> PT.ComparisonEquals
    | ST.ComparisonNotEquals -> PT.ComparisonNotEquals
    | ST.StringConcat -> PT.StringConcat

module TypeReference =
  let rec toST (t : PT.TypeReference) : ST.TypeReference =
    match t with
    | PT.TInt -> ST.TInt
    | PT.TFloat -> ST.TFloat
    | PT.TBool -> ST.TBool
    | PT.TUnit -> ST.TUnit
    | PT.TString -> ST.TString
    | PT.TList typ -> ST.TList(toST typ)
    | PT.TTuple(first, second, theRest) ->
      ST.TTuple(toST first, toST second, List.map toST theRest)
    | PT.TDict typ -> ST.TDict(toST typ)
    | PT.TDB typ -> ST.TDB(toST typ)
    | PT.TDateTime -> ST.TDateTime
    | PT.TChar -> ST.TChar
    | PT.TPassword -> ST.TPassword
    | PT.TUuid -> ST.TUuid
    | PT.TOption typ -> ST.TOption(toST typ)
    | PT.TCustomType(t, typeArgs) ->
      ST.TCustomType(FQTypeName.toST t, List.map toST typeArgs)
    | PT.TBytes -> ST.TBytes
    | PT.TResult(okType, errType) -> ST.TResult(toST okType, toST errType)
    | PT.TVariable(name) -> ST.TVariable(name)
    | PT.TFn(paramTypes, returnType) ->
      ST.TFn(List.map toST paramTypes, toST returnType)

  let rec toPT (t : ST.TypeReference) : PT.TypeReference =
    match t with
    | ST.TInt -> PT.TInt
    | ST.TFloat -> PT.TFloat
    | ST.TBool -> PT.TBool
    | ST.TUnit -> PT.TUnit
    | ST.TString -> PT.TString
    | ST.TList typ -> PT.TList(toPT typ)
    | ST.TTuple(firstType, secondType, otherTypes) ->
      PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
    | ST.TDict typ -> PT.TDict(toPT typ)
    | ST.TDB typ -> PT.TDB(toPT typ)
    | ST.TDateTime -> PT.TDateTime
    | ST.TChar -> PT.TChar
    | ST.TPassword -> PT.TPassword
    | ST.TUuid -> PT.TUuid
    | ST.TOption typ -> PT.TOption(toPT typ)
    | ST.TCustomType(t, typeArgs) ->
      PT.TCustomType(FQTypeName.toPT t, List.map toPT typeArgs)
    | ST.TBytes -> PT.TBytes
    | ST.TResult(okType, errType) -> PT.TResult(toPT okType, toPT errType)
    | ST.TVariable(name) -> PT.TVariable(name)
    | ST.TFn(paramTypes, returnType) ->
      PT.TFn(List.map toPT paramTypes, toPT returnType)

module BinaryOperation =
  let toST (op : PT.BinaryOperation) : ST.BinaryOperation =
    match op with
    | PT.BinOpAnd -> ST.BinOpAnd
    | PT.BinOpOr -> ST.BinOpOr

  let toPT (binop : ST.BinaryOperation) : PT.BinaryOperation =
    match binop with
    | ST.BinOpAnd -> PT.BinOpAnd
    | ST.BinOpOr -> PT.BinOpOr

module Infix =
  let toPT (infix : ST.Infix) : PT.Infix =
    match infix with
    | ST.InfixFnCall(fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
    | ST.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

module LetPattern =
  let rec toST (p : PT.LetPattern) : ST.LetPattern =
    match p with
    | PT.LPVariable(id, str) -> ST.LPVariable(id, str)
    | PT.LPTuple(id, first, second, theRest) ->
      ST.LPTuple(id, toST first, toST second, List.map toST theRest)

  let rec toPT (p : ST.LetPattern) : PT.LetPattern =
    match p with
    | ST.LPVariable(id, str) -> PT.LPVariable(id, str)
    | ST.LPTuple(id, first, second, theRest) ->
      PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)

module MatchPattern =
  let rec toST (p : PT.MatchPattern) : ST.MatchPattern =
    match p with
    | PT.MPVariable(id, str) -> ST.MPVariable(id, str)
    | PT.MPEnum(id, caseName, fieldPats) ->
      ST.MPEnum(id, caseName, List.map toST fieldPats)
    | PT.MPInt(id, i) -> ST.MPInt(id, i)
    | PT.MPBool(id, b) -> ST.MPBool(id, b)
    | PT.MPChar(id, c) -> ST.MPChar(id, c)
    | PT.MPString(id, s) -> ST.MPString(id, s)
    | PT.MPFloat(id, s, w, f) -> ST.MPFloat(id, s, w, f)
    | PT.MPUnit id -> ST.MPUnit id
    | PT.MPTuple(id, first, second, theRest) ->
      ST.MPTuple(id, toST first, toST second, List.map toST theRest)
    | PT.MPList(id, pats) -> ST.MPList(id, List.map toST pats)
    | PT.MPListCons(id, head, tail) -> ST.MPListCons(id, toST head, toST tail)


  let rec toPT (p : ST.MatchPattern) : PT.MatchPattern =
    match p with
    | ST.MPVariable(id, str) -> PT.MPVariable(id, str)
    | ST.MPEnum(id, caseName, fieldPats) ->
      PT.MPEnum(id, caseName, List.map toPT fieldPats)
    | ST.MPInt(id, i) -> PT.MPInt(id, i)
    | ST.MPBool(id, b) -> PT.MPBool(id, b)
    | ST.MPChar(id, c) -> PT.MPChar(id, c)
    | ST.MPString(id, s) -> PT.MPString(id, s)
    | ST.MPFloat(id, s, w, f) -> PT.MPFloat(id, s, w, f)
    | ST.MPUnit id -> PT.MPUnit id
    | ST.MPTuple(id, first, second, theRest) ->
      PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)
    | ST.MPList(id, pats) -> PT.MPList(id, List.map toPT pats)
    | ST.MPListCons(id, head, tail) -> PT.MPListCons(id, toPT head, toPT tail)


module Expr =
  let rec toST (e : PT.Expr) : ST.Expr =
    match e with
    | PT.EChar(id, char) -> ST.EChar(id, char)
    | PT.EInt(id, num) -> ST.EInt(id, num)
    | PT.EString(id, segments) -> ST.EString(id, List.map stringSegmentToST segments)
    | PT.EFloat(id, sign, whole, fraction) -> ST.EFloat(id, sign, whole, fraction)
    | PT.EBool(id, b) -> ST.EBool(id, b)
    | PT.EUnit id -> ST.EUnit id
    | PT.EVariable(id, var) -> ST.EVariable(id, var)
    | PT.EFieldAccess(id, obj, fieldname) -> ST.EFieldAccess(id, toST obj, fieldname)
    | PT.EFnCall(id, name, typeArgs, args) ->
      ST.EFnCall(
        id,
        FQFnName.toST name,
        List.map TypeReference.toST typeArgs,
        List.map toST args
      )
    | PT.EInfix(id, PT.InfixFnCall name, arg1, arg2) ->
      ST.EInfix(id, ST.InfixFnCall(InfixFnName.toST name), toST arg1, toST arg2)
    | PT.EInfix(id, PT.BinOp(op), arg1, arg2) ->
      ST.EInfix(id, ST.BinOp(BinaryOperation.toST (op)), toST arg1, toST arg2)
    | PT.ELambda(id, vars, body) -> ST.ELambda(id, vars, toST body)
    | PT.ELet(id, pat, rhs, body) ->
      ST.ELet(id, LetPattern.toST pat, toST rhs, toST body)
    | PT.EIf(id, cond, thenExpr, elseExpr) ->
      ST.EIf(id, toST cond, toST thenExpr, toST elseExpr)
    | PT.EList(id, exprs) -> ST.EList(id, List.map toST exprs)
    | PT.ETuple(id, first, second, theRest) ->
      ST.ETuple(id, toST first, toST second, List.map toST theRest)
    | PT.ERecord(id, typeName, fields) ->
      ST.ERecord(
        id,
        FQTypeName.toST typeName,
        List.map (Tuple2.mapSecond toST) fields
      )
    | PT.ERecordUpdate(id, record, updates) ->
      ST.ERecordUpdate(
        id,
        toST record,
        updates |> List.map (fun (name, expr) -> (name, toST expr))
      )
    | PT.EPipe(pipeID, expr1, expr2, rest) ->
      ST.EPipe(pipeID, toST expr1, pipeExprToST expr2, List.map pipeExprToST rest)
    | PT.EEnum(id, typeName, caseName, fields) ->
      ST.EEnum(id, FQTypeName.toST typeName, caseName, List.map toST fields)
    | PT.EMatch(id, mexpr, cases) ->
      ST.EMatch(
        id,
        toST mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toST << Tuple2.mapSecond toST) cases
      )
    | PT.EDict(id, fields) -> ST.EDict(id, List.map (Tuple2.mapSecond toST) fields)

  and stringSegmentToST (segment : PT.StringSegment) : ST.StringSegment =
    match segment with
    | PT.StringText text -> ST.StringText text
    | PT.StringInterpolation expr -> ST.StringInterpolation(toST expr)

  and pipeExprToST (pipeExpr : PT.PipeExpr) : ST.PipeExpr =
    match pipeExpr with
    | PT.EPipeVariable(id, name) -> ST.EPipeVariable(id, name)
    | PT.EPipeLambda(id, args, body) -> ST.EPipeLambda(id, args, toST body)
    | PT.EPipeInfix(id, PT.InfixFnCall name, first) ->
      ST.EPipeInfix(id, ST.InfixFnCall(InfixFnName.toST name), toST first)
    | PT.EPipeInfix(id, PT.BinOp(op), first) ->
      ST.EPipeInfix(id, ST.BinOp(BinaryOperation.toST (op)), toST first)
    | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
      ST.EPipeFnCall(
        id,
        FQFnName.toST fnName,
        List.map TypeReference.toST typeArgs,
        List.map toST args
      )
    | PT.EPipeEnum(id, typeName, caseName, fields) ->
      ST.EPipeEnum(id, FQTypeName.toST typeName, caseName, List.map toST fields)

  let rec toPT (e : ST.Expr) : PT.Expr =
    match e with
    | ST.EChar(id, char) -> PT.EChar(id, char)
    | ST.EInt(id, num) -> PT.EInt(id, num)
    | ST.EString(id, segment) -> PT.EString(id, List.map stringSegmentToPT segment)
    | ST.EFloat(id, sign, whole, fraction) -> PT.EFloat(id, sign, whole, fraction)
    | ST.EBool(id, b) -> PT.EBool(id, b)
    | ST.EUnit id -> PT.EUnit id
    | ST.EVariable(id, var) -> PT.EVariable(id, var)
    | ST.EFieldAccess(id, obj, fieldname) -> PT.EFieldAccess(id, toPT obj, fieldname)
    | ST.EFnCall(id, name, typeArgs, args) ->
      PT.EFnCall(
        id,
        FQFnName.toPT name,
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | ST.ELambda(id, vars, body) -> PT.ELambda(id, vars, toPT body)
    | ST.ELet(id, pat, rhs, body) ->
      PT.ELet(id, LetPattern.toPT pat, toPT rhs, toPT body)
    | ST.EIf(id, cond, thenExpr, elseExpr) ->
      PT.EIf(id, toPT cond, toPT thenExpr, toPT elseExpr)
    | ST.EList(id, exprs) -> PT.EList(id, List.map toPT exprs)
    | ST.ETuple(id, first, second, theRest) ->
      PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
    | ST.ERecord(id, typeName, fields) ->
      PT.ERecord(
        id,
        FQTypeName.toPT typeName,
        List.map (Tuple2.mapSecond toPT) fields
      )
    | ST.ERecordUpdate(id, record, updates) ->
      PT.ERecordUpdate(
        id,
        toPT record,
        updates |> List.map (fun (name, expr) -> (name, toPT expr))
      )
    | ST.EPipe(pipeID, expr1, expr2, rest) ->
      PT.EPipe(pipeID, toPT expr1, pipeExprToPT expr2, List.map pipeExprToPT rest)
    | ST.EEnum(id, typeName, caseName, exprs) ->
      PT.EEnum(id, FQTypeName.toPT typeName, caseName, List.map toPT exprs)
    | ST.EMatch(id, mexpr, pairs) ->
      PT.EMatch(
        id,
        toPT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toPT << Tuple2.mapSecond toPT) pairs
      )
    | ST.EInfix(id, infix, arg1, arg2) ->
      PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)
    | ST.EDict(id, pairs) -> PT.EDict(id, List.map (Tuple2.mapSecond toPT) pairs)

  and stringSegmentToPT (segment : ST.StringSegment) : PT.StringSegment =
    match segment with
    | ST.StringText text -> PT.StringText text
    | ST.StringInterpolation expr -> PT.StringInterpolation(toPT expr)

  and pipeExprToPT (pipeExpr : ST.PipeExpr) : PT.PipeExpr =
    match pipeExpr with
    | ST.EPipeVariable(id, name) -> PT.EPipeVariable(id, name)
    | ST.EPipeLambda(id, args, body) -> PT.EPipeLambda(id, args, toPT body)
    | ST.EPipeInfix(id, infix, first) ->
      PT.EPipeInfix(id, Infix.toPT infix, toPT first)
    | ST.EPipeFnCall(id, fnName, typeArgs, args) ->
      PT.EPipeFnCall(
        id,
        FQFnName.toPT fnName,
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | ST.EPipeEnum(id, typeName, caseName, fields) ->
      PT.EPipeEnum(id, FQTypeName.toPT typeName, caseName, List.map toPT fields)

module Deprecation =
  type Deprecation<'name> =
    | NotDeprecated
    | RenamedTo of 'name
    | ReplacedBy of 'name
    | DeprecatedBecause of string

  let toST
    (f : 'name1 -> 'name2)
    (d : PT.Deprecation<'name1>)
    : ST.Deprecation<'name2> =
    match d with
    | PT.DeprecatedBecause str -> ST.DeprecatedBecause str
    | PT.RenamedTo name -> ST.RenamedTo(f name)
    | PT.ReplacedBy name -> ST.ReplacedBy(f name)
    | PT.NotDeprecated -> ST.NotDeprecated

  let toPT
    (f : 'name1 -> 'name2)
    (d : ST.Deprecation<'name1>)
    : PT.Deprecation<'name2> =
    match d with
    | ST.NotDeprecated -> PT.NotDeprecated
    | ST.RenamedTo name -> PT.RenamedTo(f name)
    | ST.ReplacedBy name -> PT.ReplacedBy(f name)
    | ST.DeprecatedBecause reason -> PT.DeprecatedBecause reason



module CustomType =
  module RecordField =
    let toST (f : PT.CustomType.RecordField) : ST.CustomType.RecordField =
      { name = f.name; typ = TypeReference.toST f.typ; description = f.description }

    let toPT (f : ST.CustomType.RecordField) : PT.CustomType.RecordField =
      { name = f.name; typ = TypeReference.toPT f.typ; description = f.description }

  module EnumField =
    let toST (f : PT.CustomType.EnumField) : ST.CustomType.EnumField =
      { typ = TypeReference.toST f.typ
        label = f.label
        description = f.description }

    let toPT (f : ST.CustomType.EnumField) : PT.CustomType.EnumField =
      { typ = TypeReference.toPT f.typ
        label = f.label
        description = f.description }

  module EnumCase =
    let toST (c : PT.CustomType.EnumCase) : ST.CustomType.EnumCase =
      { name = c.name
        fields = List.map EnumField.toST c.fields
        description = c.description }

    let toPT (c : ST.CustomType.EnumCase) : PT.CustomType.EnumCase =
      { name = c.name
        fields = List.map EnumField.toPT c.fields
        description = c.description }

  let toST (d : PT.CustomType.T) : ST.CustomType.T =
    match d with
    | PT.CustomType.Alias typ -> ST.CustomType.Alias(TypeReference.toST typ)
    | PT.CustomType.Record(firstField, additionalFields) ->
      ST.CustomType.Record(
        RecordField.toST firstField,
        List.map RecordField.toST additionalFields
      )
    | PT.CustomType.Enum(firstCase, additionalCases) ->
      ST.CustomType.Enum(
        EnumCase.toST firstCase,
        List.map EnumCase.toST additionalCases
      )

  let toPT (d : ST.CustomType.T) : PT.CustomType.T =
    match d with
    | ST.CustomType.Alias typ -> PT.CustomType.Alias(TypeReference.toPT typ)
    | ST.CustomType.Record(firstField, additionalFields) ->
      PT.CustomType.Record(
        RecordField.toPT firstField,
        List.map RecordField.toPT additionalFields
      )
    | ST.CustomType.Enum(firstCase, additionalCases) ->
      PT.CustomType.Enum(
        EnumCase.toPT firstCase,
        List.map EnumCase.toPT additionalCases
      )

module Handler =
  module CronInterval =
    let toST (ci : PT.Handler.CronInterval) : ST.Handler.CronInterval =
      match ci with
      | PT.Handler.EveryDay -> ST.Handler.EveryDay
      | PT.Handler.EveryWeek -> ST.Handler.EveryWeek
      | PT.Handler.EveryFortnight -> ST.Handler.EveryFortnight
      | PT.Handler.EveryHour -> ST.Handler.EveryHour
      | PT.Handler.Every12Hours -> ST.Handler.Every12Hours
      | PT.Handler.EveryMinute -> ST.Handler.EveryMinute

    let toPT (ci : ST.Handler.CronInterval) : PT.Handler.CronInterval =
      match ci with
      | ST.Handler.EveryDay -> PT.Handler.EveryDay
      | ST.Handler.EveryWeek -> PT.Handler.EveryWeek
      | ST.Handler.EveryFortnight -> PT.Handler.EveryFortnight
      | ST.Handler.EveryHour -> PT.Handler.EveryHour
      | ST.Handler.Every12Hours -> PT.Handler.Every12Hours
      | ST.Handler.EveryMinute -> PT.Handler.EveryMinute

  module Spec =
    let toST (s : PT.Handler.Spec) : ST.Handler.Spec =
      match s with
      | PT.Handler.HTTP(route, method) -> ST.Handler.HTTP(route, method)
      | PT.Handler.Worker name -> ST.Handler.Worker name
      | PT.Handler.Cron(name, interval) ->
        ST.Handler.Cron(name, CronInterval.toST interval)
      | PT.Handler.REPL name -> ST.Handler.REPL name

    let toPT (s : ST.Handler.Spec) : PT.Handler.Spec =
      match s with
      | ST.Handler.HTTP(route, method) -> PT.Handler.HTTP(route, method)
      | ST.Handler.Worker name -> PT.Handler.Worker name
      | ST.Handler.Cron(name, interval) ->
        PT.Handler.Cron(name, CronInterval.toPT interval)
      | ST.Handler.REPL name -> PT.Handler.REPL name

  let toST (h : PT.Handler.T) : ST.Handler.T =
    { tlid = h.tlid; ast = Expr.toST h.ast; spec = Spec.toST h.spec }

  let toPT (h : ST.Handler.T) : PT.Handler.T =
    { tlid = h.tlid; ast = Expr.toPT h.ast; spec = Spec.toPT h.spec }

module DB =
  let toST (db : PT.DB.T) : ST.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toST db.typ }

  let toPT (db : ST.DB.T) : PT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toPT db.typ }

module UserType =
  let toST (t : PT.UserType.T) : ST.UserType.T =
    { tlid = t.tlid
      name = FQTypeName.UserTypeName.toST t.name
      typeParams = t.typeParams
      definition = CustomType.toST t.definition }

  let toPT (t : ST.UserType.T) : PT.UserType.T =
    { tlid = t.tlid
      name = FQTypeName.UserTypeName.toPT t.name
      typeParams = t.typeParams
      definition = CustomType.toPT t.definition }

module UserFunction =
  module Parameter =
    let toST (p : PT.UserFunction.Parameter) : ST.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toST p.typ; description = p.description }

    let toPT (p : ST.UserFunction.Parameter) : PT.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toPT p.typ; description = p.description }

  let toST (f : PT.UserFunction.T) : ST.UserFunction.T =
    { tlid = f.tlid
      name = FQFnName.UserFnName.toST f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toST f.parameters
      returnType = TypeReference.toST f.returnType
      description = f.description
      deprecated = Deprecation.toST FQFnName.toST f.deprecated
      body = Expr.toST f.body }

  let toPT (f : ST.UserFunction.T) : PT.UserFunction.T =
    { tlid = f.tlid
      name = FQFnName.UserFnName.toPT f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toPT f.parameters
      returnType = TypeReference.toPT f.returnType
      description = f.description
      deprecated = Deprecation.toPT FQFnName.toPT f.deprecated
      body = Expr.toPT f.body }

module Toplevel =
  let toST (tl : PT.Toplevel.T) : ST.Toplevel.T =
    match tl with
    | PT.Toplevel.TLHandler h -> ST.Toplevel.TLHandler(Handler.toST h)
    | PT.Toplevel.TLDB db -> ST.Toplevel.TLDB(DB.toST db)
    | PT.Toplevel.TLFunction f -> ST.Toplevel.TLFunction(UserFunction.toST f)
    | PT.Toplevel.TLType ut -> ST.Toplevel.TLType(UserType.toST ut)

  let toPT (tl : ST.Toplevel.T) : PT.Toplevel.T =
    match tl with
    | ST.Toplevel.TLHandler h -> PT.Toplevel.TLHandler(Handler.toPT h)
    | ST.Toplevel.TLDB db -> PT.Toplevel.TLDB(DB.toPT db)
    | ST.Toplevel.TLFunction f -> PT.Toplevel.TLFunction(UserFunction.toPT f)
    | ST.Toplevel.TLType ut -> PT.Toplevel.TLType(UserType.toPT ut)

module Op =
  let toST (op : PT.Op) : ST.Op =
    match op with
    | PT.SetHandler(handler) -> ST.SetHandler(Handler.toST handler)
    | PT.CreateDB(tlid, name, typ) -> ST.CreateDB(tlid, name, TypeReference.toST typ)
    | PT.DeleteTL tlid -> ST.DeleteTL tlid
    | PT.SetFunction fn -> ST.SetFunction(UserFunction.toST fn)
    | PT.UndoTL tlid -> ST.UndoTL tlid
    | PT.RedoTL tlid -> ST.RedoTL tlid
    | PT.SetExpr(tlid, id, e) -> ST.SetExpr(tlid, id, Expr.toST e)
    | PT.TLSavepoint tlid -> ST.TLSavepoint tlid
    | PT.DeleteFunction tlid -> ST.DeleteFunction tlid
    | PT.RenameDB(tlid, string) -> ST.RenameDB(tlid, string)
    | PT.SetType tipe -> ST.SetType(UserType.toST tipe)
    | PT.DeleteType tlid -> ST.DeleteType tlid

  let toPT (op : ST.Op) : Option<PT.Op> =
    match op with
    | ST.SetHandler(handler) -> Some(PT.SetHandler(Handler.toPT handler))
    | ST.CreateDB(tlid, name, typ) ->
      Some(PT.CreateDB(tlid, name, TypeReference.toPT typ))
    | ST.DeleteTL tlid -> Some(PT.DeleteTL tlid)
    | ST.SetFunction fn -> Some(PT.SetFunction(UserFunction.toPT fn))
    | ST.UndoTL tlid -> Some(PT.UndoTL tlid)
    | ST.RedoTL tlid -> Some(PT.RedoTL tlid)
    | ST.SetExpr(tlid, id, e) -> Some(PT.SetExpr(tlid, id, Expr.toPT e))
    | ST.TLSavepoint tlid -> Some(PT.TLSavepoint tlid)
    | ST.DeleteFunction tlid -> Some(PT.DeleteFunction tlid)
    | ST.RenameDB(tlid, string) -> Some(PT.RenameDB(tlid, string))
    | ST.SetType tipe -> Some(PT.SetType(UserType.toPT tipe))
    | ST.DeleteType tlid -> Some(PT.DeleteType tlid)

module PackageFn =
  module Parameter =
    let toST (p : PT.PackageFn.Parameter) : ST.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toST p.typ; description = p.description }

    let toPT (p : ST.PackageFn.Parameter) : PT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toPT p.typ; description = p.description }

  let toST (fn : PT.PackageFn.T) : ST.PackageFn.T =
    { name = FQFnName.PackageFnName.toST fn.name
      parameters = List.map Parameter.toST fn.parameters
      returnType = TypeReference.toST fn.returnType
      description = fn.description
      deprecated = Deprecation.toST FQFnName.toST fn.deprecated
      body = Expr.toST fn.body
      typeParams = fn.typeParams
      id = fn.id
      tlid = fn.tlid }

  let toPT (fn : ST.PackageFn.T) : PT.PackageFn.T =
    { name = FQFnName.PackageFnName.toPT fn.name
      parameters = List.map Parameter.toPT fn.parameters
      returnType = TypeReference.toPT fn.returnType
      description = fn.description
      deprecated = Deprecation.toPT FQFnName.toPT fn.deprecated
      body = Expr.toPT fn.body
      typeParams = fn.typeParams
      id = fn.id
      tlid = fn.tlid }

module PackageType =
  let toST (pt : PT.PackageType.T) : ST.PackageType.T =
    { name = FQTypeName.PackageTypeName.toST pt.name
      description = pt.description
      typeParams = pt.typeParams
      definition = CustomType.toST pt.definition
      deprecated = Deprecation.toST FQTypeName.toST pt.deprecated
      id = pt.id
      tlid = pt.tlid }

  let toPT (pt : ST.PackageType.T) : PT.PackageType.T =
    { name = FQTypeName.PackageTypeName.toPT pt.name
      description = pt.description
      typeParams = pt.typeParams
      definition = CustomType.toPT pt.definition
      deprecated = Deprecation.toPT FQTypeName.toPT pt.deprecated
      id = pt.id
      tlid = pt.tlid }
