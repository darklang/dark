module ClientTypes2ExecutionTypes.ProgramTypes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module CTPT = ClientTypes.Program

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

module FQTypeName =
  module UserTypeName =
    let fromCT (u : CTPT.FQTypeName.UserTypeName) : PT.FQTypeName.UserTypeName =
      { typ = u.typ; version = u.version }

    let toCT (u : PT.FQTypeName.UserTypeName) : CTPT.FQTypeName.UserTypeName =
      { typ = u.typ; version = u.version }

  module StdlibTypeName =
    let fromCT (t : CTPT.FQTypeName.StdlibTypeName) : PT.FQTypeName.StdlibTypeName =
      { module_ = t.module_; typ = t.typ; version = t.version }

    let toCT (t : PT.FQTypeName.StdlibTypeName) : CTPT.FQTypeName.StdlibTypeName =
      { module_ = t.module_; typ = t.typ; version = t.version }

  module PackageTypeName =
    let fromCT
      (t : CTPT.FQTypeName.PackageTypeName)
      : PT.FQTypeName.PackageTypeName =
      { owner = t.owner
        package = t.package
        module_ = t.module_
        typ = t.typ
        version = t.version }

    let toCT (t : PT.FQTypeName.PackageTypeName) : CTPT.FQTypeName.PackageTypeName =
      { owner = t.owner
        package = t.package
        module_ = t.module_
        typ = t.typ
        version = t.version }

  let fromCT (t : CTPT.FQTypeName.T) : PT.FQTypeName.T =
    match t with
    | CTPT.FQTypeName.Stdlib t -> PT.FQTypeName.Stdlib(StdlibTypeName.fromCT t)
    | CTPT.FQTypeName.User u -> PT.FQTypeName.User(UserTypeName.fromCT u)
    | CTPT.FQTypeName.Package p -> PT.FQTypeName.Package(PackageTypeName.fromCT p)

  let toCT (t : PT.FQTypeName.T) : CTPT.FQTypeName.T =
    match t with
    | PT.FQTypeName.Stdlib t -> CTPT.FQTypeName.Stdlib(StdlibTypeName.toCT t)
    | PT.FQTypeName.User u -> CTPT.FQTypeName.User(UserTypeName.toCT u)
    | PT.FQTypeName.Package p -> CTPT.FQTypeName.Package(PackageTypeName.toCT p)

module TypeReference =
  let rec fromCT (dtype : CTPT.TypeReference) : PT.TypeReference =
    match dtype with
    | CTPT.TypeReference.TInt -> PT.TInt
    | CTPT.TypeReference.TFloat -> PT.TFloat
    | CTPT.TypeReference.TBool -> PT.TBool
    | CTPT.TypeReference.TUnit -> PT.TUnit
    | CTPT.TypeReference.TString -> PT.TString
    | CTPT.TypeReference.TList (t) -> PT.TList(fromCT t)
    | CTPT.TypeReference.TTuple (first, second, theRest) ->
      PT.TTuple(fromCT first, fromCT second, List.map fromCT theRest)
    | CTPT.TypeReference.TDict (t) -> PT.TDict(fromCT t)
    | CTPT.TypeReference.THttpResponse (t) -> PT.THttpResponse(fromCT t)
    | CTPT.TypeReference.TDB (t) -> PT.TDB(fromCT t)
    | CTPT.TypeReference.TDateTime -> PT.TDateTime
    | CTPT.TypeReference.TChar -> PT.TChar
    | CTPT.TypeReference.TPassword -> PT.TPassword
    | CTPT.TypeReference.TUuid -> PT.TUuid
    | CTPT.TypeReference.TOption (t) -> PT.TOption(fromCT t)
    | CTPT.TypeReference.TCustomType (t, typeArgs) ->
      PT.TCustomType(FQTypeName.fromCT t, List.map fromCT typeArgs)
    | CTPT.TypeReference.TBytes -> PT.TBytes
    | CTPT.TypeReference.TResult (ok, err) -> PT.TResult(fromCT ok, fromCT err)
    | CTPT.TypeReference.TVariable (name) -> PT.TVariable(name)
    | CTPT.TypeReference.TFn (args, body) ->
      PT.TFn(List.map fromCT args, fromCT body)

  let rec toCT (dtype : PT.TypeReference) : CTPT.TypeReference =
    match dtype with
    | PT.TInt -> CTPT.TypeReference.TInt
    | PT.TFloat -> CTPT.TypeReference.TFloat
    | PT.TBool -> CTPT.TypeReference.TBool
    | PT.TUnit -> CTPT.TypeReference.TUnit
    | PT.TString -> CTPT.TypeReference.TString
    | PT.TList (t) -> CTPT.TypeReference.TList(toCT t)
    | PT.TTuple (first, second, theRest) ->
      CTPT.TypeReference.TTuple(toCT first, toCT second, List.map toCT theRest)
    | PT.TDict (t) -> CTPT.TypeReference.TDict(toCT t)
    | PT.THttpResponse (t) -> CTPT.TypeReference.THttpResponse(toCT t)
    | PT.TDB (t) -> CTPT.TypeReference.TDB(toCT t)
    | PT.TDateTime -> CTPT.TypeReference.TDateTime
    | PT.TChar -> CTPT.TypeReference.TChar
    | PT.TPassword -> CTPT.TypeReference.TPassword
    | PT.TUuid -> CTPT.TypeReference.TUuid
    | PT.TOption (t) -> CTPT.TypeReference.TOption(toCT t)
    | PT.TCustomType (t, typeArgs) ->
      CTPT.TypeReference.TCustomType(FQTypeName.toCT t, List.map toCT typeArgs)
    | PT.TBytes -> CTPT.TypeReference.TBytes
    | PT.TResult (ok, err) -> CTPT.TypeReference.TResult(toCT ok, toCT err)
    | PT.TVariable (name) -> CTPT.TypeReference.TVariable(name)
    | PT.TFn (args, body) -> CTPT.TypeReference.TFn(List.map toCT args, toCT body)


module FQFnName =
  module StdlibFnName =
    let fromCT (name : CTPT.FQFnName.StdlibFnName) : PT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

    let toCT (name : PT.FQFnName.StdlibFnName) : CTPT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }


  module UserFnName =
    let fromCT (t : CTPT.FQFnName.UserFnName) : PT.FQFnName.UserFnName = t

    let toCT (t : PT.FQFnName.UserFnName) : CTPT.FQFnName.UserFnName = t

  module PackageFnName =
    let fromCT (name : CTPT.FQFnName.PackageFnName) : PT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

    let toCT (name : PT.FQFnName.PackageFnName) : CTPT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

  let fromCT (fqfn : CTPT.FQFnName.T) : PT.FQFnName.T =
    match fqfn with
    | CTPT.FQFnName.User u -> PT.FQFnName.User(UserFnName.fromCT u)
    | CTPT.FQFnName.Stdlib fn -> PT.FQFnName.Stdlib(StdlibFnName.fromCT fn)
    | CTPT.FQFnName.Package p -> PT.FQFnName.Package(PackageFnName.fromCT p)

  let toCT (fqfn : PT.FQFnName.T) : CTPT.FQFnName.T =
    match fqfn with
    | PT.FQFnName.User u -> CTPT.FQFnName.User(UserFnName.toCT u)
    | PT.FQFnName.Stdlib fn -> CTPT.FQFnName.Stdlib(StdlibFnName.toCT fn)
    | PT.FQFnName.Package p -> CTPT.FQFnName.Package(PackageFnName.toCT p)

module InfixFnName =
  let fromCT (name : CTPT.InfixFnName) : PT.InfixFnName =
    match name with
    | CTPT.ArithmeticPlus -> PT.ArithmeticPlus
    | CTPT.ArithmeticMinus -> PT.ArithmeticMinus
    | CTPT.ArithmeticMultiply -> PT.ArithmeticMultiply
    | CTPT.ArithmeticDivide -> PT.ArithmeticDivide
    | CTPT.ArithmeticModulo -> PT.ArithmeticModulo
    | CTPT.ArithmeticPower -> PT.ArithmeticPower
    | CTPT.ComparisonGreaterThan -> PT.ComparisonGreaterThan
    | CTPT.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
    | CTPT.ComparisonLessThan -> PT.ComparisonLessThan
    | CTPT.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
    | CTPT.ComparisonEquals -> PT.ComparisonEquals
    | CTPT.ComparisonNotEquals -> PT.ComparisonNotEquals
    | CTPT.StringConcat -> PT.StringConcat

  let toCT (name : PT.InfixFnName) : CTPT.InfixFnName =
    match name with
    | PT.ArithmeticPlus -> CTPT.ArithmeticPlus
    | PT.ArithmeticMinus -> CTPT.ArithmeticMinus
    | PT.ArithmeticMultiply -> CTPT.ArithmeticMultiply
    | PT.ArithmeticDivide -> CTPT.ArithmeticDivide
    | PT.ArithmeticModulo -> CTPT.ArithmeticModulo
    | PT.ArithmeticPower -> CTPT.ArithmeticPower
    | PT.ComparisonGreaterThan -> CTPT.ComparisonGreaterThan
    | PT.ComparisonGreaterThanOrEqual -> CTPT.ComparisonGreaterThanOrEqual
    | PT.ComparisonLessThan -> CTPT.ComparisonLessThan
    | PT.ComparisonLessThanOrEqual -> CTPT.ComparisonLessThanOrEqual
    | PT.ComparisonEquals -> CTPT.ComparisonEquals
    | PT.ComparisonNotEquals -> CTPT.ComparisonNotEquals
    | PT.StringConcat -> CTPT.StringConcat

module LetPattern =
  let rec fromCT (p : CTPT.LetPattern) : PT.LetPattern =
    match p with
    | CTPT.LPVariable (id, str) -> PT.LPVariable(id, str)

  let rec toCT (p : PT.LetPattern) : CTPT.LetPattern =
    match p with
    | PT.LPVariable (id, str) -> CTPT.LPVariable(id, str)

module MatchPattern =
  let rec fromCT (pat : CTPT.MatchPattern) : PT.MatchPattern =
    match pat with
    | CTPT.MPVariable (id, str) -> PT.MPVariable(id, str)
    | CTPT.MPConstructor (id, caseName, fieldPats) ->
      PT.MPConstructor(id, caseName, List.map fromCT fieldPats)
    | CTPT.MPInt (id, i) -> PT.MPInt(id, i)
    | CTPT.MPBool (id, b) -> PT.MPBool(id, b)
    | CTPT.MPChar (id, str) -> PT.MPChar(id, str)
    | CTPT.MPString (id, str) -> PT.MPString(id, str)
    | CTPT.MPFloat (id, sign, whole, frac) -> PT.MPFloat(id, sign, whole, frac)
    | CTPT.MPUnit (id) -> PT.MPUnit(id)
    | CTPT.MPTuple (id, first, second, theRest) ->
      PT.MPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)
    | CTPT.MPList (id, pats) -> PT.MPList(id, List.map fromCT pats)

  let rec toCT (pat : PT.MatchPattern) : CTPT.MatchPattern =
    match pat with
    | PT.MPVariable (id, str) -> CTPT.MPVariable(id, str)
    | PT.MPConstructor (id, caseName, fieldPats) ->
      CTPT.MPConstructor(id, caseName, List.map toCT fieldPats)
    | PT.MPInt (id, i) -> CTPT.MPInt(id, i)
    | PT.MPBool (id, b) -> CTPT.MPBool(id, b)
    | PT.MPChar (id, str) -> CTPT.MPChar(id, str)
    | PT.MPString (id, str) -> CTPT.MPString(id, str)
    | PT.MPFloat (id, sign, whole, frac) -> CTPT.MPFloat(id, sign, whole, frac)
    | PT.MPUnit (id) -> CTPT.MPUnit(id)
    | PT.MPTuple (id, first, second, theRest) ->
      CTPT.MPTuple(id, toCT first, toCT second, List.map toCT theRest)
    | PT.MPList (id, pats) -> CTPT.MPList(id, List.map toCT pats)


module BinaryOperation =
  let fromCT (op : CTPT.BinaryOperation) : PT.BinaryOperation =
    match op with
    | CTPT.BinOpAnd -> PT.BinOpAnd
    | CTPT.BinOpOr -> PT.BinOpOr

  let toCT (op : PT.BinaryOperation) : CTPT.BinaryOperation =
    match op with
    | PT.BinOpAnd -> CTPT.BinOpAnd
    | PT.BinOpOr -> CTPT.BinOpOr

module Infix =
  let fromCT (infix : CTPT.Infix) : PT.Infix =
    match infix with
    | CTPT.InfixFnCall (name) -> PT.InfixFnCall(InfixFnName.fromCT name)
    | CTPT.BinOp (op) -> PT.BinOp(BinaryOperation.fromCT op)

  let toCT (infix : PT.Infix) : CTPT.Infix =
    match infix with
    | PT.InfixFnCall (name) -> CTPT.InfixFnCall(InfixFnName.toCT name)
    | PT.BinOp (op) -> CTPT.BinOp(BinaryOperation.toCT op)

module Expr =
  let rec fromCT (expr : CTPT.Expr) : PT.Expr =
    match expr with
    | CTPT.Expr.EInt (id, i) -> PT.EInt(id, i)
    | CTPT.Expr.EBool (id, b) -> PT.EBool(id, b)
    | CTPT.Expr.EString (id, segment) ->
      PT.EString(id, List.map stringSegmentFromCTPT segment)
    | CTPT.Expr.EChar (id, c) -> PT.EChar(id, c)
    | CTPT.Expr.EFloat (id, sign, whole, frac) -> PT.EFloat(id, sign, whole, frac)
    | CTPT.Expr.EUnit (id) -> PT.EUnit(id)
    | CTPT.Expr.ELet (id, pat, expr, body) ->
      PT.ELet(id, LetPattern.fromCT pat, fromCT expr, fromCT body)
    | CTPT.Expr.EIf (id, cond, ifExpr, thenExpr) ->
      PT.EIf(id, fromCT cond, fromCT ifExpr, fromCT thenExpr)
    | CTPT.EInfix (id, infix, first, second) ->
      PT.EInfix(id, Infix.fromCT (infix), fromCT first, fromCT second)
    | CTPT.Expr.ELambda (id, args, body) -> PT.ELambda(id, args, fromCT body)
    | CTPT.Expr.EFieldAccess (id, expr, fieldName) ->
      PT.EFieldAccess(id, fromCT expr, fieldName)
    | CTPT.Expr.EVariable (id, name) -> PT.EVariable(id, name)
    | CTPT.Expr.EFnCall (id, fnName, typeArgs, args) ->
      PT.EFnCall(
        id,
        FQFnName.fromCT fnName,
        List.map TypeReference.fromCT typeArgs,
        List.map fromCT args
      )
    | CTPT.Expr.EList (id, exprs) -> PT.EList(id, List.map fromCT exprs)
    | CTPT.Expr.ETuple (id, first, second, theRest) ->
      PT.ETuple(id, fromCT first, fromCT second, List.map fromCT theRest)
    | CTPT.Expr.ERecord (id, typeName, fields) ->
      PT.ERecord(
        id,
        Option.map FQTypeName.fromCT typeName,
        fields |> List.map (fun (name, expr) -> (name, fromCT expr))
      )
    | CTPT.Expr.EPipe (id, expr1, expr2, exprs) ->
      PT.EPipe(id, fromCT expr1, fromCT expr2, List.map fromCT exprs)
    | CTPT.Expr.EMatch (id, matchExpr, cases) ->
      PT.EMatch(
        id,
        fromCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.fromCT pat, fromCT expr))
      )
    | CTPT.Expr.EPipeTarget (id) -> PT.EPipeTarget(id)
    | CTPT.Expr.EFeatureFlag (id, name, cond, caseA, caseB) ->
      PT.EFeatureFlag(id, name, fromCT cond, fromCT caseA, fromCT caseB)
    | CTPT.EConstructor (id, typeName, caseName, fields) ->
      PT.Expr.EConstructor(
        id,
        Option.map FQTypeName.fromCT typeName,
        caseName,
        List.map fromCT fields
      )

  and stringSegmentFromCTPT (segment : CTPT.StringSegment) : PT.StringSegment =
    match segment with
    | CTPT.StringText text -> PT.StringText text
    | CTPT.StringInterpolation expr -> PT.StringInterpolation(fromCT expr)

  let rec toCT (expr : PT.Expr) : CTPT.Expr =
    match expr with
    | PT.EInt (id, i) -> CTPT.Expr.EInt(id, i)
    | PT.EBool (id, b) -> CTPT.Expr.EBool(id, b)
    | PT.EString (id, s) -> CTPT.Expr.EString(id, List.map stringSegmentToCT s)
    | PT.EChar (id, c) -> CTPT.Expr.EChar(id, c)
    | PT.EFloat (id, sign, whole, frac) -> CTPT.Expr.EFloat(id, sign, whole, frac)
    | PT.EUnit (id) -> CTPT.Expr.EUnit(id)
    | PT.ELet (id, pat, expr, body) ->
      CTPT.Expr.ELet(id, LetPattern.toCT pat, toCT expr, toCT body)
    | PT.EIf (id, cond, ifExpr, thenExpr) ->
      CTPT.Expr.EIf(id, toCT cond, toCT ifExpr, toCT thenExpr)
    | PT.EInfix (id, PT.InfixFnCall (name), first, second) ->
      CTPT.Expr.EInfix(
        id,
        CTPT.InfixFnCall(InfixFnName.toCT name),
        toCT first,
        toCT second
      )
    | PT.EInfix (id, PT.BinOp op, first, second) ->
      CTPT.EInfix(id, CTPT.BinOp(BinaryOperation.toCT op), toCT first, toCT second)
    | PT.ELambda (id, args, body) -> CTPT.Expr.ELambda(id, args, toCT body)
    | PT.EFieldAccess (id, expr, fieldName) ->
      CTPT.Expr.EFieldAccess(id, toCT expr, fieldName)
    | PT.EVariable (id, name) -> CTPT.Expr.EVariable(id, name)
    | PT.EFnCall (id, fnName, typeArgs, args) ->
      CTPT.Expr.EFnCall(
        id,
        FQFnName.toCT fnName,
        List.map TypeReference.toCT typeArgs,
        List.map toCT args
      )
    | PT.EList (id, exprs) -> CTPT.Expr.EList(id, List.map toCT exprs)
    | PT.ETuple (id, first, second, theRest) ->
      CTPT.Expr.ETuple(id, toCT first, toCT second, List.map toCT theRest)
    | PT.ERecord (id, typeName, fields) ->
      CTPT.Expr.ERecord(
        id,
        Option.map FQTypeName.toCT typeName,
        fields |> List.map (fun (name, expr) -> (name, toCT expr))
      )
    | PT.EPipe (id, expr1, expr2, exprs) ->
      CTPT.Expr.EPipe(id, toCT expr1, toCT expr2, List.map toCT exprs)
    | PT.EConstructor (id, typeName, caseName, fields) ->
      CTPT.Expr.EConstructor(
        id,
        Option.map FQTypeName.toCT typeName,
        caseName,
        List.map toCT fields
      )
    | PT.EMatch (id, matchExpr, cases) ->
      CTPT.Expr.EMatch(
        id,
        toCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.toCT pat, toCT expr))
      )
    | PT.EPipeTarget (id) -> CTPT.Expr.EPipeTarget(id)
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
      CTPT.Expr.EFeatureFlag(id, name, toCT cond, toCT caseA, toCT caseB)


  and stringSegmentToCT (segment : PT.StringSegment) : CTPT.StringSegment =
    match segment with
    | PT.StringText text -> CTPT.StringText text
    | PT.StringInterpolation expr -> CTPT.StringInterpolation(toCT expr)



module CustomType =
  module RecordField =
    let fromCT (rf : CTPT.CustomType.RecordField) : PT.CustomType.RecordField =
      { id = rf.id; name = rf.name; typ = TypeReference.fromCT rf.typ }

    let toCT (rf : PT.CustomType.RecordField) : CTPT.CustomType.RecordField =
      { id = rf.id; name = rf.name; typ = TypeReference.toCT rf.typ }

  module EnumField =
    let fromCT (ef : CTPT.CustomType.EnumField) : PT.CustomType.EnumField =
      { id = ef.id; typ = TypeReference.fromCT ef.typ; label = ef.label }

    let toCT (ef : PT.CustomType.EnumField) : CTPT.CustomType.EnumField =
      { id = ef.id; typ = TypeReference.toCT ef.typ; label = ef.label }

  module EnumCase =
    let fromCT (ec : CTPT.CustomType.EnumCase) : PT.CustomType.EnumCase =
      { id = ec.id; name = ec.name; fields = List.map EnumField.fromCT ec.fields }

    let toCT (ec : PT.CustomType.EnumCase) : CTPT.CustomType.EnumCase =
      { id = ec.id; name = ec.name; fields = List.map EnumField.toCT ec.fields }

  let fromCT (def : CTPT.CustomType.T) : PT.CustomType.T =
    match def with
    | CTPT.CustomType.Record (firstField, additionalFields) ->
      PT.CustomType.Record(
        RecordField.fromCT firstField,
        List.map RecordField.fromCT additionalFields
      )
    | CTPT.CustomType.Enum (firstCase, additionalCases) ->
      PT.CustomType.Enum(
        EnumCase.fromCT firstCase,
        List.map EnumCase.fromCT additionalCases
      )

  let toCT (def : PT.CustomType.T) : CTPT.CustomType.T =
    match def with
    | PT.CustomType.Record (firstField, additionalFields) ->
      CTPT.CustomType.Record(
        RecordField.toCT firstField,
        List.map RecordField.toCT additionalFields
      )
    | PT.CustomType.Enum (firstCase, additionalCases) ->
      CTPT.CustomType.Enum(
        EnumCase.toCT firstCase,
        List.map EnumCase.toCT additionalCases
      )


module Handler =
  module CronInterval =
    let fromCT (ci : CTPT.Handler.CronInterval) : PT.Handler.CronInterval =
      match ci with
      | CTPT.Handler.CronInterval.EveryDay -> PT.Handler.EveryDay
      | CTPT.Handler.CronInterval.EveryWeek -> PT.Handler.EveryWeek
      | CTPT.Handler.CronInterval.EveryFortnight -> PT.Handler.EveryFortnight
      | CTPT.Handler.CronInterval.EveryHour -> PT.Handler.EveryHour
      | CTPT.Handler.CronInterval.Every12Hours -> PT.Handler.Every12Hours
      | CTPT.Handler.CronInterval.EveryMinute -> PT.Handler.EveryMinute

    let toCT (ci : PT.Handler.CronInterval) : CTPT.Handler.CronInterval =
      match ci with
      | PT.Handler.EveryDay -> CTPT.Handler.CronInterval.EveryDay
      | PT.Handler.EveryWeek -> CTPT.Handler.CronInterval.EveryWeek
      | PT.Handler.EveryFortnight -> CTPT.Handler.CronInterval.EveryFortnight
      | PT.Handler.EveryHour -> CTPT.Handler.CronInterval.EveryHour
      | PT.Handler.Every12Hours -> CTPT.Handler.CronInterval.Every12Hours
      | PT.Handler.EveryMinute -> CTPT.Handler.CronInterval.EveryMinute

  // We need to keep the IDs around until we get rid of them on the client
  module ids =
    let fromCT (ids : CTPT.Handler.ids) : PT.Handler.ids =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }

    let toCT (ids : PT.Handler.ids) : CTPT.Handler.ids =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }

  module Spec =
    let fromCT (spec : CTPT.Handler.Spec) : PT.Handler.Spec =
      match spec with
      | CTPT.Handler.Spec.HTTP (route, method, i) ->
        PT.Handler.HTTP(route, method, ids.fromCT i)
      | CTPT.Handler.Spec.Worker (name, i) -> PT.Handler.Worker(name, ids.fromCT i)
      | CTPT.Handler.Spec.Cron (name, interval, i) ->
        PT.Handler.Cron(name, Option.map CronInterval.fromCT interval, ids.fromCT i)
      | CTPT.Handler.Spec.REPL (name, i) -> PT.Handler.REPL(name, ids.fromCT i)

    let toCT (spec : PT.Handler.Spec) : CTPT.Handler.Spec =
      match spec with
      | PT.Handler.HTTP (route, method, i) ->
        CTPT.Handler.Spec.HTTP(route, method, ids.toCT i)
      | PT.Handler.Worker (name, i) -> CTPT.Handler.Spec.Worker(name, ids.toCT i)
      | PT.Handler.Cron (name, interval, i) ->
        CTPT.Handler.Spec.Cron(
          name,
          Option.map CronInterval.toCT interval,
          ids.toCT i
        )
      | PT.Handler.REPL (name, i) -> CTPT.Handler.Spec.REPL(name, ids.toCT i)

  let fromCT (h : CTPT.Handler.T) : PT.Handler.T =
    { tlid = h.tlid; ast = Expr.fromCT h.ast; spec = Spec.fromCT h.spec }

  let toCT (h : PT.Handler.T) : CTPT.Handler.T =
    { tlid = h.tlid; ast = Expr.toCT h.ast; spec = Spec.toCT h.spec }


module DB =
  let fromCT (db : CTPT.DB.T) : PT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.fromCT db.typ }

  let toCT (db : PT.DB.T) : CTPT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toCT db.typ }

module UserType =
  let fromCT (ut : CTPT.UserType.T) : PT.UserType.T =
    { tlid = ut.tlid
      name = FQTypeName.UserTypeName.fromCT ut.name
      definition = CustomType.fromCT ut.definition }

  let toCT (ut : PT.UserType.T) : CTPT.UserType.T =
    { tlid = ut.tlid
      name = FQTypeName.UserTypeName.toCT ut.name
      definition = CustomType.toCT ut.definition }


module UserFunction =
  module Parameter =
    let fromCT (p : CTPT.UserFunction.Parameter) : PT.UserFunction.Parameter =
      { id = p.id
        name = p.name
        typ = TypeReference.fromCT p.typ
        description = p.description }

    let toCT (p : PT.UserFunction.Parameter) : CTPT.UserFunction.Parameter =
      { id = p.id
        name = p.name
        typ = TypeReference.toCT p.typ
        description = p.description }

  let fromCT (uf : CTPT.UserFunction.T) : PT.UserFunction.T =
    { tlid = uf.tlid
      name = uf.name
      typeParams = uf.typeParams
      parameters = List.map Parameter.fromCT uf.parameters
      returnType = TypeReference.fromCT uf.returnType
      description = uf.description
      infix = uf.infix
      body = Expr.fromCT uf.body }

  let toCT (uf : PT.UserFunction.T) : CTPT.UserFunction.T =
    { tlid = uf.tlid
      name = uf.name
      typeParams = uf.typeParams
      parameters = List.map Parameter.toCT uf.parameters
      returnType = TypeReference.toCT uf.returnType
      description = uf.description
      infix = uf.infix
      body = Expr.toCT uf.body }


module Toplevel =
  let fromCT (tl : CTPT.Toplevel) : PT.Toplevel.T =
    match tl with
    | CTPT.Toplevel.TLHandler handler ->
      PT.Toplevel.TLHandler(Handler.fromCT handler)
    | CTPT.Toplevel.TLDB db -> PT.Toplevel.TLDB(DB.fromCT db)
    | CTPT.Toplevel.TLFunction uf -> PT.Toplevel.TLFunction(UserFunction.fromCT uf)
    | CTPT.Toplevel.TLType ut -> PT.Toplevel.TLType(UserType.fromCT ut)

  let toCT (tl : PT.Toplevel.T) : CTPT.Toplevel =
    match tl with
    | PT.Toplevel.TLHandler handler -> CTPT.Toplevel.TLHandler(Handler.toCT handler)
    | PT.Toplevel.TLDB db -> CTPT.Toplevel.TLDB(DB.toCT db)
    | PT.Toplevel.TLFunction uf -> CTPT.Toplevel.TLFunction(UserFunction.toCT uf)
    | PT.Toplevel.TLType ut -> CTPT.Toplevel.TLType(UserType.toCT ut)



module Op =

  let fromCT (op : CTPT.Op) : PT.Op =
    match op with
    | CTPT.Op.SetHandler (handler) -> PT.Op.SetHandler(Handler.fromCT handler)
    | CTPT.Op.CreateDB (tlid, name, typ) ->
      PT.Op.CreateDB(tlid, name, TypeReference.fromCT typ)
    | CTPT.Op.DeleteTL (tlid) -> PT.Op.DeleteTL(tlid)
    | CTPT.Op.SetFunction (uf) -> PT.Op.SetFunction(UserFunction.fromCT uf)
    | CTPT.Op.UndoTL (tlid) -> PT.Op.UndoTL(tlid)
    | CTPT.Op.RedoTL (tlid) -> PT.Op.RedoTL(tlid)
    | CTPT.Op.SetExpr (tlid, id, expr) -> PT.Op.SetExpr(tlid, id, Expr.fromCT expr)
    | CTPT.Op.TLSavepoint (tlid) -> PT.Op.TLSavepoint(tlid)
    | CTPT.Op.DeleteFunction (tlid) -> PT.Op.DeleteFunction(tlid)
    | CTPT.Op.RenameDB (tlid, name) -> PT.Op.RenameDB(tlid, name)
    | CTPT.Op.SetType (ut) -> PT.Op.SetType(UserType.fromCT ut)
    | CTPT.Op.DeleteType (tlid) -> PT.Op.DeleteType(tlid)

  let toCT (op : PT.Op) : CTPT.Op =
    match op with
    | PT.Op.SetHandler (handler) -> CTPT.Op.SetHandler(Handler.toCT handler)
    | PT.Op.CreateDB (tlid, name, typ) ->
      CTPT.Op.CreateDB(tlid, name, TypeReference.toCT typ)
    | PT.Op.DeleteTL (tlid) -> CTPT.Op.DeleteTL(tlid)
    | PT.Op.SetFunction (uf) -> CTPT.Op.SetFunction(UserFunction.toCT uf)
    | PT.Op.UndoTL (tlid) -> CTPT.Op.UndoTL(tlid)
    | PT.Op.RedoTL (tlid) -> CTPT.Op.RedoTL(tlid)
    | PT.Op.SetExpr (tlid, id, expr) -> CTPT.Op.SetExpr(tlid, id, Expr.toCT expr)
    | PT.Op.TLSavepoint (tlid) -> CTPT.Op.TLSavepoint(tlid)
    | PT.Op.DeleteFunction (tlid) -> CTPT.Op.DeleteFunction(tlid)
    | PT.Op.RenameDB (tlid, name) -> CTPT.Op.RenameDB(tlid, name)
    | PT.Op.SetType (ut) -> CTPT.Op.SetType(UserType.toCT ut)
    | PT.Op.DeleteType (tlid) -> CTPT.Op.DeleteType(tlid)


module Secret =
  let fromCT (s : CTPT.Secret) : PT.Secret.T =
    { name = s.name; value = s.value; version = s.version }

  let toCT (s : PT.Secret.T) : CTPT.Secret =
    { name = s.name; value = s.value; version = s.version }


module Package =
  module Parameter =
    let fromCT (p : CTPT.Package.Parameter) : PT.Package.Parameter =
      { name = p.name
        typ = TypeReference.fromCT p.typ
        description = p.description }

    let toCT (p : PT.Package.Parameter) : CTPT.Package.Parameter =
      { name = p.name; typ = TypeReference.toCT p.typ; description = p.description }

  module Fn =
    let fromCT (fn : CTPT.Package.Fn) : PT.Package.Fn =
      { name = FQFnName.PackageFnName.fromCT fn.name
        body = Expr.fromCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.fromCT fn.parameters
        returnType = TypeReference.fromCT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }

    let toCT (fn : PT.Package.Fn) : CTPT.Package.Fn =
      { name = FQFnName.PackageFnName.toCT fn.name
        body = Expr.toCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.toCT fn.parameters
        returnType = TypeReference.toCT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }
