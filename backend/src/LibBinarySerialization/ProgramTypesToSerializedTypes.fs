/// Conversion functions from ProgramTypes to SerializedTypes
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

  module UserTypeName =
    let toST (u : PT.FQTypeName.UserTypeName) : ST.FQTypeName.UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }

  module PackageTypeName =
    let toST (p : PT.FQTypeName.PackageTypeName) : ST.FQTypeName.PackageTypeName =
      { owner = p.owner
        package = p.package
        modules = { head = p.modules.Head; tail = p.modules.Tail }
        typ = p.typ
        version = p.version }


  let toST (t : PT.FQTypeName.T) : ST.FQTypeName.T =
    match t with
    | PT.FQTypeName.Stdlib s -> ST.FQTypeName.Stdlib(StdlibTypeName.toST s)
    | PT.FQTypeName.User u -> ST.FQTypeName.User(UserTypeName.toST u)
    | PT.FQTypeName.Package p -> ST.FQTypeName.Package(PackageTypeName.toST p)


module FQFnName =
  module PackageFnName =
    let toST (name : PT.FQFnName.PackageFnName) : ST.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        modules = { head = name.modules.Head; tail = name.modules.Tail }
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    let toST (name : PT.FQFnName.StdlibFnName) : ST.FQFnName.StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

  module UserFnName =
    let toST (name : PT.FQFnName.UserFnName) : ST.FQFnName.UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

  let toST (fqfn : PT.FQFnName.T) : ST.FQFnName.T =
    match fqfn with
    | PT.FQFnName.User u -> ST.FQFnName.User(UserFnName.toST u)
    | PT.FQFnName.Stdlib fn -> ST.FQFnName.Stdlib(StdlibFnName.toST fn)
    | PT.FQFnName.Package p -> ST.FQFnName.Package(PackageFnName.toST p)

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


module TypeReference =
  let rec toST (t : PT.TypeReference) : ST.TypeReference =
    match t with
    | PT.TInt -> ST.TInt
    | PT.TFloat -> ST.TFloat
    | PT.TBool -> ST.TBool
    | PT.TUnit -> ST.TUnit
    | PT.TString -> ST.TString
    | PT.TList typ -> ST.TList(toST typ)
    | PT.TTuple (first, second, theRest) ->
      ST.TTuple(toST first, toST second, List.map toST theRest)
    | PT.TDict typ -> ST.TDict(toST typ)
    | PT.THttpResponse typ -> ST.THttpResponse(toST typ)
    | PT.TDB typ -> ST.TDB(toST typ)
    | PT.TDateTime -> ST.TDateTime
    | PT.TChar -> ST.TChar
    | PT.TPassword -> ST.TPassword
    | PT.TUuid -> ST.TUuid
    | PT.TOption typ -> ST.TOption(toST typ)
    | PT.TCustomType (t, typeArgs) ->
      ST.TCustomType(FQTypeName.toST t, List.map toST typeArgs)
    | PT.TBytes -> ST.TBytes
    | PT.TResult (okType, errType) -> ST.TResult(toST okType, toST errType)
    | PT.TVariable (name) -> ST.TVariable(name)
    | PT.TFn (paramTypes, returnType) ->
      ST.TFn(List.map toST paramTypes, toST returnType)


module BinaryOperation =
  let toST (op : PT.BinaryOperation) : ST.BinaryOperation =
    match op with
    | PT.BinOpAnd -> ST.BinOpAnd
    | PT.BinOpOr -> ST.BinOpOr

module LetPattern =
  let rec toST (p : PT.LetPattern) : ST.LetPattern =
    match p with
    | PT.LPVariable (id, str) -> ST.LPVariable(id, str)
    | PT.LPTuple (id, first, second, theRest) ->
      ST.LPTuple(id, toST first, toST second, List.map toST theRest)

module MatchPattern =
  let rec toST (p : PT.MatchPattern) : ST.MatchPattern =
    match p with
    | PT.MPVariable (id, str) -> ST.MPVariable(id, str)
    | PT.MPEnum (id, caseName, fieldPats) ->
      ST.MPEnum(id, caseName, List.map toST fieldPats)
    | PT.MPInt (id, i) -> ST.MPInt(id, i)
    | PT.MPBool (id, b) -> ST.MPBool(id, b)
    | PT.MPChar (id, c) -> ST.MPChar(id, c)
    | PT.MPString (id, s) -> ST.MPString(id, s)
    | PT.MPFloat (id, s, w, f) -> ST.MPFloat(id, s, w, f)
    | PT.MPUnit id -> ST.MPUnit id
    | PT.MPTuple (id, first, second, theRest) ->
      ST.MPTuple(id, toST first, toST second, List.map toST theRest)
    | PT.MPList (id, pats) -> ST.MPList(id, List.map toST pats)



module Expr =
  let rec toST (e : PT.Expr) : ST.Expr =
    match e with
    | PT.EChar (id, char) -> ST.EChar(id, char)
    | PT.EInt (id, num) -> ST.EInt(id, num)
    | PT.EString (id, segments) ->
      ST.EString(id, List.map stringSegmentToST segments)
    | PT.EFloat (id, sign, whole, fraction) -> ST.EFloat(id, sign, whole, fraction)
    | PT.EBool (id, b) -> ST.EBool(id, b)
    | PT.EUnit id -> ST.EUnit id
    | PT.EVariable (id, var) -> ST.EVariable(id, var)
    | PT.EFieldAccess (id, obj, fieldname) ->
      ST.EFieldAccess(id, toST obj, fieldname)
    | PT.EFnCall (id, name, typeArgs, args) ->
      ST.EFnCall(
        id,
        FQFnName.toST name,
        List.map TypeReference.toST typeArgs,
        List.map toST args
      )
    | PT.EInfix (id, PT.InfixFnCall name, arg1, arg2) ->
      ST.EInfix(id, ST.InfixFnCall(InfixFnName.toST name), toST arg1, toST arg2)
    | PT.EInfix (id, PT.BinOp (op), arg1, arg2) ->
      ST.EInfix(id, ST.BinOp(BinaryOperation.toST (op)), toST arg1, toST arg2)
    | PT.ELambda (id, vars, body) -> ST.ELambda(id, vars, toST body)
    | PT.ELet (id, pat, rhs, body) ->
      ST.ELet(id, LetPattern.toST pat, toST rhs, toST body)
    | PT.EIf (id, cond, thenExpr, elseExpr) ->
      ST.EIf(id, toST cond, toST thenExpr, toST elseExpr)
    | PT.EList (id, exprs) -> ST.EList(id, List.map toST exprs)
    | PT.ETuple (id, first, second, theRest) ->
      ST.ETuple(id, toST first, toST second, List.map toST theRest)
    | PT.ERecord (id, typeName, fields) ->
      ST.ERecord(
        id,
        FQTypeName.toST typeName,
        List.map (Tuple2.mapSecond toST) fields
      )
    | PT.EPipe (pipeID, expr1, expr2, rest) ->
      ST.EPipe(pipeID, toST expr1, toST expr2, List.map toST rest)
    | PT.EEnum (id, typeName, caseName, fields) ->
      ST.EEnum(
        id,
        Option.map FQTypeName.toST typeName,
        caseName,
        List.map toST fields
      )
    | PT.EMatch (id, mexpr, cases) ->
      ST.EMatch(
        id,
        toST mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toST << Tuple2.mapSecond toST) cases
      )
    | PT.EPipeTarget id -> ST.EPipeTarget id
    | PT.EDict (id, fields) -> ST.EDict(id, List.map (Tuple2.mapSecond toST) fields)

  and stringSegmentToST (segment : PT.StringSegment) : ST.StringSegment =
    match segment with
    | PT.StringText text -> ST.StringText text
    | PT.StringInterpolation expr -> ST.StringInterpolation(toST expr)



module CustomType =
  module RecordField =
    let toST (f : PT.CustomType.RecordField) : ST.CustomType.RecordField =
      { name = f.name; typ = TypeReference.toST f.typ }

  module EnumField =
    let toST (f : PT.CustomType.EnumField) : ST.CustomType.EnumField =
      { typ = TypeReference.toST f.typ; label = f.label }

  module EnumCase =
    let toST (c : PT.CustomType.EnumCase) : ST.CustomType.EnumCase =
      { name = c.name; fields = List.map EnumField.toST c.fields }

  let toST (d : PT.CustomType.T) : ST.CustomType.T =
    match d with
    | PT.CustomType.Record (firstField, additionalFields) ->
      ST.CustomType.Record(
        RecordField.toST firstField,
        List.map RecordField.toST additionalFields
      )
    | PT.CustomType.Enum (firstCase, additionalCases) ->
      ST.CustomType.Enum(
        EnumCase.toST firstCase,
        List.map EnumCase.toST additionalCases
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

  module Spec =
    let toST (s : PT.Handler.Spec) : ST.Handler.Spec =
      match s with
      | PT.Handler.HTTP (route, method) -> ST.Handler.HTTP(route, method)
      | PT.Handler.Worker name -> ST.Handler.Worker name
      | PT.Handler.Cron (name, interval) ->
        ST.Handler.Cron(name, CronInterval.toST interval)
      | PT.Handler.REPL name -> ST.Handler.REPL name

  let toST (h : PT.Handler.T) : ST.Handler.T =
    { tlid = h.tlid; ast = Expr.toST h.ast; spec = Spec.toST h.spec }

module DB =
  let toST (db : PT.DB.T) : ST.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toST db.typ }


module UserType =
  let toST (t : PT.UserType.T) : ST.UserType.T =
    { tlid = t.tlid
      name = FQTypeName.UserTypeName.toST t.name
      definition = CustomType.toST t.definition }

module UserFunction =
  module Parameter =
    let toST (p : PT.UserFunction.Parameter) : ST.UserFunction.Parameter =
      { id = p.id
        name = p.name
        typ = TypeReference.toST p.typ
        description = p.description }

  let toST (f : PT.UserFunction.T) : ST.UserFunction.T =
    { tlid = f.tlid
      name = FQFnName.UserFnName.toST f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toST f.parameters
      returnType = TypeReference.toST f.returnType
      description = f.description
      infix = f.infix
      body = Expr.toST f.body }


module Toplevel =
  let toST (tl : PT.Toplevel.T) : ST.Toplevel.T =
    match tl with
    | PT.Toplevel.TLHandler h -> ST.Toplevel.TLHandler(Handler.toST h)
    | PT.Toplevel.TLDB db -> ST.Toplevel.TLDB(DB.toST db)
    | PT.Toplevel.TLFunction f -> ST.Toplevel.TLFunction(UserFunction.toST f)
    | PT.Toplevel.TLType ut -> ST.Toplevel.TLType(UserType.toST ut)

module Op =
  let toST (op : PT.Op) : ST.Op =
    match op with
    | PT.SetHandler (handler) -> ST.SetHandler(Handler.toST handler)
    | PT.CreateDB (tlid, name, typ) ->
      ST.CreateDB(tlid, name, TypeReference.toST typ)
    | PT.DeleteTL tlid -> ST.DeleteTL tlid
    | PT.SetFunction fn -> ST.SetFunction(UserFunction.toST fn)
    | PT.UndoTL tlid -> ST.UndoTL tlid
    | PT.RedoTL tlid -> ST.RedoTL tlid
    | PT.SetExpr (tlid, id, e) -> ST.SetExpr(tlid, id, Expr.toST e)
    | PT.TLSavepoint tlid -> ST.TLSavepoint tlid
    | PT.DeleteFunction tlid -> ST.DeleteFunction tlid
    | PT.RenameDB (tlid, string) -> ST.RenameDB(tlid, string)
    | PT.SetType tipe -> ST.SetType(UserType.toST tipe)
    | PT.DeleteType tlid -> ST.DeleteType tlid
