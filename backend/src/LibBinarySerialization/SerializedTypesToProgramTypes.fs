// Conversion functions from SerializedTypes to ProgramTypes
module LibBinarySerialization.SerializedTypesToProgramTypes

open Prelude
open Tablecloth

// Used for conversion functions
module ST = SerializedTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

module FQTypeName =
  module UserTypeName =
    let toPT (u : ST.FQTypeName.UserTypeName) : PT.FQTypeName.UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }

  module StdlibTypeName =
    let toPT (s : ST.FQTypeName.StdlibTypeName) : PT.FQTypeName.StdlibTypeName =
      { modules = s.modules; typ = s.typ; version = s.version }

  module PackageTypeName =
    let toPT (p : ST.FQTypeName.PackageTypeName) : PT.FQTypeName.PackageTypeName =
      { owner = p.owner
        package = p.package
        modules = { Head = p.modules.head; Tail = p.modules.tail }
        typ = p.typ
        version = p.version }

  let toPT (t : ST.FQTypeName.T) : PT.FQTypeName.T =
    match t with
    | ST.FQTypeName.Stdlib s -> PT.FQTypeName.Stdlib(StdlibTypeName.toPT s)
    | ST.FQTypeName.User u -> PT.FQTypeName.User(UserTypeName.toPT u)
    | ST.FQTypeName.Package p -> PT.FQTypeName.Package(PackageTypeName.toPT p)



module FQFnName =
  module PackageFnName =
    let toPT (name : ST.FQFnName.PackageFnName) : PT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        modules = { Head = name.modules.head; Tail = name.modules.tail }
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    let toPT (name : ST.FQFnName.StdlibFnName) : PT.FQFnName.StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

  module UserFnName =
    let toPT (name : ST.FQFnName.UserFnName) : PT.FQFnName.UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

  let toPT (fqfn : ST.FQFnName.T) : PT.FQFnName.T =
    match fqfn with
    | ST.FQFnName.Stdlib fn -> PT.FQFnName.Stdlib(StdlibFnName.toPT fn)
    | ST.FQFnName.User fn -> PT.FQFnName.User(UserFnName.toPT fn)
    | ST.FQFnName.Package p -> PT.FQFnName.Package(PackageFnName.toPT p)

module InfixFnName =
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
  let rec toPT (t : ST.TypeReference) : PT.TypeReference =
    match t with
    | ST.TInt -> PT.TInt
    | ST.TFloat -> PT.TFloat
    | ST.TBool -> PT.TBool
    | ST.TUnit -> PT.TUnit
    | ST.TString -> PT.TString
    | ST.TList typ -> PT.TList(toPT typ)
    | ST.TTuple (firstType, secondType, otherTypes) ->
      PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
    | ST.TDict typ -> PT.TDict(toPT typ)
    | ST.TDB typ -> PT.TDB(toPT typ)
    | ST.TDateTime -> PT.TDateTime
    | ST.TChar -> PT.TChar
    | ST.TPassword -> PT.TPassword
    | ST.TUuid -> PT.TUuid
    | ST.TOption typ -> PT.TOption(toPT typ)
    | ST.TCustomType (t, typeArgs) ->
      PT.TCustomType(FQTypeName.toPT t, List.map toPT typeArgs)
    | ST.TBytes -> PT.TBytes
    | ST.TResult (okType, errType) -> PT.TResult(toPT okType, toPT errType)
    | ST.TVariable (name) -> PT.TVariable(name)
    | ST.TFn (paramTypes, returnType) ->
      PT.TFn(List.map toPT paramTypes, toPT returnType)


module BinaryOperation =
  let toPT (binop : ST.BinaryOperation) : PT.BinaryOperation =
    match binop with
    | ST.BinOpAnd -> PT.BinOpAnd
    | ST.BinOpOr -> PT.BinOpOr


module Infix =
  let toPT (infix : ST.Infix) : PT.Infix =
    match infix with
    | ST.InfixFnCall (fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
    | ST.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

module LetPattern =
  let rec toPT (p : ST.LetPattern) : PT.LetPattern =
    match p with
    | ST.LPVariable (id, str) -> PT.LPVariable(id, str)
    | ST.LPTuple (id, first, second, theRest) ->
      PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)

module MatchPattern =
  let rec toPT (p : ST.MatchPattern) : PT.MatchPattern =
    match p with
    | ST.MPVariable (id, str) -> PT.MPVariable(id, str)
    | ST.MPEnum (id, caseName, fieldPats) ->
      PT.MPEnum(id, caseName, List.map toPT fieldPats)
    | ST.MPInt (id, i) -> PT.MPInt(id, i)
    | ST.MPBool (id, b) -> PT.MPBool(id, b)
    | ST.MPChar (id, c) -> PT.MPChar(id, c)
    | ST.MPString (id, s) -> PT.MPString(id, s)
    | ST.MPFloat (id, s, w, f) -> PT.MPFloat(id, s, w, f)
    | ST.MPUnit id -> PT.MPUnit id
    | ST.MPTuple (id, first, second, theRest) ->
      PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)
    | ST.MPList (id, pats) -> PT.MPList(id, List.map toPT pats)



module Expr =
  let rec toPT (e : ST.Expr) : PT.Expr =
    match e with
    | ST.EChar (id, char) -> PT.EChar(id, char)
    | ST.EInt (id, num) -> PT.EInt(id, num)
    | ST.EString (id, segment) -> PT.EString(id, List.map stringSegmentToPT segment)
    | ST.EFloat (id, sign, whole, fraction) -> PT.EFloat(id, sign, whole, fraction)
    | ST.EBool (id, b) -> PT.EBool(id, b)
    | ST.EUnit id -> PT.EUnit id
    | ST.EVariable (id, var) -> PT.EVariable(id, var)
    | ST.EFieldAccess (id, obj, fieldname) ->
      PT.EFieldAccess(id, toPT obj, fieldname)
    | ST.EFnCall (id, name, typeArgs, args) ->
      PT.EFnCall(
        id,
        FQFnName.toPT name,
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | ST.ELambda (id, vars, body) -> PT.ELambda(id, vars, toPT body)
    | ST.ELet (id, pat, rhs, body) ->
      PT.ELet(id, LetPattern.toPT pat, toPT rhs, toPT body)
    | ST.EIf (id, cond, thenExpr, elseExpr) ->
      PT.EIf(id, toPT cond, toPT thenExpr, toPT elseExpr)
    | ST.EList (id, exprs) -> PT.EList(id, List.map toPT exprs)
    | ST.ETuple (id, first, second, theRest) ->
      PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
    | ST.ERecord (id, typeName, fields) ->
      PT.ERecord(
        id,
        FQTypeName.toPT typeName,
        List.map (Tuple2.mapSecond toPT) fields
      )
    | ST.EPipe (pipeID, expr1, expr2, rest) ->
      PT.EPipe(pipeID, toPT expr1, toPT expr2, List.map toPT rest)
    | ST.EEnum (id, typeName, caseName, exprs) ->
      PT.EEnum(id, FQTypeName.toPT typeName, caseName, List.map toPT exprs)
    | ST.EMatch (id, mexpr, pairs) ->
      PT.EMatch(
        id,
        toPT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toPT << Tuple2.mapSecond toPT) pairs
      )
    | ST.EPipeTarget id -> PT.EPipeTarget id
    | ST.EInfix (id, infix, arg1, arg2) ->
      PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)
    | ST.EDict (id, pairs) -> PT.EDict(id, List.map (Tuple2.mapSecond toPT) pairs)

  and stringSegmentToPT (segment : ST.StringSegment) : PT.StringSegment =
    match segment with
    | ST.StringText text -> PT.StringText text
    | ST.StringInterpolation expr -> PT.StringInterpolation(toPT expr)


module CustomType =
  module EnumField =
    let toPT (f : ST.CustomType.EnumField) : PT.CustomType.EnumField =
      { typ = TypeReference.toPT f.typ; label = f.label }

  module EnumCase =
    let toPT (c : ST.CustomType.EnumCase) : PT.CustomType.EnumCase =
      { name = c.name; fields = List.map EnumField.toPT c.fields }

  module RecordField =
    let toPT (f : ST.CustomType.RecordField) : PT.CustomType.RecordField =
      { name = f.name; typ = TypeReference.toPT f.typ }

  let toPT (d : ST.CustomType.T) : PT.CustomType.T =
    match d with
    | ST.CustomType.Record (firstField, additionalFields) ->
      PT.CustomType.Record(
        RecordField.toPT firstField,
        List.map RecordField.toPT additionalFields
      )
    | ST.CustomType.Enum (firstCase, additionalCases) ->
      PT.CustomType.Enum(
        EnumCase.toPT firstCase,
        List.map EnumCase.toPT additionalCases
      )


module Handler =

  module CronInterval =
    let toPT (ci : ST.Handler.CronInterval) : PT.Handler.CronInterval =
      match ci with
      | ST.Handler.EveryDay -> PT.Handler.EveryDay
      | ST.Handler.EveryWeek -> PT.Handler.EveryWeek
      | ST.Handler.EveryFortnight -> PT.Handler.EveryFortnight
      | ST.Handler.EveryHour -> PT.Handler.EveryHour
      | ST.Handler.Every12Hours -> PT.Handler.Every12Hours
      | ST.Handler.EveryMinute -> PT.Handler.EveryMinute

  module Spec =
    let toPT (s : ST.Handler.Spec) : PT.Handler.Spec =
      match s with
      | ST.Handler.HTTP (route, method) -> PT.Handler.HTTP(route, method)
      | ST.Handler.Worker name -> PT.Handler.Worker name
      | ST.Handler.Cron (name, interval) ->
        PT.Handler.Cron(name, CronInterval.toPT interval)
      | ST.Handler.REPL name -> PT.Handler.REPL name

  let toPT (h : ST.Handler.T) : PT.Handler.T =
    { tlid = h.tlid; ast = Expr.toPT h.ast; spec = Spec.toPT h.spec }

module DB =
  let toPT (db : ST.DB.T) : PT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toPT db.typ }

module UserType =
  let toPT (t : ST.UserType.T) : PT.UserType.T =
    { tlid = t.tlid
      name = FQTypeName.UserTypeName.toPT t.name
      definition = CustomType.toPT t.definition }

module UserFunction =
  module Parameter =
    let toPT (p : ST.UserFunction.Parameter) : PT.UserFunction.Parameter =
      { id = p.id
        name = p.name
        typ = TypeReference.toPT p.typ
        description = p.description }

  let toPT (f : ST.UserFunction.T) : PT.UserFunction.T =
    { tlid = f.tlid
      name = FQFnName.UserFnName.toPT f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toPT f.parameters
      returnType = TypeReference.toPT f.returnType
      description = f.description
      infix = f.infix
      body = Expr.toPT f.body }

module Toplevel =
  let toPT (tl : ST.Toplevel.T) : PT.Toplevel.T =
    match tl with
    | ST.Toplevel.TLHandler h -> PT.Toplevel.TLHandler(Handler.toPT h)
    | ST.Toplevel.TLDB db -> PT.Toplevel.TLDB(DB.toPT db)
    | ST.Toplevel.TLFunction f -> PT.Toplevel.TLFunction(UserFunction.toPT f)
    | ST.Toplevel.TLType ut -> PT.Toplevel.TLType(UserType.toPT ut)

module Op =
  let toPT (op : ST.Op) : Option<PT.Op> =
    match op with
    | ST.SetHandler (handler) -> Some(PT.SetHandler(Handler.toPT handler))
    | ST.CreateDB (tlid, name, typ) ->
      Some(PT.CreateDB(tlid, name, TypeReference.toPT typ))
    | ST.DeleteTL tlid -> Some(PT.DeleteTL tlid)
    | ST.SetFunction fn -> Some(PT.SetFunction(UserFunction.toPT fn))
    | ST.UndoTL tlid -> Some(PT.UndoTL tlid)
    | ST.RedoTL tlid -> Some(PT.RedoTL tlid)
    | ST.SetExpr (tlid, id, e) -> Some(PT.SetExpr(tlid, id, Expr.toPT e))
    | ST.TLSavepoint tlid -> Some(PT.TLSavepoint tlid)
    | ST.DeleteFunction tlid -> Some(PT.DeleteFunction tlid)
    | ST.RenameDB (tlid, string) -> Some(PT.RenameDB(tlid, string))
    | ST.SetType tipe -> Some(PT.SetType(UserType.toPT tipe))
    | ST.DeleteType tlid -> Some(PT.DeleteType tlid)
