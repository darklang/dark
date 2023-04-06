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
      { typ = u.typ; version = u.version }

  module StdlibTypeName =
    let toPT (s : ST.FQTypeName.StdlibTypeName) : PT.FQTypeName.StdlibTypeName =
      { typ = s.typ }

  let toPT (t : ST.FQTypeName.T) : PT.FQTypeName.T =
    match t with
    | ST.FQTypeName.Stdlib s -> PT.FQTypeName.Stdlib(StdlibTypeName.toPT s)
    | ST.FQTypeName.User u -> PT.FQTypeName.User(UserTypeName.toPT u)


module FQFnName =
  module PackageFnName =
    let toPT (name : ST.FQFnName.PackageFnName) : PT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    let toPT (name : ST.FQFnName.StdlibFnName) : PT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

  let toPT (fqfn : ST.FQFnName.T) : PT.FQFnName.T =
    match fqfn with
    | ST.FQFnName.User u -> PT.FQFnName.User u
    | ST.FQFnName.Stdlib fn -> PT.FQFnName.Stdlib(StdlibFnName.toPT fn)
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

module DType =
  let rec toPT (t : ST.DType) : PT.DType =
    match t with
    | ST.TInt -> PT.TInt
    | ST.TFloat -> PT.TFloat
    | ST.TBool -> PT.TBool
    | ST.TUnit -> PT.TUnit
    | ST.TStr -> PT.TStr
    | ST.TList typ -> PT.TList(toPT typ)
    | ST.TTuple (firstType, secondType, otherTypes) ->
      PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
    | ST.TDict typ -> PT.TDict(toPT typ)
    | ST.TIncomplete -> PT.TIncomplete
    | ST.TError -> PT.TError
    | ST.THttpResponse typ -> PT.THttpResponse(toPT typ)
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
    | ST.TRecord (rows) ->
      PT.TRecord(List.map (fun (f, t : ST.DType) -> f, toPT t) rows)
    | ST.TDbList typ -> PT.TDbList(toPT typ)


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

module MatchPattern =
  let rec toPT (p : ST.MatchPattern) : PT.MatchPattern =
    match p with
    | ST.MPVariable (id, str) -> PT.MPVariable(id, str)
    | ST.MPConstructor (id, caseName, fieldPats) ->
      PT.MPConstructor(id, caseName, List.map toPT fieldPats)
    | ST.MPInteger (id, i) -> PT.MPInteger(id, i)
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
    | ST.ECharacter (id, char) -> PT.ECharacter(id, char)
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
        List.map DType.toPT typeArgs,
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
        Option.map FQTypeName.toPT typeName,
        List.map (Tuple2.mapSecond toPT) fields
      )
    | ST.EPipe (pipeID, expr1, expr2, rest) ->
      PT.EPipe(pipeID, toPT expr1, toPT expr2, List.map toPT rest)
    | ST.EConstructor (id, typeName, caseName, exprs) ->
      PT.EConstructor(
        id,
        Option.map FQTypeName.toPT typeName,
        caseName,
        List.map toPT exprs
      )
    | ST.EMatch (id, mexpr, pairs) ->
      PT.EMatch(
        id,
        toPT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toPT << Tuple2.mapSecond toPT) pairs
      )
    | ST.EPipeTarget id -> PT.EPipeTarget id
    | ST.EFeatureFlag (id, name, cond, caseA, caseB) ->
      PT.EFeatureFlag(id, name, toPT cond, toPT caseA, toPT caseB)
    | ST.EInfix (id, infix, arg1, arg2) ->
      PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)

  and stringSegmentToPT (segment : ST.StringSegment) : PT.StringSegment =
    match segment with
    | ST.StringText text -> PT.StringText text
    | ST.StringInterpolation expr -> PT.StringInterpolation(toPT expr)


module CustomType =
  module EnumField =
    let toPT (f : ST.CustomType.EnumField) : PT.CustomType.EnumField =
      { id = f.id; typ = DType.toPT f.typ; label = f.label }

  module EnumCase =
    let toPT (c : ST.CustomType.EnumCase) : PT.CustomType.EnumCase =
      { id = c.id; name = c.name; fields = List.map EnumField.toPT c.fields }

  module RecordField =
    let toPT (f : ST.CustomType.RecordField) : PT.CustomType.RecordField =
      { id = f.id; name = f.name; typ = DType.toPT f.typ }

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
  module IDs =
    let toPT (ids : ST.Handler.ids) : PT.Handler.ids =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }

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
      | ST.Handler.HTTP (route, method, ids) ->
        PT.Handler.HTTP(route, method, IDs.toPT ids)
      | ST.Handler.Worker (name, ids) -> PT.Handler.Worker(name, IDs.toPT ids)
      | ST.Handler.Cron (name, interval, ids) ->
        PT.Handler.Cron(name, interval |> Option.map CronInterval.toPT, IDs.toPT ids)
      | ST.Handler.REPL (name, ids) -> PT.Handler.REPL(name, IDs.toPT ids)

  let toPT (h : ST.Handler.T) : PT.Handler.T =
    { tlid = h.tlid; ast = Expr.toPT h.ast; spec = Spec.toPT h.spec }

module DB =
  let toPT (db : ST.DB.T) : PT.DB.T =
    { tlid = db.tlid
      name = db.name
      nameID = db.nameID
      version = db.version
      cols =
        List.map
          (fun (c : ST.DB.Col) ->
            { name = c.name
              nameID = c.nameID
              typ = Option.map DType.toPT c.typ
              typeID = c.typeID })
          db.cols }

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
        typ = DType.toPT p.typ
        description = p.description }

  let toPT (f : ST.UserFunction.T) : PT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toPT f.parameters
      returnType = DType.toPT f.returnType
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
    | ST.CreateDB (tlid, name) -> Some(PT.CreateDB(tlid, name))
    | ST.AddDBCol (tlid, id1, id2) -> Some(PT.AddDBCol(tlid, id1, id2))
    | ST.SetDBColName (tlid, id, name) -> Some(PT.SetDBColName(tlid, id, name))
    | ST.SetDBColType (tlid, id, string) -> Some(PT.SetDBColType(tlid, id, string))
    | ST.DeleteTL tlid -> Some(PT.DeleteTL tlid)
    | ST.SetFunction fn -> Some(PT.SetFunction(UserFunction.toPT fn))
    | ST.ChangeDBColName (tlid, id, string) ->
      Some(PT.ChangeDBColName(tlid, id, string))
    | ST.ChangeDBColType (tlid, id, string) ->
      Some(PT.ChangeDBColType(tlid, id, string))
    | ST.UndoTL tlid -> Some(PT.UndoTL tlid)
    | ST.RedoTL tlid -> Some(PT.RedoTL tlid)
    | ST.SetExpr (tlid, id, e) -> Some(PT.SetExpr(tlid, id, Expr.toPT e))
    | ST.TLSavepoint tlid -> Some(PT.TLSavepoint tlid)
    | ST.DeleteFunction tlid -> Some(PT.DeleteFunction tlid)
    | ST.DeleteDBCol (tlid, id) -> Some(PT.DeleteDBCol(tlid, id))
    | ST.RenameDBname (tlid, string) -> Some(PT.RenameDBname(tlid, string))
    | ST.CreateDBWithBlankOr (tlid, id, string) ->
      Some(PT.CreateDBWithBlankOr(tlid, id, string))
    | ST.SetType tipe -> Some(PT.SetType(UserType.toPT tipe))
    | ST.DeleteType tlid -> Some(PT.DeleteType tlid)
    | ST.DeleteTLForever _
    | ST.DeleteFunctionForever _
    | ST.DeleteTypeForever _ -> None
