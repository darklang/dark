module ClientTypes2ExecutionTypes.ProgramTypes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module CTPT = ClientTypes.Program

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

module Position =
  let fromCT (pos : CTPT.Position) : PT.Position = { x = pos.x; y = pos.y }

  let toCT (pos : PT.Position) : CTPT.Position = { x = pos.x; y = pos.y }


module FQFnName =
  module StdlibFnName =
    let fromCT (name : CTPT.FQFnName.StdlibFnName) : PT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

    let toCT (name : PT.FQFnName.StdlibFnName) : CTPT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

  module InfixStdlibFnName =
    let fromCT
      (name : CTPT.FQFnName.InfixStdlibFnName)
      : PT.FQFnName.InfixStdlibFnName =
      { module_ = name.module_; function_ = name.function_ }

    let toCT
      (name : PT.FQFnName.InfixStdlibFnName)
      : CTPT.FQFnName.InfixStdlibFnName =
      { module_ = name.module_; function_ = name.function_ }


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


module MatchPattern =
  let rec fromCT (pat : CTPT.MatchPattern) : PT.MatchPattern =
    match pat with
    | CTPT.MPVariable (id, str) -> PT.MPVariable(id, str)
    | CTPT.MPConstructor (id, name, args) ->
      PT.MPConstructor(id, name, List.map fromCT args)
    | CTPT.MPInteger (id, i) -> PT.MPInteger(id, i)
    | CTPT.MPBool (id, b) -> PT.MPBool(id, b)
    | CTPT.MPCharacter (id, str) -> PT.MPCharacter(id, str)
    | CTPT.MPString (id, str) -> PT.MPString(id, str)
    | CTPT.MPFloat (id, sign, whole, frac) -> PT.MPFloat(id, sign, whole, frac)
    | CTPT.MPNull (id) -> PT.MPNull(id)
    | CTPT.MPBlank (id) -> PT.MPBlank(id)
    | CTPT.MPTuple (id, first, second, theRest) ->
      PT.MPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)

  let rec toCT (pat : PT.MatchPattern) : CTPT.MatchPattern =
    match pat with
    | PT.MPVariable (id, str) -> CTPT.MPVariable(id, str)
    | PT.MPConstructor (id, name, args) ->
      CTPT.MPConstructor(id, name, List.map toCT args)
    | PT.MPInteger (id, i) -> CTPT.MPInteger(id, i)
    | PT.MPBool (id, b) -> CTPT.MPBool(id, b)
    | PT.MPCharacter (id, str) -> CTPT.MPCharacter(id, str)
    | PT.MPString (id, str) -> CTPT.MPString(id, str)
    | PT.MPFloat (id, sign, whole, frac) -> CTPT.MPFloat(id, sign, whole, frac)
    | PT.MPNull (id) -> CTPT.MPNull(id)
    | PT.MPBlank (id) -> CTPT.MPBlank(id)
    | PT.MPTuple (id, first, second, theRest) ->
      CTPT.MPTuple(id, toCT first, toCT second, List.map toCT theRest)


module SendToRail =
  let fromCT (str : CTPT.SendToRail) : PT.SendToRail =
    match str with
    | CTPT.SendToRail.Rail -> PT.Rail
    | CTPT.SendToRail.NoRail -> PT.NoRail

  let toCT (str : PT.SendToRail) : CTPT.SendToRail =
    match str with
    | PT.Rail -> CTPT.SendToRail.Rail
    | PT.NoRail -> CTPT.SendToRail.NoRail

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
    | CTPT.InfixFnCall (name, ster) ->
      PT.InfixFnCall(
        FQFnName.InfixStdlibFnName.fromCT (name),
        SendToRail.fromCT ster
      )
    | CTPT.BinOp (op) -> PT.BinOp(BinaryOperation.fromCT op)

  let toCT (infix : PT.Infix) : CTPT.Infix =
    match infix with
    | PT.InfixFnCall (name, ster) ->
      CTPT.InfixFnCall(FQFnName.InfixStdlibFnName.toCT (name), SendToRail.toCT ster)
    | PT.BinOp (op) -> CTPT.BinOp(BinaryOperation.toCT op)

module Expr =
  let rec fromCT (expr : CTPT.Expr) : PT.Expr =
    match expr with
    | CTPT.Expr.EInteger (id, i) -> PT.EInteger(id, i)
    | CTPT.Expr.EBool (id, b) -> PT.EBool(id, b)
    | CTPT.Expr.EString (id, s) -> PT.EString(id, s)
    | CTPT.Expr.ECharacter (id, c) -> PT.ECharacter(id, c)
    | CTPT.Expr.EFloat (id, sign, whole, frac) -> PT.EFloat(id, sign, whole, frac)
    | CTPT.Expr.ENull (id) -> PT.ENull(id)
    | CTPT.Expr.EBlank (id) -> PT.EBlank(id)
    | CTPT.Expr.ELet (id, name, expr, body) ->
      PT.ELet(id, name, fromCT expr, fromCT body)
    | CTPT.Expr.EIf (id, cond, ifExpr, thenExpr) ->
      PT.EIf(id, fromCT cond, fromCT ifExpr, fromCT thenExpr)
    | CTPT.EInfix (id, infix, first, second) ->
      PT.EInfix(id, Infix.fromCT (infix), fromCT first, fromCT second)
    | CTPT.Expr.ELambda (id, args, body) -> PT.ELambda(id, args, fromCT body)
    | CTPT.Expr.EFieldAccess (id, expr, fieldName) ->
      PT.EFieldAccess(id, fromCT expr, fieldName)
    | CTPT.Expr.EVariable (id, name) -> PT.EVariable(id, name)
    | CTPT.Expr.EFnCall (id, fnName, args, str) ->
      PT.EFnCall(
        id,
        FQFnName.fromCT fnName,
        List.map fromCT args,
        SendToRail.fromCT str
      )
    | CTPT.Expr.EList (id, exprs) -> PT.EList(id, List.map fromCT exprs)
    | CTPT.Expr.ETuple (id, first, second, theRest) ->
      PT.ETuple(id, fromCT first, fromCT second, List.map fromCT theRest)
    | CTPT.Expr.ERecord (id, pairs) ->
      PT.ERecord(id, pairs |> List.map (fun (name, expr) -> (name, fromCT expr)))
    | CTPT.Expr.EPipe (id, expr1, expr2, exprs) ->
      PT.EPipe(id, fromCT expr1, fromCT expr2, List.map fromCT exprs)
    | CTPT.Expr.EConstructor (id, name, args) ->
      PT.EConstructor(id, name, List.map fromCT args)
    | CTPT.Expr.EMatch (id, matchExpr, cases) ->
      PT.EMatch(
        id,
        fromCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.fromCT pat, fromCT expr))
      )
    | CTPT.Expr.EPipeTarget (id) -> PT.EPipeTarget(id)
    | CTPT.Expr.EFeatureFlag (id, name, cond, caseA, caseB) ->
      PT.EFeatureFlag(id, name, fromCT cond, fromCT caseA, fromCT caseB)

  let rec toCT (expr : PT.Expr) : CTPT.Expr =
    match expr with
    | PT.EInteger (id, i) -> CTPT.Expr.EInteger(id, i)
    | PT.EBool (id, b) -> CTPT.Expr.EBool(id, b)
    | PT.EString (id, s) -> CTPT.Expr.EString(id, s)
    | PT.ECharacter (id, c) -> CTPT.Expr.ECharacter(id, c)
    | PT.EFloat (id, sign, whole, frac) -> CTPT.Expr.EFloat(id, sign, whole, frac)
    | PT.ENull (id) -> CTPT.Expr.ENull(id)
    | PT.EBlank (id) -> CTPT.Expr.EBlank(id)
    | PT.ELet (id, name, expr, body) ->
      CTPT.Expr.ELet(id, name, toCT expr, toCT body)
    | PT.EIf (id, cond, ifExpr, thenExpr) ->
      CTPT.Expr.EIf(id, toCT cond, toCT ifExpr, toCT thenExpr)
    | PT.EInfix (id, PT.InfixFnCall (name, str), first, second) ->
      CTPT.Expr.EInfix(
        id,
        CTPT.InfixFnCall(FQFnName.InfixStdlibFnName.toCT name, SendToRail.toCT str),
        toCT first,
        toCT second
      )
    | PT.EInfix (id, PT.BinOp op, first, second) ->
      CTPT.EInfix(id, CTPT.BinOp(BinaryOperation.toCT op), toCT first, toCT second)
    | PT.ELambda (id, args, body) -> CTPT.Expr.ELambda(id, args, toCT body)
    | PT.EFieldAccess (id, expr, fieldName) ->
      CTPT.Expr.EFieldAccess(id, toCT expr, fieldName)
    | PT.EVariable (id, name) -> CTPT.Expr.EVariable(id, name)
    | PT.EFnCall (id, fnName, args, str) ->
      CTPT.Expr.EFnCall(
        id,
        FQFnName.toCT fnName,
        List.map toCT args,
        SendToRail.toCT str
      )
    | PT.EList (id, exprs) -> CTPT.Expr.EList(id, List.map toCT exprs)
    | PT.ETuple (id, first, second, theRest) ->
      CTPT.Expr.ETuple(id, toCT first, toCT second, List.map toCT theRest)
    | PT.ERecord (id, pairs) ->
      CTPT.Expr.ERecord(
        id,
        pairs |> List.map (fun (name, expr) -> (name, toCT expr))
      )
    | PT.EPipe (id, expr1, expr2, exprs) ->
      CTPT.Expr.EPipe(id, toCT expr1, toCT expr2, List.map toCT exprs)
    | PT.EConstructor (id, name, args) ->
      CTPT.Expr.EConstructor(id, name, List.map toCT args)
    | PT.EMatch (id, matchExpr, cases) ->
      CTPT.Expr.EMatch(
        id,
        toCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.toCT pat, toCT expr))
      )
    | PT.EPipeTarget (id) -> CTPT.Expr.EPipeTarget(id)
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
      CTPT.Expr.EFeatureFlag(id, name, toCT cond, toCT caseA, toCT caseB)


module DType =
  let rec fromCT (dtype : CTPT.DType) : PT.DType =
    match dtype with
    | CTPT.DType.TInt -> PT.TInt
    | CTPT.DType.TFloat -> PT.TFloat
    | CTPT.DType.TBool -> PT.TBool
    | CTPT.DType.TNull -> PT.TNull
    | CTPT.DType.TStr -> PT.TStr
    | CTPT.DType.TList (t) -> PT.TList(fromCT t)
    | CTPT.DType.TTuple (first, second, theRest) ->
      PT.TTuple(fromCT first, fromCT second, List.map fromCT theRest)
    | CTPT.DType.TDict (t) -> PT.TDict(fromCT t)
    | CTPT.DType.TIncomplete -> PT.TIncomplete
    | CTPT.DType.TError -> PT.TError
    | CTPT.DType.THttpResponse (t) -> PT.THttpResponse(fromCT t)
    | CTPT.DType.TDB (t) -> PT.TDB(fromCT t)
    | CTPT.DType.TDate -> PT.TDate
    | CTPT.DType.TChar -> PT.TChar
    | CTPT.DType.TPassword -> PT.TPassword
    | CTPT.DType.TUuid -> PT.TUuid
    | CTPT.DType.TOption (t) -> PT.TOption(fromCT t)
    | CTPT.DType.TErrorRail -> PT.TErrorRail
    | CTPT.DType.TUserType (name, v) -> PT.TUserType(name, v)
    | CTPT.DType.TBytes -> PT.TBytes
    | CTPT.DType.TResult (ok, err) -> PT.TResult(fromCT ok, fromCT err)
    | CTPT.DType.TVariable (name) -> PT.TVariable(name)
    | CTPT.DType.TFn (args, body) -> PT.TFn(List.map fromCT args, fromCT body)
    | CTPT.DType.TRecord (pairs) ->
      PT.TRecord(pairs |> List.map (fun (name, v) -> (name, fromCT v)))
    | CTPT.DType.TDbList (t) -> PT.TDbList(fromCT t)

  let rec toCT (dtype : PT.DType) : CTPT.DType =
    match dtype with
    | PT.TInt -> CTPT.DType.TInt
    | PT.TFloat -> CTPT.DType.TFloat
    | PT.TBool -> CTPT.DType.TBool
    | PT.TNull -> CTPT.DType.TNull
    | PT.TStr -> CTPT.DType.TStr
    | PT.TList (t) -> CTPT.DType.TList(toCT t)
    | PT.TTuple (first, second, theRest) ->
      CTPT.DType.TTuple(toCT first, toCT second, List.map toCT theRest)
    | PT.TDict (t) -> CTPT.DType.TDict(toCT t)
    | PT.TIncomplete -> CTPT.DType.TIncomplete
    | PT.TError -> CTPT.DType.TError
    | PT.THttpResponse (t) -> CTPT.DType.THttpResponse(toCT t)
    | PT.TDB (t) -> CTPT.DType.TDB(toCT t)
    | PT.TDate -> CTPT.DType.TDate
    | PT.TChar -> CTPT.DType.TChar
    | PT.TPassword -> CTPT.DType.TPassword
    | PT.TUuid -> CTPT.DType.TUuid
    | PT.TOption (t) -> CTPT.DType.TOption(toCT t)
    | PT.TErrorRail -> CTPT.DType.TErrorRail
    | PT.TUserType (name, v) -> CTPT.DType.TUserType(name, v)
    | PT.TBytes -> CTPT.DType.TBytes
    | PT.TResult (ok, err) -> CTPT.DType.TResult(toCT ok, toCT err)
    | PT.TVariable (name) -> CTPT.DType.TVariable(name)
    | PT.TFn (args, body) -> CTPT.DType.TFn(List.map toCT args, toCT body)
    | PT.TRecord (pairs) ->
      CTPT.DType.TRecord(pairs |> List.map (fun (name, v) -> (name, toCT v)))
    | PT.TDbList (t) -> CTPT.DType.TDbList(toCT t)


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
      | CTPT.Handler.Spec.HTTPBasic (route, method, i) ->
        PT.Handler.HTTPBasic(route, method, ids.fromCT i)
      | CTPT.Handler.Spec.Worker (name, i) -> PT.Handler.Worker(name, ids.fromCT i)
      | CTPT.Handler.Spec.Cron (name, interval, i) ->
        PT.Handler.Cron(name, Option.map CronInterval.fromCT interval, ids.fromCT i)
      | CTPT.Handler.Spec.REPL (name, i) -> PT.Handler.REPL(name, ids.fromCT i)
      | CTPT.Handler.Spec.UnknownHandler (name, modifier, i) ->
        PT.Handler.UnknownHandler(name, modifier, ids.fromCT i)

    let toCT (spec : PT.Handler.Spec) : CTPT.Handler.Spec =
      match spec with
      | PT.Handler.HTTP (route, method, i) ->
        CTPT.Handler.Spec.HTTP(route, method, ids.toCT i)
      | PT.Handler.HTTPBasic (route, method, i) ->
        CTPT.Handler.Spec.HTTPBasic(route, method, ids.toCT i)
      | PT.Handler.Worker (name, i) -> CTPT.Handler.Spec.Worker(name, ids.toCT i)
      | PT.Handler.Cron (name, interval, i) ->
        CTPT.Handler.Spec.Cron(
          name,
          Option.map CronInterval.toCT interval,
          ids.toCT i
        )
      | PT.Handler.REPL (name, i) -> CTPT.Handler.Spec.REPL(name, ids.toCT i)
      | PT.Handler.UnknownHandler (name, modifier, i) ->
        CTPT.Handler.Spec.UnknownHandler(name, modifier, ids.toCT i)

  let fromCT (h : CTPT.Handler.T) : PT.Handler.T =
    { tlid = h.tlid
      pos = Position.fromCT h.pos
      ast = Expr.fromCT h.ast
      spec = Spec.fromCT h.spec }

  let toCT (h : PT.Handler.T) : CTPT.Handler.T =
    { tlid = h.tlid
      pos = Position.toCT h.pos
      ast = Expr.toCT h.ast
      spec = Spec.toCT h.spec }


module DB =
  module Col =
    let fromCT (col : CTPT.DB.Col) : PT.DB.Col =
      { name = col.name
        typ = Option.map DType.fromCT col.typ
        nameID = col.nameID
        typeID = col.typeID }

    let toCT (col : PT.DB.Col) : CTPT.DB.Col =
      { name = col.name
        typ = Option.map DType.toCT col.typ
        nameID = col.nameID
        typeID = col.typeID }

  let fromCT (db : CTPT.DB.T) : PT.DB.T =
    { tlid = db.tlid
      pos = Position.fromCT db.pos
      name = db.name
      nameID = db.nameID
      version = db.version
      cols = List.map Col.fromCT db.cols }

  let toCT (db : PT.DB.T) : CTPT.DB.T =
    { tlid = db.tlid
      pos = Position.toCT db.pos
      name = db.name
      nameID = db.nameID
      version = db.version
      cols = List.map Col.toCT db.cols }


module UserType =
  module RecordField =
    let fromCT (rf : CTPT.UserType.RecordField) : PT.UserType.RecordField =
      { name = rf.name
        typ = Option.map DType.fromCT rf.typ
        nameID = rf.nameID
        typeID = rf.typeID }

    let toCT (rf : PT.UserType.RecordField) : CTPT.UserType.RecordField =
      { name = rf.name
        typ = Option.map DType.toCT rf.typ
        nameID = rf.nameID
        typeID = rf.typeID }

  module Definition =
    let fromCT (def : CTPT.UserType.Definition) : PT.UserType.Definition =
      match def with
      | CTPT.UserType.Definition.Record fields ->
        PT.UserType.Record(List.map RecordField.fromCT fields)

    let toCT (def : PT.UserType.Definition) : CTPT.UserType.Definition =
      match def with
      | PT.UserType.Record fields ->
        CTPT.UserType.Definition.Record(List.map RecordField.toCT fields)

  let fromCT (ut : CTPT.UserType.T) : PT.UserType.T =
    { tlid = ut.tlid
      name = ut.name
      nameID = ut.nameID
      version = ut.version
      definition = Definition.fromCT ut.definition }

  let toCT (ut : PT.UserType.T) : CTPT.UserType.T =
    { tlid = ut.tlid
      name = ut.name
      nameID = ut.nameID
      version = ut.version
      definition = Definition.toCT ut.definition }


module UserFunction =
  module Parameter =
    let fromCT (p : CTPT.UserFunction.Parameter) : PT.UserFunction.Parameter =
      { name = p.name
        nameID = p.nameID
        typ = Option.map DType.fromCT p.typ
        typeID = p.typeID
        description = p.description }

    let toCT (p : PT.UserFunction.Parameter) : CTPT.UserFunction.Parameter =
      { name = p.name
        nameID = p.nameID
        typ = Option.map DType.toCT p.typ
        typeID = p.typeID
        description = p.description }

  let fromCT (uf : CTPT.UserFunction.T) : PT.UserFunction.T =
    { tlid = uf.tlid
      name = uf.name
      nameID = uf.nameID
      parameters = List.map Parameter.fromCT uf.parameters
      returnType = DType.fromCT uf.returnType
      returnTypeID = uf.returnTypeID
      description = uf.description
      infix = uf.infix
      body = Expr.fromCT uf.body }

  let toCT (uf : PT.UserFunction.T) : CTPT.UserFunction.T =
    { tlid = uf.tlid
      name = uf.name
      nameID = uf.nameID
      parameters = List.map Parameter.toCT uf.parameters
      returnType = DType.toCT uf.returnType
      returnTypeID = uf.returnTypeID
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
    | CTPT.Op.SetHandler (tlid, pos, handler) ->
      PT.Op.SetHandler(tlid, Position.fromCT pos, Handler.fromCT handler)
    | CTPT.Op.CreateDB (tlid, pos, name) ->
      PT.Op.CreateDB(tlid, Position.fromCT pos, name)
    | CTPT.Op.AddDBCol (dbid, colNameID, colTypeID) ->
      PT.Op.AddDBCol(dbid, colNameID, colTypeID)
    | CTPT.Op.SetDBColName (tlid, id, name) -> PT.Op.SetDBColName(tlid, id, name)
    | CTPT.Op.SetDBColType (tlid, id, tipe) -> PT.Op.SetDBColType(tlid, id, tipe)
    | CTPT.Op.DeleteTL (tlid) -> PT.Op.DeleteTL(tlid)
    | CTPT.Op.SetFunction (uf) -> PT.Op.SetFunction(UserFunction.fromCT uf)
    | CTPT.Op.ChangeDBColName (tlid, id, name) ->
      PT.Op.ChangeDBColName(tlid, id, name)
    | CTPT.Op.ChangeDBColType (tlid, id, tipe) ->
      PT.Op.ChangeDBColType(tlid, id, tipe)
    | CTPT.Op.UndoTL (tlid) -> PT.Op.UndoTL(tlid)
    | CTPT.Op.RedoTL (tlid) -> PT.Op.RedoTL(tlid)
    | CTPT.Op.SetExpr (tlid, id, expr) -> PT.Op.SetExpr(tlid, id, Expr.fromCT expr)
    | CTPT.Op.TLSavepoint (tlid) -> PT.Op.TLSavepoint(tlid)
    | CTPT.Op.DeleteFunction (tlid) -> PT.Op.DeleteFunction(tlid)
    | CTPT.Op.DeleteDBCol (tlid, colID) -> PT.Op.DeleteDBCol(tlid, colID)
    | CTPT.Op.RenameDBname (tlid, name) -> PT.Op.RenameDBname(tlid, name)
    | CTPT.Op.CreateDBWithBlankOr (tlid, pos, id, name) ->
      PT.Op.CreateDBWithBlankOr(tlid, Position.fromCT pos, id, name)
    | CTPT.Op.SetType (ut) -> PT.Op.SetType(UserType.fromCT ut)
    | CTPT.Op.DeleteType (tlid) -> PT.Op.DeleteType(tlid)

  let toCT (op : PT.Op) : CTPT.Op =
    match op with
    | PT.Op.SetHandler (tlid, pos, handler) ->
      CTPT.Op.SetHandler(tlid, Position.toCT pos, Handler.toCT handler)
    | PT.Op.CreateDB (tlid, pos, name) ->
      CTPT.Op.CreateDB(tlid, Position.toCT pos, name)
    | PT.Op.AddDBCol (dbid, colNameID, colTypeID) ->
      CTPT.Op.AddDBCol(dbid, colNameID, colTypeID)
    | PT.Op.SetDBColName (tlid, id, name) -> CTPT.Op.SetDBColName(tlid, id, name)
    | PT.Op.SetDBColType (tlid, id, tipe) -> CTPT.Op.SetDBColType(tlid, id, tipe)
    | PT.Op.DeleteTL (tlid) -> CTPT.Op.DeleteTL(tlid)
    | PT.Op.SetFunction (uf) -> CTPT.Op.SetFunction(UserFunction.toCT uf)
    | PT.Op.ChangeDBColName (tlid, id, name) ->
      CTPT.Op.ChangeDBColName(tlid, id, name)
    | PT.Op.ChangeDBColType (tlid, id, tipe) ->
      CTPT.Op.ChangeDBColType(tlid, id, tipe)
    | PT.Op.UndoTL (tlid) -> CTPT.Op.UndoTL(tlid)
    | PT.Op.RedoTL (tlid) -> CTPT.Op.RedoTL(tlid)
    | PT.Op.SetExpr (tlid, id, expr) -> CTPT.Op.SetExpr(tlid, id, Expr.toCT expr)
    | PT.Op.TLSavepoint (tlid) -> CTPT.Op.TLSavepoint(tlid)
    | PT.Op.DeleteFunction (tlid) -> CTPT.Op.DeleteFunction(tlid)
    | PT.Op.DeleteDBCol (tlid, colID) -> CTPT.Op.DeleteDBCol(tlid, colID)
    | PT.Op.RenameDBname (tlid, name) -> CTPT.Op.RenameDBname(tlid, name)
    | PT.Op.CreateDBWithBlankOr (tlid, pos, id, name) ->
      CTPT.Op.CreateDBWithBlankOr(tlid, Position.toCT pos, id, name)
    | PT.Op.SetType (ut) -> CTPT.Op.SetType(UserType.toCT ut)
    | PT.Op.DeleteType (tlid) -> CTPT.Op.DeleteType(tlid)


module Secret =
  let fromCT (s : CTPT.Secret) : PT.Secret.T = { name = s.name; value = s.value }

  let toCT (s : PT.Secret.T) : CTPT.Secret = { name = s.name; value = s.value }


module Package =
  module Parameter =
    let fromCT (p : CTPT.Package.Parameter) : PT.Package.Parameter =
      { name = p.name; typ = DType.fromCT p.typ; description = p.description }

    let toCT (p : PT.Package.Parameter) : CTPT.Package.Parameter =
      { name = p.name; typ = DType.toCT p.typ; description = p.description }

  module Fn =
    let fromCT (fn : CTPT.Package.Fn) : PT.Package.Fn =
      { name = FQFnName.PackageFnName.fromCT fn.name
        body = Expr.fromCT fn.body
        parameters = List.map Parameter.fromCT fn.parameters
        returnType = DType.fromCT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }

    let toCT (fn : PT.Package.Fn) : CTPT.Package.Fn =
      { name = FQFnName.PackageFnName.toCT fn.name
        body = Expr.toCT fn.body
        parameters = List.map Parameter.toCT fn.parameters
        returnType = DType.toCT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }
