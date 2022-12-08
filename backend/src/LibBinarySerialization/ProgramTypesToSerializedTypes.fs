/// Conversion functions from ProgramTypes to SerializedTypes
module LibBinarySerialization.ProgramTypesToSerializedTypes

open Prelude
open Tablecloth

// Used for conversion functions
module ST = SerializedTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

module FQFnName =
  module PackageFnName =
    let toST (name : PT.FQFnName.PackageFnName) : ST.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    let toST (name : PT.FQFnName.StdlibFnName) : ST.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

  let toST (fqfn : PT.FQFnName.T) : ST.FQFnName.T =
    match fqfn with
    | PT.FQFnName.User u -> ST.FQFnName.User u
    | PT.FQFnName.Stdlib fn -> ST.FQFnName.Stdlib(StdlibFnName.toST fn)
    | PT.FQFnName.Package p -> ST.FQFnName.Package(PackageFnName.toST p)


module SendToRail =
  let toST (ster : PT.SendToRail) : ST.SendToRail =
    match ster with
    | PT.Rail -> ST.Rail
    | PT.NoRail -> ST.NoRail

module MatchPattern =
  let rec toST (p : PT.MatchPattern) : ST.MatchPattern =
    match p with
    | PT.MPVariable (id, str) -> ST.MPVariable(id, str)
    | PT.MPConstructor (id, name, pats) ->
      ST.MPConstructor(id, name, List.map toST pats)
    | PT.MPInteger (id, i) -> ST.MPInteger(id, i)
    | PT.MPBool (id, b) -> ST.MPBool(id, b)
    | PT.MPCharacter (id, c) -> ST.MPCharacter(id, c)
    | PT.MPString (id, s) -> ST.MPString(id, s)
    | PT.MPFloat (id, s, w, f) -> ST.MPFloat(id, s, w, f)
    | PT.MPNull id -> ST.MPNull id
    | PT.MPBlank id -> ST.MPBlank id
    | PT.MPTuple (id, first, second, theRest) ->
      ST.MPTuple(id, toST first, toST second, List.map toST theRest)



module Expr =
  let rec toST (e : PT.Expr) : ST.Expr =
    match e with
    | PT.EBlank id -> ST.EBlank id
    | PT.ECharacter (id, char) -> ST.ECharacter(id, char)
    | PT.EInteger (id, num) -> ST.EInteger(id, num)
    | PT.EString (id, str) -> ST.EString(id, str)
    | PT.EFloat (id, sign, whole, fraction) -> ST.EFloat(id, sign, whole, fraction)
    | PT.EBool (id, b) -> ST.EBool(id, b)
    | PT.ENull id -> ST.ENull id
    | PT.EVariable (id, var) -> ST.EVariable(id, var)
    | PT.EFieldAccess (id, obj, fieldname) ->
      ST.EFieldAccess(id, toST obj, fieldname)
    | PT.EFnCall (id, name, args, ster) ->
      ST.EFnCall(id, FQFnName.toST name, List.map toST args, SendToRail.toST ster)
    | PT.EBinOp (id, name, arg1, arg2, ster) ->
      let module_ = Option.unwrap "" name.module_
      let isInfix = LibExecutionStdLib.StdLib.isInfixName
      assertFn2 "is a binop" isInfix module_ name.function_
      let name =
        ST.FQFnName.Stdlib
          { module_ = module_; function_ = name.function_; version = 0 }
      ST.EBinOp(id, name, toST arg1, toST arg2, SendToRail.toST ster)
    | PT.ELambda (id, vars, body) -> ST.ELambda(id, vars, toST body)
    | PT.ELet (id, lhs, rhs, body) -> ST.ELet(id, lhs, toST rhs, toST body)
    | PT.EIf (id, cond, thenExpr, elseExpr) ->
      ST.EIf(id, toST cond, toST thenExpr, toST elseExpr)
    | PT.EPartial (id, str, expr) -> ST.EPartial(id, str, toST expr)
    | PT.ERightPartial (id, str, expr) -> ST.ERightPartial(id, str, toST expr)
    | PT.ELeftPartial (id, str_, expr) -> ST.ELeftPartial(id, str_, toST expr)

    | PT.EList (id, exprs) -> ST.EList(id, List.map toST exprs)
    | PT.ETuple (id, first, second, theRest) ->
      ST.ETuple(id, toST first, toST second, List.map toST theRest)
    | PT.ERecord (id, pairs) ->
      ST.ERecord(id, List.map (Tuple2.mapSecond toST) pairs)
    | PT.EPipe (pipeID, expr1, expr2, rest) ->
      ST.EPipe(pipeID, toST expr1, toST expr2, List.map toST rest)
    | PT.EConstructor (id, name, exprs) ->
      ST.EConstructor(id, name, List.map toST exprs)
    | PT.EMatch (id, mexpr, pairs) ->
      ST.EMatch(
        id,
        toST mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toST << Tuple2.mapSecond toST) pairs
      )
    | PT.EPipeTarget id -> ST.EPipeTarget id
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
      ST.EFeatureFlag(id, name, toST cond, toST caseA, toST caseB)
    | PT.EAnd (id, expr1, expr2) -> ST.EAnd(id, toST expr1, toST expr2)
    | PT.EOr (id, expr1, expr2) -> ST.EOr(id, toST expr1, toST expr2)

module DType =
  let rec toST (t : PT.DType) : ST.DType =
    match t with
    | PT.TInt -> ST.TInt
    | PT.TFloat -> ST.TFloat
    | PT.TBool -> ST.TBool
    | PT.TNull -> ST.TNull
    | PT.TStr -> ST.TStr
    | PT.TList typ -> ST.TList(toST typ)
    | PT.TTuple (first, second, theRest) ->
      ST.TTuple(toST first, toST second, List.map toST theRest)
    | PT.TDict typ -> ST.TDict(toST typ)
    | PT.TIncomplete -> ST.TIncomplete
    | PT.TError -> ST.TError
    | PT.THttpResponse typ -> ST.THttpResponse(toST typ)
    | PT.TDB typ -> ST.TDB(toST typ)
    | PT.TDate -> ST.TDate
    | PT.TChar -> ST.TChar
    | PT.TPassword -> ST.TPassword
    | PT.TUuid -> ST.TUuid
    | PT.TOption typ -> ST.TOption(toST typ)
    | PT.TErrorRail -> ST.TErrorRail
    | PT.TUserType (name, version) -> ST.TUserType(name, version)
    | PT.TBytes -> ST.TBytes
    | PT.TResult (okType, errType) -> ST.TResult(toST okType, toST errType)
    | PT.TVariable (name) -> ST.TVariable(name)
    | PT.TFn (paramTypes, returnType) ->
      ST.TFn(List.map toST paramTypes, toST returnType)
    | PT.TRecord (rows) ->
      ST.TRecord(List.map (fun (f, t : PT.DType) -> f, toST t) rows)
    | PT.TDbList typ -> ST.TDbList(toST typ)


module Handler =
  module IDs =
    let toST (ids : PT.Handler.ids) : ST.Handler.ids =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }

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
      | PT.Handler.HTTP (route, method, ids) ->
        ST.Handler.HTTP(route, method, IDs.toST ids)
      | PT.Handler.HTTPBasic (route, method, ids) ->
        ST.Handler.HTTPBasic(route, method, IDs.toST ids)
      | PT.Handler.Worker (name, ids) -> ST.Handler.Worker(name, IDs.toST ids)
      | PT.Handler.OldWorker (modulename, name, ids) ->
        ST.Handler.OldWorker(modulename, name, IDs.toST ids)
      | PT.Handler.Cron (name, interval, ids) ->
        ST.Handler.Cron(name, interval |> Option.map CronInterval.toST, IDs.toST ids)
      | PT.Handler.REPL (name, ids) -> ST.Handler.REPL(name, IDs.toST ids)
      | PT.Handler.UnknownHandler (name, modifier, ids) ->
        ST.Handler.UnknownHandler(name, modifier, IDs.toST ids)

  let toST (h : PT.Handler.T) : ST.Handler.T =
    { tlid = h.tlid
      ast = Expr.toST h.ast
      spec = Spec.toST h.spec
      pos = { x = h.pos.x; y = h.pos.y } }

module DB =
  let toST (db : PT.DB.T) : ST.DB.T =
    { tlid = db.tlid
      pos = { x = db.pos.x; y = db.pos.y }
      name = db.name
      nameID = db.nameID
      version = db.version
      cols =
        List.map
          (fun (c : PT.DB.Col) ->
            { name = c.name
              nameID = c.nameID
              typ = Option.map DType.toST c.typ
              typeID = c.typeID })
          db.cols }

module UserType =
  module Definition =
    let toST (d : PT.UserType.Definition) : ST.UserType.Definition =
      match d with
      | PT.UserType.Record fields ->
        ST.UserType.Record(
          List.map
            (fun (rf : PT.UserType.RecordField) ->
              { name = rf.name
                nameID = rf.nameID
                typ = Option.map DType.toST rf.typ
                typeID = rf.typeID })
            fields
        )

  let toST (t : PT.UserType.T) : ST.UserType.T =
    { tlid = t.tlid
      nameID = t.nameID
      name = t.name
      version = t.version
      definition = Definition.toST t.definition }

module UserFunction =
  module Parameter =
    let toST (p : PT.UserFunction.Parameter) : ST.UserFunction.Parameter =
      { name = p.name
        nameID = p.nameID
        typ = p.typ |> Option.map DType.toST
        typeID = p.typeID
        description = p.description }

  let toST (f : PT.UserFunction.T) : ST.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      nameID = f.nameID
      parameters = List.map Parameter.toST f.parameters
      returnType = DType.toST f.returnType
      returnTypeID = f.returnTypeID
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
    | PT.SetHandler (tlid, pos, handler) ->
      let position : ST.Position = { x = pos.x; y = pos.y }
      ST.SetHandler(tlid, position, Handler.toST handler)
    | PT.CreateDB (tlid, pos, name) ->
      let position : ST.Position = { x = pos.x; y = pos.y }
      ST.CreateDB(tlid, position, name)
    | PT.AddDBCol (tlid, id1, id2) -> ST.AddDBCol(tlid, id1, id2)
    | PT.SetDBColName (tlid, id, name) -> ST.SetDBColName(tlid, id, name)
    | PT.SetDBColType (tlid, id, string) -> ST.SetDBColType(tlid, id, string)
    | PT.DeleteTL tlid -> ST.DeleteTL tlid
    | PT.MoveTL (tlid, pos) ->
      let position : ST.Position = { x = pos.x; y = pos.y }
      ST.MoveTL(tlid, position)
    | PT.SetFunction fn -> ST.SetFunction(UserFunction.toST fn)
    | PT.ChangeDBColName (tlid, id, string) -> ST.ChangeDBColName(tlid, id, string)
    | PT.ChangeDBColType (tlid, id, string) -> ST.ChangeDBColType(tlid, id, string)
    | PT.UndoTL tlid -> ST.UndoTL tlid
    | PT.RedoTL tlid -> ST.RedoTL tlid
    | PT.SetExpr (tlid, id, e) -> ST.SetExpr(tlid, id, Expr.toST e)
    | PT.TLSavepoint tlid -> ST.TLSavepoint tlid
    | PT.DeleteFunction tlid -> ST.DeleteFunction tlid
    | PT.DeleteDBCol (tlid, id) -> ST.DeleteDBCol(tlid, id)
    | PT.RenameDBname (tlid, string) -> ST.RenameDBname(tlid, string)
    | PT.CreateDBWithBlankOr (tlid, pos, id, string) ->
      let position : ST.Position = { x = pos.x; y = pos.y }
      ST.CreateDBWithBlankOr(tlid, position, id, string)
    | PT.SetType tipe -> ST.SetType(UserType.toST tipe)
    | PT.DeleteType tlid -> ST.DeleteType tlid
