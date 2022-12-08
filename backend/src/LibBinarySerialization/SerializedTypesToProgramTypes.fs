/// Conversion functions from SerializedTypes to ProgramTypes
module LibBinarySerialization.SerializedTypesToProgramTypes

open Prelude
open Tablecloth

// Used for conversion functions
module ST = SerializedTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

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
    // CLEANUP We accidentally parsed user functions with versions (that is, named
    // like "myFunction_v2") into StdLib functions. Those names are saved in the
    // serialized opcodes in the DB, and need to be migrated. However, we can
    // identify them and convert them back to UserFunctions here
    | ST.FQFnName.Stdlib fn when
      fn.module_ = ""
      && not (Set.contains fn.function_ PTParser.FQFnName.oneWordFunctions)
      && not (LibExecutionStdLib.StdLib.isInfixName fn.module_ fn.function_)
      ->
      // It must have had a "_v0" or similar to get here, so always print the
      // version, even if it's 0
      PT.FQFnName.User($"{fn.function_}_v{fn.version}")
    | ST.FQFnName.Stdlib fn -> PT.FQFnName.Stdlib(StdlibFnName.toPT fn)
    | ST.FQFnName.Package p -> PT.FQFnName.Package(PackageFnName.toPT p)


module SendToRail =
  let toPT (ster : ST.SendToRail) : PT.SendToRail =
    match ster with
    | ST.Rail -> PT.Rail
    | ST.NoRail -> PT.NoRail

module MatchPattern =
  let rec toPT (p : ST.MatchPattern) : PT.MatchPattern =
    match p with
    | ST.MPVariable (id, str) -> PT.MPVariable(id, str)
    | ST.MPConstructor (id, name, pats) ->
      PT.MPConstructor(id, name, List.map toPT pats)
    | ST.MPInteger (id, i) -> PT.MPInteger(id, i)
    | ST.MPBool (id, b) -> PT.MPBool(id, b)
    | ST.MPCharacter (id, c) -> PT.MPCharacter(id, c)
    | ST.MPString (id, s) -> PT.MPString(id, s)
    | ST.MPFloat (id, s, w, f) -> PT.MPFloat(id, s, w, f)
    | ST.MPNull id -> PT.MPNull id
    | ST.MPBlank id -> PT.MPBlank id
    | ST.MPTuple (id, first, second, theRest) ->
      PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)



module Expr =
  let rec toPT (e : ST.Expr) : PT.Expr =
    match e with
    | ST.EBlank id -> PT.EBlank id
    | ST.ECharacter (id, char) -> PT.ECharacter(id, char)
    | ST.EInteger (id, num) -> PT.EInteger(id, num)
    | ST.EString (id, str) -> PT.EString(id, str)
    | ST.EFloat (id, sign, whole, fraction) -> PT.EFloat(id, sign, whole, fraction)
    | ST.EBool (id, b) -> PT.EBool(id, b)
    | ST.ENull id -> PT.ENull id
    | ST.EVariable (id, var) -> PT.EVariable(id, var)
    | ST.EFieldAccess (id, obj, fieldname) ->
      PT.EFieldAccess(id, toPT obj, fieldname)
    | ST.EFnCall (id, name, args, ster) ->
      PT.EFnCall(id, FQFnName.toPT name, List.map toPT args, SendToRail.toPT ster)
    | ST.EBinOp (id, ST.FQFnName.Stdlib fn, arg1, arg2, ster) ->
      // CLEANUP remove Date by making Date not allowed anymore
      assertIn
        "serialized binop should have blank/Date module"
        [ ""; "Date" ]
        fn.module_
      assertEq "serialized binop should have zero version" 0 fn.version
      let isInfix = LibExecutionStdLib.StdLib.isInfixName
      assertFn2 "serialized binop should be infix" isInfix fn.module_ fn.function_
      let module_ = if fn.module_ = "" then None else Some fn.module_
      PT.EBinOp(
        id,
        { module_ = module_; function_ = fn.function_ },
        toPT arg1,
        toPT arg2,
        SendToRail.toPT ster
      )
    // CLEANUP remove from format
    | ST.EBinOp (_, ST.FQFnName.User name, _, _, _) ->
      Exception.raiseInternal "userfn serialized as a binop" [ "name", name ]
    | ST.EBinOp (_, ST.FQFnName.Package name, _, _, _) ->
      Exception.raiseInternal "package serialized as a binop" [ "name", name ]
    | ST.ELambda (id, vars, body) -> PT.ELambda(id, vars, toPT body)
    | ST.ELet (id, lhs, rhs, body) -> PT.ELet(id, lhs, toPT rhs, toPT body)
    | ST.EIf (id, cond, thenExpr, elseExpr) ->
      PT.EIf(id, toPT cond, toPT thenExpr, toPT elseExpr)
    | ST.EPartial (id, str, expr) -> PT.EPartial(id, str, toPT expr)
    | ST.ERightPartial (id, str, expr) -> PT.ERightPartial(id, str, toPT expr)
    | ST.ELeftPartial (id, str_, expr) -> PT.ELeftPartial(id, str_, toPT expr)

    | ST.EList (id, exprs) -> PT.EList(id, List.map toPT exprs)
    | ST.ETuple (id, first, second, theRest) ->
      PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
    | ST.ERecord (id, pairs) ->
      PT.ERecord(id, List.map (Tuple2.mapSecond toPT) pairs)
    | ST.EPipe (pipeID, expr1, expr2, rest) ->
      PT.EPipe(pipeID, toPT expr1, toPT expr2, List.map toPT rest)
    | ST.EConstructor (id, name, exprs) ->
      PT.EConstructor(id, name, List.map toPT exprs)
    | ST.EMatch (id, mexpr, pairs) ->
      PT.EMatch(
        id,
        toPT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toPT << Tuple2.mapSecond toPT) pairs
      )
    | ST.EPipeTarget id -> PT.EPipeTarget id
    | ST.EFeatureFlag (id, name, cond, caseA, caseB) ->
      PT.EFeatureFlag(id, name, toPT cond, toPT caseA, toPT caseB)
    | ST.EAnd (id, left, right) -> PT.EAnd(id, toPT left, toPT right)
    | ST.EOr (id, left, right) -> PT.EOr(id, toPT left, toPT right)

module DType =
  let rec toPT (t : ST.DType) : PT.DType =
    match t with
    | ST.TInt -> PT.TInt
    | ST.TFloat -> PT.TFloat
    | ST.TBool -> PT.TBool
    | ST.TNull -> PT.TNull
    | ST.TStr -> PT.TStr
    | ST.TList typ -> PT.TList(toPT typ)
    | ST.TTuple (firstType, secondType, otherTypes) ->
      PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
    | ST.TDict typ -> PT.TDict(toPT typ)
    | ST.TIncomplete -> PT.TIncomplete
    | ST.TError -> PT.TError
    | ST.THttpResponse typ -> PT.THttpResponse(toPT typ)
    | ST.TDB typ -> PT.TDB(toPT typ)
    | ST.TDate -> PT.TDate
    | ST.TChar -> PT.TChar
    | ST.TPassword -> PT.TPassword
    | ST.TUuid -> PT.TUuid
    | ST.TOption typ -> PT.TOption(toPT typ)
    | ST.TErrorRail -> PT.TErrorRail
    | ST.TUserType (name, version) -> PT.TUserType(name, version)
    | ST.TBytes -> PT.TBytes
    | ST.TResult (okType, errType) -> PT.TResult(toPT okType, toPT errType)
    | ST.TVariable (name) -> PT.TVariable(name)
    | ST.TFn (paramTypes, returnType) ->
      PT.TFn(List.map toPT paramTypes, toPT returnType)
    | ST.TRecord (rows) ->
      PT.TRecord(List.map (fun (f, t : ST.DType) -> f, toPT t) rows)
    | ST.TDbList typ -> PT.TDbList(toPT typ)


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
      | ST.Handler.HTTPBasic (route, method, ids) ->
        PT.Handler.HTTPBasic(route, method, IDs.toPT ids)
      | ST.Handler.Worker (name, ids) -> PT.Handler.Worker(name, IDs.toPT ids)
      | ST.Handler.OldWorker (modulename, name, ids) ->
        PT.Handler.OldWorker(modulename, name, IDs.toPT ids)
      | ST.Handler.Cron (name, interval, ids) ->
        PT.Handler.Cron(name, interval |> Option.map CronInterval.toPT, IDs.toPT ids)
      | ST.Handler.REPL (name, ids) -> PT.Handler.REPL(name, IDs.toPT ids)
      | ST.Handler.UnknownHandler (name, modifier, ids) ->
        PT.Handler.UnknownHandler(name, modifier, IDs.toPT ids)

  let toPT (h : ST.Handler.T) : PT.Handler.T =
    { tlid = h.tlid
      ast = Expr.toPT h.ast
      spec = Spec.toPT h.spec
      pos = { x = h.pos.x; y = h.pos.y } }

module DB =
  let toPT (db : ST.DB.T) : PT.DB.T =
    { tlid = db.tlid
      pos = { x = db.pos.x; y = db.pos.y }
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
  module Definition =
    let toPT (d : ST.UserType.Definition) : PT.UserType.Definition =
      match d with
      | ST.UserType.Record fields ->
        PT.UserType.Record(
          List.map
            (fun (rf : ST.UserType.RecordField) ->
              { name = rf.name
                nameID = rf.nameID
                typ = Option.map DType.toPT rf.typ
                typeID = rf.typeID })
            fields
        )

  let toPT (t : ST.UserType.T) : PT.UserType.T =
    { tlid = t.tlid
      nameID = t.nameID
      name = t.name
      version = t.version
      definition = Definition.toPT t.definition }

module UserFunction =
  module Parameter =
    let toPT (p : ST.UserFunction.Parameter) : PT.UserFunction.Parameter =
      { name = p.name
        nameID = p.nameID
        typ = p.typ |> Option.map DType.toPT
        typeID = p.typeID
        description = p.description }

  let toPT (f : ST.UserFunction.T) : PT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      nameID = f.nameID
      parameters = List.map Parameter.toPT f.parameters
      returnType = DType.toPT f.returnType
      returnTypeID = f.returnTypeID
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
    | ST.SetHandler (tlid, pos, handler) ->
      let position : PT.Position = { x = pos.x; y = pos.y }
      Some(PT.SetHandler(tlid, position, Handler.toPT handler))
    | ST.CreateDB (tlid, pos, name) ->
      let position : PT.Position = { x = pos.x; y = pos.y }
      Some(PT.CreateDB(tlid, position, name))
    | ST.AddDBCol (tlid, id1, id2) -> Some(PT.AddDBCol(tlid, id1, id2))
    | ST.SetDBColName (tlid, id, name) -> Some(PT.SetDBColName(tlid, id, name))
    | ST.SetDBColType (tlid, id, string) -> Some(PT.SetDBColType(tlid, id, string))
    | ST.DeleteTL tlid -> Some(PT.DeleteTL tlid)
    | ST.MoveTL (tlid, pos) ->
      let position : PT.Position = { x = pos.x; y = pos.y }
      Some(PT.MoveTL(tlid, position))
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
    | ST.CreateDBWithBlankOr (tlid, pos, id, string) ->
      let position : PT.Position = { x = pos.x; y = pos.y }
      Some(PT.CreateDBWithBlankOr(tlid, position, id, string))
    | ST.SetType tipe -> Some(PT.SetType(UserType.toPT tipe))
    | ST.DeleteType tlid -> Some(PT.DeleteType tlid)
    | ST.DeleteTLForever _
    | ST.DeleteFunctionForever _
    | ST.DeleteTypeForever _ -> None
