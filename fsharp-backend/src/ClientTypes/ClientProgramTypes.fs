/// Runtime Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// These should all directly match `ProgramTypes.res` in `client`
/// See ProgramTypes.fs for documentation of these types
module ClientTypes.Program

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

module Position =
  type T = { x : int; y : int }

  let toPT (pos : T) : PT.Position = { x = pos.x; y = pos.y }

  let fromPT (pos : PT.Position) : T = { x = pos.x; y = pos.y }


module FQFnName =

  module StdlibFnName =
    type T = { module_ : string; function_ : string; version : int }

    let toPT (name : T) : PT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

    let fromPT (name : PT.FQFnName.StdlibFnName) : T =
      { module_ = name.module_; function_ = name.function_; version = name.version }

  module InfixStdlibFnName =
    type T = { module_ : Option<string>; function_ : string }

    let toPT (name : T) : PT.FQFnName.InfixStdlibFnName =
      { module_ = name.module_; function_ = name.function_ }

    let fromPT (name : PT.FQFnName.InfixStdlibFnName) : T =
      { module_ = name.module_; function_ = name.function_ }

  module UserFnName =
    type T = string

    let toPT (t : T) : PT.FQFnName.UserFnName = t

    let fromPT (t : PT.FQFnName.UserFnName) : T = t

  module PackageFnName =
    type T =
      { owner : string
        package : string
        module_ : string
        function_ : string
        version : int }

    let toPT (name : T) : PT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

    let fromPT (name : PT.FQFnName.PackageFnName) : T =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

  type T =
    | User of UserFnName.T
    | Stdlib of StdlibFnName.T
    | Package of PackageFnName.T

  let toPT (fqfn : T) : PT.FQFnName.T =
    match fqfn with
    | User u -> PT.FQFnName.User(UserFnName.toPT u)
    | Stdlib fn -> PT.FQFnName.Stdlib(StdlibFnName.toPT fn)
    | Package p -> PT.FQFnName.Package(PackageFnName.toPT p)

  let fromPT (fqfn : PT.FQFnName.T) : T =
    match fqfn with
    | PT.FQFnName.User u -> User(UserFnName.fromPT u)
    | PT.FQFnName.Stdlib fn -> Stdlib(StdlibFnName.fromPT fn)
    | PT.FQFnName.Package p -> Package(PackageFnName.fromPT p)


module Pattern =
  type T =
    | PVariable of id * string
    | PConstructor of id * string * List<T>
    | PInteger of id * int64
    | PBool of id * bool
    | PCharacter of id * string
    | PString of id * string
    | PFloat of id * Sign * string * string
    | PNull of id
    | PBlank of id
    | PTuple of id * T * T * List<T>

  let rec toPT (pat : T) : PT.Pattern =
    match pat with
    | PVariable (id, str) -> PT.PVariable(id, str)
    | PConstructor (id, name, args) -> PT.PConstructor(id, name, List.map toPT args)
    | PInteger (id, i) -> PT.PInteger(id, i)
    | PBool (id, b) -> PT.PBool(id, b)
    | PCharacter (id, str) -> PT.PCharacter(id, str)
    | PString (id, str) -> PT.PString(id, str)
    | PFloat (id, sign, whole, frac) -> PT.PFloat(id, sign, whole, frac)
    | PNull (id) -> PT.PNull(id)
    | PBlank (id) -> PT.PBlank(id)
    | PTuple (id, first, second, theRest) ->
      PT.PTuple(id, toPT first, toPT second, List.map toPT theRest)

  let rec fromPT (pat : PT.Pattern) : T =
    match pat with
    | PT.PVariable (id, str) -> PVariable(id, str)
    | PT.PConstructor (id, name, args) ->
      PConstructor(id, name, List.map fromPT args)
    | PT.PInteger (id, i) -> PInteger(id, i)
    | PT.PBool (id, b) -> PBool(id, b)
    | PT.PCharacter (id, str) -> PCharacter(id, str)
    | PT.PString (id, str) -> PString(id, str)
    | PT.PFloat (id, sign, whole, frac) -> PFloat(id, sign, whole, frac)
    | PT.PNull (id) -> PNull(id)
    | PT.PBlank (id) -> PBlank(id)
    | PT.PTuple (id, first, second, theRest) ->
      PTuple(id, fromPT first, fromPT second, List.map fromPT theRest)


module SendToRail =
  type T =
    | Rail
    | NoRail

  let toPT (str : T) : PT.SendToRail =
    match str with
    | Rail -> PT.Rail
    | NoRail -> PT.NoRail

  let fromPT (str : PT.SendToRail) : T =
    match str with
    | PT.Rail -> Rail
    | PT.NoRail -> NoRail

module Expr =
  type T =
    | EInteger of id * int64
    | EBool of id * bool
    | EString of id * string
    | ECharacter of id * string
    | EFloat of id * Sign * string * string
    | ENull of id
    | EBlank of id
    | ELet of id * string * T * T
    | EIf of id * T * T * T
    | EBinOp of id * FQFnName.InfixStdlibFnName.T * T * T * SendToRail.T
    | ELambda of id * List<id * string> * T
    | EFieldAccess of id * T * string
    | EVariable of id * string
    | EFnCall of id * FQFnName.T * List<T> * SendToRail.T
    | EPartial of id * string * T
    | ERightPartial of id * string * T
    | ELeftPartial of id * string * T
    | EList of id * List<T>
    | ETuple of id * T * T * List<T>
    | ERecord of id * List<string * T>
    | EPipe of id * T * T * List<T>
    | EConstructor of id * string * List<T>
    | EMatch of id * T * List<Pattern.T * T>
    | EPipeTarget of id
    | EFeatureFlag of id * string * T * T * T

  let rec toPT (expr : T) : PT.Expr =
    match expr with
    | EInteger (id, i) -> PT.EInteger(id, i)
    | EBool (id, b) -> PT.EBool(id, b)
    | EString (id, s) -> PT.EString(id, s)
    | ECharacter (id, c) -> PT.ECharacter(id, c)
    | EFloat (id, sign, whole, frac) -> PT.EFloat(id, sign, whole, frac)
    | ENull (id) -> PT.ENull(id)
    | EBlank (id) -> PT.EBlank(id)
    | ELet (id, name, expr, body) -> PT.ELet(id, name, toPT expr, toPT body)
    | EIf (id, cond, ifExpr, thenExpr) ->
      PT.EIf(id, toPT cond, toPT ifExpr, toPT thenExpr)
    | EBinOp (id, name, first, second, str) ->
      PT.EBinOp(
        id,
        FQFnName.InfixStdlibFnName.toPT name,
        toPT first,
        toPT second,
        SendToRail.toPT str
      )
    | ELambda (id, args, body) -> PT.ELambda(id, args, toPT body)
    | EFieldAccess (id, expr, fieldName) -> PT.EFieldAccess(id, toPT expr, fieldName)
    | EVariable (id, name) -> PT.EVariable(id, name)
    | EFnCall (id, fnName, args, str) ->
      PT.EFnCall(id, FQFnName.toPT fnName, List.map toPT args, SendToRail.toPT str)
    | EPartial (id, part, expr) -> PT.EPartial(id, part, toPT expr)
    | ERightPartial (id, part, expr) -> PT.ERightPartial(id, part, toPT expr)
    | ELeftPartial (id, part, expr) -> PT.ELeftPartial(id, part, toPT expr)
    | EList (id, exprs) -> PT.EList(id, List.map toPT exprs)
    | ETuple (id, first, second, theRest) ->
      PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
    | ERecord (id, pairs) ->
      PT.ERecord(id, pairs |> List.map (fun (name, expr) -> (name, toPT expr)))
    | EPipe (id, expr1, expr2, exprs) ->
      PT.EPipe(id, toPT expr1, toPT expr2, List.map toPT exprs)
    | EConstructor (id, name, args) -> PT.EConstructor(id, name, List.map toPT args)
    | EMatch (id, matchExpr, cases) ->
      PT.EMatch(
        id,
        toPT matchExpr,
        cases |> List.map (fun (pat, expr) -> (Pattern.toPT pat, toPT expr))
      )
    | EPipeTarget (id) -> PT.EPipeTarget(id)
    | EFeatureFlag (id, name, cond, caseA, caseB) ->
      PT.EFeatureFlag(id, name, toPT cond, toPT caseA, toPT caseB)

  let rec fromPT (expr : PT.Expr) : T =
    match expr with
    | PT.EInteger (id, i) -> EInteger(id, i)
    | PT.EBool (id, b) -> EBool(id, b)
    | PT.EString (id, s) -> EString(id, s)
    | PT.ECharacter (id, c) -> ECharacter(id, c)
    | PT.EFloat (id, sign, whole, frac) -> EFloat(id, sign, whole, frac)
    | PT.ENull (id) -> ENull(id)
    | PT.EBlank (id) -> EBlank(id)
    | PT.ELet (id, name, expr, body) -> ELet(id, name, fromPT expr, fromPT body)
    | PT.EIf (id, cond, ifExpr, thenExpr) ->
      EIf(id, fromPT cond, fromPT ifExpr, fromPT thenExpr)
    | PT.EBinOp (id, name, first, second, str) ->
      EBinOp(
        id,
        FQFnName.InfixStdlibFnName.fromPT name,
        fromPT first,
        fromPT second,
        SendToRail.fromPT str
      )
    | PT.ELambda (id, args, body) -> ELambda(id, args, fromPT body)
    | PT.EFieldAccess (id, expr, fieldName) ->
      EFieldAccess(id, fromPT expr, fieldName)
    | PT.EVariable (id, name) -> EVariable(id, name)
    | PT.EFnCall (id, fnName, args, str) ->
      EFnCall(
        id,
        FQFnName.fromPT fnName,
        List.map fromPT args,
        SendToRail.fromPT str
      )
    | PT.EPartial (id, part, expr) -> EPartial(id, part, fromPT expr)
    | PT.ERightPartial (id, part, expr) -> ERightPartial(id, part, fromPT expr)
    | PT.ELeftPartial (id, part, expr) -> ELeftPartial(id, part, fromPT expr)
    | PT.EList (id, exprs) -> EList(id, List.map fromPT exprs)
    | PT.ETuple (id, first, second, theRest) ->
      ETuple(id, fromPT first, fromPT second, List.map fromPT theRest)
    | PT.ERecord (id, pairs) ->
      ERecord(id, pairs |> List.map (fun (name, expr) -> (name, fromPT expr)))
    | PT.EPipe (id, expr1, expr2, exprs) ->
      EPipe(id, fromPT expr1, fromPT expr2, List.map fromPT exprs)
    | PT.EConstructor (id, name, args) ->
      EConstructor(id, name, List.map fromPT args)
    | PT.EMatch (id, matchExpr, cases) ->
      EMatch(
        id,
        fromPT matchExpr,
        cases |> List.map (fun (pat, expr) -> (Pattern.fromPT pat, fromPT expr))
      )
    | PT.EPipeTarget (id) -> EPipeTarget(id)
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
      EFeatureFlag(id, name, fromPT cond, fromPT caseA, fromPT caseB)


module DType =
  type T =
    | TInt
    | TFloat
    | TBool
    | TNull
    | TStr
    | TList of T
    | TTuple of T * T * List<T>
    | TDict of T
    | TIncomplete
    | TError
    | THttpResponse of T
    | TDB of T
    | TDate
    | TChar
    | TPassword
    | TUuid
    | TOption of T
    | TErrorRail
    | TUserType of string * int
    | TBytes
    | TResult of T * T
    | TVariable of string
    | TFn of List<T> * T
    | TRecord of List<string * T>
    | TDbList of T

  let rec toPT (dtype : T) : PT.DType =
    match dtype with
    | TInt -> PT.TInt
    | TFloat -> PT.TFloat
    | TBool -> PT.TBool
    | TNull -> PT.TNull
    | TStr -> PT.TStr
    | TList (t) -> PT.TList(toPT t)
    | TTuple (first, second, theRest) ->
      PT.TTuple(toPT first, toPT second, List.map toPT theRest)
    | TDict (t) -> PT.TDict(toPT t)
    | TIncomplete -> PT.TIncomplete
    | TError -> PT.TError
    | THttpResponse (t) -> PT.THttpResponse(toPT t)
    | TDB (t) -> PT.TDB(toPT t)
    | TDate -> PT.TDate
    | TChar -> PT.TChar
    | TPassword -> PT.TPassword
    | TUuid -> PT.TUuid
    | TOption (t) -> PT.TOption(toPT t)
    | TErrorRail -> PT.TErrorRail
    | TUserType (name, v) -> PT.TUserType(name, v)
    | TBytes -> PT.TBytes
    | TResult (ok, err) -> PT.TResult(toPT ok, toPT err)
    | TVariable (name) -> PT.TVariable(name)
    | TFn (args, body) -> PT.TFn(List.map toPT args, toPT body)
    | TRecord (pairs) ->
      PT.TRecord(pairs |> List.map (fun (name, v) -> (name, toPT v)))
    | TDbList (t) -> PT.TDbList(toPT t)

  let rec fromPT (dtype : PT.DType) : T =
    match dtype with
    | PT.TInt -> TInt
    | PT.TFloat -> TFloat
    | PT.TBool -> TBool
    | PT.TNull -> TNull
    | PT.TStr -> TStr
    | PT.TList (t) -> TList(fromPT t)
    | PT.TTuple (first, second, theRest) ->
      TTuple(fromPT first, fromPT second, List.map fromPT theRest)
    | PT.TDict (t) -> TDict(fromPT t)
    | PT.TIncomplete -> TIncomplete
    | PT.TError -> TError
    | PT.THttpResponse (t) -> THttpResponse(fromPT t)
    | PT.TDB (t) -> TDB(fromPT t)
    | PT.TDate -> TDate
    | PT.TChar -> TChar
    | PT.TPassword -> TPassword
    | PT.TUuid -> TUuid
    | PT.TOption (t) -> TOption(fromPT t)
    | PT.TErrorRail -> TErrorRail
    | PT.TUserType (name, v) -> TUserType(name, v)
    | PT.TBytes -> TBytes
    | PT.TResult (ok, err) -> TResult(fromPT ok, fromPT err)
    | PT.TVariable (name) -> TVariable(name)
    | PT.TFn (args, body) -> TFn(List.map fromPT args, fromPT body)
    | PT.TRecord (pairs) ->
      TRecord(pairs |> List.map (fun (name, v) -> (name, fromPT v)))
    | PT.TDbList (t) -> TDbList(fromPT t)


module Handler =
  module CronInterval =
    type T =
      | EveryDay
      | EveryWeek
      | EveryFortnight
      | EveryHour
      | Every12Hours
      | EveryMinute

    let toPT (ci : T) : PT.Handler.CronInterval =
      match ci with
      | EveryDay -> PT.Handler.EveryDay
      | EveryWeek -> PT.Handler.EveryWeek
      | EveryFortnight -> PT.Handler.EveryFortnight
      | EveryHour -> PT.Handler.EveryHour
      | Every12Hours -> PT.Handler.Every12Hours
      | EveryMinute -> PT.Handler.EveryMinute


    let fromPT (ci : PT.Handler.CronInterval) : T =
      match ci with
      | PT.Handler.EveryDay -> EveryDay
      | PT.Handler.EveryWeek -> EveryWeek
      | PT.Handler.EveryFortnight -> EveryFortnight
      | PT.Handler.EveryHour -> EveryHour
      | PT.Handler.Every12Hours -> Every12Hours
      | PT.Handler.EveryMinute -> EveryMinute

  // TODO This comment seems...suspicious. I wonder if this can be removed or something? @pbiggar
  // We need to keep the IDs around until we get rid of them on the client
  module ids =
    type T = { moduleID : id; nameID : id; modifierID : id }

    let toPT (ids : T) : PT.Handler.ids =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }

    let fromPT (ids : PT.Handler.ids) : T =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }

  module Spec =
    type T =
      | HTTP of route : string * method : string * ids : ids.T
      | HTTPBasic of route : string * method : string * ids : ids.T
      | Worker of name : string * ids : ids.T
      | OldWorker of modulename : string * name : string * ids : ids.T
      | Cron of name : string * interval : Option<CronInterval.T> * ids : ids.T
      | REPL of name : string * ids : ids.T
      | UnknownHandler of string * string * ids.T

    let toPT (spec : T) : PT.Handler.Spec =
      match spec with
      | HTTP (route, method, i) -> PT.Handler.HTTP(route, method, ids.toPT i)
      | HTTPBasic (route, method, i) ->
        PT.Handler.HTTPBasic(route, method, ids.toPT i)
      | Worker (name, i) -> PT.Handler.Worker(name, ids.toPT i)
      | OldWorker (modulename, name, i) ->
        PT.Handler.OldWorker(modulename, name, ids.toPT i)
      | Cron (name, interval, i) ->
        PT.Handler.Cron(name, Option.map CronInterval.toPT interval, ids.toPT i)
      | REPL (name, i) -> PT.Handler.REPL(name, ids.toPT i)
      | UnknownHandler (name, modifier, i) ->
        PT.Handler.UnknownHandler(name, modifier, ids.toPT i)

    let fromPT (spec : PT.Handler.Spec) : T =
      match spec with
      | PT.Handler.HTTP (route, method, i) -> HTTP(route, method, ids.fromPT i)
      | PT.Handler.HTTPBasic (route, method, i) ->
        HTTPBasic(route, method, ids.fromPT i)
      | PT.Handler.Worker (name, i) -> Worker(name, ids.fromPT i)
      | PT.Handler.OldWorker (modulename, name, i) ->
        OldWorker(modulename, name, ids.fromPT i)
      | PT.Handler.Cron (name, interval, i) ->
        Cron(name, Option.map CronInterval.fromPT interval, ids.fromPT i)
      | PT.Handler.REPL (name, i) -> REPL(name, ids.fromPT i)
      | PT.Handler.UnknownHandler (name, modifier, i) ->
        UnknownHandler(name, modifier, ids.fromPT i)

  type T = { tlid : tlid; pos : Position.T; ast : Expr.T; spec : Spec.T }

  let toPT (h : T) : PT.Handler.T =
    { tlid = h.tlid
      pos = Position.toPT h.pos
      ast = Expr.toPT h.ast
      spec = Spec.toPT h.spec }

  let fromPT (h : PT.Handler.T) : T =
    { T.tlid = h.tlid
      pos = Position.fromPT h.pos
      ast = Expr.fromPT h.ast
      spec = Spec.fromPT h.spec }








module DB =
  module Col =
    type T =
      { name : Option<string>
        typ : Option<DType.T>
        nameID : id
        typeID : id }

    let toPT (col : T) : PT.DB.Col =
      { name = col.name
        typ = Option.map DType.toPT col.typ
        nameID = col.nameID
        typeID = col.typeID }

    let fromPT (col : PT.DB.Col) : T =
      { name = col.name
        typ = Option.map DType.fromPT col.typ
        nameID = col.nameID
        typeID = col.typeID }

  type T =
    { tlid : tlid
      pos : Position.T
      name : string
      nameID : id
      version : int
      cols : List<Col.T> }

  let toPT (db : T) : PT.DB.T =
    { tlid = db.tlid
      pos = Position.toPT db.pos
      name = db.name
      nameID = db.nameID
      version = db.version
      cols = List.map Col.toPT db.cols }

  let fromPT (db : PT.DB.T) : T =
    { tlid = db.tlid
      pos = Position.fromPT db.pos
      name = db.name
      nameID = db.nameID
      version = db.version
      cols = List.map Col.fromPT db.cols }


module UserType =
  module RecordField =
    type T = { name : string; typ : Option<DType.T>; nameID : id; typeID : id }

    let toPT (rf : T) : PT.UserType.RecordField =
      { name = rf.name
        typ = Option.map DType.toPT rf.typ
        nameID = rf.nameID
        typeID = rf.typeID }

    let fromPT (rf : PT.UserType.RecordField) : T =
      { name = rf.name
        typ = Option.map DType.fromPT rf.typ
        nameID = rf.nameID
        typeID = rf.typeID }

  module Definition =
    type T = Record of List<RecordField.T>

    let toPT (def : T) : PT.UserType.Definition =
      match def with
      | Record fields -> PT.UserType.Record(List.map RecordField.toPT fields)

    let fromPT (def : PT.UserType.Definition) : T =
      match def with
      | PT.UserType.Record fields -> Record(List.map RecordField.fromPT fields)

  type T =
    { tlid : tlid
      name : string
      nameID : id
      version : int
      definition : Definition.T }

  let toPT (ut : T) : PT.UserType.T =
    { tlid = ut.tlid
      name = ut.name
      nameID = ut.nameID
      version = ut.version
      definition = Definition.toPT ut.definition }

  let fromPT (ut : PT.UserType.T) : T =
    { tlid = ut.tlid
      name = ut.name
      nameID = ut.nameID
      version = ut.version
      definition = Definition.fromPT ut.definition }


module UserFunction =
  module Parameter =
    type T =
      { name : string
        nameID : id
        typ : Option<DType.T>
        typeID : id
        description : string }

    let toPT (p : T) : PT.UserFunction.Parameter =
      { name = p.name
        nameID = p.nameID
        typ = Option.map DType.toPT p.typ
        typeID = p.typeID
        description = p.description }

    let fromPT (p : PT.UserFunction.Parameter) : T =
      { name = p.name
        nameID = p.nameID
        typ = Option.map DType.fromPT p.typ
        typeID = p.typeID
        description = p.description }

  type T =
    { tlid : tlid
      name : string
      nameID : id
      parameters : List<Parameter.T>
      returnType : DType.T
      returnTypeID : id
      description : string
      infix : bool
      body : Expr.T }

  let toPT (uf : T) : PT.UserFunction.T =
    { tlid = uf.tlid
      name = uf.name
      nameID = uf.nameID
      parameters = List.map Parameter.toPT uf.parameters
      returnType = DType.toPT uf.returnType
      returnTypeID = uf.returnTypeID
      description = uf.description
      infix = uf.infix
      body = Expr.toPT uf.body }

  let fromPT (uf : PT.UserFunction.T) : T =
    { tlid = uf.tlid
      name = uf.name
      nameID = uf.nameID
      parameters = List.map Parameter.fromPT uf.parameters
      returnType = DType.fromPT uf.returnType
      returnTypeID = uf.returnTypeID
      description = uf.description
      infix = uf.infix
      body = Expr.fromPT uf.body }


module Toplevel =
  type T =
    | TLHandler of Handler.T
    | TLDB of DB.T
    | TLFunction of UserFunction.T
    | TLType of UserType.T

  // let toTLID (tl : T) : tlid =
  //   match tl with
  //   | TLHandler h -> h.tlid
  //   | TLDB db -> db.tlid
  //   | TLFunction f -> f.tlid
  //   | TLType t -> t.tlid

  let toPT (tl : T) : PT.Toplevel.T =
    match tl with
    | TLHandler handler -> PT.Toplevel.TLHandler(Handler.toPT handler)
    | TLDB db -> PT.Toplevel.TLDB(DB.toPT db)
    | TLFunction uf -> PT.Toplevel.TLFunction(UserFunction.toPT uf)
    | TLType ut -> PT.Toplevel.TLType(UserType.toPT ut)

  let fromPT (tl : PT.Toplevel.T) : T =
    match tl with
    | PT.Toplevel.TLHandler handler -> TLHandler(Handler.fromPT handler)
    | PT.Toplevel.TLDB db -> TLDB(DB.fromPT db)
    | PT.Toplevel.TLFunction uf -> TLFunction(UserFunction.fromPT uf)
    | PT.Toplevel.TLType ut -> TLType(UserType.fromPT ut)



module Op =
  type T =
    | SetHandler of tlid * Position.T * Handler.T
    | CreateDB of tlid * Position.T * string
    | AddDBCol of tlid * id * id
    | SetDBColName of tlid * id * string
    | SetDBColType of tlid * id * string
    | DeleteTL of tlid
    | MoveTL of tlid * Position.T
    | SetFunction of UserFunction.T
    | ChangeDBColName of tlid * id * string
    | ChangeDBColType of tlid * id * string
    | UndoTL of tlid
    | RedoTL of tlid
    | SetExpr of tlid * id * Expr.T
    | TLSavepoint of tlid
    | DeleteFunction of tlid
    | DeleteDBCol of tlid * id
    | RenameDBname of tlid * string
    | CreateDBWithBlankOr of tlid * Position.T * id * string
    | SetType of UserType.T
    | DeleteType of tlid

  let toPT (op : T) : PT.Op =
    match op with
    | SetHandler (tlid, pos, handler) ->
      PT.Op.SetHandler(tlid, Position.toPT pos, Handler.toPT handler)
    | CreateDB (tlid, pos, name) -> PT.Op.CreateDB(tlid, Position.toPT pos, name)
    | AddDBCol (dbid, colNameID, colTypeID) ->
      PT.Op.AddDBCol(dbid, colNameID, colTypeID)
    | SetDBColName (tlid, id, name) -> PT.Op.SetDBColName(tlid, id, name)
    | SetDBColType (tlid, id, tipe) -> PT.Op.SetDBColType(tlid, id, tipe)
    | DeleteTL (tlid) -> PT.Op.DeleteTL(tlid)
    | MoveTL (tlid, pos) -> PT.Op.MoveTL(tlid, Position.toPT pos)
    | SetFunction (uf) -> PT.Op.SetFunction(UserFunction.toPT uf)
    | ChangeDBColName (tlid, id, name) -> PT.Op.ChangeDBColName(tlid, id, name)
    | ChangeDBColType (tlid, id, tipe) -> PT.Op.ChangeDBColType(tlid, id, tipe)
    | UndoTL (tlid) -> PT.Op.UndoTL(tlid)
    | RedoTL (tlid) -> PT.Op.RedoTL(tlid)
    | SetExpr (tlid, id, expr) -> PT.Op.SetExpr(tlid, id, Expr.toPT expr)
    | TLSavepoint (tlid) -> PT.Op.TLSavepoint(tlid)
    | DeleteFunction (tlid) -> PT.Op.DeleteFunction(tlid)
    | DeleteDBCol (tlid, colID) -> PT.Op.DeleteDBCol(tlid, colID)
    | RenameDBname (tlid, name) -> PT.Op.RenameDBname(tlid, name)
    | CreateDBWithBlankOr (tlid, pos, id, name) ->
      PT.Op.CreateDBWithBlankOr(tlid, Position.toPT pos, id, name)
    | SetType (ut) -> PT.Op.SetType(UserType.toPT ut)
    | DeleteType (tlid) -> PT.Op.DeleteType(tlid)

  let fromPT (op : PT.Op) : T =
    match op with
    | PT.Op.SetHandler (tlid, pos, handler) ->
      SetHandler(tlid, Position.fromPT pos, Handler.fromPT handler)
    | PT.Op.CreateDB (tlid, pos, name) -> CreateDB(tlid, Position.fromPT pos, name)
    | PT.Op.AddDBCol (dbid, colNameID, colTypeID) ->
      AddDBCol(dbid, colNameID, colTypeID)
    | PT.Op.SetDBColName (tlid, id, name) -> SetDBColName(tlid, id, name)
    | PT.Op.SetDBColType (tlid, id, tipe) -> SetDBColType(tlid, id, tipe)
    | PT.Op.DeleteTL (tlid) -> DeleteTL(tlid)
    | PT.Op.MoveTL (tlid, pos) -> MoveTL(tlid, Position.fromPT pos)
    | PT.Op.SetFunction (uf) -> SetFunction(UserFunction.fromPT uf)
    | PT.Op.ChangeDBColName (tlid, id, name) -> ChangeDBColName(tlid, id, name)
    | PT.Op.ChangeDBColType (tlid, id, tipe) -> ChangeDBColType(tlid, id, tipe)
    | PT.Op.UndoTL (tlid) -> UndoTL(tlid)
    | PT.Op.RedoTL (tlid) -> RedoTL(tlid)
    | PT.Op.SetExpr (tlid, id, expr) -> SetExpr(tlid, id, Expr.fromPT expr)
    | PT.Op.TLSavepoint (tlid) -> TLSavepoint(tlid)
    | PT.Op.DeleteFunction (tlid) -> DeleteFunction(tlid)
    | PT.Op.DeleteDBCol (tlid, colID) -> DeleteDBCol(tlid, colID)
    | PT.Op.RenameDBname (tlid, name) -> RenameDBname(tlid, name)
    | PT.Op.CreateDBWithBlankOr (tlid, pos, id, name) ->
      CreateDBWithBlankOr(tlid, Position.fromPT pos, id, name)
    | PT.Op.SetType (ut) -> SetType(UserType.fromPT ut)
    | PT.Op.DeleteType (tlid) -> DeleteType(tlid)


type Oplist = List<Op.T>


type TLIDOplists = List<tlid * Oplist>


module Secret =
  type T = { name : string; value : string }

  let toPT (s : T) : PT.Secret.T = { name = s.name; value = s.value }

  let fromPT (s : PT.Secret.T) : T = { name = s.name; value = s.value }


module Package =
  module Parameter =
    type T = { name : string; typ : DType.T; description : string }

    let toPT (p : T) : PT.Package.Parameter =
      { name = p.name; typ = DType.toPT p.typ; description = p.description }

    let fromPT (p : PT.Package.Parameter) : T =
      { name = p.name; typ = DType.fromPT p.typ; description = p.description }

  module Fn =
    type T =
      { name : FQFnName.PackageFnName.T
        body : Expr.T
        parameters : List<Parameter.T>
        returnType : DType.T
        description : string
        author : string
        deprecated : bool
        tlid : tlid }

    let toPT (fn : T) : PT.Package.Fn =
      { name = FQFnName.PackageFnName.toPT fn.name
        body = Expr.toPT fn.body
        parameters = List.map Parameter.toPT fn.parameters
        returnType = DType.toPT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }

    let fromPT (fn : PT.Package.Fn) : T =
      { name = FQFnName.PackageFnName.fromPT fn.name
        body = Expr.fromPT fn.body
        parameters = List.map Parameter.fromPT fn.parameters
        returnType = DType.fromPT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }
