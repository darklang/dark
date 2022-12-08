/// Convert from ProgramTypes to RuntimeTypes
module LibExecution.ProgramTypesToRuntimeTypes

open Prelude
open VendoredTablecloth

// Used for conversion functions
module RT = RuntimeTypes
module PT = ProgramTypes

module FQFnName =
  module PackageFnName =
    let toRT (name : PT.FQFnName.PackageFnName) : RT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    let toRT (name : PT.FQFnName.StdlibFnName) : RT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

  let toRT (fqfn : PT.FQFnName.T) : RT.FQFnName.T =
    match fqfn with
    | PT.FQFnName.User u -> RT.FQFnName.User u
    | PT.FQFnName.Stdlib fn -> RT.FQFnName.Stdlib(StdlibFnName.toRT fn)
    | PT.FQFnName.Package p -> RT.FQFnName.Package(PackageFnName.toRT p)


module SendToRail =
  let toRT (ster : PT.SendToRail) : RT.SendToRail =
    match ster with
    | PT.Rail -> RT.Rail
    | PT.NoRail -> RT.NoRail

module MatchPattern =
  let rec toRT (p : PT.MatchPattern) : RT.MatchPattern =
    match p with
    | PT.MPVariable (id, str) -> RT.MPVariable(id, str)
    | PT.MPConstructor (id, name, pats) ->
      RT.MPConstructor(id, name, List.map toRT pats)
    | PT.MPInteger (id, i) -> RT.MPInteger(id, i)
    | PT.MPBool (id, b) -> RT.MPBool(id, b)
    | PT.MPCharacter (id, c) -> RT.MPCharacter(id, c)
    | PT.MPString (id, s) -> RT.MPString(id, s)
    | PT.MPFloat (id, s, w, f) ->
      let w = if w = "" then "0" else w
      RT.MPFloat(id, makeFloat s w f)
    | PT.MPNull id -> RT.MPNull id
    | PT.MPBlank id -> RT.MPBlank id
    | PT.MPTuple (id, first, second, theRest) ->
      RT.MPTuple(id, toRT first, toRT second, List.map toRT theRest)



module Expr =
  let rec toRT (e : PT.Expr) : RT.Expr =
    match e with
    | PT.EBlank id -> RT.EBlank id
    | PT.ECharacter (id, char) -> RT.ECharacter(id, char)
    | PT.EInteger (id, num) -> RT.EInteger(id, num)
    | PT.EString (id, str) -> RT.EString(id, str)
    | PT.EFloat (id, sign, whole, fraction) ->
      let whole = if whole = "" then "0" else whole
      let fraction = if fraction = "" then "0" else fraction
      RT.EFloat(id, makeFloat sign whole fraction)
    | PT.EBool (id, b) -> RT.EBool(id, b)
    | PT.ENull id -> RT.ENull id
    | PT.EVariable (id, var) -> RT.EVariable(id, var)
    | PT.EFieldAccess (id, obj, fieldname) ->
      RT.EFieldAccess(id, toRT obj, fieldname)
    | PT.EFnCall (id, name, args, ster) ->
      RT.EApply(
        id,
        RT.EFQFnValue(gid (), FQFnName.toRT name),
        List.map toRT args,
        RT.NotInPipe,
        SendToRail.toRT ster
      )
    | PT.EBinOp (id, fnName, arg1, arg2, ster) ->
      let name =
        PT.FQFnName.Stdlib(
          { module_ = Option.unwrap "" fnName.module_
            function_ = fnName.function_
            version = 0 }
        )
      toRT (PT.EFnCall(id, name, [ arg1; arg2 ], ster))
    | PT.ELambda (id, vars, body) -> RT.ELambda(id, vars, toRT body)
    | PT.ELet (id, lhs, rhs, body) -> RT.ELet(id, lhs, toRT rhs, toRT body)
    | PT.EIf (id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, toRT cond, toRT thenExpr, toRT elseExpr)
    | PT.EPartial (_, _, oldExpr)
    | PT.ERightPartial (_, _, oldExpr)
    | PT.ELeftPartial (_, _, oldExpr) -> toRT oldExpr
    | PT.EList (id, exprs) -> RT.EList(id, List.map toRT exprs)
    | PT.ETuple (id, first, second, theRest) ->
      RT.ETuple(id, toRT first, toRT second, List.map toRT theRest)
    | PT.ERecord (id, pairs) ->
      RT.ERecord(id, List.map (Tuple2.mapSecond toRT) pairs)
    | PT.EPipe (pipeID, expr1, expr2, rest) ->
      // Convert v |> fn1 a |> fn2 |> fn3 b c
      // into fn3 (fn2 (fn1 v a)) b c
      // This conversion should correspond to ast.ml:inject_param_and_execute
      // from the OCaml interpreter
      let inner = toRT expr1

      List.fold
        inner
        (fun prev next ->
          let rec convert thisExpr =
            match thisExpr with
            // TODO: support currying
            | PT.EFnCall (id, name, PT.EPipeTarget ptID :: exprs, rail) ->
              RT.EApply(
                id,
                RT.EFQFnValue(ptID, FQFnName.toRT name),
                prev :: List.map toRT exprs,
                RT.InPipe pipeID,
                SendToRail.toRT rail
              )
            // TODO: support currying
            | PT.EBinOp (id, fnName, PT.EPipeTarget ptID, expr2, rail) ->
              let name =
                PT.FQFnName.Stdlib(
                  { module_ = Option.unwrap "" fnName.module_
                    function_ = fnName.function_
                    version = 0 }
                )
              RT.EApply(
                id,
                RT.EFQFnValue(ptID, FQFnName.toRT name),
                [ prev; toRT expr2 ],
                RT.InPipe pipeID,
                SendToRail.toRT rail
              )
            // If there's a hole, run the computation right through it as if it wasn't there
            | PT.EBlank _ -> prev
            // We can ignore partials as we just want whatever is inside them
            | PT.EPartial (_, _, oldExpr) -> convert oldExpr
            // Here, the expression evaluates to an FnValue. This is for eg variables containing values
            | other ->
              RT.EApply(pipeID, toRT other, [ prev ], RT.InPipe pipeID, RT.NoRail)
          convert next)

        (expr2 :: rest)

    | PT.EConstructor (id, name, exprs) ->
      RT.EConstructor(id, name, List.map toRT exprs)
    | PT.EMatch (id, mexpr, pairs) ->
      RT.EMatch(
        id,
        toRT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toRT << Tuple2.mapSecond toRT) pairs
      )
    | PT.EPipeTarget id ->
      Exception.raiseInternal "No EPipeTargets should remain" [ "id", id ]
    | PT.EFeatureFlag (id, name, cond, caseA, caseB) ->
      RT.EFeatureFlag(id, toRT cond, toRT caseA, toRT caseB)
    | PT.EAnd (id, expr1, expr2) -> RT.EAnd(id, toRT expr1, toRT expr2)
    | PT.EOr (id, expr1, expr2) -> RT.EOr(id, toRT expr1, toRT expr2)

module DType =
  let rec toRT (t : PT.DType) : RT.DType =
    match t with
    | PT.TInt -> RT.TInt
    | PT.TFloat -> RT.TFloat
    | PT.TBool -> RT.TBool
    | PT.TNull -> RT.TNull
    | PT.TStr -> RT.TStr
    | PT.TList typ -> RT.TList(toRT typ)
    | PT.TTuple (firstType, secondType, otherTypes) ->
      RT.TTuple(toRT firstType, toRT secondType, List.map toRT otherTypes)
    | PT.TDict typ -> RT.TDict(toRT typ)
    | PT.TIncomplete -> RT.TIncomplete
    | PT.TError -> RT.TError
    | PT.THttpResponse typ -> RT.THttpResponse(toRT typ)
    | PT.TDB typ -> RT.TDB(toRT typ)
    | PT.TDate -> RT.TDate
    | PT.TChar -> RT.TChar
    | PT.TPassword -> RT.TPassword
    | PT.TUuid -> RT.TUuid
    | PT.TOption typ -> RT.TOption(toRT typ)
    | PT.TErrorRail -> RT.TErrorRail
    | PT.TUserType (name, version) -> RT.TUserType(name, version)
    | PT.TBytes -> RT.TBytes
    | PT.TResult (okType, errType) -> RT.TResult(toRT okType, toRT errType)
    | PT.TVariable (name) -> RT.TVariable(name)
    | PT.TFn (paramTypes, returnType) ->
      RT.TFn(List.map toRT paramTypes, toRT returnType)
    | PT.TRecord (rows) ->
      RT.TRecord(List.map (fun (f, t : PT.DType) -> f, toRT t) rows)
    | PT.TDbList typ -> RT.TList(toRT typ)


module Handler =
  module CronInterval =
    let toRT (ci : PT.Handler.CronInterval) : RT.Handler.CronInterval =
      match ci with
      | PT.Handler.EveryDay -> RT.Handler.EveryDay
      | PT.Handler.EveryWeek -> RT.Handler.EveryWeek
      | PT.Handler.EveryFortnight -> RT.Handler.EveryFortnight
      | PT.Handler.EveryHour -> RT.Handler.EveryHour
      | PT.Handler.Every12Hours -> RT.Handler.Every12Hours
      | PT.Handler.EveryMinute -> RT.Handler.EveryMinute

  module Spec =
    let toRT (s : PT.Handler.Spec) : RT.Handler.Spec =
      match s with
      | PT.Handler.HTTP (route, method, _ids) -> RT.Handler.HTTP(route, method)
      | PT.Handler.HTTPBasic (route, method, _ids) ->
        RT.Handler.HTTPBasic(route, method)
      | PT.Handler.Worker (name, _ids) -> RT.Handler.Worker(name)
      | PT.Handler.OldWorker (modulename, name, _ids) ->
        RT.Handler.OldWorker(modulename, name)
      | PT.Handler.Cron (name, interval, _ids) ->
        RT.Handler.Cron(name, interval |> Option.map CronInterval.toRT)
      | PT.Handler.REPL (name, _ids) -> RT.Handler.REPL(name)
      | PT.Handler.UnknownHandler (_name, _modifier, _ids) ->
        RT.Handler.UnknownHandler

  let toRT (h : PT.Handler.T) : RT.Handler.T =
    { tlid = h.tlid; ast = Expr.toRT h.ast; spec = Spec.toRT h.spec }

module DB =
  let toRT (db : PT.DB.T) : RT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      cols =
        List.filterMap
          (fun (c : PT.DB.Col) ->
            match c.name, c.typ with
            | Some n, Some t -> Some(n, DType.toRT t)
            | _ -> None)
          db.cols }

module UserType =
  module Definition =
    let toRT (d : PT.UserType.Definition) : RT.UserType.Definition =
      match d with
      | PT.UserType.Record fields ->
        RT.UserType.UTRecord(
          List.filterMap
            (fun (rf : PT.UserType.RecordField) ->
              match rf.typ with
              | Some t -> Some({ name = rf.name; typ = DType.toRT t })
              | None -> None)
            fields
        )

  let toRT (t : PT.UserType.T) : RT.UserType.T =
    { tlid = t.tlid
      name = t.name
      version = t.version
      definition = Definition.toRT t.definition }

module UserFunction =
  module Parameter =
    let toRT (p : PT.UserFunction.Parameter) : RT.UserFunction.Parameter =
      { name = p.name
        typ = p.typ |> Option.unwrap (PT.TVariable "a") |> DType.toRT
        description = p.description }

  let toRT (f : PT.UserFunction.T) : RT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      parameters = List.map Parameter.toRT f.parameters
      returnType = DType.toRT f.returnType
      description = f.description
      infix = f.infix
      body = Expr.toRT f.body }

module Toplevel =
  let toRT (tl : PT.Toplevel.T) : RT.Toplevel.T =
    match tl with
    | PT.Toplevel.TLHandler h -> RT.Toplevel.TLHandler(Handler.toRT h)
    | PT.Toplevel.TLDB db -> RT.Toplevel.TLDB(DB.toRT db)
    | PT.Toplevel.TLFunction f -> RT.Toplevel.TLFunction(UserFunction.toRT f)
    | PT.Toplevel.TLType t -> RT.Toplevel.TLType(UserType.toRT t)

module Secret =
  let toRT (s : PT.Secret.T) : RT.Secret.T = { name = s.name; value = s.value }

module Package =
  module Parameter =
    let toRT (p : PT.Package.Parameter) : RT.Package.Parameter =
      { name = p.name; typ = DType.toRT p.typ; description = p.description }

  let toRT (f : PT.Package.Fn) : RT.Package.Fn =
    { name = FQFnName.PackageFnName.toRT f.name
      body = Expr.toRT f.body
      parameters = List.map Parameter.toRT f.parameters
      returnType = DType.toRT f.returnType
      description = f.description
      author = f.author
      deprecated = f.deprecated
      tlid = f.tlid }
