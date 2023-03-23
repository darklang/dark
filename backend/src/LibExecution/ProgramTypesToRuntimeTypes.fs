/// Convert from ProgramTypes to RuntimeTypes
module LibExecution.ProgramTypesToRuntimeTypes

open Prelude
open VendoredTablecloth

// Used for conversion functions
module RT = RuntimeTypes
module PT = ProgramTypes

module FQTypeName =
  module StdlibTypeName =
    let toRT (t : PT.FQTypeName.StdlibTypeName) : RT.FQTypeName.StdlibTypeName =
      { typ = t.typ }

  module UserTypeName =
    let toRT (u : PT.FQTypeName.UserTypeName) : RT.FQTypeName.UserTypeName =
      { typ = u.typ; version = u.version }

  let toRT (t : PT.FQTypeName.T) : RT.FQTypeName.T =
    match t with
    | PT.FQTypeName.Stdlib s -> RT.FQTypeName.Stdlib(StdlibTypeName.toRT s)
    | PT.FQTypeName.User u -> RT.FQTypeName.User(UserTypeName.toRT u)


module DType =
  let rec toRT (t : PT.DType) : RT.DType =
    match t with
    | PT.TInt -> RT.TInt
    | PT.TFloat -> RT.TFloat
    | PT.TBool -> RT.TBool
    | PT.TUnit -> RT.TUnit
    | PT.TStr -> RT.TStr
    | PT.TList typ -> RT.TList(toRT typ)
    | PT.TTuple (firstType, secondType, otherTypes) ->
      RT.TTuple(toRT firstType, toRT secondType, List.map toRT otherTypes)
    | PT.TDict typ -> RT.TDict(toRT typ)
    | PT.TIncomplete -> RT.TIncomplete
    | PT.TError -> RT.TError
    | PT.THttpResponse typ -> RT.THttpResponse(toRT typ)
    | PT.TDB typ -> RT.TDB(toRT typ)
    | PT.TDateTime -> RT.TDateTime
    | PT.TChar -> RT.TChar
    | PT.TPassword -> RT.TPassword
    | PT.TUuid -> RT.TUuid
    | PT.TOption typ -> RT.TOption(toRT typ)
    | PT.TCustomType (typeName, typArgs) ->
      RT.TCustomType(FQTypeName.toRT typeName, List.map toRT typArgs)
    | PT.TBytes -> RT.TBytes
    | PT.TResult (okType, errType) -> RT.TResult(toRT okType, toRT errType)
    | PT.TVariable (name) -> RT.TVariable(name)
    | PT.TFn (paramTypes, returnType) ->
      RT.TFn(List.map toRT paramTypes, toRT returnType)
    | PT.TRecord (rows) ->
      RT.TRecord(List.map (fun (f, t : PT.DType) -> f, toRT t) rows)
    | PT.TDbList typ -> RT.TList(toRT typ)


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

module InfixFnName =
  let toFnName (name : PT.InfixFnName) : (string * string * int) =
    match name with
    | PT.ArithmeticPlus -> ("Int", "add", 0)
    | PT.ArithmeticMinus -> ("Int", "subtract", 0)
    | PT.ArithmeticMultiply -> ("Int", "multiply", 0)
    | PT.ArithmeticDivide -> ("Float", "divide", 0)
    | PT.ArithmeticModulo -> ("Int", "mod", 0)
    | PT.ArithmeticPower -> ("Int", "power", 0)
    | PT.ComparisonGreaterThan -> ("Int", "greaterThan", 0)
    | PT.ComparisonGreaterThanOrEqual -> ("Int", "greaterThanOrEqualTo", 0)
    | PT.ComparisonLessThan -> ("Int", "lessThan", 0)
    | PT.ComparisonLessThanOrEqual -> ("Int", "lessThanOrEqualTo", 0)
    | PT.StringConcat -> ("String", "append", 1)
    | PT.ComparisonEquals -> ("", "equals", 0)
    | PT.ComparisonNotEquals -> ("", "notEquals", 0)



module LetPattern =
  let rec toRT (p : PT.LetPattern) : RT.LetPattern =
    match p with
    | PT.LPVariable (id, str) -> RT.LPVariable(id, str)

module MatchPattern =
  let rec toRT (p : PT.MatchPattern) : RT.MatchPattern =
    match p with
    | PT.MPVariable (id, str) -> RT.MPVariable(id, str)
    | PT.MPConstructor (id, caseName, fieldPats) ->
      RT.MPConstructor(id, caseName, List.map toRT fieldPats)
    | PT.MPInteger (id, i) -> RT.MPInteger(id, i)
    | PT.MPBool (id, b) -> RT.MPBool(id, b)
    | PT.MPCharacter (id, c) -> RT.MPCharacter(id, c)
    | PT.MPString (id, s) -> RT.MPString(id, s)
    | PT.MPFloat (id, s, w, f) ->
      let w = if w = "" then "0" else w
      RT.MPFloat(id, makeFloat s w f)
    | PT.MPUnit id -> RT.MPUnit id
    | PT.MPTuple (id, first, second, theRest) ->
      RT.MPTuple(id, toRT first, toRT second, List.map toRT theRest)



module Expr =
  let rec toRT (e : PT.Expr) : RT.Expr =
    match e with
    | PT.ECharacter (id, char) -> RT.ECharacter(id, char)
    | PT.EInteger (id, num) -> RT.EInteger(id, num)
    | PT.EString (id, segments) ->
      RT.EString(id, List.map stringSegmentToRT segments)
    | PT.EFloat (id, sign, whole, fraction) ->
      let whole = if whole = "" then "0" else whole
      let fraction = if fraction = "" then "0" else fraction
      RT.EFloat(id, makeFloat sign whole fraction)
    | PT.EBool (id, b) -> RT.EBool(id, b)
    | PT.EUnit id -> RT.EUnit id
    | PT.EVariable (id, var) -> RT.EVariable(id, var)
    | PT.EFieldAccess (id, obj, fieldname) ->
      RT.EFieldAccess(id, toRT obj, fieldname)
    | PT.EFnCall (id, fnName, typeArgs, args) ->
      RT.EApply(
        id,
        RT.FnName(FQFnName.toRT fnName),
        List.map DType.toRT typeArgs,
        List.map toRT args,
        RT.NotInPipe
      )
    | PT.EInfix (id, PT.InfixFnCall fnName, arg1, arg2) ->
      let (module_, fn, version) = InfixFnName.toFnName fnName
      let name =
        PT.FQFnName.Stdlib({ module_ = module_; function_ = fn; version = version })
      let typeArgs = []
      toRT (PT.EFnCall(id, name, typeArgs, [ arg1; arg2 ]))
    | PT.EInfix (id, PT.BinOp PT.BinOpAnd, expr1, expr2) ->
      RT.EAnd(id, toRT expr1, toRT expr2)
    | PT.EInfix (id, PT.BinOp PT.BinOpOr, expr1, expr2) ->
      RT.EOr(id, toRT expr1, toRT expr2)
    | PT.ELambda (id, vars, body) -> RT.ELambda(id, vars, toRT body)
    | PT.ELet (id, pattern, rhs, body) ->
      RT.ELet(id, LetPattern.toRT pattern, toRT rhs, toRT body)
    | PT.EIf (id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, toRT cond, toRT thenExpr, toRT elseExpr)
    | PT.EList (id, exprs) -> RT.EList(id, List.map toRT exprs)
    | PT.ETuple (id, first, second, theRest) ->
      RT.ETuple(id, toRT first, toRT second, List.map toRT theRest)
    | PT.ERecord (id, typeName, fields) ->
      RT.ERecord(
        id,
        Option.map FQTypeName.toRT typeName,
        List.map (Tuple2.mapSecond toRT) fields
      )
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

            | PT.EFnCall (id, fnName, typeArgs, PT.EPipeTarget ptID :: exprs) ->
              RT.EApply(
                id,
                RT.FnName(FQFnName.toRT fnName),
                List.map DType.toRT typeArgs,
                prev :: List.map toRT exprs,
                RT.InPipe pipeID
              )
            | PT.EInfix (id, PT.InfixFnCall fnName, PT.EPipeTarget ptID, expr2) ->
              let (module_, fn, version) = InfixFnName.toFnName fnName
              let name =
                PT.FQFnName.Stdlib(
                  { module_ = module_; function_ = fn; version = version }
                )
              let typeArgs = []
              RT.EApply(
                id,
                RT.FnName(FQFnName.toRT name),
                typeArgs,
                [ prev; toRT expr2 ],
                RT.InPipe pipeID
              )
            // Binops work pretty naturally here
            | PT.EInfix (id, PT.BinOp op, PT.EPipeTarget _, expr2) ->
              match op with
              | PT.BinOpAnd -> RT.EAnd(id, prev, toRT expr2)
              | PT.BinOpOr -> RT.EOr(id, prev, toRT expr2)
            | other ->
              let typeArgs = [] // TODO: review
              RT.EApply(
                pipeID,
                RT.FnTargetExpr(toRT other),
                typeArgs,
                [ prev ],
                RT.InPipe pipeID
              )
          convert next)

        (expr2 :: rest)

    | PT.EMatch (id, mexpr, pairs) ->
      RT.EMatch(
        id,
        toRT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toRT << Tuple2.mapSecond toRT) pairs
      )
    | PT.EPipeTarget id ->
      Exception.raiseInternal "No EPipeTargets should remain" [ "id", id ]
    | PT.EFeatureFlag (id, _name, cond, caseA, caseB) ->
      RT.EFeatureFlag(id, toRT cond, toRT caseA, toRT caseB)
    | PT.EConstructor (id, typeName, caseName, fields) ->
      RT.EConstructor(
        id,
        Option.map FQTypeName.toRT typeName,
        caseName,
        List.map toRT fields
      )


  and stringSegmentToRT (segment : PT.StringSegment) : RT.StringSegment =
    match segment with
    | PT.StringText text -> RT.StringText text
    | PT.StringInterpolation expr -> RT.StringInterpolation(toRT expr)


module CustomType =
  module RecordField =
    let toRT (f : PT.CustomType.RecordField) : RT.CustomType.RecordField =
      { id = f.id; name = f.name; typ = DType.toRT f.typ }

  module EnumField =
    let toRT (f : PT.CustomType.EnumField) : RT.CustomType.EnumField =
      { id = f.id; typ = DType.toRT f.typ; label = f.label }

  module EnumCase =
    let toRT (c : PT.CustomType.EnumCase) : RT.CustomType.EnumCase =
      { id = c.id; name = c.name; fields = List.map EnumField.toRT c.fields }

  let toRT (d : PT.CustomType.T) : RT.CustomType.T =
    match d with
    | PT.CustomType.Record (firstField, additionalFields) ->
      RT.CustomType.Record(
        RecordField.toRT firstField,
        List.map RecordField.toRT additionalFields
      )
    | PT.CustomType.Enum (firstCase, additionalCases) ->
      RT.CustomType.Enum(
        EnumCase.toRT firstCase,
        List.map EnumCase.toRT additionalCases
      )


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
      | PT.Handler.Worker (name, _ids) -> RT.Handler.Worker(name)
      | PT.Handler.Cron (name, interval, _ids) ->
        RT.Handler.Cron(name, interval |> Option.map CronInterval.toRT)
      | PT.Handler.REPL (name, _ids) -> RT.Handler.REPL(name)

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
  let toRT (t : PT.UserType.T) : RT.UserType.T =
    { tlid = t.tlid
      name = FQTypeName.UserTypeName.toRT t.name
      definition = CustomType.toRT t.definition }

module UserFunction =
  module Parameter =
    let toRT (p : PT.UserFunction.Parameter) : RT.UserFunction.Parameter =
      { name = p.name; typ = DType.toRT p.typ; description = p.description }

  let toRT (f : PT.UserFunction.T) : RT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      typeParams = f.typeParams
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
      typeParams = f.typeParams
      parameters = List.map Parameter.toRT f.parameters
      returnType = DType.toRT f.returnType
      description = f.description
      author = f.author
      deprecated = f.deprecated
      tlid = f.tlid }

module BuiltInType =
  let toRT (t : PT.BuiltInType) : RT.BuiltInType =
    { name = FQTypeName.StdlibTypeName.toRT t.name
      typeParams = t.typeParams
      definition = CustomType.toRT t.definition
      description = t.description }
