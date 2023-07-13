/// Convert from ProgramTypes to RuntimeTypes
module LibExecution.ProgramTypesToRuntimeTypes

open Prelude
open VendoredTablecloth

// Used for conversion functions
module RT = RuntimeTypes
module PT = ProgramTypes

module TypeName =

  module BuiltIn =
    let toRT (s : PT.TypeName.BuiltIn) : RT.TypeName.BuiltIn =
      let (PT.TypeName.TypeName name) = s.name
      { modules = s.modules; name = RT.TypeName.TypeName name; version = s.version }

  module UserProgram =
    let toRT (u : PT.TypeName.UserProgram) : RT.TypeName.UserProgram =
      let (PT.TypeName.TypeName name) = u.name
      { modules = u.modules; name = RT.TypeName.TypeName name; version = u.version }

  module Package =
    let toRT (p : PT.TypeName.Package) : RT.TypeName.Package =
      let (PT.TypeName.TypeName name) = p.name
      { owner = p.owner
        modules = p.modules
        name = RT.TypeName.TypeName name
        version = p.version }

  let toRT (fqtn : PT.TypeName.T) : RT.TypeName.T =
    match fqtn with
    | PT.FQName.BuiltIn s -> RT.FQName.BuiltIn(BuiltIn.toRT s)
    | PT.FQName.UserProgram u -> RT.FQName.UserProgram(UserProgram.toRT u)
    | PT.FQName.Package p -> RT.FQName.Package(Package.toRT p)

module TypeReference =
  let rec toRT (t : PT.TypeReference) : RT.TypeReference =
    match t with
    | PT.TInt -> RT.TInt
    | PT.TFloat -> RT.TFloat
    | PT.TBool -> RT.TBool
    | PT.TUnit -> RT.TUnit
    | PT.TString -> RT.TString
    | PT.TList typ -> RT.TList(toRT typ)
    | PT.TTuple(firstType, secondType, otherTypes) ->
      RT.TTuple(toRT firstType, toRT secondType, List.map toRT otherTypes)
    | PT.TDict typ -> RT.TDict(toRT typ)
    | PT.TDB typ -> RT.TDB(toRT typ)
    | PT.TDateTime -> RT.TDateTime
    | PT.TChar -> RT.TChar
    | PT.TPassword -> RT.TPassword
    | PT.TUuid -> RT.TUuid
    | PT.TCustomType(typeName, typArgs) ->
      RT.TCustomType(TypeName.toRT typeName, List.map toRT typArgs)
    | PT.TBytes -> RT.TBytes
    | PT.TVariable(name) -> RT.TVariable(name)
    | PT.TFn(paramTypes, returnType) ->
      RT.TFn(List.map toRT paramTypes, toRT returnType)

module FnName =
  module BuiltIn =
    let toRT (s : PT.FnName.BuiltIn) : RT.FnName.BuiltIn =
      let (PT.FnName.FnName name) = s.name
      { modules = s.modules; name = RT.FnName.FnName name; version = s.version }

  module UserProgram =
    let toRT (u : PT.FnName.UserProgram) : RT.FnName.UserProgram =
      let (PT.FnName.FnName name) = u.name
      { modules = u.modules; name = RT.FnName.FnName name; version = u.version }

  module Package =
    let toRT (p : PT.FnName.Package) : RT.FnName.Package =
      let (PT.FnName.FnName name) = p.name
      { owner = p.owner
        modules = p.modules
        name = RT.FnName.FnName name
        version = p.version }

  let toRT (fqfn : PT.FnName.T) : RT.FnName.T =
    match fqfn with
    | PT.FQName.BuiltIn s -> RT.FQName.BuiltIn(BuiltIn.toRT s)
    | PT.FQName.UserProgram u -> RT.FQName.UserProgram(UserProgram.toRT u)
    | PT.FQName.Package p -> RT.FQName.Package(Package.toRT p)

module InfixFnName =
  let toFnName (name : PT.InfixFnName) : (List<string> * string * int) =
    match name with
    | PT.ArithmeticPlus -> ([ "Int" ], "add", 0)
    | PT.ArithmeticMinus -> ([ "Int" ], "subtract", 0)
    | PT.ArithmeticMultiply -> ([ "Int" ], "multiply", 0)
    | PT.ArithmeticDivide -> ([ "Float" ], "divide", 0)
    | PT.ArithmeticModulo -> ([ "Int" ], "mod", 0)
    | PT.ArithmeticPower -> ([ "Int" ], "power", 0)
    | PT.ComparisonGreaterThan -> ([ "Int" ], "greaterThan", 0)
    | PT.ComparisonGreaterThanOrEqual -> ([ "Int" ], "greaterThanOrEqualTo", 0)
    | PT.ComparisonLessThan -> ([ "Int" ], "lessThan", 0)
    | PT.ComparisonLessThanOrEqual -> ([ "Int" ], "lessThanOrEqualTo", 0)
    | PT.StringConcat -> ([ "String" ], "append", 0)
    | PT.ComparisonEquals -> ([], "equals", 0)
    | PT.ComparisonNotEquals -> ([], "notEquals", 0)



module LetPattern =
  let rec toRT (p : PT.LetPattern) : RT.LetPattern =
    match p with
    | PT.LPVariable(id, str) -> RT.LPVariable(id, str)
    | PT.LPTuple(id, first, second, theRest) ->
      RT.LPTuple(id, toRT first, toRT second, List.map toRT theRest)

module MatchPattern =
  let rec toRT (p : PT.MatchPattern) : RT.MatchPattern =
    match p with
    | PT.MPVariable(id, str) -> RT.MPVariable(id, str)
    | PT.MPEnum(id, caseName, fieldPats) ->
      RT.MPEnum(id, caseName, List.map toRT fieldPats)
    | PT.MPInt(id, i) -> RT.MPInt(id, i)
    | PT.MPBool(id, b) -> RT.MPBool(id, b)
    | PT.MPChar(id, c) -> RT.MPChar(id, c)
    | PT.MPString(id, s) -> RT.MPString(id, s)
    | PT.MPFloat(id, s, w, f) ->
      let w = if w = "" then "0" else w
      RT.MPFloat(id, makeFloat s w f)
    | PT.MPUnit id -> RT.MPUnit id
    | PT.MPTuple(id, first, second, theRest) ->
      RT.MPTuple(id, toRT first, toRT second, List.map toRT theRest)
    | PT.MPList(id, pats) -> RT.MPList(id, List.map toRT pats)
    | PT.MPListCons(id, head, tail) -> RT.MPListCons(id, toRT head, toRT tail)

module Expr =
  let rec toRT (e : PT.Expr) : RT.Expr =
    match e with
    | PT.EChar(id, char) -> RT.EChar(id, char)
    | PT.EInt(id, num) -> RT.EInt(id, num)
    | PT.EString(id, segments) -> RT.EString(id, List.map stringSegmentToRT segments)
    | PT.EFloat(id, sign, whole, fraction) ->
      let whole = if whole = "" then "0" else whole
      let fraction = if fraction = "" then "0" else fraction
      RT.EFloat(id, makeFloat sign whole fraction)
    | PT.EBool(id, b) -> RT.EBool(id, b)
    | PT.EUnit id -> RT.EUnit id
    | PT.EVariable(id, var) -> RT.EVariable(id, var)
    | PT.EFieldAccess(id, obj, fieldname) -> RT.EFieldAccess(id, toRT obj, fieldname)
    | PT.EApply(id, PT.FnTargetName(Ok fnName), typeArgs, args) ->
      RT.EApply(
        id,
        RT.FnTargetName(FnName.toRT fnName),
        List.map TypeReference.toRT typeArgs,
        List.map toRT args
      )
    | PT.EApply(id, PT.FnTargetName(Error err), typeArgs, args) ->
      RT.EError(
        id,
        Errors.toString (Errors.NameResolutionError err),
        List.map toRT args
      )
    | PT.EApply(id, PT.FnTargetExpr fnName, typeArgs, args) ->
      RT.EApply(
        id,
        RT.FnTargetExpr(toRT fnName),
        List.map TypeReference.toRT typeArgs,
        List.map toRT args
      )
    | PT.EInfix(id, PT.InfixFnCall fnName, arg1, arg2) ->
      let (modules, fn, version) = InfixFnName.toFnName fnName
      let name =
        PT.FQName.BuiltIn(
          { modules = modules; name = PT.FnName.FnName fn; version = version }
        )
      let typeArgs = []
      toRT (PT.EApply(id, PT.FnTargetName(Ok name), typeArgs, [ arg1; arg2 ]))
    | PT.EInfix(id, PT.BinOp PT.BinOpAnd, expr1, expr2) ->
      RT.EAnd(id, toRT expr1, toRT expr2)
    | PT.EInfix(id, PT.BinOp PT.BinOpOr, expr1, expr2) ->
      RT.EOr(id, toRT expr1, toRT expr2)
    | PT.ELambda(id, vars, body) -> RT.ELambda(id, vars, toRT body)
    | PT.ELet(id, pattern, rhs, body) ->
      RT.ELet(id, LetPattern.toRT pattern, toRT rhs, toRT body)
    | PT.EIf(id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, toRT cond, toRT thenExpr, toRT elseExpr)
    | PT.EList(id, exprs) -> RT.EList(id, List.map toRT exprs)
    | PT.ETuple(id, first, second, theRest) ->
      RT.ETuple(id, toRT first, toRT second, List.map toRT theRest)
    | PT.ERecord(id, Ok typeName, fields) ->
      RT.ERecord(id, TypeName.toRT typeName, List.map (Tuple2.mapSecond toRT) fields)
    | PT.ERecord(id, Error typeName, fields) ->
      RT.EError(
        id,
        Errors.toString (Errors.NameResolutionError typeName),
        fields |> List.map Tuple2.second |> List.map toRT
      )

    | PT.ERecordUpdate(id, record, updates) ->
      RT.ERecordUpdate(id, toRT record, List.map (Tuple2.mapSecond toRT) updates)
    | PT.EPipe(pipeID, expr1, expr2, rest) ->
      // Convert v |> fn1 a |> fn2 |> fn3 b c
      // into fn3 (fn2 (fn1 v a)) b c
      let folder (prev : RT.Expr) (next : PT.PipeExpr) : RT.Expr =

        let applyFn (expr : RT.Expr) =
          let typeArgs = []
          RT.EApply(pipeID, RT.FnTargetExpr(expr), typeArgs, [ prev ])

        match next with
        | PT.EPipeFnCall(id, Error fnName, typeArgs, exprs) ->
          RT.EError(
            id,
            Errors.toString (Errors.NameResolutionError fnName),
            prev :: List.map toRT exprs
          )
        | PT.EPipeFnCall(id, Ok fnName, typeArgs, exprs) ->
          RT.EApply(
            id,
            RT.FnTargetName(FnName.toRT fnName),
            List.map TypeReference.toRT typeArgs,
            prev :: List.map toRT exprs
          )
        | PT.EPipeInfix(id, PT.InfixFnCall fnName, expr) ->
          let (modules, fn, version) = InfixFnName.toFnName fnName
          let name =
            PT.FQName.BuiltIn(
              { modules = modules; name = PT.FnName.FnName fn; version = version }
            )
          let typeArgs = []
          RT.EApply(
            id,
            RT.FnTargetName(FnName.toRT name),
            typeArgs,
            [ prev; toRT expr ]
          )
        // Binops work pretty naturally here
        | PT.EPipeInfix(id, PT.BinOp op, expr) ->
          match op with
          | PT.BinOpAnd -> RT.EAnd(id, prev, toRT expr)
          | PT.BinOpOr -> RT.EOr(id, prev, toRT expr)
        | PT.EPipeEnum(id, Ok typeName, caseName, fields) ->
          let fields' = prev :: List.map toRT fields
          RT.EEnum(id, TypeName.toRT typeName, caseName, fields')
        | PT.EPipeEnum(id, Error typeName, caseName, fields) ->
          RT.EError(
            id,
            Errors.toString (Errors.NameResolutionError typeName),
            prev :: List.map toRT fields
          )
        | PT.EPipeVariable(id, name) -> RT.EVariable(id, name) |> applyFn
        | PT.EPipeLambda(id, vars, body) ->
          RT.ELambda(id, vars, toRT body) |> applyFn

      let init = toRT expr1
      List.fold init folder (expr2 :: rest)

    | PT.EMatch(id, mexpr, pairs) ->
      RT.EMatch(
        id,
        toRT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toRT << Tuple2.mapSecond toRT) pairs
      )
    | PT.EEnum(id, Ok typeName, caseName, fields) ->
      RT.EEnum(id, TypeName.toRT typeName, caseName, List.map toRT fields)
    | PT.EEnum(id, Error typeName, caseName, fields) ->
      RT.EError(
        id,
        Errors.toString (Errors.NameResolutionError typeName),
        List.map toRT fields
      )
    | PT.EDict(id, fields) -> RT.EDict(id, List.map (Tuple2.mapSecond toRT) fields)



  and stringSegmentToRT (segment : PT.StringSegment) : RT.StringSegment =
    match segment with
    | PT.StringText text -> RT.StringText text
    | PT.StringInterpolation expr -> RT.StringInterpolation(toRT expr)


module TypeDeclaration =
  module RecordField =
    let toRT (f : PT.TypeDeclaration.RecordField) : RT.TypeDeclaration.RecordField =
      { name = f.name; typ = TypeReference.toRT f.typ; description = f.description }

  module EnumField =
    let toRT (f : PT.TypeDeclaration.EnumField) : RT.TypeDeclaration.EnumField =
      { typ = TypeReference.toRT f.typ
        label = f.label
        description = f.description }

  module EnumCase =
    let toRT (c : PT.TypeDeclaration.EnumCase) : RT.TypeDeclaration.EnumCase =
      { name = c.name
        fields = List.map EnumField.toRT c.fields
        description = c.description }

  module Definition =
    let toRT (d : PT.TypeDeclaration.Definition) : RT.TypeDeclaration.Definition =
      match d with
      | PT.TypeDeclaration.Definition.Alias(typ) ->
        RT.TypeDeclaration.Alias(TypeReference.toRT typ)
      | PT.TypeDeclaration.Record(firstField, additionalFields) ->
        RT.TypeDeclaration.Record(
          RecordField.toRT firstField,
          List.map RecordField.toRT additionalFields
        )
      | PT.TypeDeclaration.Enum(firstCase, additionalCases) ->
        RT.TypeDeclaration.Enum(
          EnumCase.toRT firstCase,
          List.map EnumCase.toRT additionalCases
        )

  let toRT (t : PT.TypeDeclaration.T) : RT.TypeDeclaration.T =
    { typeParams = t.typeParams; definition = Definition.toRT t.definition }


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
      | PT.Handler.HTTP(route, method) -> RT.Handler.HTTP(route, method)
      | PT.Handler.Worker name -> RT.Handler.Worker name
      | PT.Handler.Cron(name, interval) ->
        RT.Handler.Cron(name, CronInterval.toRT interval)
      | PT.Handler.REPL name -> RT.Handler.REPL name

  let toRT (h : PT.Handler.T) : RT.Handler.T =
    { tlid = h.tlid; ast = Expr.toRT h.ast; spec = Spec.toRT h.spec }

module DB =
  let toRT (db : PT.DB.T) : RT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toRT db.typ }

module UserType =
  let toRT (t : PT.UserType.T) : RT.UserType.T =
    { tlid = t.tlid
      name = TypeName.UserProgram.toRT t.name
      declaration = TypeDeclaration.toRT t.declaration }

module UserFunction =
  module Parameter =
    let toRT (p : PT.UserFunction.Parameter) : RT.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toRT p.typ }

  let toRT (f : PT.UserFunction.T) : RT.UserFunction.T =
    { tlid = f.tlid
      name = FnName.UserProgram.toRT f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toRT f.parameters
      returnType = TypeReference.toRT f.returnType
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

module PackageFn =
  module Parameter =
    let toRT (p : PT.PackageFn.Parameter) : RT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toRT p.typ }

  let toRT (f : PT.PackageFn.T) : RT.PackageFn.T =
    { name = FnName.Package.toRT f.name
      tlid = f.tlid
      body = Expr.toRT f.body
      typeParams = f.typeParams
      parameters = List.map Parameter.toRT f.parameters
      returnType = TypeReference.toRT f.returnType }

module PackageType =
  let toRT (t : PT.PackageType.T) : RT.PackageType.T =
    { name = TypeName.Package.toRT t.name
      declaration = TypeDeclaration.toRT t.declaration }
