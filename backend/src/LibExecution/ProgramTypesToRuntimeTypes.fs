/// Convert from ProgramTypes to RuntimeTypes
module LibExecution.ProgramTypesToRuntimeTypes

open Prelude

// Used for conversion functions
module RT = RuntimeTypes
module PT = ProgramTypes

module TypeName =

  module BuiltIn =
    let toRT (s : PT.TypeName.BuiltIn) : RT.TypeName.BuiltIn =
      let (PT.TypeName.TypeName name) = s.name
      { modules = s.modules; name = RT.TypeName.TypeName name; version = s.version }

    let fromRT (s : RT.TypeName.BuiltIn) : PT.TypeName.BuiltIn =
      let (RT.TypeName.TypeName name) = s.name
      { modules = s.modules; name = PT.TypeName.TypeName name; version = s.version }

  module UserProgram =
    let toRT (u : PT.TypeName.UserProgram) : RT.TypeName.UserProgram =
      let (PT.TypeName.TypeName name) = u.name
      { modules = u.modules; name = RT.TypeName.TypeName name; version = u.version }

    let fromRT (u : RT.TypeName.UserProgram) : PT.TypeName.UserProgram =
      let (RT.TypeName.TypeName name) = u.name
      { modules = u.modules; name = PT.TypeName.TypeName name; version = u.version }

  module Package =
    let toRT (p : PT.TypeName.Package) : RT.TypeName.Package =
      let (PT.TypeName.TypeName name) = p.name
      { owner = p.owner
        modules = p.modules
        name = RT.TypeName.TypeName name
        version = p.version }

    let fromRT (p : RT.TypeName.Package) : PT.TypeName.Package =
      let (RT.TypeName.TypeName name) = p.name
      { owner = p.owner
        modules = p.modules
        name = PT.TypeName.TypeName name
        version = p.version }

  let toRT (fqtn : PT.TypeName.TypeName) : RT.TypeName.TypeName =
    match fqtn with
    | PT.FQName.BuiltIn s -> RT.FQName.BuiltIn(BuiltIn.toRT s)
    | PT.FQName.UserProgram u -> RT.FQName.UserProgram(UserProgram.toRT u)
    | PT.FQName.Package p -> RT.FQName.Package(Package.toRT p)

  let fromRT (fqtn : RT.TypeName.TypeName) : Option<PT.TypeName.TypeName> =
    match fqtn with
    | RT.FQName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.fromRT s) |> Some
    | RT.FQName.UserProgram u -> PT.FQName.UserProgram(UserProgram.fromRT u) |> Some
    | RT.FQName.Package p -> PT.FQName.Package(Package.fromRT p) |> Some


module NameResolution =
  let toRT (f : 'a -> 'b) (nr : PT.NameResolution<'a>) : Ply<RT.NameResolution<'b>> =
    uply {
      match nr with
      | Ok x -> return Ok(f x)
      | Error e ->
        let! rte = NameResolutionError.RTE.toRuntimeError e
        return Error rte
    }

module TypeReference =
  let rec toRT (t : PT.TypeReference) : Ply<RT.TypeReference> =
    uply {
      match t with
      | PT.TInt -> return RT.TInt
      | PT.TFloat -> return RT.TFloat
      | PT.TBool -> return RT.TBool
      | PT.TUnit -> return RT.TUnit
      | PT.TString -> return RT.TString
      | PT.TList inner ->
        let! inner = toRT inner
        return RT.TList(inner)
      | PT.TTuple(first, second, theRest) ->
        let! first = toRT first
        let! second = toRT second
        let! theRest = theRest |> Ply.List.mapSequentially toRT
        return RT.TTuple(first, second, theRest)
      | PT.TDict typ ->
        let! typ = toRT typ
        return RT.TDict typ
      | PT.TDB typ ->
        let! typ = toRT typ
        return RT.TDB typ
      | PT.TDateTime -> return RT.TDateTime
      | PT.TChar -> return RT.TChar
      | PT.TUuid -> return RT.TUuid
      | PT.TCustomType(typeName, typeArgs) ->
        let! typeName = NameResolution.toRT TypeName.toRT typeName
        let! typeArgs = Ply.List.mapSequentially toRT typeArgs
        return RT.TCustomType(typeName, typeArgs)
      | PT.TBytes -> return RT.TBytes
      | PT.TVariable(name) -> return RT.TVariable(name)
      | PT.TFn(paramTypes, returnType) ->
        let! paramTypes = Ply.NEList.mapSequentially toRT paramTypes
        let! returnType = toRT returnType
        return RT.TFn(paramTypes, returnType)
    }

module FnName =
  module BuiltIn =
    let toRT (s : PT.FnName.BuiltIn) : RT.FnName.BuiltIn =
      let (PT.FnName.FnName name) = s.name
      { modules = s.modules; name = RT.FnName.FnName name; version = s.version }

    let fromRT (s : RT.FnName.BuiltIn) : PT.FnName.BuiltIn =
      let (RT.FnName.FnName name) = s.name
      { modules = s.modules; name = PT.FnName.FnName name; version = s.version }

  module UserProgram =
    let toRT (u : PT.FnName.UserProgram) : RT.FnName.UserProgram =
      let (PT.FnName.FnName name) = u.name
      { modules = u.modules; name = RT.FnName.FnName name; version = u.version }

    let fromRT (u : RT.FnName.UserProgram) : PT.FnName.UserProgram =
      let (RT.FnName.FnName name) = u.name
      { modules = u.modules; name = PT.FnName.FnName name; version = u.version }

  module Package =
    let toRT (p : PT.FnName.Package) : RT.FnName.Package =
      let (PT.FnName.FnName name) = p.name
      { owner = p.owner
        modules = p.modules
        name = RT.FnName.FnName name
        version = p.version }

    let fromRT (p : RT.FnName.Package) : PT.FnName.Package =
      let (RT.FnName.FnName name) = p.name
      { owner = p.owner
        modules = p.modules
        name = PT.FnName.FnName name
        version = p.version }

  let toRT (fqfn : PT.FnName.FnName) : RT.FnName.FnName =
    match fqfn with
    | PT.FQName.BuiltIn s -> RT.FQName.BuiltIn(BuiltIn.toRT s)
    | PT.FQName.UserProgram u -> RT.FQName.UserProgram(UserProgram.toRT u)
    | PT.FQName.Package p -> RT.FQName.Package(Package.toRT p)


module ConstantName =

  module BuiltIn =
    let toRT (c : PT.ConstantName.BuiltIn) : RT.ConstantName.BuiltIn =
      let (PT.ConstantName.ConstantName name) = c.name
      { modules = c.modules
        name = RT.ConstantName.ConstantName name
        version = c.version }

    let fromRT (c : RT.ConstantName.BuiltIn) : PT.ConstantName.BuiltIn =
      let (RT.ConstantName.ConstantName name) = c.name
      { modules = c.modules
        name = PT.ConstantName.ConstantName name
        version = c.version }

  module UserProgram =
    let toRT (c : PT.ConstantName.UserProgram) : RT.ConstantName.UserProgram =
      let (PT.ConstantName.ConstantName name) = c.name
      { modules = c.modules
        name = RT.ConstantName.ConstantName name
        version = c.version }

    let fromRT (c : RT.ConstantName.UserProgram) : PT.ConstantName.UserProgram =
      let (RT.ConstantName.ConstantName name) = c.name
      { modules = c.modules
        name = PT.ConstantName.ConstantName name
        version = c.version }

  module Package =
    let toRT (c : PT.ConstantName.Package) : RT.ConstantName.Package =
      let (PT.ConstantName.ConstantName name) = c.name
      { owner = c.owner
        modules = c.modules
        name = RT.ConstantName.ConstantName name
        version = c.version }

    let fromRT (c : RT.ConstantName.Package) : PT.ConstantName.Package =
      let (RT.ConstantName.ConstantName name) = c.name
      { owner = c.owner
        modules = c.modules
        name = PT.ConstantName.ConstantName name
        version = c.version }

  let toRT
    (fqConstant : PT.ConstantName.ConstantName)
    : RT.ConstantName.ConstantName =
    match fqConstant with
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
    | PT.LPUnit id -> RT.LPUnit id
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
  let rec toRT (e : PT.Expr) : Ply<RT.Expr> =
    uply {
      match e with
      | PT.EChar(id, char) -> return RT.EChar(id, char)
      | PT.EInt(id, num) -> return RT.EInt(id, num)

      | PT.EString(id, segments) ->
        let! segments = Ply.List.mapSequentially stringSegmentToRT segments
        return RT.EString(id, segments)

      | PT.EFloat(id, sign, whole, fraction) ->
        let whole = if whole = "" then "0" else whole
        let fraction = if fraction = "" then "0" else fraction
        return RT.EFloat(id, makeFloat sign whole fraction)
      | PT.EBool(id, b) -> return RT.EBool(id, b)
      | PT.EUnit id -> return RT.EUnit id

      | PT.EConstant(id, Ok name) -> return RT.EConstant(id, ConstantName.toRT name)
      | PT.EConstant(id, Error err) ->
        let! err = NameResolutionError.RTE.toRuntimeError err
        return RT.EError(id, err, [])

      | PT.EVariable(id, var) -> return RT.EVariable(id, var)

      | PT.EFieldAccess(id, obj, fieldname) ->
        let! obj = toRT obj
        return RT.EFieldAccess(id, obj, fieldname)

      | PT.EApply(id, fnName, typeArgs, args) ->
        let! fnName = toRT fnName
        let! typeArgs = Ply.List.mapSequentially TypeReference.toRT typeArgs
        let! args = Ply.NEList.mapSequentially toRT args
        return RT.EApply(id, fnName, typeArgs, args)

      | PT.EFnName(id, Ok name) -> return RT.EFnName(id, FnName.toRT name)
      | PT.EFnName(id, Error err) ->
        let! err = NameResolutionError.RTE.toRuntimeError err
        return RT.EError(id, err, [])

      // CLEANUP tidy infix stuff - extract to another fn?
      | PT.EInfix(id, PT.InfixFnCall fnName, left, right) ->
        let (modules, fn, version) = InfixFnName.toFnName fnName
        let name =
          RT.FQName.BuiltIn(
            { modules = modules; name = RT.FnName.FnName fn; version = version }
          )
        let! left = toRT left
        let! right = toRT right
        return RT.EApply(id, RT.EFnName(id, name), [], NEList.ofList left [ right ])
      | PT.EInfix(id, PT.BinOp PT.BinOpAnd, left, right) ->
        let! left = toRT left
        let! right = toRT right
        return RT.EAnd(id, left, right)
      | PT.EInfix(id, PT.BinOp PT.BinOpOr, left, right) ->
        let! left = toRT left
        let! right = toRT right
        return RT.EOr(id, left, right)

      | PT.ELambda(id, vars, body) ->
        let vars = vars |> NEList.filter (fun (name, v) -> v <> "")
        match vars with
        | [] ->
          return
            RT.EError(
              id,
              RT.RuntimeError.oldError "Lambda must have at least one argument",
              []
            )
        | head :: tail ->
          let! body = toRT body
          return RT.ELambda(id, NEList.ofList head tail, body)

      | PT.ELet(id, pattern, rhs, body) ->
        let! rhs = toRT rhs
        let! body = toRT body
        return RT.ELet(id, LetPattern.toRT pattern, rhs, body)

      | PT.EIf(id, cond, thenExpr, elseExpr) ->
        let! cond = toRT cond
        let! thenExpr = toRT thenExpr
        let! elseExpr = elseExpr |> Ply.Option.map toRT
        return RT.EIf(id, cond, thenExpr, elseExpr)

      | PT.EList(id, exprs) ->
        let! exprs = Ply.List.mapSequentially toRT exprs
        return RT.EList(id, exprs)

      | PT.ETuple(id, first, second, theRest) ->
        let! first = toRT first
        let! second = toRT second
        let! theRest = Ply.List.mapSequentially toRT theRest
        return RT.ETuple(id, first, second, theRest)

      | PT.ERecord(id, Ok typeName, fields) ->
        return!
          uply {
            match fields with
            | [] ->
              let! fields =
                fields |> List.map Tuple2.second |> Ply.List.mapSequentially toRT
              return
                RT.EError(
                  id,
                  RT.RuntimeError.oldError "Record must have at least one field",
                  fields
                )
            | head :: tail ->
              let! fields =
                NEList.ofList head tail
                |> Ply.NEList.mapSequentially (fun (name, expr) ->
                  uply {
                    let! expr = toRT expr
                    return (name, expr)
                  })
              return RT.ERecord(id, TypeName.toRT typeName, fields)
          }
      | PT.ERecord(id, Error err, fields) ->
        let! err = NameResolutionError.RTE.toRuntimeError err
        let! fields =
          fields |> List.map Tuple2.second |> Ply.List.mapSequentially toRT
        return RT.EError(id, err, fields)

      | PT.ERecordUpdate(id, record, updates) ->
        let! record = toRT record
        let! updates =
          updates
          |> Ply.NEList.mapSequentially (fun (fieldName, update) ->
            uply {
              let! update = toRT update
              return (fieldName, update)
            })
        return RT.ERecordUpdate(id, record, updates)

      | PT.EPipe(pipeID, expr1, rest) ->
        // Convert v |> fn1 a |> fn2 |> fn3 b c
        // into fn3 (fn2 (fn1 v a)) b c
        let folder (prev : RT.Expr) (next : PT.PipeExpr) : Ply<RT.Expr> =

          let applyFn (expr : RT.Expr) (args : List<RT.Expr>) =
            let typeArgs = []
            RT.EApply(pipeID, expr, typeArgs, NEList.ofList prev args)
          uply {
            match next with
            | PT.EPipeFnCall(id, Error err, _typeArgs, exprs) ->
              let! err = NameResolutionError.RTE.toRuntimeError err
              let! addlExprs = Ply.List.mapSequentially toRT exprs
              return RT.EError(id, err, prev :: addlExprs)
            | PT.EPipeFnCall(id, Ok fnName, typeArgs, exprs) ->
              let! typeArgs = Ply.List.mapSequentially TypeReference.toRT typeArgs
              let! exprs =
                exprs
                |> Ply.List.mapSequentially toRT
                |> Ply.map (NEList.ofList prev)
              return
                RT.EApply(id, RT.EFnName(id, FnName.toRT fnName), typeArgs, exprs)
            | PT.EPipeInfix(id, PT.InfixFnCall fnName, expr) ->
              let (modules, fn, version) = InfixFnName.toFnName fnName
              let name =
                PT.FQName.BuiltIn(
                  { modules = modules
                    name = PT.FnName.FnName fn
                    version = version }
                )
              let typeArgs = []
              let! addlExpr = toRT expr
              return
                RT.EApply(
                  id,
                  RT.EFnName(id, FnName.toRT name),
                  typeArgs,
                  NEList.doubleton prev addlExpr
                )
            // Binops work pretty naturally here
            | PT.EPipeInfix(id, PT.BinOp op, expr) ->
              match op with
              | PT.BinOpAnd ->
                let! expr = toRT expr
                return RT.EAnd(id, prev, expr)
              | PT.BinOpOr ->
                let! expr = toRT expr
                return RT.EOr(id, prev, expr)
            | PT.EPipeEnum(id, Ok typeName, caseName, fields) ->
              let! addlFields = Ply.List.mapSequentially toRT fields
              return
                RT.EEnum(id, TypeName.toRT typeName, caseName, prev :: addlFields)
            | PT.EPipeEnum(id, Error err, _caseName, fields) ->
              let! err = NameResolutionError.RTE.toRuntimeError err
              let! addlFields = Ply.List.mapSequentially toRT fields
              return RT.EError(id, err, prev :: addlFields)
            | PT.EPipeVariable(id, name, exprs) ->
              let! exprs = Ply.List.mapSequentially toRT exprs
              return applyFn (RT.EVariable(id, name)) exprs
            | PT.EPipeLambda(id, vars, body) ->
              let! body = toRT body
              return applyFn (RT.ELambda(id, vars, body)) []
          }

        let! init = toRT expr1
        return! Ply.List.foldSequentially folder init rest

      | PT.EMatch(id, mexpr, pairs) ->
        match pairs with
        | [] ->
          let! mexpr = toRT mexpr
          return
            RT.EError(
              id,
              RT.RuntimeError.oldError "Match must have at least one case",
              [ mexpr ]
            )
        | head :: tail ->
          let! mexpr = toRT mexpr
          let! cases =
            NEList.ofList head tail
            |> Ply.NEList.mapSequentially (fun (mp, expr) ->
              uply {
                let! expr = toRT expr
                return (MatchPattern.toRT mp, expr)
              })
          return RT.EMatch(id, mexpr, cases)

      | PT.EEnum(id, Ok typeName, caseName, fields) ->
        let! fields = Ply.List.mapSequentially toRT fields
        return RT.EEnum(id, TypeName.toRT typeName, caseName, fields)
      | PT.EEnum(id, Error err, _caseName, fields) ->
        let! err = NameResolutionError.RTE.toRuntimeError err
        let! fields = Ply.List.mapSequentially toRT fields
        return RT.EError(id, err, fields)

      | PT.EDict(id, fields) ->
        let! fields =
          fields
          |> Ply.List.mapSequentially (fun (k, v) ->
            uply {
              let! v = toRT v
              return (k, v)
            })
        return RT.EDict(id, fields)
    }


  and stringSegmentToRT (segment : PT.StringSegment) : Ply<RT.StringSegment> =
    match segment with
    | PT.StringText text -> Ply(RT.StringText text)
    | PT.StringInterpolation expr ->
      uply {
        let! expr = toRT expr
        return RT.StringInterpolation expr
      }

module Const =

  let rec toRT (c : PT.Const) : Ply<RT.Const> =
    uply {
      match c with
      | PT.Const.CInt i -> return RT.CInt i
      | PT.Const.CBool b -> return RT.CBool b
      | PT.Const.CString s -> return RT.CString s
      | PT.Const.CChar c -> return RT.CChar c
      | PT.Const.CFloat(sign, w, f) -> return RT.CFloat(sign, w, f)
      | PT.Const.CUnit -> return RT.CUnit
      | PT.Const.CTuple(first, second, rest) ->
        let! first = toRT first
        let! second = toRT second
        let! rest = Ply.List.mapSequentially toRT rest
        return RT.CTuple(first, second, rest)
      | PT.Const.CEnum(typeName, caseName, fields) ->
        let! typeName = NameResolution.toRT TypeName.toRT typeName
        let! fields = Ply.List.mapSequentially toRT fields
        return RT.CEnum(typeName, caseName, fields)
      | PT.Const.CList items ->
        let! items = Ply.List.mapSequentially toRT items
        return RT.CList items
      | PT.Const.CDict entries ->
        let! entries =
          entries
          |> Ply.List.mapSequentially (fun (k, v) ->
            uply {
              let! v = toRT v
              return (k, v)
            })
        return RT.CDict entries
    }

module TypeDeclaration =
  module RecordField =
    let toRT
      (f : PT.TypeDeclaration.RecordField)
      : Ply<RT.TypeDeclaration.RecordField> =
      uply {
        let! typ = TypeReference.toRT f.typ
        return { name = f.name; typ = typ }
      }

  module EnumField =
    let toRT (f : PT.TypeDeclaration.EnumField) : Ply<RT.TypeReference> =
      TypeReference.toRT f.typ

  module EnumCase =
    let toRT (c : PT.TypeDeclaration.EnumCase) : Ply<RT.TypeDeclaration.EnumCase> =
      uply {
        let! fields = Ply.List.mapSequentially EnumField.toRT c.fields
        return { name = c.name; fields = fields }
      }

  module Definition =
    let toRT
      (d : PT.TypeDeclaration.Definition)
      : Ply<RT.TypeDeclaration.Definition> =
      uply {
        match d with
        | PT.TypeDeclaration.Definition.Alias(typ) ->
          let! typ = TypeReference.toRT typ
          return RT.TypeDeclaration.Alias typ
        | PT.TypeDeclaration.Record fields ->
          let! fields = Ply.NEList.mapSequentially RecordField.toRT fields
          return RT.TypeDeclaration.Record fields
        | PT.TypeDeclaration.Enum cases ->
          let! cases = Ply.NEList.mapSequentially EnumCase.toRT cases
          return RT.TypeDeclaration.Enum cases
      }

  let toRT (t : PT.TypeDeclaration.T) : Ply<RT.TypeDeclaration.T> =
    uply {
      let! def = Definition.toRT t.definition
      return { typeParams = t.typeParams; definition = def }
    }


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

  let toRT (h : PT.Handler.T) : Ply<RT.Handler.T> =
    uply {
      let! ast = Expr.toRT h.ast
      return { tlid = h.tlid; ast = ast; spec = Spec.toRT h.spec }
    }

module DB =
  let toRT (db : PT.DB.T) : Ply<RT.DB.T> =
    uply {
      let! typ = TypeReference.toRT db.typ
      return { tlid = db.tlid; name = db.name; version = db.version; typ = typ }
    }

module UserType =
  let toRT (t : PT.UserType.T) : Ply<RT.UserType.T> =
    uply {
      let! decl = TypeDeclaration.toRT t.declaration
      return
        { tlid = t.tlid
          name = TypeName.UserProgram.toRT t.name
          declaration = decl }
    }

module UserConstant =
  let toRT (c : PT.UserConstant.T) : Ply<RT.UserConstant.T> =
    uply {
      let! body = Const.toRT c.body
      return
        { tlid = c.tlid; name = ConstantName.UserProgram.toRT c.name; body = body }
    }

module UserFunction =
  module Parameter =
    let toRT (p : PT.UserFunction.Parameter) : Ply<RT.UserFunction.Parameter> =
      uply {
        let! typ = TypeReference.toRT p.typ
        return { name = p.name; typ = typ }
      }

  let toRT (f : PT.UserFunction.T) : Ply<RT.UserFunction.T> =
    uply {
      let! parameters = Ply.NEList.mapSequentially Parameter.toRT f.parameters
      let! returnType = TypeReference.toRT f.returnType
      let! body = Expr.toRT f.body
      return
        { tlid = f.tlid
          name = FnName.UserProgram.toRT f.name
          typeParams = f.typeParams
          parameters = parameters
          returnType = returnType
          body = body }
    }

module Toplevel =
  let toRT (tl : PT.Toplevel.T) : Ply<RT.Toplevel.T> =
    uply {
      match tl with
      | PT.Toplevel.TLHandler h ->
        let! h = Handler.toRT h
        return RT.Toplevel.TLHandler h

      | PT.Toplevel.TLDB db ->
        let! db = DB.toRT db
        return RT.Toplevel.TLDB db

      | PT.Toplevel.TLFunction f ->
        let! f = UserFunction.toRT f
        return RT.Toplevel.TLFunction f

      | PT.Toplevel.TLType t ->
        let! t = UserType.toRT t
        return RT.Toplevel.TLType t

      | PT.Toplevel.TLConstant c ->
        let! c = UserConstant.toRT c
        return RT.Toplevel.TLConstant c
    }

module Secret =
  let toRT (s : PT.Secret.T) : RT.Secret.T =
    { name = s.name; value = s.value; version = s.version }

module PackageFn =
  module Parameter =
    let toRT (p : PT.PackageFn.Parameter) : Ply<RT.PackageFn.Parameter> =
      uply {
        let! typ = TypeReference.toRT p.typ
        return { name = p.name; typ = typ }
      }

  let toRT (f : PT.PackageFn.T) : Ply<RT.PackageFn.T> =
    uply {
      let! body = Expr.toRT f.body
      let! parameters = Ply.NEList.mapSequentially Parameter.toRT f.parameters
      let! returnType = TypeReference.toRT f.returnType
      return
        { name = FnName.Package.toRT f.name
          tlid = f.tlid
          body = body
          typeParams = f.typeParams
          parameters = parameters
          returnType = returnType }
    }

module PackageType =
  let toRT (t : PT.PackageType.T) : Ply<RT.PackageType.T> =
    uply {
      let! decl = TypeDeclaration.toRT t.declaration
      return { name = TypeName.Package.toRT t.name; declaration = decl }
    }

module PackageConstant =
  let toRT (c : PT.PackageConstant.T) : Ply<RT.PackageConstant.T> =
    uply {
      let! body = Const.toRT c.body
      return { name = ConstantName.Package.toRT c.name; body = body }
    }
