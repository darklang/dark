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
  let toRT (f : 'a -> 'b) (nr : PT.NameResolution<'a>) : RT.NameResolution<'b> =
    match nr with
    | Ok x -> Ok(f x)
    | Error e -> Error(NameResolutionError.RTE.toRuntimeError e)

module TypeReference =
  let rec toRT (t : PT.TypeReference) : RT.TypeReference =
    match t with
    | PT.TInt -> RT.TInt
    | PT.TInt8 -> RT.TInt8
    | PT.TUInt8 -> RT.TUInt8
    | PT.TInt16 -> RT.TInt16
    | PT.TUInt16 -> RT.TUInt16
    | PT.TInt128 -> RT.TInt128
    | PT.TUInt128 -> RT.TUInt128
    | PT.TFloat -> RT.TFloat
    | PT.TBool -> RT.TBool
    | PT.TUnit -> RT.TUnit
    | PT.TString -> RT.TString
    | PT.TList inner -> RT.TList(toRT inner)
    | PT.TTuple(first, second, theRest) ->
      RT.TTuple(toRT first, toRT second, theRest |> List.map toRT)
    | PT.TDict typ -> RT.TDict(toRT typ)
    | PT.TDB typ -> RT.TDB(toRT typ)
    | PT.TDateTime -> RT.TDateTime
    | PT.TChar -> RT.TChar
    | PT.TUuid -> RT.TUuid
    | PT.TCustomType(typeName, typeArgs) ->
      RT.TCustomType(
        NameResolution.toRT TypeName.toRT typeName,
        List.map toRT typeArgs
      )
    | PT.TBytes -> RT.TBytes
    | PT.TVariable(name) -> RT.TVariable(name)
    | PT.TFn(paramTypes, returnType) ->
      RT.TFn(NEList.map toRT paramTypes, toRT returnType)

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
    | PT.MPInt8(id, i) -> RT.MPInt8(id, i)
    | PT.MPUInt8(id, i) -> RT.MPUInt8(id, i)
    | PT.MPInt16(id, i) -> RT.MPInt16(id, i)
    | PT.MPUInt16(id, i) -> RT.MPUInt16(id, i)
    | PT.MPInt128(id, i) -> RT.MPInt128(id, i)
    | PT.MPUInt128(id, i) -> RT.MPUInt128(id, i)
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
    | PT.EInt8(id, num) -> RT.EInt8(id, num)
    | PT.EUInt8(id, num) -> RT.EUInt8(id, num)
    | PT.EInt16(id, num) -> RT.EInt16(id, num)
    | PT.EUInt16(id, num) -> RT.EUInt16(id, num)
    | PT.EInt128(id, num) -> RT.EInt128(id, num)
    | PT.EUInt128(id, num) -> RT.EUInt128(id, num)

    | PT.EString(id, segments) -> RT.EString(id, List.map stringSegmentToRT segments)

    | PT.EFloat(id, sign, whole, fraction) ->
      let whole = if whole = "" then "0" else whole
      let fraction = if fraction = "" then "0" else fraction
      RT.EFloat(id, makeFloat sign whole fraction)
    | PT.EBool(id, b) -> RT.EBool(id, b)
    | PT.EUnit id -> RT.EUnit id

    | PT.EConstant(id, Ok name) -> RT.EConstant(id, ConstantName.toRT name)
    | PT.EConstant(id, Error err) ->
      RT.EError(id, NameResolutionError.RTE.toRuntimeError err, [])

    | PT.EVariable(id, var) -> RT.EVariable(id, var)

    | PT.EFieldAccess(id, obj, fieldname) -> RT.EFieldAccess(id, toRT obj, fieldname)

    | PT.EApply(id, fnName, typeArgs, args) ->
      RT.EApply(
        id,
        toRT fnName,
        List.map TypeReference.toRT typeArgs,
        NEList.map toRT args
      )

    | PT.EFnName(id, Ok name) -> RT.EFnName(id, FnName.toRT name)
    | PT.EFnName(id, Error err) ->
      RT.EError(id, NameResolutionError.RTE.toRuntimeError err, [])

    // CLEANUP tidy infix stuff - extract to another fn?
    | PT.EInfix(id, PT.InfixFnCall fnName, left, right) ->
      let (modules, fn, version) = InfixFnName.toFnName fnName
      let name =
        RT.FQName.BuiltIn(
          { modules = modules; name = RT.FnName.FnName fn; version = version }
        )
      RT.EApply(
        id,
        RT.EFnName(id, name),
        [],
        NEList.ofList (toRT left) [ toRT right ]
      )
    | PT.EInfix(id, PT.BinOp PT.BinOpAnd, left, right) ->
      RT.EAnd(id, toRT left, toRT right)
    | PT.EInfix(id, PT.BinOp PT.BinOpOr, left, right) ->
      RT.EOr(id, toRT left, toRT right)

    | PT.ELambda(id, pats, body) ->
      RT.ELambda(id, NEList.map LetPattern.toRT pats, toRT body)

    | PT.ELet(id, pattern, rhs, body) ->
      RT.ELet(id, LetPattern.toRT pattern, toRT rhs, toRT body)

    | PT.EIf(id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, toRT cond, toRT thenExpr, elseExpr |> Option.map toRT)

    | PT.EList(id, exprs) -> RT.EList(id, List.map toRT exprs)

    | PT.ETuple(id, first, second, theRest) ->
      RT.ETuple(id, toRT first, toRT second, List.map toRT theRest)

    | PT.ERecord(id, Ok typeName, fields) ->
      match fields with
      | [] ->
        let fields = fields |> List.map Tuple2.second |> List.map toRT
        RT.EError(
          id,
          RT.RuntimeError.oldError "Record must have at least one field",
          fields
        )
      | head :: tail ->
        let fields =
          NEList.ofList head tail
          |> NEList.map (fun (name, expr) -> (name, toRT expr))
        RT.ERecord(id, TypeName.toRT typeName, fields)
    | PT.ERecord(id, Error err, fields) ->
      RT.EError(
        id,
        err |> NameResolutionError.RTE.toRuntimeError,
        fields |> List.map Tuple2.second |> List.map toRT
      )

    | PT.ERecordUpdate(id, record, updates) ->
      RT.ERecordUpdate(
        id,
        toRT record,
        updates |> NEList.map (fun (fieldName, update) -> (fieldName, toRT update))
      )

    | PT.EPipe(pipeID, expr1, rest) ->
      // Convert v |> fn1 a |> fn2 |> fn3 b c
      // into fn3 (fn2 (fn1 v a)) b c
      let folder (prev : RT.Expr) (next : PT.PipeExpr) : RT.Expr =
        let applyFn (expr : RT.Expr) (args : List<RT.Expr>) =
          let typeArgs = []
          RT.EApply(pipeID, expr, typeArgs, NEList.ofList prev args)

        match next with
        | PT.EPipeFnCall(id, Error err, _typeArgs, exprs) ->
          let err = NameResolutionError.RTE.toRuntimeError err
          let addlExprs = List.map toRT exprs
          RT.EError(id, err, prev :: addlExprs)
        | PT.EPipeFnCall(id, Ok fnName, typeArgs, exprs) ->
          RT.EApply(
            id,
            RT.EFnName(id, FnName.toRT fnName),
            List.map TypeReference.toRT typeArgs,
            exprs |> List.map toRT |> NEList.ofList prev
          )
        | PT.EPipeInfix(id, PT.InfixFnCall fnName, expr) ->
          let (modules, fn, version) = InfixFnName.toFnName fnName
          let name =
            PT.FQName.BuiltIn(
              { modules = modules; name = PT.FnName.FnName fn; version = version }
            )
          RT.EApply(
            id,
            RT.EFnName(id, FnName.toRT name),
            [],
            NEList.doubleton prev (toRT expr)
          )
        // Binops work pretty naturally here
        | PT.EPipeInfix(id, PT.BinOp op, expr) ->
          match op with
          | PT.BinOpAnd -> RT.EAnd(id, prev, toRT expr)
          | PT.BinOpOr -> RT.EOr(id, prev, toRT expr)
        | PT.EPipeEnum(id, Ok typeName, caseName, fields) ->
          RT.EEnum(
            id,
            TypeName.toRT typeName,
            caseName,
            prev :: (List.map toRT fields)
          )
        | PT.EPipeEnum(id, Error err, _caseName, fields) ->
          RT.EError(
            id,
            NameResolutionError.RTE.toRuntimeError err,
            prev :: (List.map toRT fields)
          )
        | PT.EPipeVariable(id, name, exprs) ->
          applyFn (RT.EVariable(id, name)) (List.map toRT exprs)
        | PT.EPipeLambda(id, pats, body) ->
          applyFn (RT.ELambda(id, NEList.map LetPattern.toRT pats, toRT body)) []

      let init = toRT expr1
      List.fold folder init rest

    | PT.EMatch(id, mexpr, cases) ->
      match cases with
      | [] ->
        RT.EError(
          id,
          RT.RuntimeError.oldError "Match must have at least one case",
          [ toRT mexpr ]
        )
      | head :: tail ->
        let cases =
          NEList.ofList head tail
          |> NEList.map (fun case ->
            let pattern = MatchPattern.toRT case.pat
            let whenCondition = Option.map toRT case.whenCondition
            let expr = toRT case.rhs
            let result : RT.MatchCase =
              { pat = pattern; whenCondition = whenCondition; rhs = expr }
            result)

        RT.EMatch(id, toRT mexpr, cases)

    | PT.EEnum(id, Ok typeName, caseName, fields) ->
      RT.EEnum(id, TypeName.toRT typeName, caseName, List.map toRT fields)
    | PT.EEnum(id, Error err, _caseName, fields) ->
      RT.EError(id, NameResolutionError.RTE.toRuntimeError err, List.map toRT fields)

    | PT.EDict(id, entries) ->
      RT.EDict(id, entries |> List.map (Tuple2.mapSecond toRT))


  and stringSegmentToRT (segment : PT.StringSegment) : RT.StringSegment =
    match segment with
    | PT.StringText text -> RT.StringText text
    | PT.StringInterpolation expr -> RT.StringInterpolation(toRT expr)

module Const =

  let rec toRT (c : PT.Const) : RT.Const =
    match c with
    | PT.Const.CInt i -> RT.CInt i
    | PT.Const.CInt8 i -> RT.CInt8 i
    | PT.Const.CUInt8 i -> RT.CUInt8 i
    | PT.Const.CInt16 i -> RT.CInt16 i
    | PT.Const.CUInt16 i -> RT.CUInt16 i
    | PT.Const.CInt128 i -> RT.CInt128 i
    | PT.Const.CUInt128 i -> RT.CUInt128 i
    | PT.Const.CBool b -> RT.CBool b
    | PT.Const.CString s -> RT.CString s
    | PT.Const.CChar c -> RT.CChar c
    | PT.Const.CFloat(sign, w, f) -> RT.CFloat(sign, w, f)
    | PT.Const.CUnit -> RT.CUnit
    | PT.Const.CTuple(first, second, rest) ->
      RT.CTuple(toRT first, toRT second, List.map toRT rest)
    | PT.Const.CEnum(typeName, caseName, fields) ->
      RT.CEnum(
        NameResolution.toRT TypeName.toRT typeName,
        caseName,
        List.map toRT fields
      )
    | PT.Const.CList items -> RT.CList(List.map toRT items)
    | PT.Const.CDict entries -> RT.CDict(entries |> List.map (Tuple2.mapSecond toRT))

module TypeDeclaration =
  module RecordField =
    let toRT (f : PT.TypeDeclaration.RecordField) : RT.TypeDeclaration.RecordField =
      { name = f.name; typ = TypeReference.toRT f.typ }

  module EnumField =
    let toRT (f : PT.TypeDeclaration.EnumField) : RT.TypeReference =
      TypeReference.toRT f.typ

  module EnumCase =
    let toRT (c : PT.TypeDeclaration.EnumCase) : RT.TypeDeclaration.EnumCase =
      { name = c.name; fields = List.map EnumField.toRT c.fields }

  module Definition =
    let toRT (d : PT.TypeDeclaration.Definition) : RT.TypeDeclaration.Definition =
      match d with
      | PT.TypeDeclaration.Definition.Alias(typ) ->
        RT.TypeDeclaration.Alias(TypeReference.toRT typ)

      | PT.TypeDeclaration.Record fields ->
        RT.TypeDeclaration.Record(NEList.map RecordField.toRT fields)

      | PT.TypeDeclaration.Enum cases ->
        RT.TypeDeclaration.Enum(NEList.map EnumCase.toRT cases)

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

module UserConstant =
  let toRT (c : PT.UserConstant.T) : RT.UserConstant.T =
    { tlid = c.tlid
      name = ConstantName.UserProgram.toRT c.name
      body = Const.toRT c.body }

module UserFunction =
  module Parameter =
    let toRT (p : PT.UserFunction.Parameter) : RT.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toRT p.typ }

  let toRT (f : PT.UserFunction.T) : RT.UserFunction.T =
    { tlid = f.tlid
      name = FnName.UserProgram.toRT f.name
      typeParams = f.typeParams
      parameters = NEList.map Parameter.toRT f.parameters
      returnType = TypeReference.toRT f.returnType
      body = Expr.toRT f.body }

module Toplevel =
  let toRT (tl : PT.Toplevel.T) : RT.Toplevel.T =
    match tl with
    | PT.Toplevel.TLHandler h -> RT.Toplevel.TLHandler(Handler.toRT h)
    | PT.Toplevel.TLDB db -> RT.Toplevel.TLDB(DB.toRT db)
    | PT.Toplevel.TLFunction f -> RT.Toplevel.TLFunction(UserFunction.toRT f)
    | PT.Toplevel.TLType t -> RT.Toplevel.TLType(UserType.toRT t)
    | PT.Toplevel.TLConstant c -> RT.Toplevel.TLConstant(UserConstant.toRT c)

module Secret =
  let toRT (s : PT.Secret.T) : RT.Secret.T =
    { name = s.name; value = s.value; version = s.version }

module PackageFn =
  module Parameter =
    let toRT (p : PT.PackageFn.Parameter) : RT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toRT p.typ }

  let toRT (f : PT.PackageFn.T) : RT.PackageFn.T =
    { name = f.name |> FnName.Package.toRT
      tlid = f.tlid
      body = f.body |> Expr.toRT
      typeParams = f.typeParams
      parameters = f.parameters |> NEList.map Parameter.toRT
      returnType = f.returnType |> TypeReference.toRT }

module PackageType =
  let toRT (t : PT.PackageType.T) : RT.PackageType.T =
    { name = TypeName.Package.toRT t.name
      declaration = TypeDeclaration.toRT t.declaration }

module PackageConstant =
  let toRT (c : PT.PackageConstant.T) : RT.PackageConstant.T =
    { name = ConstantName.Package.toRT c.name; body = Const.toRT c.body }
