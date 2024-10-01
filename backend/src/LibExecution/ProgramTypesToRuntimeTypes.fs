/// Convert from ProgramTypes to RuntimeTypes
module LibExecution.ProgramTypesToRuntimeTypes

open Prelude

// Used for conversion functions
module RT = RuntimeTypes
module PT = ProgramTypes

module FQTypeName =
  module Package =
    let toRT (p : PT.FQTypeName.Package) : RT.FQTypeName.Package = p

    let fromRT (p : RT.FQTypeName.Package) : PT.FQTypeName.Package = p


  let toRT (fqtn : PT.FQTypeName.FQTypeName) : RT.FQTypeName.FQTypeName =
    match fqtn with
    | PT.FQTypeName.Package p -> RT.FQTypeName.Package(Package.toRT p)

  let fromRT (fqtn : RT.FQTypeName.FQTypeName) : Option<PT.FQTypeName.FQTypeName> =
    match fqtn with
    | RT.FQTypeName.Package p -> PT.FQTypeName.Package(Package.fromRT p) |> Some


module FQConstantName =
  module Builtin =
    let toRT (c : PT.FQConstantName.Builtin) : RT.FQConstantName.Builtin =
      { name = c.name; version = c.version }

    let fromRT (c : RT.FQConstantName.Builtin) : PT.FQConstantName.Builtin =
      { name = c.name; version = c.version }

  module Package =
    let toRT (c : PT.FQConstantName.Package) : RT.FQConstantName.Package = c

    let fromRT (c : RT.FQConstantName.Package) : PT.FQConstantName.Package = c


  let toRT
    (name : PT.FQConstantName.FQConstantName)
    : RT.FQConstantName.FQConstantName =
    match name with
    | PT.FQConstantName.Builtin s -> RT.FQConstantName.Builtin(Builtin.toRT s)
    | PT.FQConstantName.Package p -> RT.FQConstantName.Package(Package.toRT p)


module FQFnName =
  module Builtin =
    let toRT (s : PT.FQFnName.Builtin) : RT.FQFnName.Builtin =
      { name = s.name; version = s.version }

    let fromRT (s : RT.FQFnName.Builtin) : PT.FQFnName.Builtin =
      { name = s.name; version = s.version }

  module Package =
    let toRT (p : PT.FQFnName.Package) : RT.FQFnName.Package = p

    let fromRT (p : RT.FQFnName.Package) : PT.FQFnName.Package = p

  let toRT (fqfn : PT.FQFnName.FQFnName) : RT.FQFnName.FQFnName =
    match fqfn with
    | PT.FQFnName.Builtin s -> RT.FQFnName.Builtin(Builtin.toRT s)
    | PT.FQFnName.Package p -> RT.FQFnName.Package(Package.toRT p)


module NameResolution =
  let toRT (f : 'a -> 'b) (nr : PT.NameResolution<'a>) : RT.NameResolution<'b> =
    match nr with
    | Ok x -> Ok(f x)
    | Error e -> Error(NameResolutionError.RTE.toRuntimeError e)


module TypeReference =
  let rec toRT (t : PT.TypeReference) : RT.TypeReference =
    match t with
    | PT.TInt64 -> RT.TInt64
    | PT.TUInt64 -> RT.TUInt64
    | PT.TInt8 -> RT.TInt8
    | PT.TUInt8 -> RT.TUInt8
    | PT.TInt16 -> RT.TInt16
    | PT.TUInt16 -> RT.TUInt16
    | PT.TInt32 -> RT.TInt32
    | PT.TUInt32 -> RT.TUInt32
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
        NameResolution.toRT FQTypeName.toRT typeName,
        List.map toRT typeArgs
      )
    | PT.TVariable(name) -> RT.TVariable(name)
    | PT.TFn(paramTypes, returnType) ->
      RT.TFn(NEList.map toRT paramTypes, toRT returnType)


module InfixFnName =
  let toFnName (name : PT.InfixFnName) : (string * int) =
    match name with
    | PT.ArithmeticPlus -> ("int64Add", 0)
    | PT.ArithmeticMinus -> ("int64Subtract", 0)
    | PT.ArithmeticMultiply -> ("int64Multiply", 0)
    | PT.ArithmeticDivide -> ("floatDivide", 0)
    | PT.ArithmeticModulo -> ("int64Mod", 0)
    | PT.ArithmeticPower -> ("int64Power", 0)
    | PT.ComparisonGreaterThan -> ("int64GreaterThan", 0)
    | PT.ComparisonGreaterThanOrEqual -> ("int64GreaterThanOrEqualTo", 0)
    | PT.ComparisonLessThan -> ("int64LessThan", 0)
    | PT.ComparisonLessThanOrEqual -> ("int64LessThanOrEqualTo", 0)
    | PT.StringConcat -> ("stringAppend", 0)
    | PT.ComparisonEquals -> ("equals", 0)
    | PT.ComparisonNotEquals -> ("notEquals", 0)


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
    | PT.MPInt64(id, i) -> RT.MPInt64(id, i)
    | PT.MPUInt64(id, i) -> RT.MPUInt64(id, i)
    | PT.MPInt8(id, i) -> RT.MPInt8(id, i)
    | PT.MPUInt8(id, i) -> RT.MPUInt8(id, i)
    | PT.MPInt16(id, i) -> RT.MPInt16(id, i)
    | PT.MPUInt16(id, i) -> RT.MPUInt16(id, i)
    | PT.MPInt32(id, i) -> RT.MPInt32(id, i)
    | PT.MPUInt32(id, i) -> RT.MPUInt32(id, i)
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
  let replaceEscapeSequences (s : string) : string =
    s
    |> fun s -> s.Replace(@"\t", "\t")
    |> fun s -> s.Replace(@"\n", "\n")
    |> fun s -> s.Replace(@"\r", "\r")
    |> fun s -> s.Replace(@"\b", "\b")
    |> fun s -> s.Replace(@"\f", "\f")
    |> fun s -> s.Replace(@"\v", "\v")
    |> fun s -> s.Replace(@"\""", "\"")
    |> fun s -> s.Replace(@"\'", "'")
    |> fun s -> s.Replace(@"\\", "\\")

  let replaceHex (text : string) : string =
    // matches hexadecimal escape sequences (e.g. `\x01`)
    let regex = System.Text.RegularExpressions.Regex(@"\\x([0-9A-Fa-f]{2})")

    regex.Replace(
      text,
      fun (m : System.Text.RegularExpressions.Match) ->
        // Groups[1] refers to the first captured group (the two hex digits)
        m.Groups[1].Value
        |> fun hex -> System.Convert.ToByte(hex, 16)
        |> char
        |> string
    )

  let replaceUnicode (text : string) : string =
    // matches unicode escape sequences (e.g. `\u0001`)
    let regex = System.Text.RegularExpressions.Regex(@"\\u([0-9A-Fa-f]{4})")

    regex.Replace(
      text,
      fun (m : System.Text.RegularExpressions.Match) ->
        // Groups[1] refers to the first captured group (the four hex digits)
        m.Groups[1].Value
        |> fun hex -> System.Convert.ToInt32(hex, 16)
        |> char
        |> string
    )

  let rec toRT (e : PT.Expr) : RT.Expr =
    match e with
    | PT.EChar(id, char) -> RT.EChar(id, char |> replaceEscapeSequences)
    | PT.EInt64(id, num) -> RT.EInt64(id, num)
    | PT.EUInt64(id, num) -> RT.EUInt64(id, num)
    | PT.EInt8(id, num) -> RT.EInt8(id, num)
    | PT.EUInt8(id, num) -> RT.EUInt8(id, num)
    | PT.EInt16(id, num) -> RT.EInt16(id, num)
    | PT.EUInt16(id, num) -> RT.EUInt16(id, num)
    | PT.EInt32(id, num) -> RT.EInt32(id, num)
    | PT.EUInt32(id, num) -> RT.EUInt32(id, num)
    | PT.EInt128(id, num) -> RT.EInt128(id, num)
    | PT.EUInt128(id, num) -> RT.EUInt128(id, num)

    | PT.EString(id, segments) -> RT.EString(id, List.map stringSegmentToRT segments)

    | PT.EFloat(id, sign, whole, fraction) ->
      let whole = if whole = "" then "0" else whole
      let fraction = if fraction = "" then "0" else fraction
      RT.EFloat(id, makeFloat sign whole fraction)
    | PT.EBool(id, b) -> RT.EBool(id, b)
    | PT.EUnit id -> RT.EUnit id

    | PT.EConstant(id, Ok name) -> RT.EConstant(id, FQConstantName.toRT name)
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

    | PT.EFnName(id, Ok name) -> RT.EFnName(id, FQFnName.toRT name)
    | PT.EFnName(id, Error err) ->
      RT.EError(id, NameResolutionError.RTE.toRuntimeError err, [])

    // CLEANUP tidy infix stuff - extract to another fn?
    | PT.EInfix(id, PT.InfixFnCall fnName, left, right) ->
      let (fn, version) = InfixFnName.toFnName fnName
      let name = RT.FQFnName.Builtin({ name = fn; version = version })
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
        RT.ERecord(id, FQTypeName.toRT typeName, fields)
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
            RT.EFnName(id, FQFnName.toRT fnName),
            List.map TypeReference.toRT typeArgs,
            exprs |> List.map toRT |> NEList.ofList prev
          )
        | PT.EPipeInfix(id, PT.InfixFnCall fnName, expr) ->
          let (fn, version) = InfixFnName.toFnName fnName
          let name = PT.FQFnName.Builtin({ name = fn; version = version })
          RT.EApply(
            id,
            RT.EFnName(id, FQFnName.toRT name),
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
            FQTypeName.toRT typeName,
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
      RT.EEnum(id, FQTypeName.toRT typeName, caseName, List.map toRT fields)
    | PT.EEnum(id, Error err, _caseName, fields) ->
      RT.EError(id, NameResolutionError.RTE.toRuntimeError err, List.map toRT fields)

    | PT.EDict(id, entries) ->
      RT.EDict(id, entries |> List.map (Tuple2.mapSecond toRT))


  and stringSegmentToRT (segment : PT.StringSegment) : RT.StringSegment =
    match segment with
    | PT.StringText text ->
      text |> replaceEscapeSequences |> replaceHex |> replaceUnicode  |> RT.StringText
    | PT.StringInterpolation expr -> RT.StringInterpolation(toRT expr)


module Const =
  let rec toRT (c : PT.Const) : RT.Const =
    match c with
    | PT.Const.CInt64 i -> RT.CInt64 i
    | PT.Const.CUInt64 i -> RT.CUInt64 i
    | PT.Const.CInt8 i -> RT.CInt8 i
    | PT.Const.CUInt8 i -> RT.CUInt8 i
    | PT.Const.CInt16 i -> RT.CInt16 i
    | PT.Const.CUInt16 i -> RT.CUInt16 i
    | PT.Const.CInt32 i -> RT.CInt32 i
    | PT.Const.CUInt32 i -> RT.CUInt32 i
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
        NameResolution.toRT FQTypeName.toRT typeName,
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


// --
// Package stuff
// --
module PackageType =
  let toRT (t : PT.PackageType.PackageType) : RT.PackageType.PackageType =
    { id = t.id; declaration = TypeDeclaration.toRT t.declaration }

module PackageConstant =
  let toRT
    (c : PT.PackageConstant.PackageConstant)
    : RT.PackageConstant.PackageConstant =
    { id = c.id; body = Const.toRT c.body }

module PackageFn =
  module Parameter =
    let toRT (p : PT.PackageFn.Parameter) : RT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toRT p.typ }

  let toRT (f : PT.PackageFn.PackageFn) : RT.PackageFn.PackageFn =
    { id = f.id
      body = f.body |> Expr.toRT
      typeParams = f.typeParams
      parameters = f.parameters |> NEList.map Parameter.toRT
      returnType = f.returnType |> TypeReference.toRT }



// --
// User stuff
// --
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

module Secret =
  let toRT (s : PT.Secret.T) : RT.Secret.T =
    { name = s.name; value = s.value; version = s.version }



module PackageManager =
  let toRT (pm : PT.PackageManager) : RT.PackageManager =
    { getType = fun id -> pm.getType id |> Ply.map (Option.map PackageType.toRT)
      getConstant =
        fun id -> pm.getConstant id |> Ply.map (Option.map PackageConstant.toRT)
      getFn = fun id -> pm.getFn id |> Ply.map (Option.map PackageFn.toRT)

      init = pm.init }
