/// Transforms AST expressions by replacing UUID references during propagation.
module LibPackageManager.AstTransformer

open Prelude

module PT = LibExecution.ProgramTypes


type UuidMapping = Map<uuid, uuid>

let private replaceUuid (mapping : UuidMapping) (id : uuid) : uuid =
  mapping |> Map.tryFind id |> Option.defaultValue id

let private transformNameResolution
  (mapping : UuidMapping)
  (nr : PT.NameResolution<'a>)
  (transform : uuid -> 'a) // rebuild the value from a new UUID
  (getPackageId : 'a -> Option<uuid>) // extracts the UUID from a resolved name
  : PT.NameResolution<'a> =
  match nr with
  | Ok resolved ->
    match getPackageId resolved with
    | Some id ->
      let newId = replaceUuid mapping id
      if newId = id then nr else Ok(transform newId)
    | None -> nr
  | Error _ -> nr

let private getFnPackageId (fn : PT.FQFnName.FQFnName) : Option<uuid> =
  match fn with
  | PT.FQFnName.Package id -> Some id
  | PT.FQFnName.Builtin _ -> None

let private getTypePackageId (typ : PT.FQTypeName.FQTypeName) : Option<uuid> =
  match typ with
  | PT.FQTypeName.Package id -> Some id

let private getValuePackageId (value : PT.FQValueName.FQValueName) : Option<uuid> =
  match value with
  | PT.FQValueName.Package id -> Some id
  | PT.FQValueName.Builtin _ -> None

let rec private transformTypeRef
  (mapping : UuidMapping)
  (typeRef : PT.TypeReference)
  : PT.TypeReference =
  /// CLEANUP introduce `let r = ...`
  match typeRef with
  | PT.TUnit
  | PT.TBool
  | PT.TInt8
  | PT.TUInt8
  | PT.TInt16
  | PT.TUInt16
  | PT.TInt32
  | PT.TUInt32
  | PT.TInt64
  | PT.TUInt64
  | PT.TInt128
  | PT.TUInt128
  | PT.TFloat
  | PT.TChar
  | PT.TString
  | PT.TUuid
  | PT.TDateTime
  | PT.TVariable _ -> typeRef

  | PT.TList inner -> PT.TList(transformTypeRef mapping inner)
  | PT.TDict inner -> PT.TDict(transformTypeRef mapping inner)
  | PT.TDB inner -> PT.TDB(transformTypeRef mapping inner)

  | PT.TTuple(first, second, rest) ->
    PT.TTuple(
      transformTypeRef mapping first,
      transformTypeRef mapping second,
      rest |> List.map (transformTypeRef mapping)
    )

  | PT.TCustomType(nr, typeArgs) ->
    PT.TCustomType(
      transformNameResolution mapping nr PT.FQTypeName.Package getTypePackageId,
      typeArgs |> List.map (transformTypeRef mapping)
    )

  | PT.TFn(args, ret) ->
    PT.TFn(
      args |> NEList.map (transformTypeRef mapping),
      transformTypeRef mapping ret
    )

/// RenamedTo/ReplacedBy hold a UUID pointing to another package item.
/// When that item gets a new UUID during propagation, we must rewrite the reference.
let private transformDeprecation
  (mapping : UuidMapping)
  (transform : uuid -> 'a)
  (getPackageId : 'a -> Option<uuid>)
  (dep : PT.Deprecation<'a>)
  : PT.Deprecation<'a> =
  match dep with
  | PT.NotDeprecated -> PT.NotDeprecated
  | PT.DeprecatedBecause reason -> PT.DeprecatedBecause reason
  | PT.RenamedTo name ->
    match getPackageId name with
    | Some id -> PT.RenamedTo(transform (replaceUuid mapping id))
    | None -> dep
  | PT.ReplacedBy name ->
    match getPackageId name with
    | Some id -> PT.ReplacedBy(transform (replaceUuid mapping id))
    | None -> dep

let rec private transformStringSegment
  (mapping : UuidMapping)
  (segment : PT.StringSegment)
  : PT.StringSegment =
  match segment with
  | PT.StringText _ -> segment
  | PT.StringInterpolation expr -> PT.StringInterpolation(transformExpr mapping expr)

and private transformMatchCase
  (mapping : UuidMapping)
  (case : PT.MatchCase)
  : PT.MatchCase =
  { pat = case.pat // Patterns don't contain UUID references
    whenCondition = case.whenCondition |> Option.map (transformExpr mapping)
    rhs = transformExpr mapping case.rhs }

and private transformPipeExpr
  (mapping : UuidMapping)
  (pipeExpr : PT.PipeExpr)
  : PT.PipeExpr =
  match pipeExpr with
  | PT.EPipeLambda(id, pats, body) ->
    PT.EPipeLambda(id, pats, transformExpr mapping body)

  | PT.EPipeInfix(id, infix, rhs) ->
    PT.EPipeInfix(id, infix, transformExpr mapping rhs)

  | PT.EPipeFnCall(id, nr, typeArgs, args) ->
    PT.EPipeFnCall(
      id,
      transformNameResolution mapping nr PT.FQFnName.Package getFnPackageId,
      typeArgs |> List.map (transformTypeRef mapping),
      args |> List.map (transformExpr mapping)
    )

  | PT.EPipeEnum(id, nr, caseName, fields) ->
    PT.EPipeEnum(
      id,
      transformNameResolution mapping nr PT.FQTypeName.Package getTypePackageId,
      caseName,
      fields |> List.map (transformExpr mapping)
    )

  | PT.EPipeVariable(id, varName, args) ->
    PT.EPipeVariable(id, varName, args |> List.map (transformExpr mapping))

and private transformExpr (mapping : UuidMapping) (expr : PT.Expr) : PT.Expr =
  match expr with
  | PT.EUnit _
  | PT.EBool _
  | PT.EInt8 _
  | PT.EUInt8 _
  | PT.EInt16 _
  | PT.EUInt16 _
  | PT.EInt32 _
  | PT.EUInt32 _
  | PT.EInt64 _
  | PT.EUInt64 _
  | PT.EInt128 _
  | PT.EUInt128 _
  | PT.EFloat _
  | PT.EChar _
  | PT.EVariable _
  | PT.EArg _
  | PT.ESelf _ -> expr

  | PT.EString(id, segments) ->
    PT.EString(id, segments |> List.map (transformStringSegment mapping))

  | PT.EIf(id, cond, thenExpr, elseExpr) ->
    PT.EIf(
      id,
      transformExpr mapping cond,
      transformExpr mapping thenExpr,
      elseExpr |> Option.map (transformExpr mapping)
    )

  | PT.EPipe(id, lhs, parts) ->
    PT.EPipe(
      id,
      transformExpr mapping lhs,
      parts |> List.map (transformPipeExpr mapping)
    )

  | PT.EMatch(id, arg, cases) ->
    PT.EMatch(
      id,
      transformExpr mapping arg,
      cases |> List.map (transformMatchCase mapping)
    )

  | PT.ELet(id, pat, value, body) ->
    PT.ELet(id, pat, transformExpr mapping value, transformExpr mapping body)

  | PT.EList(id, items) -> PT.EList(id, items |> List.map (transformExpr mapping))

  | PT.EDict(id, pairs) ->
    PT.EDict(id, pairs |> List.map (fun (k, v) -> (k, transformExpr mapping v)))

  | PT.ETuple(id, first, second, rest) ->
    PT.ETuple(
      id,
      transformExpr mapping first,
      transformExpr mapping second,
      rest |> List.map (transformExpr mapping)
    )

  | PT.EApply(id, fnExpr, typeArgs, args) ->
    PT.EApply(
      id,
      transformExpr mapping fnExpr,
      typeArgs |> List.map (transformTypeRef mapping),
      args |> NEList.map (transformExpr mapping)
    )

  | PT.EFnName(id, nr) ->
    PT.EFnName(
      id,
      transformNameResolution mapping nr PT.FQFnName.Package getFnPackageId
    )

  | PT.ELambda(id, pats, body) -> PT.ELambda(id, pats, transformExpr mapping body)

  | PT.EInfix(id, infix, lhs, rhs) ->
    PT.EInfix(id, infix, transformExpr mapping lhs, transformExpr mapping rhs)

  | PT.ERecord(id, nr, typeArgs, fields) ->
    PT.ERecord(
      id,
      transformNameResolution mapping nr PT.FQTypeName.Package getTypePackageId,
      typeArgs |> List.map (transformTypeRef mapping),
      fields |> List.map (fun (name, expr) -> (name, transformExpr mapping expr))
    )

  | PT.ERecordFieldAccess(id, record, fieldName) ->
    PT.ERecordFieldAccess(id, transformExpr mapping record, fieldName)

  | PT.ERecordUpdate(id, record, updates) ->
    PT.ERecordUpdate(
      id,
      transformExpr mapping record,
      updates |> NEList.map (fun (name, expr) -> (name, transformExpr mapping expr))
    )

  | PT.EEnum(id, nr, typeArgs, caseName, fields) ->
    PT.EEnum(
      id,
      transformNameResolution mapping nr PT.FQTypeName.Package getTypePackageId,
      typeArgs |> List.map (transformTypeRef mapping),
      caseName,
      fields |> List.map (transformExpr mapping)
    )

  | PT.EValue(id, nr) ->
    PT.EValue(
      id,
      transformNameResolution mapping nr PT.FQValueName.Package getValuePackageId
    )

  | PT.EStatement(id, first, next) ->
    PT.EStatement(id, transformExpr mapping first, transformExpr mapping next)

let transformFn
  (mapping : UuidMapping)
  (fn : PT.PackageFn.PackageFn)
  : PT.PackageFn.PackageFn =
  { fn with
      body = transformExpr mapping fn.body
      parameters =
        fn.parameters
        |> NEList.map (fun p -> { p with typ = transformTypeRef mapping p.typ })
      returnType = transformTypeRef mapping fn.returnType
      deprecated =
        transformDeprecation mapping PT.FQFnName.Package getFnPackageId fn.deprecated }

let transformValue
  (mapping : UuidMapping)
  (value : PT.PackageValue.PackageValue)
  : PT.PackageValue.PackageValue =
  { value with
      body = transformExpr mapping value.body
      deprecated =
        transformDeprecation
          mapping
          PT.FQValueName.Package
          getValuePackageId
          value.deprecated }

let private transformTypeDefinition
  (mapping : UuidMapping)
  (def : PT.TypeDeclaration.Definition)
  : PT.TypeDeclaration.Definition =
  match def with
  | PT.TypeDeclaration.Alias typeRef ->
    PT.TypeDeclaration.Alias(transformTypeRef mapping typeRef)

  | PT.TypeDeclaration.Record fields ->
    PT.TypeDeclaration.Record(
      fields |> NEList.map (fun f -> { f with typ = transformTypeRef mapping f.typ })
    )

  | PT.TypeDeclaration.Enum cases ->
    PT.TypeDeclaration.Enum(
      cases
      |> NEList.map (fun c ->
        { c with
            fields =
              c.fields
              |> List.map (fun f -> { f with typ = transformTypeRef mapping f.typ }) })
    )

let transformType
  (mapping : UuidMapping)
  (typ : PT.PackageType.PackageType)
  : PT.PackageType.PackageType =
  { typ with
      declaration =
        { typ.declaration with
            definition = transformTypeDefinition mapping typ.declaration.definition }
      deprecated =
        transformDeprecation
          mapping
          PT.FQTypeName.Package
          getTypePackageId
          typ.deprecated }
