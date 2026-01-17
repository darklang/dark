/// Extracts dependency references from AST expressions.
/// Used to track what functions/types/values reference what other functions/types/values.
module LibPackageManager.DependencyExtractor

open Prelude

module PT = LibExecution.ProgramTypes


/// A dependency is just the UUID of what's being depended on.
type Dependency = uuid

/// Extract package UUID from a NameResolution if it resolved to a Package
let private extractFromNameResolution
  (nr : PT.NameResolution<'a>)
  (extractPackageId : 'a -> Option<uuid>)
  : List<Dependency> =
  match nr with
  | Ok resolved ->
    match extractPackageId resolved with
    | Some id -> [ id ]
    | None -> []
  | Error _ -> []


/// Extract package UUID from FQFnName
let private extractFnId (fn : PT.FQFnName.FQFnName) : Option<uuid> =
  match fn with
  | PT.FQFnName.Package id -> Some id
  | PT.FQFnName.Builtin _ -> None


/// Extract package UUID from FQTypeName
let private extractTypeId (typ : PT.FQTypeName.FQTypeName) : Option<uuid> =
  match typ with
  | PT.FQTypeName.Package id -> Some id


/// Extract package UUID from FQValueName
let private extractValueId (value : PT.FQValueName.FQValueName) : Option<uuid> =
  match value with
  | PT.FQValueName.Package id -> Some id
  | PT.FQValueName.Builtin _ -> None


/// Extract dependencies from a Deprecation field
let private extractFromDeprecation
  (extractId : 'a -> Option<uuid>)
  (dep : PT.Deprecation<'a>)
  : List<Dependency> =
  match dep with
  | PT.NotDeprecated -> []
  | PT.DeprecatedBecause _ -> []
  | PT.RenamedTo name ->
    match extractId name with
    | Some id -> [ id ]
    | None -> []
  | PT.ReplacedBy name ->
    match extractId name with
    | Some id -> [ id ]
    | None -> []


/// Extract dependencies from a TypeReference
let rec private extractFromTypeRef (typeRef : PT.TypeReference) : List<Dependency> =
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
  | PT.TVariable _ -> []

  | PT.TList inner -> extractFromTypeRef inner
  | PT.TDict inner -> extractFromTypeRef inner
  | PT.TDB inner -> extractFromTypeRef inner

  | PT.TTuple(first, second, rest) ->
    [ extractFromTypeRef first
      extractFromTypeRef second
      rest |> List.collect extractFromTypeRef ]
    |> List.concat

  | PT.TCustomType(nr, typeArgs) ->
    List.concat
      [ extractFromNameResolution nr extractTypeId
        typeArgs |> List.collect extractFromTypeRef ]

  | PT.TFn(args, ret) ->
    List.concat
      [ args |> NEList.toList |> List.collect extractFromTypeRef
        extractFromTypeRef ret ]


/// Extract references from a StringSegment
let rec private extractFromStringSegment
  (segment : PT.StringSegment)
  : List<Dependency> =
  match segment with
  | PT.StringText _ -> []
  | PT.StringInterpolation expr -> extractFromExpr expr


/// Extract references from a LetPattern (no references in patterns themselves)
and private extractFromLetPattern (_pat : PT.LetPattern) : List<Dependency> = []


/// Extract references from a MatchPattern (no type references in match patterns)
and private extractFromMatchPattern (pat : PT.MatchPattern) : List<Dependency> =
  match pat with
  | PT.MPUnit _
  | PT.MPBool _
  | PT.MPInt8 _
  | PT.MPUInt8 _
  | PT.MPInt16 _
  | PT.MPUInt16 _
  | PT.MPInt32 _
  | PT.MPUInt32 _
  | PT.MPInt64 _
  | PT.MPUInt64 _
  | PT.MPInt128 _
  | PT.MPUInt128 _
  | PT.MPFloat _
  | PT.MPChar _
  | PT.MPString _
  | PT.MPVariable _ -> []

  | PT.MPList(_, pats) -> pats |> List.collect extractFromMatchPattern

  | PT.MPListCons(_, head, tail) ->
    List.concat [ extractFromMatchPattern head; extractFromMatchPattern tail ]

  | PT.MPTuple(_, first, second, rest) ->
    [ extractFromMatchPattern first
      extractFromMatchPattern second
      rest |> List.collect extractFromMatchPattern ]
    |> List.concat

  | PT.MPEnum(_, _, fieldPats) -> fieldPats |> List.collect extractFromMatchPattern

  | PT.MPOr(_, pats) -> pats |> NEList.toList |> List.collect extractFromMatchPattern


/// Extract references from a MatchCase
and private extractFromMatchCase (case : PT.MatchCase) : List<Dependency> =
  List.concat
    [ extractFromMatchPattern case.pat
      case.whenCondition |> Option.map extractFromExpr |> Option.defaultValue []
      extractFromExpr case.rhs ]


/// Extract references from a PipeExpr
and private extractFromPipeExpr (pipeExpr : PT.PipeExpr) : List<Dependency> =
  match pipeExpr with
  | PT.EPipeLambda(_, _, body) -> extractFromExpr body

  | PT.EPipeInfix(_, _, rhs) -> extractFromExpr rhs

  | PT.EPipeFnCall(_, nr, typeArgs, args) ->
    List.concat
      [ extractFromNameResolution nr extractFnId
        typeArgs |> List.collect extractFromTypeRef
        args |> List.collect extractFromExpr ]

  | PT.EPipeEnum(_, nr, _, fields) ->
    List.concat
      [ extractFromNameResolution nr extractTypeId
        fields |> List.collect extractFromExpr ]

  | PT.EPipeVariable(_, _, args) -> args |> List.collect extractFromExpr


/// Extract all references from an expression (recursive AST walk)
and extractFromExpr (expr : PT.Expr) : List<Dependency> =
  match expr with
  // Simple expressions with no references
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
  | PT.ESelf _ -> []

  | PT.EString(_, segments) -> segments |> List.collect extractFromStringSegment

  // Flow control
  | PT.EIf(_, cond, thenExpr, elseExpr) ->
    List.concat
      [ extractFromExpr cond
        extractFromExpr thenExpr
        elseExpr |> Option.map extractFromExpr |> Option.defaultValue [] ]

  | PT.EPipe(_, lhs, parts) ->
    List.concat [ extractFromExpr lhs; parts |> List.collect extractFromPipeExpr ]

  | PT.EMatch(_, arg, cases) ->
    List.concat [ extractFromExpr arg; cases |> List.collect extractFromMatchCase ]

  | PT.ELet(_, pat, value, body) ->
    List.concat
      [ extractFromLetPattern pat; extractFromExpr value; extractFromExpr body ]

  // Basic structures
  | PT.EList(_, items) -> items |> List.collect extractFromExpr

  | PT.EDict(_, pairs) -> pairs |> List.collect (snd >> extractFromExpr)

  | PT.ETuple(_, first, second, rest) ->
    [ extractFromExpr first
      extractFromExpr second
      rest |> List.collect extractFromExpr ]
    |> List.concat

  // Function application
  | PT.EApply(_, fnExpr, typeArgs, args) ->
    List.concat
      [ extractFromExpr fnExpr
        typeArgs |> List.collect extractFromTypeRef
        args |> NEList.toList |> List.collect extractFromExpr ]

  | PT.EFnName(_, nr) -> extractFromNameResolution nr extractFnId

  | PT.ELambda(_, _, body) -> extractFromExpr body

  | PT.EInfix(_, _, lhs, rhs) ->
    List.concat [ extractFromExpr lhs; extractFromExpr rhs ]

  // Records and custom types
  | PT.ERecord(_, nr, typeArgs, fields) ->
    List.concat
      [ extractFromNameResolution nr extractTypeId
        typeArgs |> List.collect extractFromTypeRef
        fields |> List.collect (snd >> extractFromExpr) ]

  | PT.ERecordFieldAccess(_, record, _) -> extractFromExpr record

  | PT.ERecordUpdate(_, record, updates) ->
    List.concat
      [ extractFromExpr record
        updates |> NEList.toList |> List.collect (snd >> extractFromExpr) ]

  | PT.EEnum(_, nr, typeArgs, _, fields) ->
    List.concat
      [ extractFromNameResolution nr extractTypeId
        typeArgs |> List.collect extractFromTypeRef
        fields |> List.collect extractFromExpr ]

  | PT.EValue(_, nr) -> extractFromNameResolution nr extractValueId

  | PT.EStatement(_, first, next) ->
    List.concat [ extractFromExpr first; extractFromExpr next ]


/// Extract all references from a function definition
let extractFromFn (fn : PT.PackageFn.PackageFn) : List<Dependency> =
  // Deduplicate references
  List.concat
    [ extractFromExpr fn.body
      fn.parameters
      |> NEList.toList
      |> List.collect (fun p -> extractFromTypeRef p.typ)
      extractFromTypeRef fn.returnType
      extractFromDeprecation extractFnId fn.deprecated ]
  |> List.distinct


/// Extract all references from a value definition
let extractFromValue (value : PT.PackageValue.PackageValue) : List<Dependency> =
  List.concat
    [ extractFromExpr value.body
      extractFromDeprecation extractValueId value.deprecated ]
  |> List.distinct


/// Extract all references from a type definition
let extractFromType (typ : PT.PackageType.PackageType) : List<Dependency> =
  let extractFromDefinition
    (def : PT.TypeDeclaration.Definition)
    : List<Dependency> =
    match def with
    | PT.TypeDeclaration.Alias typeRef -> extractFromTypeRef typeRef

    | PT.TypeDeclaration.Record fields ->
      fields |> NEList.toList |> List.collect (fun f -> extractFromTypeRef f.typ)

    | PT.TypeDeclaration.Enum cases ->
      cases
      |> NEList.toList
      |> List.collect (fun c ->
        c.fields |> List.collect (fun f -> extractFromTypeRef f.typ))

  List.concat
    [ extractFromDefinition typ.declaration.definition
      extractFromDeprecation extractTypeId typ.deprecated ]
  |> List.distinct
