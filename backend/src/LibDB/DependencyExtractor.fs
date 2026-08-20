/// Extracts dependency references from AST expressions.
/// Used to track what functions/types/values reference what other functions/types/values.
module LibDB.DependencyExtractor

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module PackageItem = LibDB.PackageItem


/// A dep edge: referenced item's kind + hash + user-typed FQN. `location`
/// is `None` for builtins and unresolved names.
// Later, we might extend dependency-tracking to include Secrets and DBs
// that are referenced, as well as other things that can be referenced.
// And we may track the _purity_ of the dependencies in the same space.
type Dependency =
  { hash : Hash; itemKind : PT.ItemKind; location : Option<PT.PackageLocation> }

/// Extract package Hash from a NameResolution if it resolved to a Package.
/// The location stored on the NR rides along into the Dependency record.
let private extractFromNameResolution
  (nr : PT.NameResolution<'a>)
  (itemKind : PT.ItemKind)
  (getPackageHash : 'a -> Option<Hash>)
  : List<Dependency> =
  match nr.resolved with
  | Ok resolved ->
    match getPackageHash resolved.name with
    | Some hash ->
      [ { hash = hash; itemKind = itemKind; location = resolved.location } ]
    | None -> []
  | Error _ -> []


/// One node in the explicit dependency-discovery work list. Package declarations
/// are persisted input, so none of these walks may consume the process call stack:
/// a `StackOverflowException` cannot be caught by the at-rest checker boundary.
type private Work =
  | TypeRef of PT.TypeReference
  | Expr of PT.Expr
  | StringSegment of PT.StringSegment
  | MatchPattern of PT.MatchPattern
  | MatchCase of PT.MatchCase
  | PipeExpr of PT.PipeExpr

let private extract (roots : List<Work>) : List<Dependency> =
  let work = System.Collections.Generic.Stack<Work>()
  let mutable dependencies : List<Dependency> = []

  let pushInOrder (items : List<Work>) : unit =
    items |> List.rev |> List.iter work.Push

  let pushTypesInOrder (types : List<PT.TypeReference>) : unit =
    types |> List.rev |> List.iter (TypeRef >> work.Push)

  let pushExprsInOrder (exprs : List<PT.Expr>) : unit =
    exprs |> List.rev |> List.iter (Expr >> work.Push)

  let addNameResolution
    (nr : PT.NameResolution<'a>)
    (itemKind : PT.ItemKind)
    (getPackageHash : 'a -> Option<Hash>)
    : unit =
    for dependency in extractFromNameResolution nr itemKind getPackageHash do
      dependencies <- dependency :: dependencies

  pushInOrder roots

  while work.Count > 0 do
    match work.Pop() with
    | TypeRef typeRef ->
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
      | PT.TInt
      | PT.TFloat
      | PT.TChar
      | PT.TString
      | PT.TUuid
      | PT.TDateTime
      | PT.TBlob
      | PT.TVariable _ -> ()

      | PT.TStream inner
      | PT.TList inner
      | PT.TDict inner
      | PT.TDB inner -> work.Push(TypeRef inner)

      | PT.TTuple(first, second, rest) ->
        pushTypesInOrder rest
        work.Push(TypeRef second)
        work.Push(TypeRef first)

      | PT.TCustomType(nr, typeArgs) ->
        addNameResolution nr PT.ItemKind.Type PackageItem.typePackageHash
        pushTypesInOrder typeArgs

      | PT.TFn(args, ret) ->
        work.Push(TypeRef ret)
        pushTypesInOrder (NEList.toList args)

    | StringSegment segment ->
      match segment with
      | PT.StringText _ -> ()
      | PT.StringInterpolation expr -> work.Push(Expr expr)

    | MatchPattern pattern ->
      // Match patterns do not contain package references, but they are still
      // traversed iteratively so adding one later cannot reintroduce this bug.
      match pattern with
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
      | PT.MPInt _
      | PT.MPFloat _
      | PT.MPChar _
      | PT.MPString _
      | PT.MPVariable _ -> ()

      | PT.MPList(_, patterns)
      | PT.MPEnum(_, _, patterns) ->
        patterns |> List.rev |> List.iter (MatchPattern >> work.Push)

      | PT.MPListCons(_, head, tail) ->
        work.Push(MatchPattern tail)
        work.Push(MatchPattern head)

      | PT.MPTuple(_, first, second, rest) ->
        rest |> List.rev |> List.iter (MatchPattern >> work.Push)
        work.Push(MatchPattern second)
        work.Push(MatchPattern first)

      | PT.MPOr(_, patterns) ->
        patterns
        |> NEList.toList
        |> List.rev
        |> List.iter (MatchPattern >> work.Push)

    | MatchCase case ->
      work.Push(Expr case.rhs)
      case.whenCondition |> Option.iter (Expr >> work.Push)
      work.Push(MatchPattern case.pat)

    | PipeExpr pipeExpr ->
      match pipeExpr with
      | PT.EPipeLambda(_, _, body)
      | PT.EPipeInfix(_, _, body) -> work.Push(Expr body)

      | PT.EPipeFnCall(_, nr, typeArgs, args) ->
        addNameResolution nr PT.ItemKind.Fn PackageItem.fnPackageHash
        pushExprsInOrder args
        pushTypesInOrder typeArgs

      | PT.EPipeEnum(_, nr, _, fields) ->
        addNameResolution nr PT.ItemKind.Type PackageItem.typePackageHash
        pushExprsInOrder fields

      | PT.EPipeVariable(_, _, args) -> pushExprsInOrder args

    | Expr expr ->
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
      | PT.EInt _
      | PT.EFloat _
      | PT.EChar _
      | PT.EVariable _
      | PT.EArg _
      | PT.ESelf _ -> ()

      | PT.EString(_, segments) ->
        segments |> List.rev |> List.iter (StringSegment >> work.Push)

      | PT.EIf(_, condition, thenExpr, elseExpr) ->
        elseExpr |> Option.iter (Expr >> work.Push)
        work.Push(Expr thenExpr)
        work.Push(Expr condition)

      | PT.EPipe(_, lhs, parts) ->
        parts |> List.rev |> List.iter (PipeExpr >> work.Push)
        work.Push(Expr lhs)

      | PT.EMatch(_, arg, cases) ->
        cases |> List.rev |> List.iter (MatchCase >> work.Push)
        work.Push(Expr arg)

      | PT.ELet(_, _pattern, value, body) ->
        work.Push(Expr body)
        work.Push(Expr value)

      | PT.EList(_, items) -> pushExprsInOrder items

      | PT.EDict(_, pairs) -> pairs |> List.map snd |> pushExprsInOrder

      | PT.ETuple(_, first, second, rest) ->
        pushExprsInOrder rest
        work.Push(Expr second)
        work.Push(Expr first)

      | PT.EApply(_, fnExpr, typeArgs, args) ->
        pushExprsInOrder (NEList.toList args)
        pushTypesInOrder typeArgs
        work.Push(Expr fnExpr)

      | PT.EFnName(_, nr) ->
        addNameResolution nr PT.ItemKind.Fn PackageItem.fnPackageHash

      | PT.ELambda(_, _, body) -> work.Push(Expr body)

      | PT.EInfix(_, _, lhs, rhs) ->
        work.Push(Expr rhs)
        work.Push(Expr lhs)

      | PT.ERecord(_, nr, typeArgs, fields) ->
        addNameResolution nr PT.ItemKind.Type PackageItem.typePackageHash
        fields |> List.map snd |> pushExprsInOrder
        pushTypesInOrder typeArgs

      | PT.ERecordFieldAccess(_, record, _) -> work.Push(Expr record)

      | PT.ERecordUpdate(_, record, updates) ->
        updates |> NEList.toList |> List.map snd |> pushExprsInOrder
        work.Push(Expr record)

      | PT.EEnum(_, nr, typeArgs, _, fields) ->
        addNameResolution nr PT.ItemKind.Type PackageItem.typePackageHash
        pushExprsInOrder fields
        pushTypesInOrder typeArgs

      | PT.EValue(_, nr) ->
        addNameResolution nr PT.ItemKind.Value PackageItem.valuePackageHash

      | PT.EStatement(_, first, next) ->
        work.Push(Expr next)
        work.Push(Expr first)

  List.rev dependencies


/// Extract all references from an expression without recursive stack use.
let extractFromExpr (expr : PT.Expr) : List<Dependency> = extract [ Expr expr ]


/// Extract all references from a function definition
let extractFromFn (fn : PT.PackageFn.PackageFn) : List<Dependency> =
  // Deduplicate references
  extract (
    Expr fn.body
    :: (fn.parameters
        |> NEList.toList
        |> List.map (fun parameter -> TypeRef parameter.typ))
    @ [ TypeRef fn.returnType ]
  )
  |> List.distinct


/// Extract references from a function's signature only (parameters and return
/// type), not its body. Enough to type-check a call to it.
let extractFromFnSignature (fn : PT.PackageFn.PackageFn) : List<Dependency> =
  extract (
    (fn.parameters
     |> NEList.toList
     |> List.map (fun parameter -> TypeRef parameter.typ))
    @ [ TypeRef fn.returnType ]
  )
  |> List.distinct


/// Extract all references from a value definition
let extractFromValue (value : PT.PackageValue.PackageValue) : List<Dependency> =
  extract [ Expr value.body ] |> List.distinct


/// Extract all references from a type definition
let extractFromType (typ : PT.PackageType.PackageType) : List<Dependency> =
  let roots =
    match typ.declaration.definition with
    | PT.TypeDeclaration.Alias typeRef -> [ TypeRef typeRef ]
    | PT.TypeDeclaration.Record fields ->
      fields |> NEList.toList |> List.map (fun field -> TypeRef field.typ)
    | PT.TypeDeclaration.Enum cases ->
      cases
      |> NEList.toList
      |> List.collect (fun case ->
        case.fields |> List.map (fun field -> TypeRef field.typ))

  extract roots |> List.distinct
