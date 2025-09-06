module LibPackageManager.Hashing

open System
open System.Text
open System.Security.Cryptography
open System.Text.Json
open System.Text.Json.Serialization

open Prelude
open LibExecution.ProgramTypes

// Custom JSON options for deterministic serialization
let private jsonOptions =
  let options = JsonSerializerOptions()
  options.WriteIndented <- false
  options.DefaultIgnoreCondition <- JsonIgnoreCondition.Never
  options.MaxDepth <- System.Int32.MaxValue
  options.Converters.Add(
    JsonFSharpConverter(unionEncoding = JsonUnionEncoding.ExternalTag)
  )
  options

// Compute SHA-256 hash of a string
let private computeSHA256 (input : string) : string =
  use sha256 = SHA256.Create()
  let bytes = Encoding.UTF8.GetBytes(input)
  let hashBytes = sha256.ComputeHash(bytes)
  BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant()

// Canonical representation of a PackageType for hashing
type private CanonicalPackageType =
  { owner : string
    modules : string list
    name : string
    typeParams : string list
    definition : string }

// Canonical representation of a PackageValue for hashing
type private CanonicalPackageValue =
  { owner : string; modules : string list; name : string; body : string } // expression serialized to string

// Canonical representation of a PackageFn for hashing
type private CanonicalPackageFn =
  { owner : string
    modules : string list
    name : string
    typeParams : string list
    parameters : string list // Simplified for now
    returnType : string
    body : string } // expression serialized to string

// Helper functions for canonical representation
let rec private exprToCanonicalString
  (currentFnName : PackageFn.Name option)
  (expr : Expr)
  : string =
  match expr with
  | EUnit _ -> "Unit"
  | EBool(_, b) -> $"Bool({b})"
  | EInt8(_, i) -> $"Int8({i})"
  | EUInt8(_, i) -> $"UInt8({i})"
  | EInt16(_, i) -> $"Int16({i})"
  | EUInt16(_, i) -> $"UInt16({i})"
  | EInt32(_, i) -> $"Int32({i})"
  | EUInt32(_, i) -> $"UInt32({i})"
  | EInt64(_, i) -> $"Int64({i})"
  | EUInt64(_, i) -> $"UInt64({i})"
  | EInt128(_, i) -> $"Int128({i})"
  | EUInt128(_, i) -> $"UInt128({i})"
  | EFloat(_, sign, whole, part) -> $"Float({sign},{whole},{part})"
  | EChar(_, c) -> $"Char({c})"
  | EString(_, segments) ->
    let segStrings =
      segments |> List.map (stringSegmentToString currentFnName) |> String.concat ","
    $"String([{segStrings}])"
  | EIf(_, cond, thenExpr, elseExpr) ->
    let elseStr =
      match elseExpr with
      | Some e -> exprToCanonicalString currentFnName e
      | None -> "None"
    $"If({exprToCanonicalString currentFnName cond},{exprToCanonicalString currentFnName thenExpr},{elseStr})"
  | EPipe(_, lhs, parts) ->
    let partStrings =
      parts |> List.map (pipeExprToString currentFnName) |> String.concat ","
    $"Pipe({exprToCanonicalString currentFnName lhs},[{partStrings}])"
  | EMatch(_, arg, cases) ->
    let caseStrings =
      cases
      |> List.map (fun case ->
        let whenStr =
          match case.whenCondition with
          | Some e -> exprToCanonicalString currentFnName e
          | None -> "None"
        $"{matchPatternToString case.pat}|{whenStr}|{exprToCanonicalString currentFnName case.rhs}")
      |> String.concat ","
    $"Match({exprToCanonicalString currentFnName arg},[{caseStrings}])"
  | ELet(_, pattern, value, body) ->
    $"Let({letPatternToString pattern},{exprToCanonicalString currentFnName value},{exprToCanonicalString currentFnName body})"
  | EVariable(_, name) -> $"Variable({name})"
  | EList(_, exprs) ->
    let exprStrings =
      exprs |> List.map (exprToCanonicalString currentFnName) |> String.concat ","
    $"List([{exprStrings}])"
  | EDict(_, pairs) ->
    let pairStrings =
      pairs
      |> List.sortBy fst
      |> List.map (fun (k, v) -> $"{k}={exprToCanonicalString currentFnName v}")
      |> String.concat ","
    $"Dict([{pairStrings}])"
  | ETuple(_, first, second, rest) ->
    let all = first :: second :: rest
    let exprStrings =
      all |> List.map (exprToCanonicalString currentFnName) |> String.concat ","
    $"Tuple([{exprStrings}])"
  | EApply(_, expr, typeArgs, args) ->
    let typeArgStrings =
      typeArgs |> List.map (typeRefToCanonicalString None) |> String.concat ","
    let argStrings =
      args
      |> NEList.toList
      |> List.map (exprToCanonicalString currentFnName)
      |> String.concat ","
    $"Apply({exprToCanonicalString currentFnName expr},[{typeArgStrings}],[{argStrings}])"
  | EFnName(_, nameRes) ->
    $"FnName({fnNameResolutionToString currentFnName nameRes})"
  | ELambda(_, pats, body) ->
    let patStrings =
      pats |> NEList.toList |> List.map letPatternToString |> String.concat ","
    $"Lambda([{patStrings}],{exprToCanonicalString currentFnName body})"
  | EInfix(_, infix, lhs, rhs) ->
    $"Infix({infix},{exprToCanonicalString currentFnName lhs},{exprToCanonicalString currentFnName rhs})"
  | ERecord(_, typeName, typeArgs, fields) ->
    let typeArgStrings =
      typeArgs |> List.map (typeRefToCanonicalString None) |> String.concat ","
    let fieldStrings =
      fields
      |> List.sortBy fst
      |> List.map (fun (name, expr) ->
        $"{name}={exprToCanonicalString currentFnName expr}")
      |> String.concat ","
    $"Record({typeNameResolutionToString typeName},[{typeArgStrings}],[{fieldStrings}])"
  | ERecordFieldAccess(_, record, fieldName) ->
    $"FieldAccess({exprToCanonicalString currentFnName record},{fieldName})"
  | ERecordUpdate(_, record, updates) ->
    let updateStrings =
      updates
      |> NEList.toList
      |> List.sortBy fst
      |> List.map (fun (name, expr) ->
        $"{name}={exprToCanonicalString currentFnName expr}")
      |> String.concat ","
    $"RecordUpdate({exprToCanonicalString currentFnName record},[{updateStrings}])"
  | EEnum(_, typeName, typeArgs, caseName, fields) ->
    let typeArgStrings =
      typeArgs |> List.map (typeRefToCanonicalString None) |> String.concat ","
    let fieldStrings =
      fields |> List.map (exprToCanonicalString currentFnName) |> String.concat ","
    $"Enum({typeNameResolutionToString typeName},[{typeArgStrings}],{caseName},[{fieldStrings}])"
  | EValue(_, nameRes) -> $"Value({valueNameResolutionToString nameRes})"
  | EStatement(_, first, next) ->
    $"Statement({exprToCanonicalString currentFnName first},{exprToCanonicalString currentFnName next})"

// Helper to convert type reference to canonical string
// currentTypeName is used to detect self-references in recursive types
and private typeRefToCanonicalString
  (currentTypeName : PackageType.Name option)
  (typeRef : TypeReference)
  : string =
  match typeRef with
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TInt8 -> "Int8"
  | TUInt8 -> "UInt8"
  | TInt16 -> "Int16"
  | TUInt16 -> "UInt16"
  | TInt32 -> "Int32"
  | TUInt32 -> "UInt32"
  | TInt64 -> "Int64"
  | TUInt64 -> "UInt64"
  | TInt128 -> "Int128"
  | TUInt128 -> "UInt128"
  | TFloat -> "Float"
  | TChar -> "Char"
  | TString -> "String"
  | TUuid -> "Uuid"
  | TDateTime -> "DateTime"
  | TList inner -> $"List({typeRefToCanonicalString currentTypeName inner})"
  | TTuple(first, second, rest) ->
    let all = first :: second :: rest
    let typeStrings =
      all |> List.map (typeRefToCanonicalString currentTypeName) |> String.concat ","
    $"Tuple([{typeStrings}])"
  | TDict inner -> $"Dict({typeRefToCanonicalString currentTypeName inner})"
  | TCustomType(nameRes, typeArgs) ->
    let typeArgStrings =
      typeArgs
      |> List.map (typeRefToCanonicalString currentTypeName)
      |> String.concat ","
    // Check for self-reference to avoid circular dependency
    // We need to detect if this type reference is to the same type we're currently hashing
    let isCanonicalSelfReference =
      match nameRes, currentTypeName with
      | Ok(FQTypeName.Package _), Some _ ->
        // Check if we're referencing the same type by looking at the canonical name
        // We can't check by hash since we're calculating it
        match typeNameResolutionToString nameRes with
        | s when s.Contains("Package") -> true // Assume any package reference could be self-ref during calculation
        | _ -> false
      | _ -> false

    if isCanonicalSelfReference && currentTypeName.IsSome then
      // For self-references, use the type name instead of hash to break circularity
      let currentName = currentTypeName.Value
      let modulesStr = String.concat "." currentName.modules
      let nameStr = $"SelfRef({currentName.owner}.{modulesStr}.{currentName.name})"
      $"Custom({nameStr},[{typeArgStrings}])"
    else
      $"Custom({typeNameResolutionToString nameRes},[{typeArgStrings}])"
  | TFn(args, ret) ->
    let argStrings =
      args
      |> NEList.toList
      |> List.map (typeRefToCanonicalString currentTypeName)
      |> String.concat ","
    $"Fn([{argStrings}],{typeRefToCanonicalString currentTypeName ret})"
  | TVariable name -> $"Variable({name})"
  | TDB inner -> $"DB({typeRefToCanonicalString currentTypeName inner})"

// Helper functions for canonical representation - for name resolution
and private fnNameResolutionToString
  (currentFnName : PackageFn.Name option)
  (nr : NameResolution<FQFnName.FQFnName>)
  : string =
  match nr with
  | Ok fqName ->
    match fqName with
    | FQFnName.Builtin b -> $"Ok(Builtin({b.name},{b.version}))"
    | FQFnName.Package hash ->
      // Check for self-reference to avoid circular dependency
      match currentFnName with
      | Some currentName ->
        let modulesStr = String.concat "." currentName.modules
        $"Ok(Package(SelfRef({currentName.owner}.{modulesStr}.{currentName.name})))"
      | None -> $"Ok(Package({hash}))"
  | Error err -> $"Error({err})"

and private typeNameResolutionToString
  (nr : NameResolution<FQTypeName.FQTypeName>)
  : string =
  match nr with
  | Ok fqName ->
    match fqName with
    | FQTypeName.Package hash -> $"Ok(Package({hash}))"
  | Error err -> $"Error({err})"

and private valueNameResolutionToString
  (nr : NameResolution<FQValueName.FQValueName>)
  : string =
  match nr with
  | Ok fqName ->
    match fqName with
    | FQValueName.Builtin b -> $"Ok(Builtin({b.name},{b.version}))"
    | FQValueName.Package hash -> $"Ok(Package({hash}))"
  | Error err -> $"Error({err})"

and private stringSegmentToString
  (currentFnName : PackageFn.Name option)
  (seg : StringSegment)
  : string =
  match seg with
  | StringText s -> $"Text({s})"
  | StringInterpolation expr -> $"Interp({exprToCanonicalString currentFnName expr})"

and private letPatternToString (pat : LetPattern) : string =
  match pat with
  | LPVariable(_, name) -> $"Var({name})"
  | LPTuple(_, first, second, rest) ->
    let all = first :: second :: rest
    let patStrings = all |> List.map letPatternToString |> String.concat ","
    $"Tuple([{patStrings}])"
  | LPUnit _ -> "Unit"

and private matchPatternToString (pat : MatchPattern) : string =
  match pat with
  | MPUnit _ -> "Unit"
  | MPBool(_, b) -> $"Bool({b})"
  | MPInt8(_, i) -> $"Int8({i})"
  | MPUInt8(_, i) -> $"UInt8({i})"
  | MPInt16(_, i) -> $"Int16({i})"
  | MPUInt16(_, i) -> $"UInt16({i})"
  | MPInt32(_, i) -> $"Int32({i})"
  | MPUInt32(_, i) -> $"UInt32({i})"
  | MPInt64(_, i) -> $"Int64({i})"
  | MPUInt64(_, i) -> $"UInt64({i})"
  | MPInt128(_, i) -> $"Int128({i})"
  | MPUInt128(_, i) -> $"UInt128({i})"
  | MPFloat(_, sign, whole, part) -> $"Float({sign},{whole},{part})"
  | MPChar(_, c) -> $"Char({c})"
  | MPString(_, s) -> $"String({s})"
  | MPList(_, patterns) ->
    let patStrings = patterns |> List.map matchPatternToString |> String.concat ","
    $"List([{patStrings}])"
  | MPListCons(_, head, tail) ->
    $"ListCons({matchPatternToString head},{matchPatternToString tail})"
  | MPTuple(_, first, second, rest) ->
    let all = first :: second :: rest
    let patStrings = all |> List.map matchPatternToString |> String.concat ","
    $"Tuple([{patStrings}])"
  | MPEnum(_, caseName, fieldPats) ->
    let fieldStrings =
      fieldPats |> List.map matchPatternToString |> String.concat ","
    $"Enum({caseName},[{fieldStrings}])"
  | MPVariable(_, name) -> $"Variable({name})"
  | MPOr(_, patterns) ->
    let patStrings =
      patterns |> NEList.toList |> List.map matchPatternToString |> String.concat ","
    $"Or([{patStrings}])"

and private pipeExprToString
  (currentFnName : PackageFn.Name option)
  (pexpr : PipeExpr)
  : string =
  match pexpr with
  | EPipeLambda(_, pats, body) ->
    let patStrings =
      pats |> NEList.toList |> List.map letPatternToString |> String.concat ","
    $"Lambda([{patStrings}],{exprToCanonicalString currentFnName body})"
  | EPipeInfix(_, infix, expr) ->
    $"Infix({infix},{exprToCanonicalString currentFnName expr})"
  | EPipeFnCall(_, fnName, typeArgs, args) ->
    let typeArgStrings =
      typeArgs |> List.map (typeRefToCanonicalString None) |> String.concat ","
    let argStrings =
      args |> List.map (exprToCanonicalString currentFnName) |> String.concat ","
    $"FnCall({fnNameResolutionToString currentFnName fnName},[{typeArgStrings}],[{argStrings}])"
  | EPipeEnum(_, typeName, caseName, fields) ->
    let fieldStrings =
      fields |> List.map (exprToCanonicalString currentFnName) |> String.concat ","
    $"Enum({typeNameResolutionToString typeName},{caseName},[{fieldStrings}])"
  | EPipeVariable(_, varName, args) ->
    let argStrings =
      args |> List.map (exprToCanonicalString currentFnName) |> String.concat ","
    $"Variable({varName},[{argStrings}])"

// Helper functions for TypeDeclaration canonical representation
and private recordFieldToString
  (currentTypeName : PackageType.Name option)
  (field : TypeDeclaration.RecordField)
  : string =
  $"{field.name}:{typeRefToCanonicalString currentTypeName field.typ}:{field.description}"

and private enumFieldToString
  (currentTypeName : PackageType.Name option)
  (field : TypeDeclaration.EnumField)
  : string =
  let labelStr =
    match field.label with
    | Some l -> l
    | None -> ""
  $"{typeRefToCanonicalString currentTypeName field.typ}:{labelStr}:{field.description}"

and private enumCaseToString
  (currentTypeName : PackageType.Name option)
  (case : TypeDeclaration.EnumCase)
  : string =
  let fieldStrings =
    case.fields |> List.map (enumFieldToString currentTypeName) |> String.concat ","
  $"{case.name}:([{fieldStrings}]):{case.description}"

and private definitionToString
  (currentTypeName : PackageType.Name option)
  (def : TypeDeclaration.Definition)
  : string =
  match def with
  | TypeDeclaration.Alias typeRef ->
    $"Alias({typeRefToCanonicalString currentTypeName typeRef})"
  | TypeDeclaration.Record fields ->
    let fieldStrings =
      fields
      |> NEList.toList
      |> List.map (recordFieldToString currentTypeName)
      |> String.concat ","
    $"Record([{fieldStrings}])"
  | TypeDeclaration.Enum cases ->
    let caseStrings =
      cases
      |> NEList.toList
      |> List.map (enumCaseToString currentTypeName)
      |> String.concat ","
    $"Enum([{caseStrings}])"

// Calculate hash for a PackageType
let hashPackageType (pt : PackageType.PackageType) : Hash =
  let canonical =
    { owner = pt.name.owner
      modules = pt.name.modules
      name = pt.name.name
      typeParams = pt.declaration.typeParams
      definition = definitionToString (Some pt.name) pt.declaration.definition }

  let json = JsonSerializer.Serialize(canonical, jsonOptions)
  Hash(computeSHA256 json)

// Calculate hash for a PackageValue
let hashPackageValue (pv : PackageValue.PackageValue) : Hash =
  let canonical =
    { owner = pv.name.owner
      modules = pv.name.modules
      name = pv.name.name
      body = exprToCanonicalString None pv.body }

  let json = JsonSerializer.Serialize(canonical, jsonOptions)
  Hash(computeSHA256 json)

// Calculate hash for a PackageFn
let hashPackageFn (pf : PackageFn.PackageFn) : Hash =
  let canonical =
    { owner = pf.name.owner
      modules = pf.name.modules
      name = pf.name.name
      typeParams = pf.typeParams
      parameters =
        pf.parameters
        |> NEList.toList
        |> List.map (fun p -> $"{p.name}:{typeRefToCanonicalString None p.typ}")
      returnType = typeRefToCanonicalString None pf.returnType
      body = exprToCanonicalString (Some pf.name) pf.body }

  let json = JsonSerializer.Serialize(canonical, jsonOptions)
  Hash(computeSHA256 json)
