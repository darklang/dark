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
  options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
  options.ReferenceHandler <- ReferenceHandler.IgnoreCycles
  options.MaxDepth <- 32
  options.Converters.Add(JsonFSharpConverter())
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
    definition : TypeDeclaration.Definition }

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
let rec private exprToCanonicalString (expr : Expr) : string =
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
    let segStrings = segments |> List.map stringSegmentToString |> String.concat ","
    $"String([{segStrings}])"
  | EIf(_, cond, thenExpr, elseExpr) ->
    let elseStr =
      match elseExpr with
      | Some e -> exprToCanonicalString e
      | None -> "None"
    $"If({exprToCanonicalString cond},{exprToCanonicalString thenExpr},{elseStr})"
  | EPipe(_, lhs, parts) ->
    let partStrings = parts |> List.map pipeExprToString |> String.concat ","
    $"Pipe({exprToCanonicalString lhs},[{partStrings}])"
  | EMatch(_, arg, cases) ->
    let caseStrings =
      cases
      |> List.map (fun case ->
        let whenStr =
          match case.whenCondition with
          | Some e -> exprToCanonicalString e
          | None -> "None"
        $"{matchPatternToString case.pat}|{whenStr}|{exprToCanonicalString case.rhs}")
      |> String.concat ","
    $"Match({exprToCanonicalString arg},[{caseStrings}])"
  | ELet(_, pattern, value, body) ->
    $"Let({letPatternToString pattern},{exprToCanonicalString value},{exprToCanonicalString body})"
  | EVariable(_, name) -> $"Variable({name})"
  | EList(_, exprs) ->
    let exprStrings = exprs |> List.map exprToCanonicalString |> String.concat ","
    $"List([{exprStrings}])"
  | EDict(_, pairs) ->
    let pairStrings =
      pairs
      |> List.sortBy fst
      |> List.map (fun (k, v) -> $"{k}={exprToCanonicalString v}")
      |> String.concat ","
    $"Dict([{pairStrings}])"
  | ETuple(_, first, second, rest) ->
    let all = first :: second :: rest
    let exprStrings = all |> List.map exprToCanonicalString |> String.concat ","
    $"Tuple([{exprStrings}])"
  | EApply(_, expr, typeArgs, args) ->
    let typeArgStrings =
      typeArgs |> List.map typeRefToCanonicalString |> String.concat ","
    let argStrings =
      args |> NEList.toList |> List.map exprToCanonicalString |> String.concat ","
    $"Apply({exprToCanonicalString expr},[{typeArgStrings}],[{argStrings}])"
  | EFnName(_, nameRes) -> $"FnName({fnNameResolutionToString nameRes})"
  | ELambda(_, pats, body) ->
    let patStrings =
      pats |> NEList.toList |> List.map letPatternToString |> String.concat ","
    $"Lambda([{patStrings}],{exprToCanonicalString body})"
  | EInfix(_, infix, lhs, rhs) ->
    $"Infix({infix},{exprToCanonicalString lhs},{exprToCanonicalString rhs})"
  | ERecord(_, typeName, typeArgs, fields) ->
    let typeArgStrings =
      typeArgs |> List.map typeRefToCanonicalString |> String.concat ","
    let fieldStrings =
      fields
      |> List.sortBy fst
      |> List.map (fun (name, expr) -> $"{name}={exprToCanonicalString expr}")
      |> String.concat ","
    $"Record({typeNameResolutionToString typeName},[{typeArgStrings}],[{fieldStrings}])"
  | ERecordFieldAccess(_, record, fieldName) ->
    $"FieldAccess({exprToCanonicalString record},{fieldName})"
  | ERecordUpdate(_, record, updates) ->
    let updateStrings =
      updates
      |> NEList.toList
      |> List.sortBy fst
      |> List.map (fun (name, expr) -> $"{name}={exprToCanonicalString expr}")
      |> String.concat ","
    $"RecordUpdate({exprToCanonicalString record},[{updateStrings}])"
  | EEnum(_, typeName, typeArgs, caseName, fields) ->
    let typeArgStrings =
      typeArgs |> List.map typeRefToCanonicalString |> String.concat ","
    let fieldStrings = fields |> List.map exprToCanonicalString |> String.concat ","
    $"Enum({typeNameResolutionToString typeName},[{typeArgStrings}],{caseName},[{fieldStrings}])"
  | EValue(_, nameRes) -> $"Value({valueNameResolutionToString nameRes})"
  | EStatement(_, first, next) ->
    $"Statement({exprToCanonicalString first},{exprToCanonicalString next})"

// Helper to convert type reference to canonical string
and private typeRefToCanonicalString (typeRef : TypeReference) : string =
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
  | TList inner -> $"List({typeRefToCanonicalString inner})"
  | TTuple(first, second, rest) ->
    let all = first :: second :: rest
    let typeStrings = all |> List.map typeRefToCanonicalString |> String.concat ","
    $"Tuple([{typeStrings}])"
  | TDict inner -> $"Dict({typeRefToCanonicalString inner})"
  | TCustomType(nameRes, typeArgs) ->
    let typeArgStrings =
      typeArgs |> List.map typeRefToCanonicalString |> String.concat ","
    $"Custom({typeNameResolutionToString nameRes},[{typeArgStrings}])"
  | TFn(args, ret) ->
    let argStrings =
      args |> NEList.toList |> List.map typeRefToCanonicalString |> String.concat ","
    $"Fn([{argStrings}],{typeRefToCanonicalString ret})"
  | TVariable name -> $"Variable({name})"
  | TDB inner -> $"DB({typeRefToCanonicalString inner})"

// Helper functions for canonical representation - for name resolution
and private fnNameResolutionToString
  (nr : NameResolution<FQFnName.FQFnName>)
  : string =
  match nr with
  | Ok fqName ->
    match fqName with
    | FQFnName.Builtin b -> $"Ok(Builtin({b.name},{b.version}))"
    | FQFnName.Package _ -> "Ok(Package)" // TODO: fix: instead of uuid Package will be referenced by hash
  | Error err -> $"Error({err})"

and private typeNameResolutionToString
  (nr : NameResolution<FQTypeName.FQTypeName>)
  : string =
  match nr with
  | Ok fqName ->
    match fqName with
    | FQTypeName.Package _ -> "Ok(Package)" // TODO: fix: instead of uuid Package will be referenced by hash
  | Error err -> $"Error({err})"

and private valueNameResolutionToString
  (nr : NameResolution<FQValueName.FQValueName>)
  : string =
  match nr with
  | Ok fqName ->
    match fqName with
    | FQValueName.Builtin b -> $"Ok(Builtin({b.name},{b.version}))"
    | FQValueName.Package _ -> "Ok(Package)" // TODO: fix: instead of uuid Package will be referenced by hash
  | Error err -> $"Error({err})"

and private stringSegmentToString (seg : StringSegment) : string =
  match seg with
  | StringText s -> $"Text({s})"
  | StringInterpolation expr -> $"Interp({exprToCanonicalString expr})"

and private letPatternToString (pat : LetPattern) : string =
  match pat with
  | LPVariable(_, name) -> $"Var({name})"
  | LPTuple(_, first, second, rest) ->
    let all = first :: second :: rest
    let patStrings = all |> List.map letPatternToString |> String.concat ","
    $"Tuple([{patStrings}])"
  | LPUnit _ -> "Unit"

and private matchPatternToString (pat : MatchPattern) : string = sprintf "%A" pat

and private pipeExprToString (pexpr : PipeExpr) : string =
  match pexpr with
  | EPipeLambda(_, pats, body) ->
    let patStrings =
      pats |> NEList.toList |> List.map letPatternToString |> String.concat ","
    $"Lambda([{patStrings}],{exprToCanonicalString body})"
  | EPipeInfix(_, infix, expr) -> $"Infix({infix},{exprToCanonicalString expr})"
  | EPipeFnCall(_, fnName, typeArgs, args) ->
    let typeArgStrings =
      typeArgs |> List.map typeRefToCanonicalString |> String.concat ","
    let argStrings = args |> List.map exprToCanonicalString |> String.concat ","
    $"FnCall({fnNameResolutionToString fnName},[{typeArgStrings}],[{argStrings}])"
  | EPipeEnum(_, typeName, caseName, fields) ->
    let fieldStrings = fields |> List.map exprToCanonicalString |> String.concat ","
    $"Enum({typeNameResolutionToString typeName},{caseName},[{fieldStrings}])"
  | EPipeVariable(_, varName, args) ->
    let argStrings = args |> List.map exprToCanonicalString |> String.concat ","
    $"Variable({varName},[{argStrings}])"

// Calculate hash for a PackageType
let hashPackageType (pt : PackageType.PackageType) : Hash =
  let canonical =
    { owner = pt.name.owner
      modules = pt.name.modules
      name = pt.name.name
      typeParams = pt.declaration.typeParams
      definition = pt.declaration.definition }

  let json = JsonSerializer.Serialize(canonical, jsonOptions)
  Hash(computeSHA256 json)

// Calculate hash for a PackageValue
let hashPackageValue (pv : PackageValue.PackageValue) : Hash =
  let canonical =
    { owner = pv.name.owner
      modules = pv.name.modules
      name = pv.name.name
      body = exprToCanonicalString pv.body }

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
        |> List.map (fun p -> $"{p.name}:{typeRefToCanonicalString p.typ}")
      returnType = typeRefToCanonicalString pf.returnType
      body = exprToCanonicalString pf.body }

  let json = JsonSerializer.Serialize(canonical, jsonOptions)
  Hash(computeSHA256 json)
