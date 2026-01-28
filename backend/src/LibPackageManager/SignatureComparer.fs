/// Compares signatures of package items to detect compatible vs breaking changes.
/// Compatible = same signature, different implementation (e.g., body only changed)
/// Breaking = signature changed (parameters, return type, type structure, etc.)
module LibPackageManager.SignatureComparer

open Prelude

module PT = LibExecution.ProgramTypes


/// Result of comparing two items
type ComparisonResult =
  /// Signatures are identical - items are fully equivalent
  | Identical
  /// Content differs but signature is compatible - safe to auto-update dependents
  | Compatible
  /// Signature changed - breaking change, dependents need manual update
  | Breaking


/// Deep equality for TypeReference
let rec typeRefsEqual (a : PT.TypeReference) (b : PT.TypeReference) : bool =
  match a, b with
  | PT.TUnit, PT.TUnit
  | PT.TBool, PT.TBool
  | PT.TInt8, PT.TInt8
  | PT.TUInt8, PT.TUInt8
  | PT.TInt16, PT.TInt16
  | PT.TUInt16, PT.TUInt16
  | PT.TInt32, PT.TInt32
  | PT.TUInt32, PT.TUInt32
  | PT.TInt64, PT.TInt64
  | PT.TUInt64, PT.TUInt64
  | PT.TInt128, PT.TInt128
  | PT.TUInt128, PT.TUInt128
  | PT.TFloat, PT.TFloat
  | PT.TChar, PT.TChar
  | PT.TString, PT.TString
  | PT.TUuid, PT.TUuid
  | PT.TDateTime, PT.TDateTime -> true

  | PT.TVariable nameA, PT.TVariable nameB -> nameA = nameB

  | PT.TList innerA, PT.TList innerB -> typeRefsEqual innerA innerB
  | PT.TDict innerA, PT.TDict innerB -> typeRefsEqual innerA innerB
  | PT.TDB innerA, PT.TDB innerB -> typeRefsEqual innerA innerB

  | PT.TTuple(a1, a2, aRest), PT.TTuple(b1, b2, bRest) ->
    typeRefsEqual a1 b1
    && typeRefsEqual a2 b2
    && List.length aRest = List.length bRest
    && List.forall2 typeRefsEqual aRest bRest

  | PT.TCustomType(nrA, argsA), PT.TCustomType(nrB, argsB) ->
    nameResolutionsEqual nrA nrB
    && List.length argsA = List.length argsB
    && List.forall2 typeRefsEqual argsA argsB

  | PT.TFn(argsA, retA), PT.TFn(argsB, retB) ->
    let argsAList = NEList.toList argsA
    let argsBList = NEList.toList argsB
    List.length argsAList = List.length argsBList
    && List.forall2 typeRefsEqual argsAList argsBList
    && typeRefsEqual retA retB

  | _, _ -> false


/// Compare NameResolution for types
and nameResolutionsEqual
  (a : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  (b : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  : bool =
  match a, b with
  | Ok(PT.FQTypeName.Package idA), Ok(PT.FQTypeName.Package idB) -> idA = idB
  | Error errA, Error errB -> errA = errB
  | _, _ -> false


/// Compare function parameters (name, type, description)
let parametersEqual
  (a : PT.PackageFn.Parameter)
  (b : PT.PackageFn.Parameter)
  : bool =
  a.name = b.name && typeRefsEqual a.typ b.typ
// Note: description changes don't affect signature compatibility


/// Transform a UUID using the provided mapping (for normalizing fresh UUIDs to stable ones)
let mapUuid
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (id : uuid)
  : uuid =
  match mapping.TryGetValue(id) with
  | true, stableId -> stableId
  | false, _ -> id


/// Collect all package item UUIDs referenced in an expression
let rec collectReferencedUuids (expr : PT.Expr) : Set<uuid> =
  let collect = collectReferencedUuids
  match expr with
  | PT.EUnit _ | PT.EBool _ | PT.EInt8 _ | PT.EUInt8 _ | PT.EInt16 _ | PT.EUInt16 _
  | PT.EInt32 _ | PT.EUInt32 _ | PT.EInt64 _ | PT.EUInt64 _ | PT.EInt128 _ | PT.EUInt128 _
  | PT.EFloat _ | PT.EChar _ | PT.EVariable _ | PT.EArg _ | PT.ESelf _ -> Set.empty
  | PT.EString(_, parts) ->
    parts |> List.collect collectStringSegmentUuids |> Set.ofList
  | PT.ELet(_, _, rhs, body) -> Set.union (collect rhs) (collect body)
  | PT.EIf(_, cond, thenE, elseE) ->
    let elseUuids = elseE |> Option.map collect |> Option.defaultValue Set.empty
    Set.union (Set.union (collect cond) (collect thenE)) elseUuids
  | PT.ELambda(_, _, body) -> collect body
  | PT.ERecordFieldAccess(_, obj, _) -> collect obj
  | PT.EList(_, items) -> items |> List.map collect |> Set.unionMany
  | PT.EDict(_, pairs) -> pairs |> List.map (snd >> collect) |> Set.unionMany
  | PT.ETuple(_, e1, e2, rest) ->
    (e1 :: e2 :: rest) |> List.map collect |> Set.unionMany
  | PT.ERecord(_, typeName, _, fields) ->
    let typeUuids = collectTypeNameResUuids typeName
    let fieldUuids = fields |> List.map (snd >> collect) |> Set.unionMany
    Set.union typeUuids fieldUuids
  | PT.ERecordUpdate(_, record, updates) ->
    let recordUuids = collect record
    let updateUuids = updates |> NEList.toList |> List.map (snd >> collect) |> Set.unionMany
    Set.union recordUuids updateUuids
  | PT.EEnum(_, typeName, _, _, fields) ->
    let typeUuids = collectTypeNameResUuids typeName
    let fieldUuids = fields |> List.map collect |> Set.unionMany
    Set.union typeUuids fieldUuids
  | PT.EMatch(_, expr, cases) ->
    let exprUuids = collect expr
    let caseUuids = cases |> List.map collectMatchCaseUuids |> Set.unionMany
    Set.union exprUuids caseUuids
  | PT.EPipe(_, e1, rest) ->
    let e1Uuids = collect e1
    let restUuids = rest |> List.map collectPipeExprUuids |> Set.unionMany
    Set.union e1Uuids restUuids
  | PT.EInfix(_, _, l, r) -> Set.union (collect l) (collect r)
  | PT.EApply(_, fn, _, args) ->
    let fnUuids = collect fn
    let argUuids = args |> NEList.toList |> List.map collect |> Set.unionMany
    Set.union fnUuids argUuids
  | PT.EFnName(_, fnName) -> collectFnNameResUuids fnName
  | PT.EValue(_, valueName) -> collectValueNameResUuids valueName
  | PT.EStatement(_, first, next) -> Set.union (collect first) (collect next)

and private collectStringSegmentUuids (seg : PT.StringSegment) : List<uuid> =
  match seg with
  | PT.StringText _ -> []
  | PT.StringInterpolation e -> collectReferencedUuids e |> Set.toList

and private collectMatchCaseUuids (case : PT.MatchCase) : Set<uuid> =
  let whenUuids = case.whenCondition |> Option.map collectReferencedUuids |> Option.defaultValue Set.empty
  Set.union whenUuids (collectReferencedUuids case.rhs)

and private collectPipeExprUuids (pipeExpr : PT.PipeExpr) : Set<uuid> =
  match pipeExpr with
  | PT.EPipeVariable(_, _, args) -> args |> List.map collectReferencedUuids |> Set.unionMany
  | PT.EPipeLambda(_, _, body) -> collectReferencedUuids body
  | PT.EPipeInfix(_, _, arg) -> collectReferencedUuids arg
  | PT.EPipeFnCall(_, fnName, _, args) ->
    let fnUuids = collectFnNameResUuids fnName
    let argUuids = args |> List.map collectReferencedUuids |> Set.unionMany
    Set.union fnUuids argUuids
  | PT.EPipeEnum(_, typeName, _, fields) ->
    let typeUuids = collectTypeNameResUuids typeName
    let fieldUuids = fields |> List.map collectReferencedUuids |> Set.unionMany
    Set.union typeUuids fieldUuids

and private collectTypeNameResUuids (nr : PT.NameResolution<PT.FQTypeName.FQTypeName>) : Set<uuid> =
  match nr with
  | Ok(PT.FQTypeName.Package id) -> Set.singleton id
  | _ -> Set.empty

and private collectFnNameResUuids (nr : PT.NameResolution<PT.FQFnName.FQFnName>) : Set<uuid> =
  match nr with
  | Ok(PT.FQFnName.Package id) -> Set.singleton id
  | _ -> Set.empty

and private collectValueNameResUuids (nr : PT.NameResolution<PT.FQValueName.FQValueName>) : Set<uuid> =
  match nr with
  | Ok(PT.FQValueName.Package id) -> Set.singleton id
  | _ -> Set.empty

/// Compute a content hash for an expression, ignoring expression IDs.
/// Uses the provided ID mapping to normalize UUIDs before hashing.
/// This allows comparing expressions for semantic equality across parsing sessions.
let rec exprContentHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (expr : PT.Expr)
  : string =
  let hash = exprContentHashWithMapping mapping
  match expr with
  | PT.EUnit _ -> "unit"
  | PT.EBool(_, b) -> $"bool:{b}"
  | PT.EInt8(_, n) -> $"i8:{n}"
  | PT.EUInt8(_, n) -> $"u8:{n}"
  | PT.EInt16(_, n) -> $"i16:{n}"
  | PT.EUInt16(_, n) -> $"u16:{n}"
  | PT.EInt32(_, n) -> $"i32:{n}"
  | PT.EUInt32(_, n) -> $"u32:{n}"
  | PT.EInt64(_, n) -> $"i64:{n}"
  | PT.EUInt64(_, n) -> $"u64:{n}"
  | PT.EInt128(_, n) -> $"i128:{n}"
  | PT.EUInt128(_, n) -> $"u128:{n}"
  | PT.EFloat(_, sign, w, f) -> $"float:{sign}:{w}:{f}"
  | PT.EChar(_, c) -> $"char:{c}"
  | PT.EString(_, parts) ->
    let partsHash =
      parts |> List.map (stringSegmentHashWithMapping mapping) |> String.concat ","
    $"string:[{partsHash}]"
  | PT.EVariable(_, name) -> $"var:{name}"
  | PT.EArg(_, idx) -> $"arg:{idx}"
  | PT.ELet(_, pat, rhs, body) -> $"let:{letPatternHash pat}:{hash rhs}:{hash body}"
  | PT.EIf(_, cond, thenE, elseE) ->
    let elseHash = elseE |> Option.map hash |> Option.defaultValue "none"
    $"if:{hash cond}:{hash thenE}:{elseHash}"
  | PT.ELambda(_, pats, body) ->
    let patsHash =
      pats |> NEList.toList |> List.map letPatternHash |> String.concat ","
    $"lambda:[{patsHash}]:{hash body}"
  | PT.ERecordFieldAccess(_, obj, field) -> $"field:{hash obj}:{field}"
  | PT.EList(_, items) ->
    let itemsHash = items |> List.map hash |> String.concat ","
    $"list:[{itemsHash}]"
  | PT.EDict(_, pairs) ->
    let pairsHash =
      pairs |> List.map (fun (k, v) -> $"{k}:{hash v}") |> String.concat ","
    $"dict:[{pairsHash}]"
  | PT.ETuple(_, e1, e2, rest) ->
    let restHash = rest |> List.map hash |> String.concat ","
    $"tuple:{hash e1}:{hash e2}:[{restHash}]"
  | PT.ERecord(_, typeName, typeArgs, fields) ->
    let typeArgsHash =
      typeArgs |> List.map (typeRefHashWithMapping mapping) |> String.concat ","
    let fieldsHash =
      fields |> List.map (fun (n, e) -> $"{n}:{hash e}") |> String.concat ","
    $"record:{typeNameResHashWithMapping mapping typeName}:[{typeArgsHash}]:[{fieldsHash}]"
  | PT.ERecordUpdate(_, record, updates) ->
    let updatesHash =
      updates
      |> NEList.toList
      |> List.map (fun (n, e) -> $"{n}:{hash e}")
      |> String.concat ","
    $"recordUpdate:{hash record}:[{updatesHash}]"
  | PT.EEnum(_, typeName, typeArgs, caseName, fields) ->
    let typeArgsHash =
      typeArgs |> List.map (typeRefHashWithMapping mapping) |> String.concat ","
    let fieldsHash = fields |> List.map hash |> String.concat ","
    $"enum:{typeNameResHashWithMapping mapping typeName}:[{typeArgsHash}]:{caseName}:[{fieldsHash}]"
  | PT.EMatch(_, expr, cases) ->
    let casesHash =
      cases |> List.map (matchCaseHashWithMapping mapping) |> String.concat ","
    $"match:{hash expr}:[{casesHash}]"
  | PT.EPipe(_, e1, rest) ->
    let restHash =
      rest |> List.map (pipeExprHashWithMapping mapping) |> String.concat ","
    $"pipe:{hash e1}:[{restHash}]"
  | PT.EInfix(_, op, l, r) -> $"infix:{infixHash op}:{hash l}:{hash r}"
  | PT.EApply(_, fn, typeArgs, args) ->
    let typeArgsHash =
      typeArgs |> List.map (typeRefHashWithMapping mapping) |> String.concat ","
    let argsHash = args |> NEList.toList |> List.map hash |> String.concat ","
    $"apply:{hash fn}:[{typeArgsHash}]:[{argsHash}]"
  | PT.EFnName(_, fnName) -> $"fnName:{fnNameResHashWithMapping mapping fnName}"
  | PT.EValue(_, valueName) ->
    $"value:{valueNameResHashWithMapping mapping valueName}"
  | PT.EStatement(_, first, next) -> $"stmt:{hash first}:{hash next}"
  | PT.ESelf _ -> "self"

and stringSegmentHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (seg : PT.StringSegment)
  : string =
  match seg with
  | PT.StringText s -> $"text:{s}"
  | PT.StringInterpolation e -> $"interp:{exprContentHashWithMapping mapping e}"

and letPatternHash (pat : PT.LetPattern) : string =
  match pat with
  | PT.LPVariable(_, name) -> $"var:{name}"
  | PT.LPUnit _ -> "unit"
  | PT.LPTuple(_, p1, p2, rest) ->
    let restHash = rest |> List.map letPatternHash |> String.concat ","
    $"tuple:{letPatternHash p1}:{letPatternHash p2}:[{restHash}]"

and matchPatternHash (pat : PT.MatchPattern) : string =
  match pat with
  | PT.MPVariable(_, name) -> $"var:{name}"
  | PT.MPUnit _ -> "unit"
  | PT.MPBool(_, b) -> $"bool:{b}"
  | PT.MPInt8(_, n) -> $"i8:{n}"
  | PT.MPUInt8(_, n) -> $"u8:{n}"
  | PT.MPInt16(_, n) -> $"i16:{n}"
  | PT.MPUInt16(_, n) -> $"u16:{n}"
  | PT.MPInt32(_, n) -> $"i32:{n}"
  | PT.MPUInt32(_, n) -> $"u32:{n}"
  | PT.MPInt64(_, n) -> $"i64:{n}"
  | PT.MPUInt64(_, n) -> $"u64:{n}"
  | PT.MPInt128(_, n) -> $"i128:{n}"
  | PT.MPUInt128(_, n) -> $"u128:{n}"
  | PT.MPFloat(_, sign, w, f) -> $"float:{sign}:{w}:{f}"
  | PT.MPChar(_, c) -> $"char:{c}"
  | PT.MPString(_, s) -> $"string:{s}"
  | PT.MPList(_, pats) ->
    let patsHash = pats |> List.map matchPatternHash |> String.concat ","
    $"list:[{patsHash}]"
  | PT.MPListCons(_, head, tail) ->
    $"listCons:{matchPatternHash head}:{matchPatternHash tail}"
  | PT.MPTuple(_, p1, p2, rest) ->
    let restHash = rest |> List.map matchPatternHash |> String.concat ","
    $"tuple:{matchPatternHash p1}:{matchPatternHash p2}:[{restHash}]"
  | PT.MPEnum(_, caseName, pats) ->
    let patsHash = pats |> List.map matchPatternHash |> String.concat ","
    $"enum:{caseName}:[{patsHash}]"
  | PT.MPOr(_, pats) ->
    let patsHash =
      pats |> NEList.toList |> List.map matchPatternHash |> String.concat ","
    $"or:[{patsHash}]"

and matchCaseHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (case : PT.MatchCase)
  : string =
  let hash = exprContentHashWithMapping mapping
  let whenHash = case.whenCondition |> Option.map hash |> Option.defaultValue "none"
  $"case:{matchPatternHash case.pat}:{whenHash}:{hash case.rhs}"

and pipeExprHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (pipeExpr : PT.PipeExpr)
  : string =
  let hash = exprContentHashWithMapping mapping
  match pipeExpr with
  | PT.EPipeVariable(_, name, args) ->
    let argsHash = args |> List.map hash |> String.concat ","
    $"pipeVar:{name}:[{argsHash}]"
  | PT.EPipeLambda(_, pats, body) ->
    let patsHash =
      pats |> NEList.toList |> List.map letPatternHash |> String.concat ","
    $"pipeLambda:[{patsHash}]:{hash body}"
  | PT.EPipeInfix(_, op, arg) -> $"pipeInfix:{infixHash op}:{hash arg}"
  | PT.EPipeFnCall(_, fnName, typeArgs, args) ->
    let typeArgsHash =
      typeArgs |> List.map (typeRefHashWithMapping mapping) |> String.concat ","
    let argsHash = args |> List.map hash |> String.concat ","
    $"pipeFnCall:{fnNameResHashWithMapping mapping fnName}:[{typeArgsHash}]:[{argsHash}]"
  | PT.EPipeEnum(_, typeName, caseName, fields) ->
    let fieldsHash = fields |> List.map hash |> String.concat ","
    $"pipeEnum:{typeNameResHashWithMapping mapping typeName}:{caseName}:[{fieldsHash}]"

and infixHash (op : PT.Infix) : string =
  match op with
  | PT.InfixFnCall fn -> infixFnNameHash fn
  | PT.BinOp binOp ->
    match binOp with
    | PT.BinOpAnd -> "and"
    | PT.BinOpOr -> "or"

and infixFnNameHash (op : PT.InfixFnName) : string =
  match op with
  | PT.ArithmeticPlus -> "+"
  | PT.ArithmeticMinus -> "-"
  | PT.ArithmeticMultiply -> "*"
  | PT.ArithmeticDivide -> "/"
  | PT.ArithmeticModulo -> "%"
  | PT.ArithmeticPower -> "^"
  | PT.ComparisonGreaterThan -> ">"
  | PT.ComparisonGreaterThanOrEqual -> ">="
  | PT.ComparisonLessThan -> "<"
  | PT.ComparisonLessThanOrEqual -> "<="
  | PT.ComparisonEquals -> "=="
  | PT.ComparisonNotEquals -> "!="
  | PT.StringConcat -> "++"

and typeRefHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (tr : PT.TypeReference)
  : string =
  let hash = typeRefHashWithMapping mapping
  match tr with
  | PT.TUnit -> "unit"
  | PT.TBool -> "bool"
  | PT.TInt8 -> "i8"
  | PT.TUInt8 -> "u8"
  | PT.TInt16 -> "i16"
  | PT.TUInt16 -> "u16"
  | PT.TInt32 -> "i32"
  | PT.TUInt32 -> "u32"
  | PT.TInt64 -> "i64"
  | PT.TUInt64 -> "u64"
  | PT.TInt128 -> "i128"
  | PT.TUInt128 -> "u128"
  | PT.TFloat -> "float"
  | PT.TChar -> "char"
  | PT.TString -> "string"
  | PT.TUuid -> "uuid"
  | PT.TDateTime -> "datetime"
  | PT.TVariable name -> $"var:{name}"
  | PT.TList inner -> $"list:{hash inner}"
  | PT.TDict inner -> $"dict:{hash inner}"
  | PT.TDB inner -> $"db:{hash inner}"
  | PT.TTuple(t1, t2, rest) ->
    let restHash = rest |> List.map hash |> String.concat ","
    $"tuple:{hash t1}:{hash t2}:[{restHash}]"
  | PT.TCustomType(nr, args) ->
    let argsHash = args |> List.map hash |> String.concat ","
    $"custom:{typeNameResHashWithMapping mapping nr}:[{argsHash}]"
  | PT.TFn(args, ret) ->
    let argsHash = args |> NEList.toList |> List.map hash |> String.concat ","
    $"fn:[{argsHash}]:{hash ret}"

and typeNameResHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (nr : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  : string =
  match nr with
  | Ok(PT.FQTypeName.Package id) -> $"pkg:{mapUuid mapping id}"
  | Error err -> $"err:{err}"

and fnNameResHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (nr : PT.NameResolution<PT.FQFnName.FQFnName>)
  : string =
  match nr with
  | Ok(PT.FQFnName.Builtin name) -> $"builtin:{name.name}:{name.version}"
  | Ok(PT.FQFnName.Package id) -> $"pkg:{mapUuid mapping id}"
  | Error err -> $"err:{err}"

and valueNameResHashWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (nr : PT.NameResolution<PT.FQValueName.FQValueName>)
  : string =
  match nr with
  | Ok(PT.FQValueName.Builtin name) -> $"builtin:{name.name}:{name.version}"
  | Ok(PT.FQValueName.Package id) -> $"pkg:{mapUuid mapping id}"
  | Error err -> $"err:{err}"

/// Convenience wrapper for hashing without mapping (for backwards compatibility)
let exprContentHash (expr : PT.Expr) : string =
  exprContentHashWithMapping
    (System.Collections.Generic.Dictionary<uuid, uuid>())
    expr


/// Compare function signatures (parameters, return type, type params)
/// Returns Breaking if signature changed, Compatible if only body/description changed,
/// Identical if everything matches (including body)
/// The mapping is used to normalize UUIDs in the new function's body to match stable UUIDs.
let fnSignaturesCompatibleWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (existingFn : PT.PackageFn.PackageFn)
  (newFn : PT.PackageFn.PackageFn)
  : ComparisonResult =
  let existingParams = NEList.toList existingFn.parameters
  let newParams = NEList.toList newFn.parameters

  let paramsCompatible =
    List.length existingParams = List.length newParams
    && List.forall2 parametersEqual existingParams newParams

  let returnTypeCompatible = typeRefsEqual existingFn.returnType newFn.returnType

  let typeParamsCompatible = existingFn.typeParams = newFn.typeParams

  if not (paramsCompatible && returnTypeCompatible && typeParamsCompatible) then
    Breaking
  else
    // Signature is compatible - check if body content is identical
    // We compare content hashes which ignore expression IDs
    // Use empty mapping for existing (already has stable IDs), provided mapping for new
    let emptyMapping = System.Collections.Generic.Dictionary<uuid, uuid>()
    let existingBodyHash = exprContentHashWithMapping emptyMapping existingFn.body
    let newBodyHash = exprContentHashWithMapping mapping newFn.body
    if existingBodyHash = newBodyHash then Identical else Compatible

/// Convenience wrapper without mapping (for backwards compatibility)
let fnSignaturesCompatible
  (existingFn : PT.PackageFn.PackageFn)
  (newFn : PT.PackageFn.PackageFn)
  : ComparisonResult =
  fnSignaturesCompatibleWithMapping
    (System.Collections.Generic.Dictionary<uuid, uuid>())
    existingFn
    newFn


/// Compare record fields
let recordFieldsEqual
  (a : PT.TypeDeclaration.RecordField)
  (b : PT.TypeDeclaration.RecordField)
  : bool =
  a.name = b.name && typeRefsEqual a.typ b.typ
// Note: description changes don't affect signature compatibility


/// Compare enum fields
let enumFieldsEqual
  (a : PT.TypeDeclaration.EnumField)
  (b : PT.TypeDeclaration.EnumField)
  : bool =
  typeRefsEqual a.typ b.typ && a.label = b.label
// Note: description changes don't affect signature compatibility


/// Compare enum cases
let enumCasesEqual
  (a : PT.TypeDeclaration.EnumCase)
  (b : PT.TypeDeclaration.EnumCase)
  : bool =
  a.name = b.name
  && List.length a.fields = List.length b.fields
  && List.forall2 enumFieldsEqual a.fields b.fields


/// Compare type definitions
let typeDefinitionsEqual
  (a : PT.TypeDeclaration.Definition)
  (b : PT.TypeDeclaration.Definition)
  : bool =
  match a, b with
  | PT.TypeDeclaration.Alias typeRefA, PT.TypeDeclaration.Alias typeRefB ->
    typeRefsEqual typeRefA typeRefB

  | PT.TypeDeclaration.Record fieldsA, PT.TypeDeclaration.Record fieldsB ->
    let fieldsAList = NEList.toList fieldsA
    let fieldsBList = NEList.toList fieldsB
    List.length fieldsAList = List.length fieldsBList
    && List.forall2 recordFieldsEqual fieldsAList fieldsBList

  | PT.TypeDeclaration.Enum casesA, PT.TypeDeclaration.Enum casesB ->
    let casesAList = NEList.toList casesA
    let casesBList = NEList.toList casesB
    List.length casesAList = List.length casesBList
    && List.forall2 enumCasesEqual casesAList casesBList

  | _, _ -> false


/// Compare type declarations (type params + definition)
/// For types, if the definition is structurally equal, they are Identical
let typeDeclarationsCompatible
  (existingType : PT.PackageType.PackageType)
  (newType : PT.PackageType.PackageType)
  : ComparisonResult =
  let typeParamsCompatible =
    existingType.declaration.typeParams = newType.declaration.typeParams

  let definitionCompatible =
    typeDefinitionsEqual
      existingType.declaration.definition
      newType.declaration.definition

  if not (typeParamsCompatible && definitionCompatible) then
    Breaking
  else
    // Type definitions are structurally equal, so they're identical
    Identical


/// Compare two functions and determine if the change is breaking
/// Uses the provided mapping to normalize UUIDs in the new function's body
let compareFnsWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (existingFn : PT.PackageFn.PackageFn)
  (newFn : PT.PackageFn.PackageFn)
  : ComparisonResult =
  fnSignaturesCompatibleWithMapping mapping existingFn newFn

/// Compare two functions without mapping (for backwards compatibility)
let compareFns
  (existingFn : PT.PackageFn.PackageFn)
  (newFn : PT.PackageFn.PackageFn)
  : ComparisonResult =
  fnSignaturesCompatible existingFn newFn


/// Compare two types and determine if the change is breaking
let compareTypes
  (existingType : PT.PackageType.PackageType)
  (newType : PT.PackageType.PackageType)
  : ComparisonResult =
  typeDeclarationsCompatible existingType newType


/// Compare two values - check if the body expression is identical
/// Uses the provided mapping to normalize UUIDs in the new value's body
let compareValuesWithMapping
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (existingValue : PT.PackageValue.PackageValue)
  (newValue : PT.PackageValue.PackageValue)
  : ComparisonResult =
  // Compare body content (ignoring expression IDs)
  // Use empty mapping for existing (already has stable IDs), provided mapping for new
  let emptyMapping = System.Collections.Generic.Dictionary<uuid, uuid>()
  let existingBodyHash = exprContentHashWithMapping emptyMapping existingValue.body
  let newBodyHash = exprContentHashWithMapping mapping newValue.body
  if existingBodyHash = newBodyHash then
    Identical
  else
    // Values don't have breaking changes since they're referenced by name
    Compatible

/// Compare two values without mapping (for backwards compatibility)
let compareValues
  (existingValue : PT.PackageValue.PackageValue)
  (newValue : PT.PackageValue.PackageValue)
  : ComparisonResult =
  compareValuesWithMapping
    (System.Collections.Generic.Dictionary<uuid, uuid>())
    existingValue
    newValue


// ----------------------
// UUID Transformation
// ----------------------

/// Transform a name resolution using the provided mapping
let private transformTypeNameRes
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (nr : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  : PT.NameResolution<PT.FQTypeName.FQTypeName> =
  match nr with
  | Ok(PT.FQTypeName.Package id) -> Ok(PT.FQTypeName.Package(mapUuid mapping id))
  | other -> other

let private transformFnNameRes
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (nr : PT.NameResolution<PT.FQFnName.FQFnName>)
  : PT.NameResolution<PT.FQFnName.FQFnName> =
  match nr with
  | Ok(PT.FQFnName.Package id) -> Ok(PT.FQFnName.Package(mapUuid mapping id))
  | other -> other

let private transformValueNameRes
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (nr : PT.NameResolution<PT.FQValueName.FQValueName>)
  : PT.NameResolution<PT.FQValueName.FQValueName> =
  match nr with
  | Ok(PT.FQValueName.Package id) -> Ok(PT.FQValueName.Package(mapUuid mapping id))
  | other -> other

/// Transform all UUIDs in a TypeReference using the mapping
let rec transformTypeRef
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (tr : PT.TypeReference)
  : PT.TypeReference =
  let t = transformTypeRef mapping
  match tr with
  | PT.TUnit -> PT.TUnit
  | PT.TBool -> PT.TBool
  | PT.TInt8 -> PT.TInt8
  | PT.TUInt8 -> PT.TUInt8
  | PT.TInt16 -> PT.TInt16
  | PT.TUInt16 -> PT.TUInt16
  | PT.TInt32 -> PT.TInt32
  | PT.TUInt32 -> PT.TUInt32
  | PT.TInt64 -> PT.TInt64
  | PT.TUInt64 -> PT.TUInt64
  | PT.TInt128 -> PT.TInt128
  | PT.TUInt128 -> PT.TUInt128
  | PT.TFloat -> PT.TFloat
  | PT.TChar -> PT.TChar
  | PT.TString -> PT.TString
  | PT.TUuid -> PT.TUuid
  | PT.TDateTime -> PT.TDateTime
  | PT.TVariable name -> PT.TVariable name
  | PT.TList inner -> PT.TList(t inner)
  | PT.TDict inner -> PT.TDict(t inner)
  | PT.TDB inner -> PT.TDB(t inner)
  | PT.TTuple(t1, t2, rest) -> PT.TTuple(t t1, t t2, List.map t rest)
  | PT.TCustomType(nr, args) ->
    PT.TCustomType(transformTypeNameRes mapping nr, List.map t args)
  | PT.TFn(args, ret) -> PT.TFn(NEList.map t args, t ret)

/// Transform all UUIDs in an expression using the mapping
let rec transformExprUuids
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (expr : PT.Expr)
  : PT.Expr =
  let t = transformExprUuids mapping
  let tTypeRef = transformTypeRef mapping
  match expr with
  | PT.EUnit id -> PT.EUnit id
  | PT.EBool(id, b) -> PT.EBool(id, b)
  | PT.EInt8(id, n) -> PT.EInt8(id, n)
  | PT.EUInt8(id, n) -> PT.EUInt8(id, n)
  | PT.EInt16(id, n) -> PT.EInt16(id, n)
  | PT.EUInt16(id, n) -> PT.EUInt16(id, n)
  | PT.EInt32(id, n) -> PT.EInt32(id, n)
  | PT.EUInt32(id, n) -> PT.EUInt32(id, n)
  | PT.EInt64(id, n) -> PT.EInt64(id, n)
  | PT.EUInt64(id, n) -> PT.EUInt64(id, n)
  | PT.EInt128(id, n) -> PT.EInt128(id, n)
  | PT.EUInt128(id, n) -> PT.EUInt128(id, n)
  | PT.EFloat(id, sign, w, f) -> PT.EFloat(id, sign, w, f)
  | PT.EChar(id, c) -> PT.EChar(id, c)
  | PT.EString(id, parts) ->
    PT.EString(id, List.map (transformStringSegment mapping) parts)
  | PT.EVariable(id, name) -> PT.EVariable(id, name)
  | PT.EArg(id, idx) -> PT.EArg(id, idx)
  | PT.ELet(id, pat, rhs, body) -> PT.ELet(id, pat, t rhs, t body)
  | PT.EIf(id, cond, thenE, elseE) ->
    PT.EIf(id, t cond, t thenE, Option.map t elseE)
  | PT.ELambda(id, pats, body) -> PT.ELambda(id, pats, t body)
  | PT.ERecordFieldAccess(id, obj, field) -> PT.ERecordFieldAccess(id, t obj, field)
  | PT.EList(id, items) -> PT.EList(id, List.map t items)
  | PT.EDict(id, pairs) -> PT.EDict(id, List.map (fun (k, v) -> (k, t v)) pairs)
  | PT.ETuple(id, e1, e2, rest) -> PT.ETuple(id, t e1, t e2, List.map t rest)
  | PT.ERecord(id, typeName, typeArgs, fields) ->
    PT.ERecord(
      id,
      transformTypeNameRes mapping typeName,
      List.map tTypeRef typeArgs,
      List.map (fun (n, e) -> (n, t e)) fields
    )
  | PT.ERecordUpdate(id, record, updates) ->
    PT.ERecordUpdate(id, t record, NEList.map (fun (n, e) -> (n, t e)) updates)
  | PT.EEnum(id, typeName, typeArgs, caseName, fields) ->
    PT.EEnum(
      id,
      transformTypeNameRes mapping typeName,
      List.map tTypeRef typeArgs,
      caseName,
      List.map t fields
    )
  | PT.EMatch(id, expr, cases) ->
    PT.EMatch(id, t expr, List.map (transformMatchCase mapping) cases)
  | PT.EPipe(id, e1, rest) ->
    PT.EPipe(id, t e1, List.map (transformPipeExpr mapping) rest)
  | PT.EInfix(id, op, l, r) -> PT.EInfix(id, op, t l, t r)
  | PT.EApply(id, fn, typeArgs, args) ->
    PT.EApply(id, t fn, List.map tTypeRef typeArgs, NEList.map t args)
  | PT.EFnName(id, fnName) ->
    PT.EFnName(id, transformFnNameRes mapping fnName)
  | PT.EValue(id, valueName) ->
    PT.EValue(id, transformValueNameRes mapping valueName)
  | PT.EStatement(id, first, next) -> PT.EStatement(id, t first, t next)
  | PT.ESelf id -> PT.ESelf id

and private transformStringSegment
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (seg : PT.StringSegment)
  : PT.StringSegment =
  match seg with
  | PT.StringText s -> PT.StringText s
  | PT.StringInterpolation e -> PT.StringInterpolation(transformExprUuids mapping e)

and private transformMatchCase
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (case : PT.MatchCase)
  : PT.MatchCase =
  { pat = case.pat
    whenCondition = Option.map (transformExprUuids mapping) case.whenCondition
    rhs = transformExprUuids mapping case.rhs }

and private transformPipeExpr
  (mapping : System.Collections.Generic.Dictionary<uuid, uuid>)
  (pipeExpr : PT.PipeExpr)
  : PT.PipeExpr =
  let t = transformExprUuids mapping
  let tTypeRef = transformTypeRef mapping
  match pipeExpr with
  | PT.EPipeVariable(id, name, args) ->
    PT.EPipeVariable(id, name, List.map t args)
  | PT.EPipeLambda(id, pats, body) -> PT.EPipeLambda(id, pats, t body)
  | PT.EPipeInfix(id, op, arg) -> PT.EPipeInfix(id, op, t arg)
  | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
    PT.EPipeFnCall(
      id,
      transformFnNameRes mapping fnName,
      List.map tTypeRef typeArgs,
      List.map t args
    )
  | PT.EPipeEnum(id, typeName, caseName, fields) ->
    PT.EPipeEnum(
      id,
      transformTypeNameRes mapping typeName,
      caseName,
      List.map t fields
    )
