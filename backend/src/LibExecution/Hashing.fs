/// Hashing functions for package items (types, functions, values)
///
/// Hashes uniquely identify package items across the system. They are stable and
/// deterministic - the same input always produces the same hash.
///
/// What's included in hashes:
/// - Types: type structure + qualified name (e.g., "MyModule.Person")
/// - Functions: parameter types + return type + body (NO parameter names)
/// - Values: body expression only
///
/// Type names are included because structurally identical types with different
/// names are distinct types. Function/value names are excluded to allow renaming.
module LibExecution.Hashing

open System.Security.Cryptography
open System.Text

open Prelude
open ProgramTypes


/// Compute a SHA-256 hash of the given string and return it as a Hash
let private sha256 (input : string) : Hash =
  let bytes = Encoding.UTF8.GetBytes(input)
  let hashBytes = SHA256.HashData(System.ReadOnlySpan(bytes))
  let hashString = System.Convert.ToHexString(hashBytes).ToLowerInvariant()
  Hash hashString


/// Serialize a package item to a stable string representation for hashing
module private Serialization =
  let rec typeReference (t : TypeReference) : string =
    match t with
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
    | TList inner -> $"List<{typeReference inner}>"
    | TTuple(first, second, rest) ->
      let items = first :: second :: rest |> List.map typeReference
      let joined = String.concat ", " items
      $"({joined})"
    | TDict inner -> $"Dict<{typeReference inner}>"
    | TCustomType(Ok(FQTypeName.Package(Hash h)), typeArgs) ->
      let args =
        if List.isEmpty typeArgs then
          ""
        else
          let argStrs = typeArgs |> List.map typeReference
          let joined = String.concat ", " argStrs
          $"<{joined}>"
      $"CustomType({h}){args}"
    | TCustomType(Error err, _) -> $"UnresolvedType({err})"
    | TFn(args, ret) ->
      let argStrs = args |> NEList.toList |> List.map typeReference
      let joined = String.concat ", " argStrs
      $"Fn({joined}) -> {typeReference ret}"
    | TVariable name -> $"'{name}"
    | TDB inner -> $"DB<{typeReference inner}>"

  let rec expr (e : Expr) : string =
    match e with
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
    | EFloat(_, sign, w, f) -> $"Float({sign},{w},{f})"
    | EChar(_, c) -> $"Char({c})"
    | EString(_, segments) ->
      let segStr =
        segments
        |> List.map (function
          | StringText s -> $"Text({s})"
          | StringInterpolation e -> $"Interp({expr e})")
        |> String.concat ";"
      $"String[{segStr}]"
    | EVariable(_, name) -> $"Var({name})"
    | EArg(_, index) -> $"Arg({index})"
    | ELet(_, pat, rhs, body) -> $"Let({letPattern pat}, {expr rhs}, {expr body})"
    | EIf(_, cond, thenExpr, elseExpr) ->
      let elseStr =
        match elseExpr with
        | Some e -> expr e
        | None -> "None"
      $"If({expr cond}, {expr thenExpr}, {elseStr})"
    | EList(_, items) ->
      let joined = items |> List.map expr |> String.concat "; "
      $"List[{joined}]"
    | EDict(_, pairs) ->
      let pairStrs = pairs |> List.map (fun (k, v) -> k + ":" + expr v)
      let joined = String.concat "; " pairStrs
      $"Dict[{joined}]"
    | ETuple(_, first, second, rest) ->
      let items = first :: second :: rest |> List.map expr
      let joined = String.concat ", " items
      $"Tuple({joined})"
    | ERecord(_, Ok(FQTypeName.Package(Hash h)), typeArgs, fields) ->
      let typeArgsStr =
        if List.isEmpty typeArgs then
          ""
        else
          let joinedArgs = typeArgs |> List.map typeReference |> String.concat ", "
          $"<{joinedArgs}>"
      let fieldPairs = fields |> List.map (fun (k, v) -> k + "=" + expr v)
      let fieldStrs = String.concat "; " fieldPairs
      $"Record({h}{typeArgsStr}, [{fieldStrs}])"
    | ERecord(_, Error err, typeArgs, fields) ->
      let typeArgsStr =
        if List.isEmpty typeArgs then
          ""
        else
          let joinedArgs = typeArgs |> List.map typeReference |> String.concat ", "
          $"<{joinedArgs}>"
      let fieldPairs = fields |> List.map (fun (k, v) -> k + "=" + expr v)
      let fieldStrs = String.concat "; " fieldPairs
      $"UnresolvedRecord({err}{typeArgsStr}, [{fieldStrs}])"
    | ERecordFieldAccess(_, record, fieldName) ->
      $"FieldAccess({expr record}, {fieldName})"
    | ERecordUpdate(_, record, updates) ->
      let updateStrs =
        updates
        |> NEList.toList
        |> List.map (fun (k, v) -> $"{k}={expr v}")
        |> String.concat "; "
      $"RecordUpdate({expr record}, [{updateStrs}])"
    | EEnum(_, Ok(FQTypeName.Package(Hash h)), typeArgs, caseName, fields) ->
      let typeArgsStr =
        if List.isEmpty typeArgs then
          ""
        else
          let joinedArgs = typeArgs |> List.map typeReference |> String.concat ", "

          $"<{joinedArgs}>"
      let fieldStrs = fields |> List.map expr |> String.concat ", "
      $"Enum({h}{typeArgsStr}, {caseName}, [{fieldStrs}])"
    | EEnum(_, Error err, typeArgs, caseName, fields) ->
      let typeArgsStr =
        if List.isEmpty typeArgs then
          ""
        else
          let joinedArgs = typeArgs |> List.map typeReference |> String.concat ", "
          $"<{joinedArgs}>"
      let fieldStrs = fields |> List.map expr |> String.concat ", "
      $"UnresolvedEnum({err}{typeArgsStr}, {caseName}, [{fieldStrs}])"
    | EMatch(_, arg, cases) ->
      let caseStrs =
        cases
        |> List.map (fun case ->
          let whenStr =
            match case.whenCondition with
            | Some e -> $" when {expr e}"
            | None -> ""
          $"{matchPattern case.pat}{whenStr} -> {expr case.rhs}")
        |> String.concat "; "
      $"Match({expr arg}, [{caseStrs}])"
    | EApply(_, fnExpr, typeArgs, args) ->
      let typeArgsStr =
        if List.isEmpty typeArgs then
          ""
        else
          let joinedArgs = typeArgs |> List.map typeReference |> String.concat ", "

          $"<{joinedArgs}>"
      let argStrs = args |> NEList.toList |> List.map expr |> String.concat ", "
      $"Apply({expr fnExpr}{typeArgsStr}, [{argStrs}])"
    | EFnName(_, Ok(FQFnName.Builtin b)) -> $"Builtin({b.name}, {b.version})"
    | EFnName(_, Ok(FQFnName.Package(Hash h))) -> $"PackageFn({h})"
    | EFnName(_, Error err) -> $"UnresolvedFn({err})"
    | ELambda(_, pats, body) ->
      let patStrs =
        pats |> NEList.toList |> List.map letPattern |> String.concat ", "
      $"Lambda([{patStrs}], {expr body})"
    | EInfix(_, infix, lhs, rhs) ->
      $"Infix({infixName infix}, {expr lhs}, {expr rhs})"
    | EPipe(_, lhs, parts) ->
      let partStrs = parts |> List.map pipeExpr |> String.concat " |> "
      $"Pipe({expr lhs} |> {partStrs})"
    | EValue(_, Ok(FQValueName.Builtin b)) -> $"BuiltinValue({b.name}, {b.version})"
    | EValue(_, Ok(FQValueName.Package(Hash h))) -> $"PackageValue({h})"
    | EValue(_, Error err) -> $"UnresolvedValue({err})"
    | EStatement(_, first, next) -> $"Statement({expr first}; {expr next})"
    | ESelf _ -> "Self"

  and letPattern (p : LetPattern) : string =
    match p with
    | LPVariable(_, name) -> name
    | LPTuple(_, first, second, rest) ->
      let items = first :: second :: rest |> List.map letPattern
      let joined = String.concat ", " items
      $"({joined})"
    | LPUnit _ -> "()"

  and matchPattern (p : MatchPattern) : string =
    match p with
    | MPUnit _ -> "()"
    | MPBool(_, b) -> string b
    | MPInt8(_, i) -> $"{i}y"
    | MPUInt8(_, i) -> $"{i}uy"
    | MPInt16(_, i) -> $"{i}s"
    | MPUInt16(_, i) -> $"{i}us"
    | MPInt32(_, i) -> $"{i}l"
    | MPUInt32(_, i) -> $"{i}ul"
    | MPInt64(_, i) -> $"{i}L"
    | MPUInt64(_, i) -> $"{i}UL"
    | MPInt128(_, i) -> $"{i}Q"
    | MPUInt128(_, i) -> $"{i}Z"
    | MPFloat(_, sign, w, f) -> $"{sign}{w}.{f}"
    | MPChar(_, c) -> $"'{c}'"
    | MPString(_, s) -> $"\"{s}\""
    | MPList(_, items) ->
      let joined = items |> List.map matchPattern |> String.concat "; "
      $"[{joined}]"
    | MPListCons(_, head, tail) -> $"{matchPattern head} :: {matchPattern tail}"
    | MPTuple(_, first, second, rest) ->
      let items = first :: second :: rest |> List.map matchPattern
      let joined = String.concat ", " items
      $"({joined})"
    | MPEnum(_, caseName, fields) ->
      let fieldStrs = fields |> List.map matchPattern |> String.concat ", "
      $"{caseName}({fieldStrs})"
    | MPVariable(_, name) -> name
    | MPOr(_, patterns) ->
      patterns |> NEList.toList |> List.map matchPattern |> String.concat " | "

  and pipeExpr (pe : PipeExpr) : string =
    match pe with
    | EPipeLambda(_, pats, body) ->
      let patStrs =
        pats |> NEList.toList |> List.map letPattern |> String.concat ", "
      $"fun ({patStrs}) -> {expr body}"
    | EPipeInfix(_, infix, e) -> $"{infixName infix} {expr e}"
    | EPipeFnCall(_, Ok(FQFnName.Builtin b), typeArgs, args) ->
      let typeArgsStr =
        if List.isEmpty typeArgs then
          ""
        else
          let joinedArgs = typeArgs |> List.map typeReference |> String.concat ", "

          $"<{joinedArgs}>"
      let argStrs =
        if List.isEmpty args then
          ""
        else
          let joinedArgs = args |> List.map expr |> String.concat " "

          $" {joinedArgs}"
      $"Builtin({b.name}, {b.version}){typeArgsStr}{argStrs}"
    | EPipeFnCall(_, Ok(FQFnName.Package(Hash h)), typeArgs, args) ->
      let typeArgsStr =
        if List.isEmpty typeArgs then
          ""
        else
          let joinedArgs = typeArgs |> List.map typeReference |> String.concat ", "

          $"<{joinedArgs}>"
      let argStrs =
        if List.isEmpty args then
          ""
        else
          let joinedArgs = args |> List.map expr |> String.concat " "

          $" {joinedArgs}"
      $"PackageFn({h}){typeArgsStr}{argStrs}"
    | EPipeFnCall(_, Error err, _, _) -> $"UnresolvedPipeFn({err})"
    | EPipeEnum(_, Ok(FQTypeName.Package(Hash h)), caseName, fields) ->
      let fieldStrs = fields |> List.map expr |> String.concat ", "
      $"Enum({h}, {caseName}, [{fieldStrs}])"
    | EPipeEnum(_, Error err, caseName, fields) ->
      let fieldStrs = fields |> List.map expr |> String.concat ", "
      $"UnresolvedPipeEnum({err}, {caseName}, [{fieldStrs}])"
    | EPipeVariable(_, varName, args) ->
      let argStrs =
        if List.isEmpty args then
          ""
        else
          let joinedArgs = args |> List.map expr |> String.concat " "

          $" {joinedArgs}"
      $"{varName}{argStrs}"

  and infixName (i : Infix) : string =
    match i with
    | InfixFnCall ifn ->
      match ifn with
      | ArithmeticPlus -> "+"
      | ArithmeticMinus -> "-"
      | ArithmeticMultiply -> "*"
      | ArithmeticDivide -> "/"
      | ArithmeticModulo -> "%"
      | ArithmeticPower -> "^"
      | ComparisonGreaterThan -> ">"
      | ComparisonGreaterThanOrEqual -> ">="
      | ComparisonLessThan -> "<"
      | ComparisonLessThanOrEqual -> "<="
      | ComparisonEquals -> "=="
      | ComparisonNotEquals -> "!="
      | StringConcat -> "++"
    | BinOp op ->
      match op with
      | BinOpAnd -> "&&"
      | BinOpOr -> "||"

  let typeDeclaration (decl : TypeDeclaration.T) : string =
    let typeParamsStr =
      if List.isEmpty decl.typeParams then
        ""
      else
        let joinedParams = String.concat ", " decl.typeParams

        $"<{joinedParams}>"

    match decl.definition with
    | TypeDeclaration.Alias typ -> $"Alias{typeParamsStr} = {typeReference typ}"
    | TypeDeclaration.Record fields ->
      let fieldStrs =
        fields
        |> NEList.toList
        |> List.map (fun f -> $"{f.name}: {typeReference f.typ}")
        |> String.concat "; "
      $"Record{typeParamsStr} {{ {fieldStrs} }}"
    | TypeDeclaration.Enum cases ->
      let caseStrs =
        cases
        |> NEList.toList
        |> List.map (fun c ->
          if List.isEmpty c.fields then
            c.name
          else
            let fieldStrs =
              c.fields
              |> List.map (fun f -> typeReference f.typ)
              |> String.concat " * "
            $"{c.name} of {fieldStrs}")
        |> String.concat " | "
      $"Enum{typeParamsStr} = {caseStrs}"


module TypeDeclaration =
  /// Hash without the type name. Used only as a fallback; prefer hashWithName.
  let hash (decl : TypeDeclaration.T) : Hash =
    let serialized = Serialization.typeDeclaration decl
    let content = $"PackageType|{serialized}"
    sha256 content

  /// Hash a type declaration including its qualified name.
  ///
  /// The name is included because:
  /// 1. Type includes the name - MyModule.Person and OtherModule.Employee
  ///    are different types even if structurally identical
  /// 2. Prevents collisions when types have unresolved references during parsing
  let hashWithName (name : PackageType.Name) (decl : TypeDeclaration.T) : Hash =
    let serialized = Serialization.typeDeclaration decl
    let fullName = $"""{name.owner}.{String.concat "." name.modules}.{name.name}"""
    let content = $"PackageType|{fullName}|{serialized}"
    sha256 content


module PackageValue =
  /// Hash a value based on its body expression.
  /// The value's name is not included, so renaming doesn't change the hash.
  let hash (body : Expr) : Hash =
    let content = $"PackageValue|{Serialization.expr body}"
    sha256 content


module PackageFn =
  /// Hash a function based on parameter types, return type, and body.
  ///
  /// Parameter names are excluded because the body uses position-based references
  /// (EArg), not name-based (EVariable). This allows parameter renaming without
  /// changing the hash. The function's own name is also excluded.
  let hash
    (
      parameters : NEList<PackageFn.Parameter>,
      returnType : TypeReference,
      body : Expr
    ) : Hash =
    let paramStrs =
      parameters
      |> NEList.toList
      |> List.map (fun p -> Serialization.typeReference p.typ)
      |> String.concat ", "
    let retStr = Serialization.typeReference returnType
    let bodyStr = Serialization.expr body
    let content = $"PackageFn|({paramStrs}) -> {retStr}|{bodyStr}"
    sha256 content
