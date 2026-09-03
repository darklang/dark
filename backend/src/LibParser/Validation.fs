/// Validates a parsed WrittenTypes tree before it is lowered to ProgramTypes.
/// Parsing is shared by scripts, packages, and tests, and may produce recovery
/// nodes after syntax errors. This module applies the rules for each file mode
/// and checks structural invariants, such as unique binders and compatible
/// or-pattern bindings, that lowering relies on. It does not resolve names,
/// check types, or evaluate expressions.
module LibParser.Validation

open Prelude

module WT = WrittenTypes

type Mode =
  | Script
  | Package
  | Test

type IssueCode =
  | DuplicateBinder
  | OrBindingMismatch
  | EmptyOrPattern
  | RecoveryHole
  | EmptyLambda
  | EmptyMatch
  | AnonymousRecord
  | EmptyRecordUpdate
  | PackageExpression
  | TestAssertion
  | DBMode
  | DBShape
  | TestMode

module IssueCode =
  let toString (code : IssueCode) : string =
    match code with
    | DuplicateBinder -> "VALIDATION-DUPLICATE-BINDER"
    | OrBindingMismatch -> "VALIDATION-OR-BINDINGS"
    | EmptyOrPattern -> "VALIDATION-OR-PATTERN"
    | RecoveryHole -> "VALIDATION-RECOVERY-HOLE"
    | EmptyLambda -> "VALIDATION-LAMBDA"
    | EmptyMatch -> "VALIDATION-MATCH"
    | AnonymousRecord -> "VALIDATION-ANONYMOUS-RECORD"
    | EmptyRecordUpdate -> "VALIDATION-RECORD-UPDATE"
    | PackageExpression -> "VALIDATION-PACKAGE-EXPR"
    | TestAssertion -> "VALIDATION-TEST-ASSERTION"
    | DBMode -> "VALIDATION-DB-MODE"
    | DBShape -> "VALIDATION-DB-SHAPE"
    | TestMode -> "VALIDATION-TEST-MODE"

type Issue =
  { range : WT.Range
    code : IssueCode
    message : string
    related : List<WT.Range * string>
    hint : Option<string> }

/// A source file that passed both structural and file-purpose validation.
/// The case is private so production parsing paths cannot create one without
/// calling `validate`.
type ValidatedSourceFile = private ValidatedSourceFile of Mode * WT.SourceFile

module ValidatedSourceFile =
  let mode (ValidatedSourceFile(mode, _)) : Mode = mode

  let toWrittenTypes (ValidatedSourceFile(_, sourceFile)) : WT.SourceFile =
    sourceFile

let private detailedIssue
  (range : WT.Range)
  (code : IssueCode)
  (message : string)
  (related : List<WT.Range * string>)
  (hint : Option<string>)
  : Issue =
  { range = range; code = code; message = message; related = related; hint = hint }

let private issue range code message = detailedIssue range code message [] None

let private isIgnoredName (name : string) = name = "" || name.StartsWith "_"

let rec private letBindings (pattern : WT.LetPattern) : List<string * WT.Range> =
  match pattern with
  | WT.LPVariable(range, name) -> [ (name, range) ]
  | WT.LPTuple(_, first, _, second, rest, _, _) ->
    letBindings first
    @ letBindings second
    @ (rest |> List.collect (snd >> letBindings))
  | WT.LPUnit _
  | WT.LPWildcard _ -> []

let rec private matchBindings (pattern : WT.MatchPattern) : List<string * WT.Range> =
  match pattern with
  | WT.MPVariable(range, name) -> [ (name, range) ]
  | WT.MPList(_, contents, _, _) -> contents |> List.collect (fst >> matchBindings)
  | WT.MPListCons(_, head, tail, _) -> matchBindings head @ matchBindings tail
  | WT.MPTuple(_, first, _, second, rest, _, _) ->
    matchBindings first
    @ matchBindings second
    @ (rest |> List.collect (snd >> matchBindings))
  | WT.MPEnum(_, _, fields) -> fields |> List.collect matchBindings
  // A valid or-pattern has one logical binding set. Use the first alternative
  // as its representative when checking an enclosing tuple/list pattern.
  | WT.MPOr(_, first :: _) -> matchBindings first
  | WT.MPOr(_, [])
  | WT.MPUnit _
  | WT.MPBool _
  | WT.MPInt _
  | WT.MPInt64 _
  | WT.MPInt8 _
  | WT.MPUInt8 _
  | WT.MPInt16 _
  | WT.MPUInt16 _
  | WT.MPInt32 _
  | WT.MPUInt32 _
  | WT.MPUInt64 _
  | WT.MPInt128 _
  | WT.MPUInt128 _
  | WT.MPFloat _
  | WT.MPChar _
  | WT.MPString _
  | WT.MPError _ -> []

let private usableBindingNames (pattern : WT.MatchPattern) : Set<string> =
  pattern
  |> matchBindings
  |> List.map fst
  |> List.filter (isIgnoredName >> not)
  |> Set.ofList

let private duplicateIssues (bindings : List<string * WT.Range>) : List<Issue> =
  bindings
  |> List.filter (fst >> isIgnoredName >> not)
  |> List.groupBy fst
  |> Map.values
  |> List.collect (function
    | (name, firstRange) :: (_ :: _ as duplicates) ->
      duplicates
      |> List.map (fun (_, duplicateRange) ->
        detailedIssue
          duplicateRange
          DuplicateBinder
          $"Duplicate binding '{name}' in the same pattern"
          [ (firstRange, $"'{name}' was first bound here") ]
          (Some "use a different name or '_' for a value you do not need"))
    | _ -> [])

let rec private duplicatePatternIssues (pattern : WT.MatchPattern) : List<Issue> =
  match pattern with
  | WT.MPOr(_, alternatives) -> alternatives |> List.collect duplicatePatternIssues
  | other -> duplicateIssues (matchBindings other)

let rec private structuralPatternIssues (pattern : WT.MatchPattern) : List<Issue> =
  let recurse = structuralPatternIssues
  match pattern with
  | WT.MPOr(range, []) ->
    [ issue range EmptyOrPattern "An or-pattern must have at least one alternative" ]
  | WT.MPOr(_, (first :: rest as alternatives)) ->
    let nested = alternatives |> List.collect recurse
    let firstNames = usableBindingNames first
    let unequal =
      rest
      |> List.choose (fun alternative ->
        if usableBindingNames alternative = firstNames then
          None
        else
          Some(
            detailedIssue
              (WT.mpRange alternative)
              OrBindingMismatch
              "Every branch of an or-pattern must bind the same names"
              [ (WT.mpRange first, "the first branch binds a different set") ]
              None
          ))
    nested @ unequal
  | WT.MPError range ->
    [ issue range RecoveryHole "A recovered pattern cannot be lowered" ]
  | WT.MPList(_, contents, _, _) -> contents |> List.collect (fst >> recurse)
  | WT.MPListCons(_, head, tail, _) -> recurse head @ recurse tail
  | WT.MPTuple(_, first, _, second, rest, _, _) ->
    recurse first @ recurse second @ (rest |> List.collect (snd >> recurse))
  | WT.MPEnum(_, _, fields) -> fields |> List.collect recurse
  | WT.MPUnit _
  | WT.MPVariable _
  | WT.MPBool _
  | WT.MPInt _
  | WT.MPInt64 _
  | WT.MPInt8 _
  | WT.MPUInt8 _
  | WT.MPInt16 _
  | WT.MPUInt16 _
  | WT.MPInt32 _
  | WT.MPUInt32 _
  | WT.MPUInt64 _
  | WT.MPInt128 _
  | WT.MPUInt128 _
  | WT.MPFloat _
  | WT.MPChar _
  | WT.MPString _ -> []

let private patternIssues (pattern : WT.MatchPattern) : List<Issue> =
  duplicatePatternIssues pattern @ structuralPatternIssues pattern

let rec private exprIssues (expr : WT.Expr) : List<Issue> =
  let recurse = exprIssues
  match expr with
  | WT.EError range ->
    [ issue range RecoveryHole "A recovered expression cannot be lowered" ]
  | WT.ELambda(range, patterns, body, _, _) ->
    let required =
      if List.isEmpty patterns then
        [ issue range EmptyLambda "A lambda must have at least one parameter" ]
      else
        []
    required @ duplicateIssues (patterns |> List.collect letBindings) @ recurse body
  | WT.ELet(_, pattern, value, body, _, _) ->
    duplicateIssues (letBindings pattern) @ recurse value @ recurse body
  | WT.EMatch(range, value, cases, _, _) ->
    let required =
      if List.isEmpty cases then
        [ issue range EmptyMatch "A match must have at least one case" ]
      else
        []
    required
    @ recurse value
    @ (cases
       |> List.collect (fun case ->
         patternIssues case.pat
         @ (case.whenCondition
            |> Option.map (snd >> recurse)
            |> Option.defaultValue [])
         @ recurse case.rhs))
  | WT.ERecord(range, typeName, fields, _, _) ->
    let named =
      if typeName.typ.name = "" then
        [ issue range AnonymousRecord "Anonymous records are not supported" ]
      else
        []
    named @ (fields |> List.collect (fun (_, _, value) -> recurse value))
  | WT.ERecordUpdate(range, record, updates, _, _, _) ->
    let required =
      if List.isEmpty updates then
        [ issue
            range
            EmptyRecordUpdate
            "A record update must contain at least one 'field = value'" ]
      else
        []
    required
    @ recurse record
    @ (updates |> List.collect (fun (_, _, value) -> recurse value))
  | WT.EString(_, _, segments, _, _) ->
    segments
    |> List.collect (function
      | WT.StringText _ -> []
      | WT.StringInterpolation(_, value, _, _) -> recurse value)
  | WT.EInfix(_, _, left, right)
  | WT.EStatement(_, left, right) -> recurse left @ recurse right
  | WT.EApply(_, callee, _, args) -> recurse callee @ (args |> List.collect recurse)
  | WT.EList(_, contents, _, _) -> contents |> List.collect (fst >> recurse)
  | WT.ETuple(_, first, _, second, rest, _, _) ->
    recurse first @ recurse second @ (rest |> List.collect (snd >> recurse))
  | WT.EIf(_, condition, thenExpr, elseExpr, _, _, _) ->
    recurse condition
    @ recurse thenExpr
    @ (elseExpr |> Option.map recurse |> Option.defaultValue [])
  | WT.ERecordFieldAccess(_, record, _, _) -> recurse record
  | WT.EDict(_, entries, _, _, _) ->
    entries |> List.collect (fun (_, key, _, value) -> recurse key @ recurse value)
  | WT.EEnum(_, _, _, fields, _) -> fields |> List.collect recurse
  | WT.EPipe(_, first, parts) ->
    let pipeIssues part =
      match part with
      | WT.EPipeLambda(range, patterns, body, _, _) ->
        let required =
          if List.isEmpty patterns then
            [ issue range EmptyLambda "A lambda must have at least one parameter" ]
          else
            []
        required
        @ duplicateIssues (patterns |> List.collect letBindings)
        @ recurse body
      | WT.EPipeInfix(_, _, value) -> recurse value
      | WT.EPipeFnCall(_, _, _, args) -> args |> List.collect recurse
      | WT.EPipeEnum(_, _, _, fields, _) -> fields |> List.collect recurse
      | WT.EPipeVariableOrFnCall _ -> []
    recurse first @ (parts |> List.collect (snd >> pipeIssues))
  | WT.EUnit _
  | WT.EBool _
  | WT.EInt _
  | WT.EInt64 _
  | WT.EInt8 _
  | WT.EUInt8 _
  | WT.EInt16 _
  | WT.EUInt16 _
  | WT.EInt32 _
  | WT.EUInt32 _
  | WT.EUInt64 _
  | WT.EInt128 _
  | WT.EUInt128 _
  | WT.EFloat _
  | WT.EChar _
  | WT.EVariable _
  | WT.EFnName _ -> []

let rec private declarationStructureIssues
  (declaration : WT.Declaration)
  : List<Issue> =
  match declaration with
  | WT.DFunction fn ->
    duplicateIssues (
      fn.parameters
      |> List.choose (function
        | WT.FPNormal(_, name, _, _, _, _) -> Some(name.name, name.range)
        | WT.FPUnit _ -> None)
    )
    @ exprIssues fn.body
  | WT.DValue value -> exprIssues value.body
  | WT.DType _ -> []
  | WT.DModule modul -> modul.declarations |> List.collect declarationStructureIssues
  | WT.DExpr expr -> exprIssues expr
  | WT.DTypeDB typ ->
    match typ.definition with
    | WT.TDAlias _ -> []
    | _ -> [ issue typ.range DBShape "[<DB>] type must be a type alias" ]
  | WT.DTest test ->
    let expectedIssues =
      match test.expected with
      | WT.TEExpr expr -> exprIssues expr
      | WT.TEError _
      | WT.TESqlError _ -> []
    exprIssues test.actual @ expectedIssues

/// Check mode-independent invariants required by WrittenTypes lowering.
let validateStructure (sourceFile : WT.SourceFile) : List<Issue> =
  (sourceFile.declarations |> List.collect declarationStructureIssues)
  @ (sourceFile.exprsToEval |> List.collect exprIssues)

let rec private declarationPurposeIssues
  (mode : Mode)
  (declaration : WT.Declaration)
  : List<Issue> =
  match declaration with
  | WT.DFunction _
  | WT.DValue _
  | WT.DType _ -> []
  // WrittenTypes does not distinguish a file module header (`module A.B`),
  // which may be empty, from a block module (`module X =`), which may not. The
  // parser validates the block form while it still has that syntax detail.
  | WT.DModule modul ->
    modul.declarations |> List.collect (declarationPurposeIssues mode)
  | WT.DExpr expr ->
    match mode with
    | Package ->
      [ issue
          (WT.exprRange expr)
          PackageExpression
          "Expressions are not allowed in package files" ]
    | Test ->
      [ issue
          (WT.exprRange expr)
          TestAssertion
          "Test expressions must use 'actual = expected'" ]
    | Script -> []
  | WT.DTypeDB typ ->
    match mode with
    | Test -> []
    | _ ->
      [ issue typ.range DBMode "[<DB>] declarations are only allowed in test files" ]
  | WT.DTest test ->
    match mode with
    | Test -> []
    | _ ->
      [ issue test.range TestMode "Test assertions are only allowed in test files" ]

/// Check only the rules that depend on whether the source is a script, package,
/// or test file.
let validatePurpose (mode : Mode) (sourceFile : WT.SourceFile) : List<Issue> =
  let declarationIssues =
    sourceFile.declarations |> List.collect (declarationPurposeIssues mode)
  let trailingExpressionIssues =
    sourceFile.exprsToEval
    |> List.collect (fun expr ->
      match mode with
      | Package ->
        [ issue
            (WT.exprRange expr)
            PackageExpression
            "Expressions are not allowed in package files" ]
      | Test ->
        [ issue
            (WT.exprRange expr)
            TestAssertion
            "Test expressions must use 'actual = expected'" ]
      | Script -> [])
  declarationIssues @ trailingExpressionIssues

/// Validate every pre-lowering rule and return an opaque wrapper on success.
let validate
  (mode : Mode)
  (sourceFile : WT.SourceFile)
  : Result<ValidatedSourceFile, NEList<Issue>> =
  match validateStructure sourceFile @ validatePurpose mode sourceFile with
  | [] -> Ok(ValidatedSourceFile(mode, sourceFile))
  | first :: rest -> Error(NEList.ofList first rest)
