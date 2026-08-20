module Tests.AtRestTypeChecker

open Expecto
open Prelude

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Checker = LibExecution.AtRestTypeChecker
module AuthoringChecker = Builtins.Matter.Libs.PM.AtRestTypeChecker
module HashStabilization = LibDB.HashStabilization

open Tests.MultiInstanceHarness

let private parameter
  (name : string)
  (typ : PT.TypeReference)
  : PT.PackageFn.Parameter =
  { name = name; typ = typ; description = "" }

let private fn
  (parameters : NEList<PT.PackageFn.Parameter>)
  (returnType : PT.TypeReference)
  (body : PT.Expr)
  : PT.PackageFn.PackageFn =
  { hash = PT.Hash "function-under-test"
    typeParams = []
    parameters = parameters
    returnType = returnType
    body = body
    description = "" }

let private oneArgFn
  (parameterType : PT.TypeReference)
  (returnType : PT.TypeReference)
  (body : PT.Expr)
  : PT.PackageFn.PackageFn =
  fn (NEList.singleton (parameter "value" parameterType)) returnType body

let private expectChecked (verdict : Checker.Verdict) : unit =
  match verdict with
  | Checker.Checked _ -> ()
  | Checker.Failed report ->
    failtestf "Expected Checked, got Failed: %A" report.diagnostics
  | Checker.Incomplete report ->
    failtestf "Expected Checked, got Incomplete: %A" report.blockers

let private expectDiagnostic
  (code : Checker.DiagnosticCode)
  (verdict : Checker.Verdict)
  : unit =
  match verdict with
  | Checker.Failed report ->
    Expect.isTrue
      (report.diagnostics |> List.exists (fun diagnostic -> diagnostic.code = code))
      $"Expected diagnostic {code}, got {report.diagnostics}"
  | other -> failtestf "Expected Failed, got %A" other

let private expectBlocker
  (code : Checker.BlockerCode)
  (verdict : Checker.Verdict)
  : unit =
  match verdict with
  | Checker.Incomplete report ->
    Expect.isTrue
      (report.blockers |> List.exists (fun blocker -> blocker.code = code))
      $"Expected blocker {code}, got {report.blockers}"
  | other -> failtestf "Expected Incomplete, got %A" other

/// The real builtin registry, as the checker sees it.
let private builtinEnvironment () : Checker.TypeEnvironment =
  match
    Checker.TypeEnvironment.addBuiltins
      (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
      Checker.TypeEnvironment.empty
  with
  | Ok environment -> environment
  | Error errors -> failtestf "Could not construct builtin environment: %A" errors

let private verdictIsChecked (item : Checker.ItemVerdict) : bool =
  match item.verdict with
  | Checker.Checked _ -> true
  | _ -> false

let private enumType
  (hash : string)
  (cases : NEList<PT.TypeDeclaration.EnumCase>)
  : PT.FQTypeName.Package * PT.TypeDeclaration.T =
  let name = PT.FQTypeName.package hash
  name, { typeParams = []; definition = PT.TypeDeclaration.Enum cases }

let private enumCase
  (name : string)
  (fields : List<PT.TypeReference>)
  : PT.TypeDeclaration.EnumCase =
  { name = name
    fields =
      fields |> List.map (fun typ -> { typ = typ; label = None; description = "" })
    description = "" }

let private customType (name : PT.FQTypeName.Package) : PT.TypeReference =
  PT.TCustomType(PT.NameResolution.ok (PT.FQTypeName.Package name), [])

let private unitTests =
  testList
    "checker"
    [ test "checks a body against its declared return type" {
        oneArgFn PT.TInt PT.TInt (PT.EArg(1UL, 0))
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "function parameters are available by their source names" {
        oneArgFn PT.TInt PT.TInt (PT.EVariable(45UL, "value"))
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "reports a definite return type mismatch" {
        oneArgFn PT.TInt PT.TString (PT.EArg(2UL, 0))
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectDiagnostic Checker.TypeMismatch
      }

      test "checks enum constructor field count before runtime" {
        let optionName, declaration =
          enumType
            "option-int"
            (NEList.ofList
              (enumCase "None" [])
              [ enumCase "Some" [ PT.TTuple(PT.TInt, PT.TString, []) ] ])
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType optionName declaration
        let optionType = customType optionName
        let invalid =
          PT.EEnum(
            3UL,
            PT.NameResolution.ok (PT.FQTypeName.Package optionName),
            [],
            "Some",
            [ PT.EInt(4UL, 1I); PT.EString(5UL, [ PT.StringText "url" ]) ]
          )
        oneArgFn PT.TUnit optionType invalid
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.EnumFieldCountMismatch
      }

      test "accepts a tuple as one enum field" {
        let optionName, declaration =
          enumType
            "option-tuple"
            (NEList.ofList
              (enumCase "None" [])
              [ enumCase "Some" [ PT.TTuple(PT.TInt, PT.TString, []) ] ])
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType optionName declaration
        let value =
          PT.EEnum(
            6UL,
            PT.NameResolution.ok (PT.FQTypeName.Package optionName),
            [],
            "Some",
            [ PT.ETuple(
                7UL,
                PT.EInt(8UL, 1I),
                PT.EString(9UL, [ PT.StringText "url" ]),
                []
              ) ]
          )
        oneArgFn PT.TUnit (customType optionName) value
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "enum construction follows public type aliases" {
        let enumName, declaration =
          enumType "aliased-enum-target" (NEList.singleton (enumCase "Only" []))
        let aliasName = PT.FQTypeName.package "aliased-enum"
        let aliasDeclaration : PT.TypeDeclaration.T =
          { typeParams = []
            definition =
              PT.TypeDeclaration.Alias(
                PT.TCustomType(
                  PT.NameResolution.ok (PT.FQTypeName.Package enumName),
                  []
                )
              ) }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType enumName declaration
          |> Checker.TypeEnvironment.addType aliasName aliasDeclaration
        let value =
          PT.EEnum(
            46UL,
            PT.NameResolution.ok (PT.FQTypeName.Package aliasName),
            [],
            "Only",
            []
          )
        oneArgFn PT.TUnit (customType aliasName) value
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "wildcards do not create duplicate match bindings" {
        let body =
          PT.EMatch(
            47UL,
            PT.EArg(48UL, 0),
            [ { pat =
                  PT.MPTuple(
                    49UL,
                    PT.MPVariable(50UL, "_"),
                    PT.MPVariable(51UL, "_"),
                    []
                  )
                whenCondition = None
                rhs = PT.EInt(52UL, 1I) } ]
          )
        oneArgFn (PT.TTuple(PT.TInt, PT.TString, [])) PT.TInt body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "duplicate bindings inside an or-pattern alternative are rejected" {
        // `(x, x) | (x, x)`: each alternative binds `x` twice. The parser rejects
        // this, but serialized ops reach the checker without passing through it.
        let alternative (id : uint64) =
          PT.MPTuple(
            id,
            PT.MPVariable(id + 1UL, "x"),
            PT.MPVariable(id + 2UL, "x"),
            []
          )
        let body =
          PT.EMatch(
            47UL,
            PT.EArg(48UL, 0),
            [ { pat =
                  PT.MPOr(
                    49UL,
                    NEList.ofList (alternative 50UL) [ alternative 60UL ]
                  )
                whenCondition = None
                rhs = PT.EVariable(70UL, "x") } ]
          )
        oneArgFn (PT.TTuple(PT.TInt, PT.TInt, [])) PT.TInt body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectDiagnostic Checker.DuplicatePatternBinding
      }

      test "a missing dependency is incomplete, never checked" {
        let name = PT.FQFnName.fqBuiltIn "missing" 0
        let call =
          PT.EApply(
            10UL,
            PT.EFnName(11UL, PT.NameResolution.ok name),
            [],
            NEList.singleton (PT.EInt(12UL, 1I))
          )
        oneArgFn PT.TUnit PT.TInt call
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectBlocker Checker.MissingFunctionSignature
      }

      test "proves an unguarded bool match exhaustive" {
        let body =
          PT.EMatch(
            13UL,
            PT.EArg(14UL, 0),
            [ { pat = PT.MPBool(15UL, true)
                whenCondition = None
                rhs = PT.EString(16UL, [ PT.StringText "yes" ]) }
              { pat = PT.MPBool(17UL, false)
                whenCondition = None
                rhs = PT.EString(18UL, [ PT.StringText "no" ]) } ]
          )
        oneArgFn PT.TBool PT.TString body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "proves fixed-length and recursive list patterns exhaustive" {
        let wildcard nodeId = PT.MPVariable(nodeId, "_")
        let atLeastTwo =
          PT.MPListCons(
            133UL,
            wildcard 134UL,
            PT.MPListCons(135UL, wildcard 136UL, wildcard 137UL)
          )
        let body =
          PT.EMatch(
            138UL,
            PT.EArg(139UL, 0),
            [ { pat = PT.MPList(140UL, [])
                whenCondition = None
                rhs = PT.EInt(141UL, 0I) }
              { pat = PT.MPList(142UL, [ wildcard 143UL ])
                whenCondition = None
                rhs = PT.EInt(144UL, 1I) }
              { pat = atLeastTwo; whenCondition = None; rhs = PT.EInt(145UL, 2I) } ]
          )
        oneArgFn (PT.TList PT.TString) PT.TInt body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "proves exhaustive combinations of two lists" {
        let empty nodeId = PT.MPList(nodeId, [])
        let nonEmpty nodeId =
          PT.MPListCons(
            nodeId,
            PT.MPVariable(nodeId + 1UL, "_"),
            PT.MPVariable(nodeId + 2UL, "_")
          )
        let pair nodeId left right = PT.MPTuple(nodeId, left, right, [])
        let case nodeId pattern (value : int) : PT.MatchCase =
          { pat = pattern
            whenCondition = None
            rhs = PT.EInt(nodeId, bigint value) }
        let body =
          PT.EMatch(
            146UL,
            PT.EArg(147UL, 0),
            [ case 148UL (pair 149UL (empty 150UL) (empty 151UL)) 0
              case 152UL (pair 153UL (nonEmpty 154UL) (empty 157UL)) 1
              case 158UL (pair 159UL (empty 160UL) (nonEmpty 161UL)) 2
              case 164UL (pair 165UL (nonEmpty 166UL) (nonEmpty 169UL)) 3 ]
          )
        oneArgFn
          (PT.TTuple(PT.TList PT.TString, PT.TList PT.TString, []))
          PT.TInt
          body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "does not treat restrictive enum fields as exhaustive" {
        let enumName, declaration =
          enumType
            "enum-with-payload"
            (NEList.ofList (enumCase "A" [ PT.TBool ]) [ enumCase "B" [] ])
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType enumName declaration
        let body =
          PT.EMatch(
            32UL,
            PT.EArg(33UL, 0),
            [ { pat = PT.MPEnum(34UL, "A", [ PT.MPBool(35UL, true) ])
                whenCondition = None
                rhs = PT.EInt(36UL, 1I) }
              { pat = PT.MPEnum(37UL, "B", [])
                whenCondition = None
                rhs = PT.EInt(38UL, 2I) } ]
          )
        match
          oneArgFn (customType enumName) PT.TInt body
          |> Checker.checkPackageFunction environment
        with
        | Checker.Incomplete report ->
          let blocker =
            report.blockers
            |> List.tryFind (fun blocker ->
              blocker.code = Checker.NonExhaustiveMatch)
            |> Option.defaultWith (fun () ->
              failtest "missing exhaustiveness blocker")
          Expect.equal
            blocker.context
            "Match is not exhaustive; an uncovered pattern is A(false)"
            "the checker identifies a concrete uncovered pattern"
        | verdict -> failtestf "Expected Incomplete, got %A" verdict
      }

      test "an unresolved match input does not create ambiguity cascades" {
        let unresolvedValue : PT.NameResolution<PT.FQValueName.FQValueName> =
          { originalName = [ "Missing"; "value" ]
            resolved = Error PT.NameResolutionError.NotFound }
        let body =
          PT.EMatch(
            170UL,
            PT.EValue(171UL, unresolvedValue),
            [ { pat = PT.MPEnum(172UL, "Some", [ PT.MPVariable(173UL, "value") ])
                whenCondition = None
                rhs = PT.EVariable(174UL, "value") } ]
          )
        match
          oneArgFn PT.TUnit PT.TInt body
          |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        with
        | Checker.Incomplete report ->
          Expect.equal
            (report.blockers |> List.map _.code)
            [ Checker.UnresolvedValueName ]
            "only the unresolved value is reported"
        | verdict -> failtestf "Expected Incomplete, got %A" verdict
      }

      test "allows an unconstrained type confined to a discarded value" {
        let body =
          PT.ELet(19UL, PT.LPWildcard 20UL, PT.EList(21UL, []), PT.EInt(22UL, 1I))
        oneArgFn PT.TUnit PT.TInt body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "irrelevant inference variables do not obscure a definite diagnostic" {
        let body =
          PT.ELet(
            53UL,
            PT.LPWildcard 54UL,
            PT.EList(55UL, []),
            PT.EString(56UL, [ PT.StringText "wrong" ])
          )
        oneArgFn PT.TUnit PT.TInt body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectDiagnostic Checker.TypeMismatch
      }

      test "a diagnostic involving an unknown type remains incomplete" {
        let body = PT.EList(64UL, [])
        match
          oneArgFn PT.TUnit PT.TBlob body
          |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        with
        | Checker.Incomplete report ->
          Expect.isTrue
            (report.diagnostics
             |> List.exists (fun diagnostic ->
               diagnostic.code = Checker.TypeMismatch))
            "the provisional mismatch is retained"
          Expect.isTrue
            (report.blockers
             |> List.exists (fun blocker -> blocker.code = Checker.AmbiguousType))
            "an unknown type prevents treating the mismatch as definite"
        | verdict -> failtestf "Expected Incomplete, got %A" verdict
      }

      test "a definite diagnostic is not hidden by an unrelated blocker" {
        let body =
          PT.EMatch(
            175UL,
            PT.EArg(176UL, 0),
            [ { pat = PT.MPBool(177UL, true)
                whenCondition = None
                rhs = PT.EInt(178UL, 1I) } ]
          )
        match
          oneArgFn PT.TBool PT.TString body
          |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        with
        | Checker.Failed report ->
          Expect.isTrue
            (report.diagnostics
             |> List.exists (fun diagnostic ->
               diagnostic.code = Checker.TypeMismatch))
            "the concrete return mismatch remains definite"
          Expect.isTrue
            (report.blockers
             |> List.exists (fun blocker ->
               blocker.code = Checker.NonExhaustiveMatch))
            "the incomplete exhaustiveness proof is retained"
        | verdict -> failtestf "Expected Failed, got %A" verdict
      }

      test "allows an unknown generic field when only a concrete field is observed" {
        let foundValueName = PT.FQTypeName.package "phantom-found-value"
        let foundValueDeclaration : PT.TypeDeclaration.T =
          { typeParams = [ "a" ]
            definition =
              PT.TypeDeclaration.Record(
                NEList.ofList
                  { name = "path"; typ = PT.TString; description = "" }
                  [ { name = "value"; typ = PT.TVariable "a"; description = "" } ]
              ) }
        let foundValue typ =
          PT.TCustomType(
            PT.NameResolution.ok (PT.FQTypeName.Package foundValueName),
            [ typ ]
          )
        let sourceName = PT.FQFnName.fqBuiltIn "phantomSource" 0
        let sourceSignature : Checker.FunctionSignature =
          { typeParams = [ "a" ]
            parameters = NEList.singleton PT.TUnit
            returnType = foundValue (PT.TVariable "a") }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType foundValueName foundValueDeclaration
          |> Checker.TypeEnvironment.addFunction sourceName sourceSignature
        let sourceCall =
          PT.EApply(
            57UL,
            PT.EFnName(58UL, PT.NameResolution.ok sourceName),
            [],
            NEList.singleton (PT.EUnit 59UL)
          )
        let body =
          PT.ELet(
            60UL,
            PT.LPVariable(61UL, "discovered"),
            sourceCall,
            PT.ERecordFieldAccess(62UL, PT.EVariable(63UL, "discovered"), "path")
          )
        oneArgFn PT.TUnit PT.TString body
        |> Checker.checkPackageFunction environment
        |> expectChecked

        sourceCall
        |> Checker.checkExpression environment
        |> expectBlocker Checker.AmbiguousType
      }

      test "keeps unresolved field constraints incomplete" {
        PT.ELambda(
          64UL,
          NEList.singleton (PT.LPVariable(65UL, "record")),
          PT.ERecordFieldAccess(66UL, PT.EVariable(67UL, "record"), "path")
        )
        |> Checker.checkExpression Checker.TypeEnvironment.empty
        |> expectBlocker Checker.AmbiguousType
      }

      test "detects aliases that form a cycle" {
        let a = PT.FQTypeName.package "alias-a"
        let b = PT.FQTypeName.package "alias-b"
        let alias target : PT.TypeDeclaration.T =
          { typeParams = []
            definition =
              PT.TypeDeclaration.Alias(
                PT.TCustomType(
                  PT.NameResolution.ok (PT.FQTypeName.Package target),
                  []
                )
              ) }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType a (alias b)
          |> Checker.TypeEnvironment.addType b (alias a)
        let typ = customType a
        oneArgFn typ typ (PT.EArg(23UL, 0))
        |> Checker.checkPackageFunction environment
        |> expectBlocker Checker.AliasCycle
      }

      test "detects alias cycles through containers" {
        let aliasName = PT.FQTypeName.package "structurally-recursive-alias"
        let declaration : PT.TypeDeclaration.T =
          { typeParams = []
            definition = PT.TypeDeclaration.Alias(PT.TList(customType aliasName)) }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType aliasName declaration
        oneArgFn (customType aliasName) PT.TUnit (PT.EUnit 183UL)
        |> Checker.checkPackageFunction environment
        |> expectBlocker Checker.AliasCycle
      }

      test "allows finite nested uses of the same alias" {
        let aliasName = PT.FQTypeName.package "finite-nested-alias"
        let declaration : PT.TypeDeclaration.T =
          { typeParams = [ "a" ]
            definition = PT.TypeDeclaration.Alias(PT.TList(PT.TVariable "a")) }
        let aliasOf typ =
          PT.TCustomType(
            PT.NameResolution.ok (PT.FQTypeName.Package aliasName),
            [ typ ]
          )
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType aliasName declaration
        oneArgFn (aliasOf (aliasOf PT.TInt)) PT.TUnit (PT.EUnit 184UL)
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "nested missing type dependencies keep signatures incomplete" {
        let missing = PT.FQTypeName.package "nested-missing-type"
        let nested = PT.TList(PT.TDict(customType missing))
        oneArgFn nested PT.TUnit (PT.EUnit 180UL)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectBlocker Checker.MissingTypeDeclaration
      }

      test "valid recursive record dependencies remain checked" {
        let nodeName = PT.FQTypeName.package "recursive-record-node"
        let declaration : PT.TypeDeclaration.T =
          { typeParams = []
            definition =
              PT.TypeDeclaration.Record(
                NEList.singleton
                  { name = "children"
                    typ = PT.TList(customType nodeName)
                    description = "" }
              ) }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType nodeName declaration
        oneArgFn (customType nodeName) PT.TUnit (PT.EUnit 187UL)
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "validates custom type arity inside nested signatures" {
        let pairName = PT.FQTypeName.package "nested-pair"
        let pairDeclaration : PT.TypeDeclaration.T =
          { typeParams = [ "a"; "b" ]
            definition =
              PT.TypeDeclaration.Record(
                NEList.ofList
                  { name = "first"; typ = PT.TVariable "a"; description = "" }
                  [ { name = "second"; typ = PT.TVariable "b"; description = "" } ]
              ) }
        let pair args =
          PT.TCustomType(PT.NameResolution.ok (PT.FQTypeName.Package pairName), args)
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType pairName pairDeclaration

        oneArgFn
          (PT.TTuple(PT.TInt, PT.TList(pair [ PT.TString ]), []))
          PT.TUnit
          (PT.EUnit 181UL)
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.TypeMismatch

        oneArgFn
          (PT.TTuple(PT.TInt, PT.TList(pair [ PT.TString; PT.TBool ]), []))
          PT.TUnit
          (PT.EUnit 182UL)
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "malformed custom type arity cannot crash field substitution" {
        let pairHash = PT.Hash "malformed-arity-pair"
        let pairName = PT.FQTypeName.package "malformed-arity-pair"
        let pairDeclaration : PT.TypeDeclaration.T =
          { typeParams = [ "a"; "b" ]
            definition =
              PT.TypeDeclaration.Record(
                NEList.ofList
                  { name = "first"; typ = PT.TVariable "a"; description = "" }
                  [ { name = "second"; typ = PT.TVariable "b"; description = "" } ]
              ) }
        let pairType : PT.PackageType.PackageType =
          { hash = pairHash; description = ""; declaration = pairDeclaration }
        let malformedType =
          PT.TCustomType(
            PT.NameResolution.ok (PT.FQTypeName.Package pairName),
            [ PT.TInt ]
          )
        let malformedFn =
          { oneArgFn
              malformedType
              PT.TInt
              (PT.ERecordFieldAccess(185UL, PT.EArg(186UL, 0), "first")) with
              hash = PT.Hash "malformed-arity-field-access" }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType pairName pairDeclaration

        malformedFn
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.TypeMismatch

        let report =
          AuthoringChecker.checkPackageOps
            PT.PackageManager.empty
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            [ PT.PackageOp.AddType pairType; PT.PackageOp.AddFn malformedFn ]
          |> _.Result
        Expect.equal
          report.verdict
          AuthoringChecker.Failed
          "the authoring boundary preserves the definite arity failure"
        let functionReport =
          report.items
          |> List.find (fun item ->
            item.item = PT.Reference.PackageFn malformedFn.hash)
          |> Option.defaultWith (fun () ->
            failtest "Missing malformed function report")
        Expect.equal
          functionReport.verdict
          AuthoringChecker.Failed
          "the malformed function itself remains failed"
        Expect.isTrue
          (functionReport.diagnostics
           |> List.exists (fun diagnostic -> diagnostic.code = Checker.TypeMismatch))
          "the arity diagnostic reaches the authoring report"
      }

      test "type aliases validate nested dependencies" {
        let missing = PT.FQTypeName.package "alias-nested-missing"
        let alias : PT.PackageType.PackageType =
          { hash = PT.Hash "alias-with-nested-missing"
            description = ""
            declaration =
              { typeParams = []
                definition =
                  PT.TypeDeclaration.Alias(
                    PT.TTuple(PT.TInt, PT.TList(customType missing), [])
                  ) } }
        let result =
          Checker.checkPackageBatch Checker.TypeEnvironment.empty [ alias ] [] []
        match result.types with
        | [ item ] -> expectBlocker Checker.MissingTypeDeclaration item.verdict
        | items -> failtestf "Expected one type verdict, got %A" items
      }

      test "batch checking infers package values in dependency order" {
        let firstHash = PT.Hash "first-value"
        let secondHash = PT.Hash "second-value"
        let first : PT.PackageValue.PackageValue =
          { hash = firstHash
            description = ""
            body =
              PT.EValue(
                24UL,
                PT.NameResolution.ok (PT.FQValueName.Package secondHash)
              ) }
        let second : PT.PackageValue.PackageValue =
          { hash = secondHash
            description = ""
            body = PT.EString(25UL, [ PT.StringText "ready" ]) }
        let result =
          Checker.checkPackageBatch
            Checker.TypeEnvironment.empty
            []
            [ first; second ]
            []
        Expect.equal result.values.Length 2 "both values have verdicts"
        Expect.isTrue
          (result.values |> List.forall verdictIsChecked)
          "a forward value dependency is inferred after its dependency"
      }

      test "recursive package values remain incomplete" {
        let firstHash = PT.Hash "recursive-first"
        let secondHash = PT.Hash "recursive-second"
        let value hash target nodeId : PT.PackageValue.PackageValue =
          { hash = hash
            description = ""
            body =
              PT.EValue(nodeId, PT.NameResolution.ok (PT.FQValueName.Package target)) }
        let result =
          Checker.checkPackageBatch
            Checker.TypeEnvironment.empty
            []
            [ value firstHash secondHash 26UL; value secondHash firstHash 27UL ]
            []
        Expect.isTrue
          (result.values
           |> List.forall (fun item ->
             match item.verdict with
             | Checker.Incomplete _ -> true
             | _ -> false))
          "recursive values have no declared type and cannot be proven"
      }

      test "batch validation rejects duplicate record fields" {
        let typ : PT.PackageType.PackageType =
          { hash = PT.Hash "duplicate-record-fields"
            description = ""
            declaration =
              { typeParams = []
                definition =
                  PT.TypeDeclaration.Record(
                    NEList.ofList
                      { name = "field"; typ = PT.TInt; description = "" }
                      [ { name = "field"; typ = PT.TString; description = "" } ]
                  ) } }
        let result =
          Checker.checkPackageBatch Checker.TypeEnvironment.empty [ typ ] [] []
        match result.types with
        | [ item ] -> expectDiagnostic Checker.DuplicateTypeMember item.verdict
        | items -> failtestf "Expected one type verdict, got %A" items
      }

      test "imports builtin signatures without executable bodies" {
        let environment =
          match
            Checker.TypeEnvironment.addBuiltins
              (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
              Checker.TypeEnvironment.empty
          with
          | Ok environment -> environment
          | Error errors ->
            failtestf "Could not construct builtin environment: %A" errors
        let add = PT.FQFnName.fqBuiltIn "int64Add" 0
        let body =
          PT.EApply(
            28UL,
            PT.EFnName(29UL, PT.NameResolution.ok add),
            [],
            NEList.ofList (PT.EInt64(30UL, 1L)) [ PT.EInt64(31UL, 2L) ]
          )
        oneArgFn PT.TUnit PT.TInt64 body
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "operator builtins called by name are checked as the operator" {
        // `Builtin.lessThan` declares independent `'a`/`'b` parameters but is what
        // `<` lowers to, and raises at runtime unless both are the same numeric
        // type. Trusting the signature would prove `lessThan 1 "x" : Bool`; the
        // checker applies the operator's rule instead.
        let environment = builtinEnvironment ()
        let call name lhs rhs =
          PT.EApply(
            28UL,
            PT.EFnName(29UL, PT.NameResolution.ok (PT.FQFnName.fqBuiltIn name 0)),
            [],
            NEList.ofList lhs [ rhs ]
          )
        let one = PT.EInt(30UL, 1I)
        let two = PT.EInt(31UL, 2I)
        let text = PT.EString(32UL, [ PT.StringText "x" ])

        oneArgFn PT.TUnit PT.TBool (call "lessThan" one text)
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.TypeMismatch

        oneArgFn PT.TUnit PT.TInt (call "add" one two)
        |> Checker.checkPackageFunction environment
        |> expectChecked

        oneArgFn PT.TUnit PT.TString (call "add" one two)
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.TypeMismatch

        oneArgFn PT.TUnit PT.TBool (call "greaterThan" text text)
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.InvalidInfixOperand

        oneArgFn PT.TUnit PT.TBool (call "equals" text text)
        |> Checker.checkPackageFunction environment
        |> expectChecked

        // As a value or partially applied there is no signature to give it.
        oneArgFn
          PT.TUnit
          PT.TInt
          (PT.EApply(
            33UL,
            PT.EFnName(34UL, PT.NameResolution.ok (PT.FQFnName.fqBuiltIn "add" 0)),
            [],
            NEList.singleton one
          ))
        |> Checker.checkPackageFunction environment
        |> expectBlocker Checker.UnsupportedConstruct

        // In a pipe the input is the left operand.
        oneArgFn
          PT.TInt
          PT.TInt
          (PT.EPipe(
            35UL,
            PT.EArg(36UL, 0),
            [ PT.EPipeFnCall(
                37UL,
                PT.NameResolution.ok (PT.FQFnName.fqBuiltIn "subtract" 0),
                [],
                [ one ]
              ) ]
          ))
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "exponentiation rejects Int128 and UInt128 operands" {
        let infix nodeId operation =
          PT.EInfix(
            nodeId,
            PT.InfixFnCall operation,
            PT.EArg(nodeId + 1UL, 0),
            PT.EArg(nodeId + 2UL, 0)
          )

        oneArgFn PT.TInt128 PT.TInt128 (infix 187UL PT.ArithmeticPower)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectDiagnostic Checker.InvalidInfixOperand

        oneArgFn PT.TUInt128 PT.TUInt128 (infix 190UL PT.ArithmeticPower)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectDiagnostic Checker.InvalidInfixOperand

        oneArgFn PT.TInt128 PT.TInt128 (infix 193UL PT.ArithmeticPlus)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked

        oneArgFn PT.TUInt128 PT.TUInt128 (infix 196UL PT.ArithmeticMultiply)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "pipeline and by-name power use the same restricted domain" {
        let pipeline operation =
          PT.EPipe(
            199UL,
            PT.EArg(200UL, 0),
            [ PT.EPipeInfix(201UL, PT.InfixFnCall operation, PT.EArg(202UL, 0)) ]
          )

        oneArgFn PT.TInt128 PT.TInt128 (pipeline PT.ArithmeticPower)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectDiagnostic Checker.InvalidInfixOperand

        oneArgFn PT.TInt128 PT.TInt128 (pipeline PT.ArithmeticPlus)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked

        let builtinPower =
          PT.EApply(
            203UL,
            PT.EFnName(204UL, PT.NameResolution.ok (PT.FQFnName.fqBuiltIn "power" 0)),
            [],
            NEList.ofList (PT.EArg(205UL, 0)) [ PT.EArg(206UL, 0) ]
          )
        oneArgFn PT.TUInt128 PT.TUInt128 builtinPower
        |> Checker.checkPackageFunction (builtinEnvironment ())
        |> expectDiagnostic Checker.InvalidInfixOperand
      }

      test "unary minus is checked as negation" {
        // The parser lowers `-x` on a non-literal to `Builtin.negate`, whose
        // declared `'a -> 'a` would accept anything.
        let environment = builtinEnvironment ()
        let negate =
          PT.EApply(
            40UL,
            PT.EFnName(41UL, PT.NameResolution.ok (PT.FQFnName.fqBuiltIn "negate" 0)),
            [],
            NEList.singleton (PT.EArg(42UL, 0))
          )
        oneArgFn PT.TInt PT.TInt negate
        |> Checker.checkPackageFunction environment
        |> expectChecked
        oneArgFn PT.TFloat PT.TFloat negate
        |> Checker.checkPackageFunction environment
        |> expectChecked
        oneArgFn PT.TInt PT.TString negate
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.TypeMismatch
        oneArgFn PT.TUInt8 PT.TUInt8 negate
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.InvalidInfixOperand
        oneArgFn PT.TString PT.TString negate
        |> Checker.checkPackageFunction environment
        |> expectDiagnostic Checker.InvalidInfixOperand
      }

      test "builtin checkability follows the signature" {
        let allBuiltins = TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT
        let addFn = allBuiltins.fns[RT.FQFnName.builtin "add" 0]
        let call name =
          PT.EApply(
            133UL,
            PT.EFnName(134UL, PT.NameResolution.ok name),
            [],
            NEList.ofList (PT.EInt(135UL, 1I)) [ PT.EInt(136UL, 2I) ]
          )
        let environmentFor builtins =
          match
            Checker.TypeEnvironment.addBuiltins
              builtins
              Checker.TypeEnvironment.empty
          with
          | Ok environment -> environment
          | Error errors ->
            failtestf "Could not construct builtin environment: %A" errors

        // The old checker special-cased this name. A normal signature under the
        // same name is now trusted because policy, not spelling, controls it.
        let declaredRTName = RT.FQFnName.builtin "unwrap" 0
        let declaredBuiltins =
          LibExecution.Builtin.make [] [ { addFn with name = declaredRTName } ]
        oneArgFn PT.TUnit PT.TInt (call (PT.FQFnName.fqBuiltIn "unwrap" 0))
        |> Checker.checkPackageFunction (environmentFor declaredBuiltins)
        |> expectChecked

        let unconstrainedRTName = RT.FQFnName.builtin "unconstrainedResult" 0
        let unconstrainedBuiltins =
          LibExecution.Builtin.make
            []
            [ { addFn with
                  name = unconstrainedRTName
                  typeParams = [ "result" ]
                  returnType = RT.TVariable "result" } ]
        // A result-only variable is not a universal result: it must not be trusted.
        oneArgFn
          PT.TUnit
          PT.TInt
          (call (PT.FQFnName.fqBuiltIn "unconstrainedResult" 0))
        |> Checker.checkPackageFunction (environmentFor unconstrainedBuiltins)
        |> expectBlocker Checker.UnsupportedConstruct

        // The real `unwrap : optOrRes -> 'a` has a result-only variable, so it is
        // not trusted even though nothing marks it by name.
        oneArgFn
          PT.TUnit
          PT.TInt
          (PT.EApply(
            137UL,
            PT.EFnName(
              138UL,
              PT.NameResolution.ok (PT.FQFnName.fqBuiltIn "unwrap" 0)
            ),
            [],
            NEList.singleton (PT.EInt(139UL, 1I))
          ))
        |> Checker.checkPackageFunction (environmentFor allBuiltins)
        |> expectBlocker Checker.UnsupportedConstruct
      }

      test "explicit type arguments do not create phantom inference variables" {
        let identity = PT.FQFnName.fqBuiltIn "identityForCheckerTest" 0
        let signature : Checker.FunctionSignature =
          { typeParams = [ "a" ]
            parameters = NEList.singleton (PT.TVariable "a")
            returnType = PT.TVariable "a" }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addFunction identity signature
        let body =
          PT.EApply(
            60UL,
            PT.EFnName(61UL, PT.NameResolution.ok identity),
            [ PT.TInt ],
            NEList.singleton (PT.EInt(62UL, 1I))
          )
        oneArgFn PT.TUnit PT.TInt body
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "explicit type arguments resolve enclosing function parameters" {
        let identity = PT.FQFnName.fqBuiltIn "genericIdentityForCheckerTest" 0
        let signature : Checker.FunctionSignature =
          { typeParams = [ "value" ]
            parameters = NEList.singleton (PT.TVariable "value")
            returnType = PT.TVariable "value" }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addFunction identity signature
        let body =
          PT.EApply(
            128UL,
            PT.EFnName(129UL, PT.NameResolution.ok identity),
            [ PT.TVariable "a" ],
            NEList.singleton (PT.EArg(130UL, 0))
          )
        { oneArgFn (PT.TVariable "a") (PT.TVariable "a") body with
            typeParams = [ "a" ] }
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "custom type arguments resolve enclosing function parameters" {
        let boxName = PT.FQTypeName.package "generic-box"
        let declaration : PT.TypeDeclaration.T =
          { typeParams = [ "item" ]
            definition =
              PT.TypeDeclaration.Enum(
                NEList.singleton (enumCase "Box" [ PT.TVariable "item" ])
              ) }
        let boxOfA =
          PT.TCustomType(
            PT.NameResolution.ok (PT.FQTypeName.Package boxName),
            [ PT.TVariable "a" ]
          )
        let body =
          PT.EEnum(
            131UL,
            PT.NameResolution.ok (PT.FQTypeName.Package boxName),
            [ PT.TVariable "a" ],
            "Box",
            [ PT.EArg(132UL, 0) ]
          )
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType boxName declaration
        { oneArgFn (PT.TVariable "a") boxOfA body with typeParams = [ "a" ] }
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "generalizes immutable package values" {
        let emptyHash = PT.Hash "polymorphic-empty-list"
        let empty : PT.PackageValue.PackageValue =
          { hash = emptyHash; description = ""; body = PT.EList(63UL, []) }
        let useEmpty nodeId returnType : PT.PackageFn.PackageFn =
          oneArgFn
            PT.TUnit
            returnType
            (PT.EValue(
              nodeId,
              PT.NameResolution.ok (PT.FQValueName.Package emptyHash)
            ))
        let result =
          Checker.checkPackageBatch
            Checker.TypeEnvironment.empty
            []
            [ empty ]
            [ useEmpty 64UL (PT.TList PT.TInt); useEmpty 65UL (PT.TList PT.TString) ]
        Expect.isTrue
          (result.values |> List.forall verdictIsChecked)
          "the empty value itself has a polymorphic proof"
        Expect.isTrue
          (result.functions |> List.forall verdictIsChecked)
          "each reference receives a fresh instantiation"
      }

      test "generalizes let-bound lambdas" {
        let identity =
          PT.ELambda(
            66UL,
            NEList.singleton (PT.LPVariable(67UL, "x")),
            PT.EVariable(68UL, "x")
          )
        let apply nodeId arg =
          PT.EApply(
            nodeId,
            PT.EVariable(nodeId + 1UL, "identity"),
            [],
            NEList.singleton arg
          )
        let body =
          PT.ELet(
            69UL,
            PT.LPVariable(70UL, "identity"),
            identity,
            PT.ETuple(
              71UL,
              apply 72UL (PT.EInt(74UL, 1I)),
              apply 75UL (PT.EString(77UL, [ PT.StringText "ok" ])),
              []
            )
          )
        oneArgFn PT.TUnit (PT.TTuple(PT.TInt, PT.TString, [])) body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "keeps bindings from an invalid match pattern in scope" {
        let enumName, declaration =
          enumType "pattern-recovery" (NEList.singleton (enumCase "Valid" []))
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType enumName declaration
        let body =
          PT.EMatch(
            78UL,
            PT.EArg(79UL, 0),
            [ { pat = PT.MPEnum(80UL, "Missing", [ PT.MPVariable(81UL, "x") ])
                whenCondition = None
                rhs = PT.EVariable(82UL, "x") } ]
          )
        let verdict =
          oneArgFn (customType enumName) PT.TInt body
          |> Checker.checkPackageFunction environment
        match verdict with
        | Checker.Incomplete report
        | Checker.Failed report ->
          Expect.isFalse
            (report.diagnostics
             |> List.exists (fun diagnostic ->
               diagnostic.code = Checker.UnknownVariable))
            "pattern errors must not create cascading scope errors"
          Expect.isFalse
            (report.blockers
             |> List.exists (fun blocker ->
               blocker.code = Checker.NonExhaustiveMatch))
            "an invalid pattern must not create an exhaustiveness cascade"
        | Checker.Checked _ -> failtest "the invalid enum case must be rejected"
      }

      test "defers record field constraints until lambda calls are known" {
        let recordName = PT.FQTypeName.package "deferred-record-field"
        let declaration : PT.TypeDeclaration.T =
          { typeParams = []
            definition =
              PT.TypeDeclaration.Record(
                NEList.singleton { name = "count"; typ = PT.TInt; description = "" }
              ) }
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType recordName declaration
        let helper =
          PT.ELambda(
            83UL,
            NEList.singleton (PT.LPVariable(84UL, "record")),
            PT.ERecordFieldAccess(85UL, PT.EVariable(86UL, "record"), "count")
          )
        let body =
          PT.ELet(
            87UL,
            PT.LPVariable(88UL, "getCount"),
            helper,
            PT.EApply(
              89UL,
              PT.EVariable(90UL, "getCount"),
              [],
              NEList.singleton (PT.EArg(91UL, 0))
            )
          )
        oneArgFn (customType recordName) PT.TInt body
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "generalizes field constraints for reusable local helpers" {
        let makeRecord name fieldType =
          let typeName = PT.FQTypeName.package name
          let declaration : PT.TypeDeclaration.T =
            { typeParams = []
              definition =
                PT.TypeDeclaration.Record(
                  NEList.singleton
                    { name = "value"; typ = fieldType; description = "" }
                ) }
          typeName, declaration
        let intRecord, intDeclaration = makeRecord "row-int" PT.TInt
        let stringRecord, stringDeclaration = makeRecord "row-string" PT.TString
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType intRecord intDeclaration
          |> Checker.TypeEnvironment.addType stringRecord stringDeclaration
        let helper =
          PT.ELambda(
            101UL,
            NEList.singleton (PT.LPVariable(102UL, "record")),
            PT.ERecordFieldAccess(103UL, PT.EVariable(104UL, "record"), "value")
          )
        let apply nodeId argument =
          PT.EApply(
            nodeId,
            PT.EVariable(nodeId + 1UL, "getValue"),
            [],
            NEList.singleton argument
          )
        let body =
          PT.ELet(
            105UL,
            PT.LPVariable(106UL, "getValue"),
            helper,
            PT.ETuple(
              107UL,
              apply 108UL (PT.EArg(110UL, 0)),
              apply 111UL (PT.EArg(113UL, 1)),
              []
            )
          )
        fn
          (NEList.ofList
            (parameter "intRecord" (customType intRecord))
            [ parameter "stringRecord" (customType stringRecord) ])
          (PT.TTuple(PT.TInt, PT.TString, []))
          body
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "proves nested enum patterns exhaustive" {
        let innerName, innerDeclaration =
          enumType
            "nested-inner"
            (NEList.singleton (enumCase "SourceFile" [ PT.TString ]))
        let outerName, outerDeclaration =
          enumType
            "nested-outer"
            (NEList.ofList
              (enumCase "None" [])
              [ enumCase "Some" [ customType innerName ] ])
        let environment =
          Checker.TypeEnvironment.empty
          |> Checker.TypeEnvironment.addType innerName innerDeclaration
          |> Checker.TypeEnvironment.addType outerName outerDeclaration
        let body =
          PT.EMatch(
            92UL,
            PT.EArg(93UL, 0),
            [ { pat = PT.MPEnum(94UL, "None", [])
                whenCondition = None
                rhs = PT.EInt(95UL, 0I) }
              { pat =
                  PT.MPEnum(
                    96UL,
                    "Some",
                    [ PT.MPEnum(97UL, "SourceFile", [ PT.MPVariable(98UL, "_") ]) ]
                  )
                whenCondition = None
                rhs = PT.EInt(99UL, 1I) } ]
          )
        oneArgFn (customType outerName) PT.TInt body
        |> Checker.checkPackageFunction environment
        |> expectChecked
      }

      test "proves tuple patterns exhaustive" {
        let tuplePattern nodeId first =
          PT.MPTuple(
            nodeId,
            PT.MPBool(nodeId + 1UL, first),
            PT.MPVariable(nodeId + 2UL, "_"),
            []
          )
        let body =
          PT.EMatch(
            118UL,
            PT.EArg(119UL, 0),
            [ { pat = tuplePattern 120UL true
                whenCondition = None
                rhs = PT.EInt(123UL, 1I) }
              { pat = tuplePattern 124UL false
                whenCondition = None
                rhs = PT.EInt(127UL, 0I) } ]
          )
        oneArgFn (PT.TTuple(PT.TBool, PT.TBool, [])) PT.TInt body
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectChecked
      }

      test "ambiguous type blockers identify the source expression" {
        match
          Checker.checkExpression Checker.TypeEnvironment.empty (PT.EList(100UL, []))
        with
        | Checker.Incomplete report ->
          let blocker =
            report.blockers
            |> List.tryFind (fun blocker -> blocker.code = Checker.AmbiguousType)
          Expect.equal
            (blocker |> Option.bind (fun blocker -> blocker.nodeId))
            (Some 100UL)
            "the ambiguity is located"
        | verdict -> failtestf "Expected Incomplete, got %A" verdict
      }

      test "authoring adapter loads only the resolved dependency closure" {
        let payloadHash = PT.Hash "adapter-payload"
        let payloadType : PT.PackageType.PackageType =
          { hash = payloadHash
            description = ""
            declaration =
              { typeParams = []
                definition =
                  PT.TypeDeclaration.Enum(
                    NEList.singleton (enumCase "Payload" [ PT.TInt ])
                  ) } }
        let payloadRef = customType payloadHash
        let identityHash = PT.Hash "adapter-identity"
        let identity =
          { fn
              (NEList.singleton (parameter "payload" payloadRef))
              payloadRef
              (PT.EArg(39UL, 0)) with
              hash = identityHash }
        let candidateHash = PT.Hash "adapter-candidate"
        let candidate =
          { fn
              (NEList.singleton (parameter "payload" payloadRef))
              payloadRef
              (PT.EApply(
                40UL,
                PT.EFnName(
                  41UL,
                  PT.NameResolution.ok (PT.FQFnName.Package identityHash)
                ),
                [],
                NEList.singleton (PT.EArg(42UL, 0))
              )) with
              hash = candidateHash }
        let location name : PT.PackageLocation =
          { owner = "Test"; modules = [ "AtRest" ]; name = name }
        let pm =
          PT.PackageManager.empty
          |> PT.PackageManager.withExtras
            [ payloadType, location "Payload" ]
            []
            [ identity, location "identity" ]
        let report =
          AuthoringChecker.checkPackageOps
            pm
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            [ PT.PackageOp.AddFn candidate ]
          |> _.Result
        Expect.equal
          report.verdict
          AuthoringChecker.Checked
          "the adapter loads the referenced function signature and its custom type"

        Expect.equal report.items.Length 1 "the candidate has an item report"
        Expect.equal
          report.items.Head.item
          (PT.Reference.PackageFn candidateHash)
          "the item report retains the candidate identity"
      }

      test "authoring adapter validates fields of trusted dependency types" {
        let missingName = PT.FQTypeName.package "trusted-box-missing"
        let boxHash = PT.Hash "trusted-bad-box"
        let boxName = PT.FQTypeName.package "trusted-bad-box"
        let boxType : PT.PackageType.PackageType =
          { hash = boxHash
            description = ""
            declaration =
              { typeParams = []
                definition =
                  PT.TypeDeclaration.Record(
                    NEList.singleton
                      { name = "hidden"
                        typ = customType missingName
                        description = "" }
                  ) } }
        let candidateHash = PT.Hash "trusted-bad-box-candidate"
        let candidate =
          { oneArgFn (customType boxName) PT.TUnit (PT.EUnit 188UL) with
              hash = candidateHash }
        let location : PT.PackageLocation =
          { owner = "Test"; modules = [ "AtRest" ]; name = "BadBox" }
        let pm =
          PT.PackageManager.empty
          |> PT.PackageManager.withExtras [ boxType, location ] [] []
        let report =
          AuthoringChecker.checkPackageOps
            pm
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            [ PT.PackageOp.AddFn candidate ]
          |> _.Result

        Expect.equal
          report.verdict
          AuthoringChecker.Incomplete
          "a malformed trusted type cannot produce Checked"
        match report.items with
        | [ item ] ->
          Expect.equal
            item.item
            (PT.Reference.PackageFn candidateHash)
            "the blocker belongs to the candidate"
          Expect.isTrue
            (item.blockers
             |> List.exists (fun blocker ->
               blocker.code = Checker.MissingTypeDeclaration))
            "the missing type inside the trusted record is reported"
        | items -> failtestf "Expected one candidate report, got %A" items
      }

      test "authoring reports preserve each candidate verdict" {
        let validHash = PT.Hash "adapter-valid"
        let invalidHash = PT.Hash "adapter-invalid"
        let valid =
          { oneArgFn PT.TInt PT.TInt (PT.EArg(43UL, 0)) with hash = validHash }
        let invalid =
          { oneArgFn PT.TInt PT.TString (PT.EArg(44UL, 0)) with hash = invalidHash }
        let report =
          AuthoringChecker.checkPackageOps
            PT.PackageManager.empty
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            [ PT.PackageOp.AddFn valid; PT.PackageOp.AddFn invalid ]
          |> _.Result

        Expect.equal
          report.verdict
          AuthoringChecker.Failed
          "one failure fails the batch"
        Expect.equal report.items.Length 2 "both candidates have item reports"
        let find reference =
          match report.items |> List.find (fun item -> item.item = reference) with
          | Some item -> item
          | None -> failtestf "Missing item report for %A" reference
        Expect.equal
          (find (PT.Reference.PackageFn validHash)).verdict
          AuthoringChecker.Checked
          "the valid candidate remains checked"
        Expect.equal
          (find (PT.Reference.PackageFn invalidHash)).verdict
          AuthoringChecker.Failed
          "the invalid candidate remains failed"
      }

      test "stabilization preserves candidates with provisional hash collisions" {
        let provisionalHash = PT.Hash ""
        let valid =
          { oneArgFn PT.TInt PT.TInt (PT.EArg(45UL, 0)) with hash = provisionalHash }
        let invalid =
          { oneArgFn PT.TInt PT.TString (PT.EArg(46UL, 0)) with
              hash = provisionalHash }
        let location name : PT.PackageLocation =
          { owner = "Test"; modules = [ "AtRest" ]; name = name }
        let ops =
          [ PT.PackageOp.AddFn valid
            PT.PackageOp.SetName(
              location "valid",
              PT.Reference.PackageFn provisionalHash
            )
            PT.PackageOp.AddFn invalid
            PT.PackageOp.SetName(
              location "invalid",
              PT.Reference.PackageFn provisionalHash
            ) ]
          |> HashStabilization.computeRealHashes
        let report =
          AuthoringChecker.checkPackageOps
            PT.PackageManager.empty
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            ops
          |> _.Result

        Expect.equal report.verdict AuthoringChecker.Failed "the invalid item fails"
        Expect.equal report.items.Length 2 "both stabilized candidates were checked"
        let identities = report.items |> List.map _.item |> Set.ofList
        Expect.equal
          identities.Count
          2
          "each candidate has a distinct stable identity"
      }

      // Deeply nested input must come back as a verdict, not a stack overflow: an
      // overflow is not an exception and takes the CLI, LSP or server with it.
      // These run in-process because the checker probes the stack before each
      // recursive step and converts exhaustion into an Incomplete blocker.
      test "a deeply nested type is reported as Incomplete, not a crash" {
        let deep = [ 1..1_000_000 ] |> List.fold (fun typ _ -> PT.TList typ) PT.TInt
        oneArgFn deep PT.TUnit (PT.EUnit 1UL)
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectBlocker Checker.UnsupportedConstruct
      }

      test "a deeply nested expression is reported as Incomplete, not a crash" {
        let deep =
          [ 1..1_000_000 ]
          |> List.fold
            (fun expr index -> PT.EList(uint64 index + 1UL, [ expr ]))
            (PT.EInt(1UL, 0I))
        oneArgFn PT.TUnit PT.TUnit deep
        |> Checker.checkPackageFunction Checker.TypeEnvironment.empty
        |> expectBlocker Checker.UnsupportedConstruct
      }

      test "authoring dependency discovery handles a deeply nested candidate type" {
        let deep = [ 1..1_000_000 ] |> List.fold (fun typ _ -> PT.TList typ) PT.TInt
        let candidate =
          { oneArgFn deep PT.TUnit (PT.EUnit 1UL) with
              hash = PT.Hash "deep-authoring-type" }
        let report =
          AuthoringChecker.checkPackageOps
            PT.PackageManager.empty
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            [ PT.PackageOp.AddFn candidate ]
          |> _.Result

        Expect.equal
          report.verdict
          AuthoringChecker.Incomplete
          "the adapter returns the core checker's depth blocker"
        Expect.isTrue
          (report.blockers
           |> List.exists (fun blocker ->
             blocker.code = Checker.UnsupportedConstruct))
          "the batch reports that the declaration is too deeply nested"
      }

      test "serialized deeply nested input fails with a catchable exception" {
        let typeName = PT2DT.TypeReference.typeName ()
        let deep =
          [ 1..1_000_000 ]
          |> List.fold
            (fun inner _ -> RT.DEnum(typeName, typeName, [], "TList", [ inner ]))
            (RT.DEnum(typeName, typeName, [], "TInt", []))
        let safelyRejected =
          try
            let _converted : PT.TypeReference = PT2DT.TypeReference.fromDT deep
            false
          with :? System.InsufficientExecutionStackException ->
            true

        Expect.isTrue
          safelyRejected
          "the builtin boundary can convert excessive depth to Incomplete"
      }

      test
        "authoring dependency discovery handles a deeply nested candidate expression" {
        let deep =
          [ 1..1_000_000 ]
          |> List.fold
            (fun expr index -> PT.EList(uint64 index + 1UL, [ expr ]))
            (PT.EInt(1UL, 0I))
        let candidate =
          { oneArgFn PT.TUnit PT.TUnit deep with
              hash = PT.Hash "deep-authoring-expression" }
        let report =
          AuthoringChecker.checkPackageOps
            PT.PackageManager.empty
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            [ PT.PackageOp.AddFn candidate ]
          |> _.Result

        Expect.equal
          report.verdict
          AuthoringChecker.Incomplete
          "the adapter returns the core checker's depth blocker"
        Expect.isTrue
          (report.blockers
           |> List.exists (fun blocker ->
             blocker.code = Checker.UnsupportedConstruct))
          "the batch reports that the declaration is too deeply nested"
      }

      test "dependency loading handles a deeply nested stored signature" {
        let deep = [ 1..1_000_000 ] |> List.fold (fun typ _ -> PT.TList typ) PT.TInt
        let dependencyHash = PT.Hash "deep-stored-signature"
        let dependency =
          { oneArgFn deep PT.TUnit (PT.EUnit 1UL) with hash = dependencyHash }
        let candidate =
          { oneArgFn
              PT.TUnit
              PT.TUnit
              (PT.EApply(
                2UL,
                PT.EFnName(
                  3UL,
                  PT.NameResolution.ok (PT.FQFnName.Package dependencyHash)
                ),
                [],
                NEList.singleton (PT.EUnit 4UL)
              )) with
              hash = PT.Hash "deep-stored-signature-candidate" }
        let location : PT.PackageLocation =
          { owner = "Test"; modules = [ "AtRest" ]; name = "deepDependency" }
        let pm =
          PT.PackageManager.empty
          |> PT.PackageManager.withExtras [] [] [ dependency, location ]
        let report =
          AuthoringChecker.checkPackageOps
            pm
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            [ PT.PackageOp.AddFn candidate ]
          |> _.Result

        Expect.equal
          report.verdict
          AuthoringChecker.Incomplete
          "loading and checking the deeply nested signature does not crash"
        Expect.isTrue
          (report.blockers
           |> List.exists (fun blocker ->
             blocker.code = Checker.UnsupportedConstruct))
          "the batch reports that the stored signature is too deeply nested"
      }

      test "a deeply nested alias chain is reported as Incomplete, not a crash" {
        // Each alias wraps the next in a container, so expanding one means
        // descending into it: the walk cannot tail-call its way through. (A flat
        // chain of bare aliases does, and completes as Checked.)
        let depth = 300_000
        let aliasName (index : int) = PT.FQTypeName.package $"alias-{index}"
        let environment =
          [ 0 .. depth - 1 ]
          |> List.fold
            (fun environment index ->
              let target =
                if index = depth - 1 then
                  PT.TInt
                else
                  PT.TList(customType (aliasName (index + 1)))
              let declaration : PT.TypeDeclaration.T =
                { typeParams = []; definition = PT.TypeDeclaration.Alias target }
              environment
              |> Checker.TypeEnvironment.addType (aliasName index) declaration)
            Checker.TypeEnvironment.empty
        oneArgFn (customType (aliasName 0)) PT.TUnit (PT.EUnit 1UL)
        |> Checker.checkPackageFunction environment
        |> expectBlocker Checker.UnsupportedConstruct
      }

      test "authoring dependency discovery is stack-safe for corpus batches" {
        let payloadHash = PT.Hash "large-batch-payload"
        let payloadType : PT.PackageType.PackageType =
          { hash = payloadHash
            description = ""
            declaration =
              { typeParams = []
                definition =
                  PT.TypeDeclaration.Enum(NEList.singleton (enumCase "Payload" [])) } }
        let payloadRef = customType payloadHash
        let functions =
          [ 1..8000 ]
          |> List.map (fun index ->
            { fn
                (NEList.singleton (parameter "payload" payloadRef))
                payloadRef
                (PT.EArg(uint64 index, 0)) with
                hash = PT.Hash $"large-batch-{index}" })
        let ops =
          PT.PackageOp.AddType payloadType
          :: (functions |> List.map PT.PackageOp.AddFn)
        let report =
          AuthoringChecker.checkPackageOps
            PT.PackageManager.empty
            (TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT)
            ops
          |> _.Result

        Expect.equal report.verdict AuthoringChecker.Checked "the batch is valid"
        Expect.equal report.items.Length 8001 "every candidate was checked"
      } ]


// The rollout policy end to end: `dark fn` and `dark module` store through
// `SCM.PackageOps.addAuthored`, which saves Failed, Checked and Incomplete alike and
// reports the verdict; `commit` refuses while a Failed declaration is in the batch,
// and `commit --allow-type-errors` takes it anyway. Runs the real CLI against an
// isolated seeded
// store, so it is sequenced like the other harness users.
let private authoringTypeCheckPolicy =
  testTask "authoring warns on Failed; commit refuses it unless allowed" {
    let! instance = seededInstance "authoring-type-check-policy"
    try
      activate instance
      let! state = buildCliState ()

      // Commit is intentionally authenticated. Do not rely on a developer's
      // persisted CLI session: CI and a clean checkout start logged out.
      let! loginOutput = runCli state [ "login"; "Stachu" ]
      Expect.stringContains
        loginOutput
        "Logged in as Stachu"
        "the isolated CLI fixture has an authenticated commit author"

      let! failedOutput =
        runCli state [ "fn"; "Test.AtRestAuthoring.invalid"; "(x: Int): String = x" ]
      Expect.stringContains
        failedOutput
        "Created function"
        "a definite type error is still saved as WIP"
      Expect.stringContains
        failedOutput
        "At-rest type check failed (saved as WIP; `commit` will refuse it until fixed)"
        "and the author is told what that means"
      Expect.stringContains failedOutput "[TypeMismatch]" "with the diagnostic"
      let! failedSearch =
        runCli state [ "search"; "Test.AtRestAuthoring.invalid"; "--exact" ]
      Expect.stringContains
        failedSearch
        "Test.AtRestAuthoring.invalid"
        "the failed declaration is visible on the branch"

      let! checkedOutput =
        runCli state [ "fn"; "Test.AtRestAuthoring.valid"; "(x: Int): Int = x + 1" ]
      Expect.stringContains
        checkedOutput
        "Created function"
        "a checked declaration is persisted"
      Expect.isFalse
        (checkedOutput.Contains "At-rest type check")
        "a checked declaration says nothing about the checker"

      let! incompleteOutput =
        runCli
          state
          [ "fn"
            "Test.AtRestAuthoring.incomplete"
            "(x: Bool): String = match x with | true -> \"yes\"" ]
      Expect.stringContains
        incompleteOutput
        "At-rest type check was incomplete (save continued)"
        "an incomplete proof is reported without rejecting the declaration"

      let! mixedOutput =
        runCli
          state
          [ "fn"
            "Test.AtRestAuthoring.mixed"
            "(x: Bool): String = match x with | true -> 1" ]
      Expect.stringContains
        mixedOutput
        "At-rest type check failed (saved as WIP; `commit` will refuse it until fixed)"
        "a definite error is not hidden by an unrelated incomplete proof"
      Expect.stringContains
        mixedOutput
        "[TypeMismatch]"
        "definite diagnostics are included alongside incomplete blockers"

      // `module` batches several declarations through the same door: the bad
      // one is reported, the whole batch is still saved.
      let modulePath =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          "at-rest-authoring-module.dark"
        )
      System.IO.File.WriteAllText(
        modulePath,
        "let fine (x: Int) : Int = x + 1\nlet broken (x: Bool) : String = x\n"
      )
      let! moduleOutput =
        runCli state [ "module"; "Test.AtRestAuthoring.Mod"; modulePath ]
      Expect.stringContains
        moduleOutput
        "Defined 2 declarations"
        "the module is saved"
      Expect.stringContains
        moduleOutput
        "At-rest type check failed (saved as WIP"
        "and its bad declaration is reported"

      // The gate. Failed declarations are in WIP, so a plain commit is refused,
      // names the culprits, and commits nothing.
      let! refused = runCli state [ "commit"; "gate"; "--yes" ]
      Expect.stringContains
        refused
        "Cannot commit: 3 declarations have definite type errors"
        "commit refuses while Failed declarations are in the batch"
      Expect.stringContains refused "Test.AtRestAuthoring.invalid" "and names them"
      Expect.stringContains
        refused
        "Test.AtRestAuthoring.mixed"
        "including mixed reports"
      Expect.stringContains refused "Test.AtRestAuthoring.Mod.broken" "all of them"
      Expect.isFalse (refused.Contains "Created commit") "nothing was committed"
      let! statusAfterRefusal = runCli state [ "status" ]
      Expect.stringContains
        statusAfterRefusal
        "Uncommitted changes"
        "the WIP is untouched"

      // A partial commit of only the good declarations goes through.
      let! partial =
        runCli
          state
          [ "commit"; "good ones"; "--include=Test.AtRestAuthoring.valid"; "--yes" ]
      Expect.stringContains
        partial
        "Created commit"
        "Checked declarations commit on their own"

      // Fixing the error un-refuses it (the batch is re-checked at commit time),
      // and --allow-type-errors takes whatever is left. --force is a different
      // override (unresolved references) and must not open this gate.
      let! fixedOutput =
        runCli
          state
          [ "fn"
            "Test.AtRestAuthoring.invalid"
            "(x: Int): String = Stdlib.Int.toString x" ]
      Expect.stringContains fixedOutput "Updated function" "the fix is saved"
      let! stillRefused = runCli state [ "commit"; "still broken"; "--yes" ]
      Expect.stringContains
        stillRefused
        "Cannot commit: 2 declarations have definite type errors"
        "the fixed one no longer counts; the remaining definite errors still do"
      let! forced = runCli state [ "commit"; "take it"; "--yes"; "--force" ]
      Expect.stringContains
        forced
        "Cannot commit: 2 declarations have definite type errors"
        "--force does not allow type errors"
      let! allowed =
        runCli state [ "commit"; "take it"; "--yes"; "--allow-type-errors" ]
      Expect.stringContains
        allowed
        "Created commit"
        "--allow-type-errors commits Failed declarations"
      let! statusAfterAllow = runCli state [ "status" ]
      Expect.stringContains
        statusAfterAllow
        "No uncommitted changes"
        "everything was committed"
    finally
      teardown [ instance ]
  }

let private authoringTests =
  testSequenced <| testList "authoring" [ authoringTypeCheckPolicy ]


let tests = testList "AtRestTypeChecker" [ unitTests; authoringTests ]
