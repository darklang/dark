/// Type checking and package linting, from one inference pass.
///
/// The rule is generic and lives in `AtRest.Lint`; the policy -- which type a caller must
/// use -- is here, because this is the layer that may name `Stdlib.Test.T` through
/// `PackageRefs`. Warnings never enter a `CheckReport`: a type proof carries no warnings,
/// so a warning cannot fail a check or block a commit.
module Builtins.Matter.Libs.PM.PackageAnalysis

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module TypeChecker = AtRestTypeChecker
module Checker = LibExecution.AtRest.Types
module Lint = LibExecution.AtRest.Lint
module LocalBindingUsage = LibExecution.AtRest.LocalBindingUsage
module Dval = LibExecution.Dval
module NR = LibExecution.RuntimeTypes.NameResolution
module PackageRefs = LibExecution.PackageRefs
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes


// -- Lint policy --
//
// Must-use types remain a policy here until type declarations can carry that property.
// Ordinary binding use is syntax-level and comes from `AtRest.Lint`.

type WarningCode =
  | UnusedBinding
  | UnusedTestResult

type Warning = { code : WarningCode; nodeId : Option<id>; name : string }

type LintResults = List<PT.Reference * List<Warning>>

let lint
  (ops : List<PT.PackageOp>)
  (results : List<Checker.ItemVerdict>)
  : LintResults =
  let testResultType = PT.FQTypeName.package (PackageRefs.Type.Stdlib.testT ())
  let requiresUse typ =
    match typ with
    | Checker.TCustom(name, []) -> name = testResultType
    | _ -> false
  let bindingUses =
    ops
    |> List.choose (fun op ->
      match op with
      | PT.PackageOp.AddFn fn ->
        Some(PT.Reference.PackageFn fn.hash, LocalBindingUsage.analyzeFunction fn)
      | PT.PackageOp.AddValue value ->
        Some(
          PT.Reference.PackageValue value.hash,
          LocalBindingUsage.analyzeExpression value.body
        )
      | _ -> None)
    |> Map.ofList
  results
  |> List.choose (fun result ->
    let itemBindingUses =
      Map.tryFind result.item bindingUses |> Option.defaultValue []
    let unusedResults =
      Lint.unusedResults
        requiresUse
        itemBindingUses
        (Lint.bindingsOf result.verdict)
    let ignoredResultWarnings =
      unusedResults
      |> List.map (fun unused ->
        { code = UnusedTestResult
          nodeId = Some unused.nodeId
          name = unused.name })
    let unusedBindingWarnings =
      itemBindingUses
      |> Lint.unusedBindings
      // Prefer the more specific ignored-result diagnostic for the same let.
      |> List.filter (fun binding ->
        unusedResults
        |> List.exists (fun unused ->
          binding.introducingNodeId = Some unused.nodeId
          && binding.name = unused.name)
        |> not)
      |> List.map (fun binding ->
        { code = UnusedBinding; nodeId = binding.nodeId; name = binding.name })
    let warnings = unusedBindingWarnings @ ignoredResultWarnings
    if List.isEmpty warnings then None else Some(result.item, warnings))


// -- One inference pass, both outputs --

let analyzePackageOps
  (pm : PT.PackageManager)
  (builtins : Builtins)
  (ops : List<PT.PackageOp>)
  : Ply<TypeChecker.CheckReport * LintResults> =
  uply {
    match! TypeChecker.inferPackageOps pm builtins ops with
    | Error report -> return report, []
    | Ok results -> return TypeChecker.aggregate results, lint ops results
  }

/// Analyze every declaration the given package manager can see.
///
/// No branch parameter: a branch is an overlay carried by the pm itself, so `pm` decides what the
/// search below reaches.
let analyzeBranch
  (pm : PT.PackageManager)
  (builtins : Builtins)
  : Ply<TypeChecker.CheckReport * LintResults> =
  uply {
    let query : PT.Search.SearchQuery =
      { currentModule = []
        text = ""
        searchDepth = PT.Search.AllDescendants
        entityTypes = []
        exactMatch = false }
    let! results = pm.search query
    let ops =
      List.concat
        [ results.types |> List.map (fun item -> PT.PackageOp.AddType item.entity)
          results.values |> List.map (fun item -> PT.PackageOp.AddValue item.entity)
          results.fns |> List.map (fun item -> PT.PackageOp.AddFn item.entity) ]
    return! analyzePackageOps pm builtins ops
  }


module private DarkTypes =
  let warningCodeName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Lint.warningCode ())

  let warningName () =
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.Lint.warning ())

  let warningToDT (warning : Warning) : Dval =
    let codeName = warningCodeName ()
    let caseName =
      match warning.code with
      | UnusedBinding -> "UnusedBinding"
      | UnusedTestResult -> "UnusedTestResult"
    let typeName = warningName ()
    DRecord(
      typeName,
      typeName,
      [],
      Map
        [ "code", DEnum(codeName, codeName, [], caseName, [])
          "nodeId", warning.nodeId |> Option.map DUInt64 |> Dval.option KTUInt64
          "name", DString warning.name ]
    )

  let lintResultsType () : TypeReference =
    TList(
      TTuple(
        TCustomType(NR.ok (PT2DT.Reference.typeName ()), []),
        TList(TCustomType(NR.ok (warningName ()), [])),
        []
      )
    )

  let lintResultsToDT (results : LintResults) : Dval =
    let warningType = KTCustomType(warningName (), [])
    let itemType =
      KTTuple(
        ValueType.Known(PT2DT.Reference.knownType ()),
        ValueType.Known(KTList(ValueType.Known warningType)),
        []
      )
    results
    |> List.map (fun (item, warnings) ->
      DTuple(
        PT2DT.Reference.toDT item,
        warnings |> List.map warningToDT |> Dval.list warningType,
        []
      ))
    |> Dval.list itemType

  let analysisType () : TypeReference =
    TTuple(TypeChecker.DarkTypes.reportType (), lintResultsType (), [])

  let analysisToDT (report : TypeChecker.CheckReport, lint : LintResults) : Dval =
    DTuple(TypeChecker.DarkTypes.reportToDT report, lintResultsToDT lint, [])

  let unavailableAnalysis (detail : string) : Dval =
    analysisToDT (TypeChecker.unavailableReport detail, [])


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "packageAnalyzeOps" 0
      typeParams = []
      parameters =
        [ Param.make
            "ops"
            (TList(
              TCustomType(
                NR.ok (
                  FQTypeName.fqPackage (
                    PackageRefs.Type.LanguageTools.ProgramTypes.packageOp ()
                  )
                ),
                []
              )
            ))
            "Candidate AddType, AddValue, and AddFn package operations" ]
      returnType = DarkTypes.analysisType ()
      description =
        "Type-checks and lints candidate package declarations against their transitive dependency closure without persisting them."
      fn =
        (function
        | exeState, _, _, [| DList(_, ops) |] ->
          uply {
            try
              let decoded = ops |> List.map PT2DT.PackageOp.fromDT
              if decoded |> List.exists Option.isNone then
                return
                  DarkTypes.unavailableAnalysis
                    "Package analysis received an invalid package op"
              else
                let ops = decoded |> List.choose (fun value -> value)
                let builtins = exeState.builtins
                let! analysis = analyzePackageOps pm builtins ops
                return DarkTypes.analysisToDT analysis
            with ex ->
              return
                DarkTypes.unavailableAnalysis
                  $"Package analysis unavailable: {ex.Message}"
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "packageAnalyzeBranch" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "the branch a caller means" ]
      returnType = DarkTypes.analysisType ()
      description =
        "Type-checks and lints every visible package declaration on a branch without persisting anything."
      fn =
        (function
        | exeState, _, _, [| DUuid branchId |] ->
          uply {
            try
              // The branch's own overlay, not this builtin set's pm (which is main's): on a branch
              // the check has to see the branch's declarations, or it type-checks main and calls
              // it the branch.
              let branchPm =
                LibDB.PackageManager.ptForBranch (PT.BranchId.Id branchId)

              let! analysis = analyzeBranch branchPm exeState.builtins
              return DarkTypes.analysisToDT analysis
            with ex ->
              return
                DarkTypes.unavailableAnalysis
                  $"Branch analysis unavailable: {ex.Message}"
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins (pm : PT.PackageManager) = LibExecution.Builtin.make [] (fns pm)
