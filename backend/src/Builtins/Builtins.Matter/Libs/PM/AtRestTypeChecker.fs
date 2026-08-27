/// Authoring adapter for the at-rest type checker.
///
/// Loads candidate dependencies, invokes the storage-independent checker, and
/// converts its report to structured Dark values without persisting anything.
module Builtins.Matter.Libs.PM.AtRestTypeChecker

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

// The checker's vocabulary (types, verdicts, diagnostic codes, the type environment)
// lives in AtRest.Types; the entry points that run it live in AtRestTypeChecker.
module Checker = LibExecution.AtRest.Types
module CheckerApi = LibExecution.AtRestTypeChecker
module Dval = LibExecution.Dval
module NR = LibExecution.RuntimeTypes.NameResolution
module PackageRefs = LibExecution.PackageRefs
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Dependencies = LibDB.DependencyExtractor


type private Closure =
  { types : Map<PT.Hash, PT.PackageType.PackageType>
    values : Map<PT.Hash, PT.PackageValue.PackageValue>
    functions : Map<PT.Hash, PT.PackageFn.PackageFn> }

let private emptyClosure : Closure =
  { types = Map.empty; values = Map.empty; functions = Map.empty }

let private referenceOfDependency
  (dependency : Dependencies.Dependency)
  : PT.Reference =
  PT.Reference.fromHashAndKind (dependency.hash, dependency.itemKind)

let private candidateItems
  (ops : List<PT.PackageOp>)
  : List<PT.Reference> * Closure =
  ops
  |> List.fold
    (fun ((items, closure) : List<PT.Reference> * Closure) op ->
      match op with
      | PT.PackageOp.AddType typ ->
        (PT.Reference.PackageType typ.hash :: items,
         { closure with types = Map.add typ.hash typ closure.types })
      | PT.PackageOp.AddValue value ->
        (PT.Reference.PackageValue value.hash :: items,
         { closure with values = Map.add value.hash value closure.values })
      | PT.PackageOp.AddFn fn ->
        (PT.Reference.PackageFn fn.hash :: items,
         { closure with functions = Map.add fn.hash fn closure.functions })
      | PT.PackageOp.SetName _
      | PT.PackageOp.Deprecate _
      | PT.PackageOp.Undeprecate _
      | PT.PackageOp.PropagateUpdate _
      | PT.PackageOp.RevertPropagation _ -> items, closure)
    ([], emptyClosure)
  |> fun (items, closure) -> List.rev items, closure

let private candidateDependencies (closure : Closure) : List<PT.Reference> =
  List.concat
    [ closure.types.Values
      |> Seq.collect Dependencies.extractFromType
      |> Seq.map referenceOfDependency
      |> Seq.toList
      closure.values.Values
      |> Seq.collect Dependencies.extractFromValue
      |> Seq.map referenceOfDependency
      |> Seq.toList
      closure.functions.Values
      |> Seq.collect Dependencies.extractFromFn
      |> Seq.map referenceOfDependency
      |> Seq.toList ]

let private loadDependencyClosure
  (pm : PT.PackageManager)
  (candidates : Closure)
  : Ply<Closure> =
  uply {
    // An explicit work list is important here. Recursing through Ply for every
    // already-seen item retains synchronous continuation frames and overflows on
    // a full package corpus even though the algorithm is tail-recursive on paper.
    let mutable seen : Set<PT.Reference> =
      Set.unionMany
        [ candidates.types.Keys |> Seq.map PT.Reference.PackageType |> Set.ofSeq
          candidates.values.Keys |> Seq.map PT.Reference.PackageValue |> Set.ofSeq
          candidates.functions.Keys |> Seq.map PT.Reference.PackageFn |> Set.ofSeq ]
    let mutable pending = candidateDependencies candidates
    let mutable closure = emptyClosure

    while not (List.isEmpty pending) do
      match pending with
      | [] -> ()
      | reference :: rest ->
        pending <- rest
        if not (Set.contains reference seen) then
          seen <- Set.add reference seen
          match reference with
          | PT.Reference.PackageType hash ->
            let! item = pm.getType hash
            match item with
            | None -> ()
            | Some typ ->
              pending <-
                (typ
                 |> Dependencies.extractFromType
                 |> List.map referenceOfDependency)
                @ pending
              closure <- { closure with types = Map.add typ.hash typ closure.types }
          | PT.Reference.PackageValue hash ->
            let! item = pm.getValue hash
            match item with
            | None -> ()
            | Some value ->
              pending <-
                (value
                 |> Dependencies.extractFromValue
                 |> List.map referenceOfDependency)
                @ pending
              closure <-
                { closure with values = Map.add value.hash value closure.values }
          | PT.Reference.PackageFn hash ->
            let! item = pm.getFn hash
            match item with
            | None -> ()
            | Some fn ->
              // Existing function bodies are not needed to check a caller. Only
              // recursively load types referenced by the callable signature.
              pending <-
                (fn
                 |> Dependencies.extractFromFnSignature
                 |> List.map referenceOfDependency)
                @ pending
              closure <-
                { closure with functions = Map.add fn.hash fn closure.functions }

    return closure
  }

let private addTrustedDependencyDeclarations
  (dependencies : Closure)
  (environment : Checker.TypeEnvironment)
  : Checker.TypeEnvironment =
  let environment =
    dependencies.types.Values
    |> Seq.fold
      (fun environment typ -> Checker.TypeEnvironment.addPackageType typ environment)
      environment
  dependencies.functions.Values
  |> Seq.fold
    (fun environment fn ->
      Checker.TypeEnvironment.addPackageFunctionSignature fn environment)
    environment

type CheckVerdict =
  | Checked
  | Failed
  | Incomplete

type ItemCheckReport =
  { item : PT.Reference
    verdict : CheckVerdict
    diagnostics : List<Checker.Diagnostic>
    blockers : List<Checker.Blocker> }

type CheckReport =
  { verdict : CheckVerdict
    items : List<ItemCheckReport>
    diagnostics : List<Checker.Diagnostic>
    blockers : List<Checker.Blocker> }

let unavailableReport (context : string) : CheckReport =
  { verdict = Incomplete
    items = []
    diagnostics = []
    blockers =
      [ { code = Checker.UnsupportedConstruct; nodeId = None; context = context } ] }

let private itemReport (result : Checker.ItemVerdict) : ItemCheckReport =
  match result.verdict with
  | Checker.Checked _ ->
    { item = result.item; verdict = Checked; diagnostics = []; blockers = [] }
  | Checker.Failed report ->
    { item = result.item
      verdict = Failed
      diagnostics = report.diagnostics
      blockers = report.blockers }
  | Checker.Incomplete report ->
    { item = result.item
      verdict = Incomplete
      diagnostics = report.diagnostics
      blockers = report.blockers }

let private aggregate
  (candidateRefs : Set<PT.Reference>)
  (batch : Checker.BatchResult)
  : CheckReport =
  let items =
    List.concat [ batch.types; batch.values; batch.functions ]
    |> List.filter (fun result -> Set.contains result.item candidateRefs)
    |> List.map itemReport

  let diagnostics = items |> List.collect (fun item -> item.diagnostics)
  let blockers = items |> List.collect (fun item -> item.blockers)
  let verdict =
    if items |> List.exists (fun item -> item.verdict = Failed) then
      Failed
    elif items |> List.exists (fun item -> item.verdict = Incomplete) then
      Incomplete
    else
      Checked
  { verdict = verdict
    items = items
    diagnostics = diagnostics
    blockers = blockers }

let checkPackageOps
  (pm : PT.PackageManager)
  (builtins : Builtins)
  (ops : List<PT.PackageOp>)
  : Ply<CheckReport> =
  uply {
    let candidateRefs, candidates = candidateItems ops
    let! dependencies = loadDependencyClosure pm candidates
    match
      Checker.TypeEnvironment.empty |> Checker.TypeEnvironment.addBuiltins builtins
    with
    | Error errors ->
      return
        errors
        |> List.map (fun error -> $"Could not import builtin signature: {error}")
        |> String.concat "; "
        |> unavailableReport
    | Ok environment ->
      let environment = addTrustedDependencyDeclarations dependencies environment
      let values =
        Map.fold
          (fun values hash value -> Map.add hash value values)
          dependencies.values
          candidates.values
      let batch =
        CheckerApi.checkPackageBatch
          environment
          (candidates.types.Values |> Seq.toList)
          (values.Values |> Seq.toList)
          (candidates.functions.Values |> Seq.toList)
      return aggregate (Set.ofList candidateRefs) batch
  }

let checkBranch
  (pm : PT.PackageManager)
  (builtins : Builtins)
  (branchId : PT.BranchId)
  : Ply<CheckReport> =
  uply {
    let query : PT.Search.SearchQuery =
      { currentModule = []
        text = ""
        searchDepth = PT.Search.AllDescendants
        entityTypes = []
        exactMatch = false }
    let! results = pm.search (branchId, query)
    let ops =
      List.concat
        [ results.types |> List.map (fun item -> PT.PackageOp.AddType item.entity)
          results.values |> List.map (fun item -> PT.PackageOp.AddValue item.entity)
          results.fns |> List.map (fun item -> PT.PackageOp.AddFn item.entity) ]
    return! checkPackageOps pm builtins ops
  }


module private DarkTypes =
  module Refs = PackageRefs.Type.LanguageTools.AtRestTypeChecker

  let staticTypeName () = FQTypeName.fqPackage (Refs.staticType ())
  let verdictName () = FQTypeName.fqPackage (Refs.verdict ())
  let issueCodeName () = FQTypeName.fqPackage (Refs.issueCode ())
  let issueName () = FQTypeName.fqPackage (Refs.issue ())
  let itemReportName () = FQTypeName.fqPackage (Refs.itemReport ())
  let reportName () = FQTypeName.fqPackage (Refs.report ())

  let private enumValue typeName caseName fields =
    DEnum(typeName, typeName, [], caseName, fields)

  let rec staticTypeToDT (typ : Checker.StaticType) : Dval =
    let typeName = staticTypeName ()
    let make caseName fields = enumValue typeName caseName fields
    let list types =
      types |> List.map staticTypeToDT |> Dval.list (KTCustomType(typeName, []))
    match typ with
    | Checker.TUnit -> make "TUnit" []
    | Checker.TBool -> make "TBool" []
    | Checker.TInt8 -> make "TInt8" []
    | Checker.TUInt8 -> make "TUInt8" []
    | Checker.TInt16 -> make "TInt16" []
    | Checker.TUInt16 -> make "TUInt16" []
    | Checker.TInt32 -> make "TInt32" []
    | Checker.TUInt32 -> make "TUInt32" []
    | Checker.TInt64 -> make "TInt64" []
    | Checker.TUInt64 -> make "TUInt64" []
    | Checker.TInt128 -> make "TInt128" []
    | Checker.TUInt128 -> make "TUInt128" []
    | Checker.TInt -> make "TInt" []
    | Checker.TFloat -> make "TFloat" []
    | Checker.TChar -> make "TChar" []
    | Checker.TString -> make "TString" []
    | Checker.TUuid -> make "TUuid" []
    | Checker.TDateTime -> make "TDateTime" []
    | Checker.TBlob -> make "TBlob" []
    | Checker.TStream inner -> make "TStream" [ staticTypeToDT inner ]
    | Checker.TList inner -> make "TList" [ staticTypeToDT inner ]
    | Checker.TTuple(first, second, rest) ->
      make "TTuple" [ staticTypeToDT first; staticTypeToDT second; list rest ]
    | Checker.TDict inner -> make "TDict" [ staticTypeToDT inner ]
    | Checker.TCustom(hash, args) ->
      make "TCustom" [ PT2DT.Hash.toDT hash; list args ]
    | Checker.TFn(parameters, returnType) ->
      make "TFn" [ parameters |> NEList.toList |> list; staticTypeToDT returnType ]
    | Checker.TDB inner -> make "TDB" [ staticTypeToDT inner ]
    | Checker.TRigidVar name -> make "TRigidVariable" [ DString name ]
    | Checker.TInferenceVar variable ->
      make "TInferenceVariable" [ Dval.int (bigint variable) ]

  let verdictToDT (verdict : CheckVerdict) : Dval =
    let caseName =
      match verdict with
      | Checked -> "Checked"
      | Failed -> "Failed"
      | Incomplete -> "Incomplete"
    enumValue (verdictName ()) caseName []

  let diagnosticCodeToDT (code : Checker.DiagnosticCode) : Dval =
    let caseName =
      match code with
      | Checker.TypeMismatch -> "TypeMismatch"
      | Checker.OccursCheckFailed -> "OccursCheckFailed"
      | Checker.UnknownVariable -> "UnknownVariable"
      | Checker.InvalidArgumentIndex -> "InvalidArgumentIndex"
      | Checker.NotCallable -> "NotCallable"
      | Checker.ExplicitTypeArgumentCountMismatch ->
        "ExplicitTypeArgumentCountMismatch"
      | Checker.InvalidPattern -> "InvalidPattern"
      | Checker.DuplicatePatternBinding -> "DuplicatePatternBinding"
      | Checker.UnknownRecordField -> "UnknownRecordField"
      | Checker.MissingRecordField -> "MissingRecordField"
      | Checker.DuplicateRecordField -> "DuplicateRecordField"
      | Checker.UnknownEnumCase -> "UnknownEnumCase"
      | Checker.EnumFieldCountMismatch -> "EnumFieldCountMismatch"
      | Checker.InvalidInfixOperand -> "InvalidInfixOperand"
      | Checker.DuplicateTypeParameter -> "DuplicateTypeParameter"
      | Checker.DuplicateTypeMember -> "DuplicateTypeMember"
    enumValue (issueCodeName ()) caseName []

  let blockerCodeToDT (code : Checker.BlockerCode) : Dval =
    let caseName =
      match code with
      | Checker.UnresolvedTypeName -> "UnresolvedTypeName"
      | Checker.UnresolvedFunctionName -> "UnresolvedFunctionName"
      | Checker.UnresolvedValueName -> "UnresolvedValueName"
      | Checker.MissingTypeDeclaration -> "MissingTypeDeclaration"
      | Checker.MissingFunctionSignature -> "MissingFunctionSignature"
      | Checker.MissingValueSignature -> "MissingValueSignature"
      | Checker.AliasCycle -> "AliasCycle"
      | Checker.UnknownDeclaredTypeVariable -> "UnknownDeclaredTypeVariable"
      | Checker.AmbiguousType -> "AmbiguousType"
      | Checker.NonExhaustiveMatch -> "NonExhaustiveMatch"
      | Checker.UnsupportedConstruct -> "UnsupportedConstruct"
    enumValue (issueCodeName ()) caseName []

  let private optionNodeId (nodeId : Option<id>) : Dval =
    nodeId |> Option.map DUInt64 |> Dval.option KTUInt64

  let private optionStaticType (typ : Option<Checker.StaticType>) : Dval =
    let knownType = KTCustomType(staticTypeName (), [])
    typ |> Option.map staticTypeToDT |> Dval.option knownType

  let private issue
    (code : Dval)
    (nodeId : Option<id>)
    (expected : Option<Checker.StaticType>)
    (actual : Option<Checker.StaticType>)
    (context : string)
    : Dval =
    let typeName = issueName ()
    DRecord(
      typeName,
      typeName,
      [],
      Map
        [ "code", code
          "nodeId", optionNodeId nodeId
          "expected", optionStaticType expected
          "actual", optionStaticType actual
          "context", DString context ]
    )

  let diagnosticToDT (diagnostic : Checker.Diagnostic) : Dval =
    issue
      (diagnosticCodeToDT diagnostic.code)
      diagnostic.nodeId
      diagnostic.expected
      diagnostic.actual
      diagnostic.context

  let blockerToDT (blocker : Checker.Blocker) : Dval =
    issue (blockerCodeToDT blocker.code) blocker.nodeId None None blocker.context

  let itemReportToDT (report : ItemCheckReport) : Dval =
    let typeName = itemReportName ()
    let issueType = KTCustomType(issueName (), [])
    DRecord(
      typeName,
      typeName,
      [],
      Map
        [ "item", PT2DT.Reference.toDT report.item
          "verdict", verdictToDT report.verdict
          "diagnostics",
          report.diagnostics |> List.map diagnosticToDT |> Dval.list issueType
          "blockers", report.blockers |> List.map blockerToDT |> Dval.list issueType ]
    )

  let reportToDT (report : CheckReport) : Dval =
    let typeName = reportName ()
    let issueType = KTCustomType(issueName (), [])
    let itemReportType = KTCustomType(itemReportName (), [])
    DRecord(
      typeName,
      typeName,
      [],
      Map
        [ "verdict", verdictToDT report.verdict
          "items",
          report.items |> List.map itemReportToDT |> Dval.list itemReportType
          "diagnostics",
          report.diagnostics |> List.map diagnosticToDT |> Dval.list issueType
          "blockers", report.blockers |> List.map blockerToDT |> Dval.list issueType ]
    )


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "atRestCheckPackageOps" 0
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
      returnType = TCustomType(NR.ok (DarkTypes.reportName ()), [])
      description =
        "Checks candidate package declarations against their transitive dependency closure without persisting them."
      fn =
        (function
        | exeState, _, _, [| DList(_, ops) |] ->
          uply {
            try
              let decoded = ops |> List.map PT2DT.PackageOp.fromDT
              if decoded |> List.exists Option.isNone then
                return
                  unavailableReport "At-rest checker received an invalid package op"
                  |> DarkTypes.reportToDT
              else
                let ops = decoded |> List.choose (fun value -> value)
                let builtins = exeState.builtins
                let! report = checkPackageOps pm builtins ops
                return DarkTypes.reportToDT report
            with ex ->
              return
                unavailableReport $"At-rest checker unavailable: {ex.Message}"
                |> DarkTypes.reportToDT
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "atRestCheckBranch" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch to check" ]
      returnType = TCustomType(NR.ok (DarkTypes.reportName ()), [])
      description =
        "Checks every visible package declaration on a branch without persisting anything."
      fn =
        (function
        | exeState, _, _, [| DUuid branchId |] ->
          uply {
            try
              let! report = checkBranch pm exeState.builtins branchId
              return DarkTypes.reportToDT report
            with ex ->
              return
                unavailableReport $"At-rest branch check unavailable: {ex.Message}"
                |> DarkTypes.reportToDT
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]

let builtins (pm : PT.PackageManager) = LibExecution.Builtin.make [] (fns pm)
