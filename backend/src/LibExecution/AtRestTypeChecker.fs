/// Static checking for serialized ProgramTypes.
///
/// Storage-independent and conservative: only complete proofs are `Checked`; missing
/// information is `Incomplete`. This module is the entry point; the checker itself is
/// `LibExecution.AtRest.*`, and its vocabulary is `AtRest.Types`.
///
/// CLEANUP: this should eventually be Darklang, not F#. It is F# for throughput (it
/// runs on every save, on every commit, and over the whole package corpus for
/// `typecheck`), which is an argument from the shape of the work rather than a
/// measurement. The reason to move it is that today the set of at-rest checks is fixed:
/// people should be able to choose which checks apply to their packages and write their
/// own, and that needs the checks to be ordinary Darklang code rather than an F# module
/// with a builtin in front of it. See docs/at-rest-type-checker.md.
module LibExecution.AtRestTypeChecker

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

open LibExecution.AtRest.Types
open LibExecution.AtRest.Unification
open LibExecution.AtRest.Patterns
open LibExecution.AtRest.Inference


// --------------------
// Public checking API
// --------------------

let private resolvePendingFieldAccesses (state : State) : unit =
  for nodeId, recordType, fieldName, resultType in state.PendingFieldAccesses do
    let recordType = normalizeAliases state (Some nodeId) Set.empty recordType
    match declarationForCustom state (Some nodeId) recordType with
    | Some(_, typeArgs, declaration) ->
      match declaration.definition with
      | TypeDeclaration.Record fields ->
        match
          fields
          |> NEList.toList
          |> List.tryFind (fun field -> field.name = fieldName)
        with
        | Some field ->
          let fieldType =
            declarationFieldType
              state
              (Some nodeId)
              declaration.typeParams
              typeArgs
              field.typ
          unify state (Some nodeId) RecordFieldAccess fieldType resultType
        | None ->
          state.Error(
            UnknownRecordField,
            Some nodeId,
            None,
            Some recordType,
            Identifier fieldName
          )
      | TypeDeclaration.Enum _
      | TypeDeclaration.Alias _ ->
        state.Error(
          TypeMismatch,
          Some nodeId,
          None,
          Some recordType,
          RecordRequiredForFieldAccess
        )
    | None ->
      if not (containsTaintedInferenceVariable state recordType) then
        state.Block(AmbiguousType, Some nodeId, Ambiguous RecordType)

let private displayType (scheme : TypeScheme) : StaticType =
  let names =
    scheme.quantified
    |> Set.toList
    |> List.mapi (fun index var -> var, TRigidVariable $"t{index + 1}")
    |> Map.ofList
  let rec replace typ =
    match typ with
    | TInferenceVariable var -> Map.tryFind var names |> Option.defaultValue typ
    | TStream inner -> TStream(replace inner)
    | TList inner -> TList(replace inner)
    | TTuple(first, second, rest) ->
      TTuple(replace first, replace second, List.map replace rest)
    | TDict(key, value) -> TDict(replace key, replace value)
    | TCustom(name, args) -> TCustom(name, List.map replace args)
    | TFn(args, ret) -> TFn(NEList.map replace args, replace ret)
    | TDB inner -> TDB(replace inner)
    | typ -> typ
  replace scheme.typ

let private finish
  (state : State)
  (nodeId : Option<id>)
  (scheme : TypeScheme)
  : Verdict =
  resolvePendingFieldAccesses state
  let scheme = { scheme with typ = applySubstitutions state scheme.typ }
  let inferredType = displayType scheme
  // Only inference variables observable in the item's type or diagnostics weaken
  // the proof. Variables confined to discarded polymorphic results do not.
  let variablesInDiagnostic (diagnostic : Diagnostic) : Set<int> =
    [ diagnostic.expected; diagnostic.actual ]
    |> List.collect Option.toList
    |> List.map (applySubstitutions state >> inferenceVariables)
    |> Set.unionMany
  let diagnosticIsDefinite (diagnostic : Diagnostic) : bool =
    // A diagnostic with unresolved, unquantified variables is provisional;
    // solving them may remove the apparent mismatch.
    variablesInDiagnostic diagnostic
    |> fun variables -> Set.difference variables scheme.quantified
    |> Set.isEmpty
  let unsolvedVariables =
    Set.union
      (inferenceVariables scheme.typ)
      (state.Diagnostics |> Seq.map variablesInDiagnostic |> Set.unionMany)
    |> fun variables -> Set.difference variables scheme.quantified
    |> Set.filter (state.IsTainted >> not)
    |> Set.toList
  unsolvedVariables
  |> List.fold
    (fun origins var ->
      let origin = state.InferenceVariableOrigin var |> Option.orElse nodeId
      Map.change
        origin
        (fun vars -> Some(var :: Option.defaultValue [] vars))
        origins)
    Map.empty
  |> Map.iter (fun origin _variables ->
    state.Block(AmbiguousType, origin, Ambiguous ItemType))
  let report =
    { inferredType = Some inferredType
      diagnostics = List.ofSeq state.Diagnostics
      blockers = List.ofSeq state.Blockers
      dependencies = state.Dependencies }
  if List.exists diagnosticIsDefinite report.diagnostics then
    Failed report
  elif not (List.isEmpty report.blockers) then
    Incomplete report
  elif not (List.isEmpty report.diagnostics) then
    Failed report
  else
    Checked
      { inferredType = inferredType
        scheme = scheme
        dependencies = state.Dependencies }

let checkExpression (environment : TypeEnvironment) (expr : Expr) : Verdict =
  guardingStack (Some(Expr.toID expr)) (fun () ->
    let state = State environment
    let typ = inferExpr state emptyEnv expr
    finish state (Some(Expr.toID expr)) (monomorphic typ))

let private checkInferredPackageValue
  (environment : TypeEnvironment)
  (value : PackageValue.PackageValue)
  : Verdict =
  guardingStack (Some(Expr.toID value.body)) (fun () ->
    let state = State environment
    let inferred = inferExpr state emptyEnv value.body
    let scheme =
      if isNonExpansive value.body then
        generalize state emptyEnv inferred
      else
        monomorphic inferred
    finish state (Some(Expr.toID value.body)) scheme)

let checkPackageFunction
  (environment : TypeEnvironment)
  (fn : PackageFn.PackageFn)
  : Verdict =
  guardingStack (Some(Expr.toID fn.body)) (fun () ->
    let state = State environment
    for name in duplicateNames fn.typeParams do
      state.Error(
        DuplicateTypeParameter,
        None,
        None,
        None,
        Duplicate(name, InFunctionSignature)
      )
    let rigidVars =
      fn.typeParams
      |> List.map (fun name -> name, TRigidVariable name)
      |> Map.ofList
    let parameters =
      fn.parameters
      |> NEList.map (fun parameter ->
        convertType state None rigidVars parameter.typ)
    let returnType = convertType state None rigidVars fn.returnType
    let selfType = TFn(parameters, returnType)
    validateTypeClosure state None selfType
    let env =
      { locals =
          List.zip
            (fn.parameters |> NEList.toList |> List.map _.name)
            (NEList.toList parameters)
          |> List.map (fun (name, typ) -> name, monomorphic typ)
          |> Map.ofList
        arguments = NEList.toList parameters
        self = Some selfType
        typeVariables = rigidVars }
    checkExprWithContext state env returnType fn.body FunctionReturnValue
    finish state (Some(Expr.toID fn.body)) (monomorphic selfType))

let checkPackageValue
  (environment : TypeEnvironment)
  (expectedType : TypeReference)
  (value : PackageValue.PackageValue)
  : Verdict =
  guardingStack (Some(Expr.toID value.body)) (fun () ->
    let state = State environment
    let expectedType = convertType state None Map.empty expectedType
    validateTypeClosure state None expectedType
    checkExprWithContext state emptyEnv expectedType value.body ValueBody
    finish state (Some(Expr.toID value.body)) (monomorphic expectedType))



let private validateTypeDeclaration
  (environment : TypeEnvironment)
  (packageType : PackageType.PackageType)
  : Verdict =
  guardingStack None (fun () ->
    let state = State environment
    let declaration = packageType.declaration
    for name in duplicateNames declaration.typeParams do
      state.Error(
        DuplicateTypeParameter,
        None,
        None,
        None,
        Duplicate(name, InTypeDeclaration)
      )

    let rigidVars =
      declaration.typeParams
      |> List.map (fun name -> name, TRigidVariable name)
      |> Map.ofList
    let validateReference (typ : TypeReference) : unit =
      let typ = convertType state None rigidVars typ
      // This declaration's own fields/cases are visited by the loop below. Seed it
      // as structurally seen so a legal recursive record or enum terminates without
      // walking the same declaration twice.
      validateTypeClosureFrom
        state
        None
        Set.empty
        (Set.singleton packageType.hash)
        typ

    match declaration.definition with
    | TypeDeclaration.Alias typ -> validateReference typ
    | TypeDeclaration.Record fields ->
      for name in fields |> NEList.toList |> List.map _.name |> duplicateNames do
        state.Error(
          DuplicateTypeMember,
          None,
          None,
          None,
          Duplicate(name, InTypeDeclaration)
        )
      fields |> NEList.iter (fun field -> validateReference field.typ)
    | TypeDeclaration.Enum cases ->
      for name in cases |> NEList.toList |> List.map _.name |> duplicateNames do
        state.Error(
          DuplicateTypeMember,
          None,
          None,
          None,
          Duplicate(name, InTypeDeclaration)
        )
      cases
      |> NEList.iter (fun case ->
        case.fields |> List.iter (fun field -> validateReference field.typ))
    finish state None (monomorphic TUnit))

let private addBatchDeclarations
  (baseEnvironment : TypeEnvironment)
  (types : List<PackageType.PackageType>)
  (functions : List<PackageFn.PackageFn>)
  : TypeEnvironment =
  let withTypes =
    types
    |> List.fold
      (fun environment typ -> TypeEnvironment.addPackageType typ environment)
      baseEnvironment
  functions
  |> List.fold
    (fun environment fn ->
      TypeEnvironment.addPackageFunctionSignature fn environment)
    withTypes

let private checkValuesInDependencyOrder
  (initialEnvironment : TypeEnvironment)
  (values : List<PackageValue.PackageValue>)
  : TypeEnvironment * List<ItemVerdict> =
  let rec loop
    (environment : TypeEnvironment)
    (completed : Map<FQValueName.Package, Verdict>)
    (pending : List<PackageValue.PackageValue>)
    : TypeEnvironment * Map<FQValueName.Package, Verdict> =
    let environment, newlyCompleted, stillPending =
      pending
      |> List.fold
        (fun (environment, completed, pending) value ->
          match checkInferredPackageValue environment value with
          | Checked proof as verdict ->
            let environment =
              TypeEnvironment.addCheckedValue
                (FQValueName.Package value.hash)
                proof
                environment
            environment, Map.add value.hash verdict completed, pending
          | Failed _ as verdict ->
            environment, Map.add value.hash verdict completed, pending
          | Incomplete _ -> environment, completed, value :: pending)
        (environment, Map.empty, [])

    match Map.isEmpty newlyCompleted, stillPending with
    | true, remaining ->
      let completed =
        remaining
        |> List.fold
          (fun completed value ->
            Map.add
              value.hash
              (checkInferredPackageValue environment value)
              completed)
          completed
      environment, completed
    | false, [] ->
      environment,
      Map.fold
        (fun completed hash verdict -> Map.add hash verdict completed)
        completed
        newlyCompleted
    | false, _ ->
      let completed =
        Map.fold
          (fun completed hash verdict -> Map.add hash verdict completed)
          completed
          newlyCompleted
      loop environment completed (List.rev stillPending)

  let environment, verdicts = loop initialEnvironment Map.empty values
  let ordered =
    values
    |> List.map (fun value ->
      { item = Reference.PackageValue value.hash; verdict = verdicts[value.hash] })
  environment, ordered

/// Check a closed package batch against a base environment. Types and function
/// signatures are predeclared, making declaration order irrelevant. Values are
/// inferred in dependency order; recursive value groups remain `Incomplete`.
let checkPackageBatch
  (baseEnvironment : TypeEnvironment)
  (types : List<PackageType.PackageType>)
  (values : List<PackageValue.PackageValue>)
  (functions : List<PackageFn.PackageFn>)
  : BatchResult =
  let declaredEnvironment = addBatchDeclarations baseEnvironment types functions
  let typeResults =
    types
    |> List.map (fun typ ->
      { item = Reference.PackageType typ.hash
        verdict = validateTypeDeclaration declaredEnvironment typ })
  let environment, valueResults =
    checkValuesInDependencyOrder declaredEnvironment values
  let functionResults =
    functions
    |> List.map (fun fn ->
      { item = Reference.PackageFn fn.hash
        verdict = checkPackageFunction environment fn })
  { environment = environment
    types = typeResults
    values = valueResults
    functions = functionResults }
