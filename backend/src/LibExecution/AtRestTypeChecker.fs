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

// --------------------
// Traits: impl lookup for bounds, method calls and receiver calls
// --------------------

/// The dispatch head of a static type, or None while it is still an inference
/// variable. Mirrors `Traits.headOfKnownType` at runtime.
let private headOfStatic (typ : StaticType) : Option<string> =
  match typ with
  | TUnit -> Some "Unit"
  | TBool -> Some "Bool"
  | TInt8 -> Some "Int8"
  | TUInt8 -> Some "UInt8"
  | TInt16 -> Some "Int16"
  | TUInt16 -> Some "UInt16"
  | TInt32 -> Some "Int32"
  | TUInt32 -> Some "UInt32"
  | TInt64 -> Some "Int64"
  | TUInt64 -> Some "UInt64"
  | TInt128 -> Some "Int128"
  | TUInt128 -> Some "UInt128"
  | TInt -> Some "Int"
  | TFloat -> Some "Float"
  | TChar -> Some "Char"
  | TString -> Some "String"
  | TUuid -> Some "Uuid"
  | TDateTime -> Some "DateTime"
  | TBlob -> Some "Blob"
  | TStream _ -> Some "Stream"
  | TList _ -> Some "List"
  | TTuple _ -> Some "Tuple"
  | TDict _ -> Some "Dict"
  | TCustom(PT.Hash h, _) -> Some("custom:" + h)
  | TFn _ -> Some "Fn"
  | TDB _ -> Some "DB"
  | TRigidVariable name -> Some("rigid:" + name)
  | TInferenceVariable _ -> None

/// The dispatch head an impl is for; `None` for a blanket impl (`for 'a`).
let private headOfImplSelf (self : TypeReference) : Option<string> =
  match self with
  | TypeReference.TUnit -> Some "Unit"
  | TypeReference.TBool -> Some "Bool"
  | TypeReference.TInt8 -> Some "Int8"
  | TypeReference.TUInt8 -> Some "UInt8"
  | TypeReference.TInt16 -> Some "Int16"
  | TypeReference.TUInt16 -> Some "UInt16"
  | TypeReference.TInt32 -> Some "Int32"
  | TypeReference.TUInt32 -> Some "UInt32"
  | TypeReference.TInt64 -> Some "Int64"
  | TypeReference.TUInt64 -> Some "UInt64"
  | TypeReference.TInt128 -> Some "Int128"
  | TypeReference.TUInt128 -> Some "UInt128"
  | TypeReference.TInt -> Some "Int"
  | TypeReference.TFloat -> Some "Float"
  | TypeReference.TChar -> Some "Char"
  | TypeReference.TString -> Some "String"
  | TypeReference.TUuid -> Some "Uuid"
  | TypeReference.TDateTime -> Some "DateTime"
  | TypeReference.TBlob -> Some "Blob"
  | TypeReference.TStream _ -> Some "Stream"
  | TypeReference.TList _ -> Some "List"
  | TypeReference.TTuple _ -> Some "Tuple"
  | TypeReference.TDict _ -> Some "Dict"
  | TypeReference.TCustomType({ resolved = Ok { name = FQTypeName.Package(PT.Hash h) } }, _) ->
    Some("custom:" + h)
  | TypeReference.TCustomType _ -> Some "unresolved"
  | TypeReference.TFn _ -> Some "Fn"
  | TypeReference.TDB _ -> Some "DB"
  | TypeReference.TVariable _ -> None

/// The impls of a trait that apply to a concrete head: the specific ones, else the
/// blanket ones.
let private implsFor
  (state : State)
  (trait_ : FQTypeName.Package)
  (head : string)
  : List<ImplEntry> =
  let ofTrait =
    state.Environment.impls
    |> Map.toList
    |> List.map snd
    |> List.filter (fun e -> e.trait_ = trait_)
  match ofTrait |> List.filter (fun e -> headOfImplSelf e.self = Some head) with
  | [] -> ofTrait |> List.filter (fun e -> headOfImplSelf e.self = None)
  | specific -> specific

/// Discharge every bound the item accumulated, now that its substitutions are
/// known. A concrete type needs a visible impl; the item's own rigid type param
/// needs a declared bound; an inference variable is a blocker, not an error.
let private dischargeConstraints (state : State) : unit =
  for nodeId, trait_, typ, method_ in List.rev state.Constraints do
    let typ = applySubstitutions state typ
    let typ = normalizeAliases state nodeId Set.empty typ
    match typ with
    | TInferenceVariable _ ->
      if not (containsTaintedInferenceVariable state typ) then
        state.Block(AmbiguousType, nodeId, Ambiguous ConstrainedType)
    | TRigidVariable name ->
      let declared =
        state.DeclaredBounds
        |> List.exists (fun b ->
          b.param = name
          && (match b.trait_.trait_.resolved with
              | Ok { name = FQTypeName.Package t } -> t = trait_
              | _ -> false))
      if not declared then
        state.Error(UnboundTypeParameter, nodeId, None, Some typ, TraitNeeded(trait_, method_))
    | concrete ->
      match headOfStatic concrete with
      | None -> ()
      | Some head ->
        match implsFor state trait_ head with
        | [] -> state.Error(MissingImpl, nodeId, None, Some concrete, TraitNeeded(trait_, method_))
        | [ _ ] -> ()
        | _ -> state.Error(AmbiguousImpl, nodeId, None, Some concrete, TraitNeeded(trait_, method_))

/// `x.m` where `x` has no field `m`: the one visible impl, of any trait, with a
/// method `m` for `x`'s head types the access as that method with `x` consumed.
/// Mirrors the runtime's receiver call.
let private receiverMethodType
  (state : State)
  (nodeId : id)
  (receiverType : StaticType)
  (methodName : string)
  : Option<StaticType> =
  match headOfStatic receiverType with
  | None -> None
  | Some head ->
    let candidates =
      state.Environment.impls
      |> Map.toList
      |> List.map snd
      |> List.filter (fun e -> List.contains methodName e.methods)
      |> fun all ->
        match all |> List.filter (fun e -> headOfImplSelf e.self = Some head) with
        | [] -> all |> List.filter (fun e -> headOfImplSelf e.self = None)
        | specific -> specific
    match candidates with
    | [] -> None
    | [ entry ] ->
      match Map.tryFind entry.trait_ state.Environment.types with
      | Some { typeParams = selfParam :: rest; definition = TypeDeclaration.Record fields } ->
        fields
        |> NEList.toList
        |> List.tryPick (fun f ->
          match f.typ with
          | TypeReference.TFn(parameters, returnType) when f.name = methodName ->
            let vars =
              (selfParam, receiverType)
              :: (rest |> List.map (fun p -> p, state.Fresh(Some nodeId)))
              |> Map.ofList
            let paramTypes = parameters |> NEList.toList |> List.map (convertType state (Some nodeId) vars)
            let returnType = convertType state (Some nodeId) vars returnType
            match paramTypes with
            | self :: remaining ->
              unify state (Some nodeId) RecordFieldAccess self receiverType
              match remaining with
              | [] -> Some returnType
              | r :: rs -> Some(TFn(NEList.ofList r rs, returnType))
            | [] -> None
          | _ -> None)
      | _ -> None
    | several ->
      state.Error(
        AmbiguousImpl,
        Some nodeId,
        None,
        Some receiverType,
        TraitNeeded(several.Head.trait_, Some methodName)
      )
      None

let private resolvePendingFieldAccesses (state : State) : unit =
  for nodeId, recordType, fieldName, resultType in state.PendingFieldAccesses do
    let recordType = normalizeAliases state (Some nodeId) Set.empty recordType
    // A receiver call, when the type is known and no record field fits.
    let asReceiverCall () : bool =
      match receiverMethodType state nodeId recordType fieldName with
      | Some typ ->
        unify state (Some nodeId) RecordFieldAccess typ resultType
        true
      | None -> false
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
          if not (asReceiverCall ()) then
            state.Error(
              UnknownRecordField,
              Some nodeId,
              None,
              Some recordType,
              Identifier fieldName
            )
      | TypeDeclaration.Enum _
      | TypeDeclaration.Alias _ ->
        if not (asReceiverCall ()) then
          state.Error(
            TypeMismatch,
            Some nodeId,
            None,
            Some recordType,
            RecordRequiredForFieldAccess
          )
    | None ->
      // Not a custom type at all (`(5).show`), or not yet known.
      if not (asReceiverCall ()) then
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
  dischargeConstraints state
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
    state.DeclaredBounds <- fn.bounds
    for b in fn.bounds do
      match b.trait_.trait_.resolved with
      | Ok { name = FQTypeName.Package traitHash } -> state.AddDependency(TypeDependency traitHash)
      | _ -> ()
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
  (values : List<PackageValue.PackageValue>)
  (functions : List<PackageFn.PackageFn>)
  : TypeEnvironment =
  let withTypes =
    types
    |> List.fold
      (fun environment typ -> TypeEnvironment.addPackageType typ environment)
      baseEnvironment
  // An impl's instance value is an impl the batch's own callers can rely on,
  // before the value itself has been checked (its type is fixed by its shape).
  let withImpls =
    values
    |> List.fold
      (fun environment value -> TypeEnvironment.addImplIfValue value environment)
      withTypes
  functions
  |> List.fold
    (fun environment fn ->
      TypeEnvironment.addPackageFunctionSignature fn environment)
    withImpls

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
  let declaredEnvironment = addBatchDeclarations baseEnvironment types values functions
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
