/// Mutable checking state, conversion from `ProgramTypes` into the checker's type
/// language, and unification over it.
///
/// This is where a type stops being something the author wrote and becomes something
/// the checker can solve: aliases expand, declared type parameters stay rigid,
/// inference variables get substitutions, and the occurs check runs.
module LibExecution.AtRest.Unification

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

open LibExecution.AtRest.Types


// --------------------
// Checker state
// --------------------

type internal State(environment : TypeEnvironment) =
  let diagnostics = ResizeArray<Diagnostic>()
  let blockers = ResizeArray<Blocker>()
  let mutable dependencies : Set<Dependency> = Set.empty
  let mutable nextVar = 0
  let mutable substitutions : Map<int, StaticType> = Map.empty
  let mutable inferenceVariableOrigins : Map<int, Option<id>> = Map.empty
  let mutable taintedInferenceVariables : Set<int> = Set.empty
  let mutable pendingFieldAccesses : List<id * StaticType * string * StaticType> = []

  member _.Environment = environment
  member _.Diagnostics = diagnostics
  member _.Blockers = blockers
  member _.Dependencies = dependencies
  member _.PendingFieldAccesses
    with get () = pendingFieldAccesses
    and set value = pendingFieldAccesses <- value
  member _.Substitutions
    with get () = substitutions
    and set value = substitutions <- value

  member _.InferenceVariableOrigin(variable : int) : Option<id> =
    Map.tryFind variable inferenceVariableOrigins |> Option.flatten

  member _.IsTainted(variable : int) : bool =
    Set.contains variable taintedInferenceVariables

  member _.MarkTainted(typ : StaticType) : unit =
    let rec collect typ =
      match typ with
      | TInferenceVariable variable -> Set.singleton variable
      | TStream inner
      | TList inner
      | TDict inner
      | TDB inner -> collect inner
      | TTuple(first, second, rest) ->
        Set.unionMany
          [ collect first
            collect second
            rest |> List.map collect |> Set.unionMany ]
      | TCustom(_, args) -> args |> List.map collect |> Set.unionMany
      | TFn(args, returnType) ->
        Set.union
          (args |> NEList.toList |> List.map collect |> Set.unionMany)
          (collect returnType)
      | _ -> Set.empty
    taintedInferenceVariables <- Set.union taintedInferenceVariables (collect typ)

  member _.Fresh(nodeId : Option<id>) : StaticType =
    let result = TInferenceVariable nextVar
    inferenceVariableOrigins <- Map.add nextVar nodeId inferenceVariableOrigins
    nextVar <- nextVar + 1
    result

  member this.FreshTainted(nodeId : Option<id>) : StaticType =
    let result = this.Fresh nodeId
    this.MarkTainted result
    result

  member _.AddDependency(dependency : Dependency) : unit =
    dependencies <- Set.add dependency dependencies

  member _.Error
    (
      code : DiagnosticCode,
      nodeId : Option<id>,
      expected : Option<StaticType>,
      actual : Option<StaticType>,
      context : Context
    ) : unit =
    diagnostics.Add
      { code = code
        nodeId = nodeId
        expected = expected
        actual = actual
        context = context }

  member _.Block(code : BlockerCode, nodeId : Option<id>, context : Context) : unit =
    blockers.Add { code = code; nodeId = nodeId; context = context }


// --------------------
// Conversion and unification
// --------------------

/// Probe before recursive walks so deeply nested persisted input produces a catchable
/// `InsufficientExecutionStackException`, which `guardingStack` below converts to an
/// `Incomplete` verdict, rather than a process-ending stack overflow that would take
/// the whole process with it. `ProgramTypesToDarkTypes.ensureSufficientExecutionStack`
/// is the same technique on the deserialization side.
let internal ensureStack () : unit =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

let private tooDeepBlocker (nodeId : Option<id>) : Blocker =
  { code = UnsupportedConstruct; nodeId = nodeId; context = DeclarationTooDeep }

let internal guardingStack
  (nodeId : Option<id>)
  (check : unit -> Verdict)
  : Verdict =
  try
    check ()
  with :? System.InsufficientExecutionStackException ->
    Incomplete
      { inferredType = None
        diagnostics = []
        blockers = [ tooDeepBlocker nodeId ]
        dependencies = Set.empty }


let internal resolvedName (name : NameResolution<'a>) : Option<'a> =
  match name.resolved with
  | Ok resolved -> Some resolved.name
  | Error _ -> None

let rec internal convertType
  (state : State)
  (nodeId : Option<id>)
  (typeVariables : Map<string, StaticType>)
  (typ : TypeReference)
  : StaticType =
  ensureStack ()
  let recurse = convertType state nodeId typeVariables
  match typ with
  | TypeReference.TUnit -> TUnit
  | TypeReference.TBool -> TBool
  | TypeReference.TInt8 -> TInt8
  | TypeReference.TUInt8 -> TUInt8
  | TypeReference.TInt16 -> TInt16
  | TypeReference.TUInt16 -> TUInt16
  | TypeReference.TInt32 -> TInt32
  | TypeReference.TUInt32 -> TUInt32
  | TypeReference.TInt64 -> TInt64
  | TypeReference.TUInt64 -> TUInt64
  | TypeReference.TInt128 -> TInt128
  | TypeReference.TUInt128 -> TUInt128
  | TypeReference.TInt -> TInt
  | TypeReference.TFloat -> TFloat
  | TypeReference.TChar -> TChar
  | TypeReference.TString -> TString
  | TypeReference.TUuid -> TUuid
  | TypeReference.TDateTime -> TDateTime
  | TypeReference.TBlob -> TBlob
  | TypeReference.TStream inner -> TStream(recurse inner)
  | TypeReference.TList inner -> TList(recurse inner)
  | TypeReference.TTuple(first, second, rest) ->
    TTuple(recurse first, recurse second, List.map recurse rest)
  | TypeReference.TDict inner -> TDict(recurse inner)
  | TypeReference.TFn(args, ret) -> TFn(NEList.map recurse args, recurse ret)
  | TypeReference.TDB inner -> TDB(recurse inner)
  | TypeReference.TVariable name ->
    match Map.tryFind name typeVariables with
    | Some typ -> typ
    | None ->
      state.Block(UnknownDeclaredTypeVariable, nodeId, Identifier name)
      state.FreshTainted nodeId
  | TypeReference.TCustomType(name, args) ->
    match resolvedName name with
    | Some(FQTypeName.Package packageName) ->
      state.AddDependency(TypeDependency packageName)
      TCustom(packageName, List.map recurse args)
    | None ->
      state.Block(UnresolvedTypeName, nodeId, Unresolved name.originalName)
      state.FreshTainted nodeId

let rec internal applySubstitutions (state : State) (typ : StaticType) : StaticType =
  ensureStack ()
  let recurse = applySubstitutions state
  match typ with
  | TInferenceVariable var ->
    match Map.tryFind var state.Substitutions with
    | None -> typ
    | Some replacement ->
      let replacement = recurse replacement
      state.Substitutions <- Map.add var replacement state.Substitutions
      replacement
  | TStream inner -> TStream(recurse inner)
  | TList inner -> TList(recurse inner)
  | TTuple(first, second, rest) ->
    TTuple(recurse first, recurse second, List.map recurse rest)
  | TDict inner -> TDict(recurse inner)
  | TCustom(name, args) -> TCustom(name, List.map recurse args)
  | TFn(args, ret) -> TFn(NEList.map recurse args, recurse ret)
  | TDB inner -> TDB(recurse inner)
  | _ -> typ

let rec internal containsTaintedInferenceVariable
  (state : State)
  (typ : StaticType)
  : bool =
  ensureStack ()
  let recurse = containsTaintedInferenceVariable state
  match typ with
  | TInferenceVariable variable ->
    state.IsTainted variable
    || (Map.tryFind variable state.Substitutions |> Option.exists recurse)
  | TStream inner
  | TList inner
  | TDict inner
  | TDB inner -> recurse inner
  | TTuple(first, second, rest) ->
    recurse first || recurse second || List.exists recurse rest
  | TCustom(_, args) -> List.exists recurse args
  | TFn(args, returnType) ->
    List.exists recurse (NEList.toList args) || recurse returnType
  | _ -> false

let rec private containsInferenceVar
  (state : State)
  (needle : int)
  (typ : StaticType)
  : bool =
  ensureStack ()
  let typ = applySubstitutions state typ
  let recurse = containsInferenceVar state needle
  match typ with
  | TInferenceVariable var -> var = needle
  | TStream inner
  | TList inner
  | TDict inner
  | TDB inner -> recurse inner
  | TTuple(first, second, rest) ->
    recurse first || recurse second || List.exists recurse rest
  | TCustom(_, args) -> List.exists recurse args
  | TFn(args, ret) -> List.exists recurse (NEList.toList args) || recurse ret
  | _ -> false

let private expandAliasOnce
  (state : State)
  (nodeId : Option<id>)
  (seen : Set<FQTypeName.Package>)
  (typ : StaticType)
  : Option<StaticType * Set<FQTypeName.Package>> =
  match typ with
  | TCustom(name, args) ->
    match Map.tryFind name state.Environment.types with
    | None ->
      state.Block(MissingTypeDeclaration, nodeId, TypeUnavailable name)
      None
    | Some declaration ->
      if List.length declaration.typeParams <> List.length args then
        let expected = List.length declaration.typeParams
        let actual = List.length args
        state.Error(
          TypeMismatch,
          nodeId,
          None,
          Some typ,
          TypeArity(name, expected, actual)
        )
        None
      else
        match declaration.definition with
        | TypeDeclaration.Alias target ->
          if Set.contains name seen then
            state.Block(AliasCycle, nodeId, AliasCycleReferenced)
            None
          else
            let mapping = List.zip declaration.typeParams args |> Map.ofList
            let target = convertType state nodeId mapping target
            Some(target, Set.add name seen)
        | TypeDeclaration.Record _
        | TypeDeclaration.Enum _ -> None
  | _ -> None

let rec internal normalizeAliases
  (state : State)
  (nodeId : Option<id>)
  (seen : Set<FQTypeName.Package>)
  (typ : StaticType)
  : StaticType =
  ensureStack ()
  let typ = applySubstitutions state typ
  match expandAliasOnce state nodeId seen typ with
  | Some(expanded, seen) -> normalizeAliases state nodeId seen expanded
  | None -> typ

/// Validate every component reachable from a type. Declaration parameters remain
/// rigid; nominal record/enum recursion stops after one structural visit, while
/// transparent alias cycles remain errors.
let rec internal validateTypeClosureFrom
  (state : State)
  (nodeId : Option<id>)
  (seenAliases : Set<FQTypeName.Package>)
  (seenDeclarations : Set<FQTypeName.Package>)
  (typ : StaticType)
  : unit =
  ensureStack ()
  let typ = applySubstitutions state typ
  let recurse = validateTypeClosureFrom state nodeId seenAliases seenDeclarations
  match typ with
  | TStream inner
  | TList inner
  | TDict inner
  | TDB inner -> recurse inner
  | TTuple(first, second, rest) ->
    recurse first
    recurse second
    rest |> List.iter recurse
  | TCustom(name, args) ->
    args |> List.iter recurse
    match Map.tryFind name state.Environment.types with
    | None -> state.Block(MissingTypeDeclaration, nodeId, TypeUnavailable name)
    | Some declaration ->
      if List.length declaration.typeParams <> List.length args then
        let expected = List.length declaration.typeParams
        let actual = List.length args
        state.Error(
          TypeMismatch,
          nodeId,
          None,
          Some typ,
          TypeArity(name, expected, actual)
        )
      else
        match declaration.definition with
        | TypeDeclaration.Alias target ->
          if Set.contains name seenAliases then
            state.Block(AliasCycle, nodeId, AliasCycleReferenced)
          else
            let rigidParams =
              declaration.typeParams
              |> List.map (fun name -> name, TRigidVariable name)
              |> Map.ofList
            let target = convertType state nodeId rigidParams target
            validateTypeClosureFrom
              state
              nodeId
              (Set.add name seenAliases)
              seenDeclarations
              target
        | TypeDeclaration.Record fields ->
          if not (Set.contains name seenDeclarations) then
            let rigidParams =
              declaration.typeParams
              |> List.map (fun name -> name, TRigidVariable name)
              |> Map.ofList
            let validateField (field : TypeDeclaration.RecordField) : unit =
              let fieldType = convertType state nodeId rigidParams field.typ
              validateTypeClosureFrom
                state
                nodeId
                Set.empty
                (Set.add name seenDeclarations)
                fieldType
            fields |> NEList.iter validateField
        | TypeDeclaration.Enum cases ->
          if not (Set.contains name seenDeclarations) then
            let rigidParams =
              declaration.typeParams
              |> List.map (fun name -> name, TRigidVariable name)
              |> Map.ofList
            let validateField (field : TypeDeclaration.EnumField) : unit =
              let fieldType = convertType state nodeId rigidParams field.typ
              validateTypeClosureFrom
                state
                nodeId
                Set.empty
                (Set.add name seenDeclarations)
                fieldType
            cases |> NEList.iter (fun case -> case.fields |> List.iter validateField)
  | TFn(parameters, returnType) ->
    parameters |> NEList.iter recurse
    recurse returnType
  | TUnit
  | TBool
  | TInt8
  | TUInt8
  | TInt16
  | TUInt16
  | TInt32
  | TUInt32
  | TInt64
  | TUInt64
  | TInt128
  | TUInt128
  | TInt
  | TFloat
  | TChar
  | TString
  | TUuid
  | TDateTime
  | TBlob
  | TRigidVariable _
  | TInferenceVariable _ -> ()

let internal validateTypeClosure
  (state : State)
  (nodeId : Option<id>)
  (typ : StaticType)
  : unit =
  validateTypeClosureFrom state nodeId Set.empty Set.empty typ

let private samePrimitive (left : StaticType) (right : StaticType) : bool =
  match left, right with
  | TUnit, TUnit
  | TBool, TBool
  | TInt8, TInt8
  | TUInt8, TUInt8
  | TInt16, TInt16
  | TUInt16, TUInt16
  | TInt32, TInt32
  | TUInt32, TUInt32
  | TInt64, TInt64
  | TUInt64, TUInt64
  | TInt128, TInt128
  | TUInt128, TUInt128
  | TInt, TInt
  | TFloat, TFloat
  | TChar, TChar
  | TString, TString
  | TUuid, TUuid
  | TDateTime, TDateTime
  | TBlob, TBlob -> true
  | _ -> false

let rec internal unify
  (state : State)
  (nodeId : Option<id>)
  (site : Site)
  (expected : StaticType)
  (actual : StaticType)
  : unit =
  ensureStack ()
  let involvesTaintedType =
    containsTaintedInferenceVariable state expected
    || containsTaintedInferenceVariable state actual
  let expected = normalizeAliases state nodeId Set.empty expected
  let actual = normalizeAliases state nodeId Set.empty actual
  match expected, actual with
  | TInferenceVariable left, TInferenceVariable right when left = right -> ()
  | TInferenceVariable var, replacement
  | replacement, TInferenceVariable var ->
    if containsInferenceVar state var replacement then
      if not involvesTaintedType then
        state.Error(OccursCheckFailed, nodeId, Some expected, Some actual, At site)
    else
      if involvesTaintedType then state.MarkTainted replacement
      state.Substitutions <- Map.add var replacement state.Substitutions
  | TRigidVariable left, TRigidVariable right when left = right -> ()
  | left, right when samePrimitive left right -> ()
  | TStream left, TStream right
  | TList left, TList right
  | TDict left, TDict right
  | TDB left, TDB right -> unify state nodeId site left right
  | TTuple(l1, l2, lr), TTuple(r1, r2, rr) when List.length lr = List.length rr ->
    unify state nodeId site l1 r1
    unify state nodeId site l2 r2
    List.iter2 (unify state nodeId site) lr rr
  | TCustom(leftName, leftArgs), TCustom(rightName, rightArgs) when
    leftName = rightName && List.length leftArgs = List.length rightArgs
    ->
    List.iter2 (unify state nodeId site) leftArgs rightArgs
  | TFn(leftArgs, leftRet), TFn(rightArgs, rightRet) when
    NEList.length leftArgs = NEList.length rightArgs
    ->
    List.iter2
      (unify state nodeId site)
      (NEList.toList leftArgs)
      (NEList.toList rightArgs)
    unify state nodeId site leftRet rightRet
  | _ ->
    if not involvesTaintedType then
      state.Error(TypeMismatch, nodeId, Some expected, Some actual, At site)

let internal typeVariables
  (state : State)
  (nodeId : Option<id>)
  (scope : Map<string, StaticType>)
  (names : List<string>)
  (explicitArgs : List<TypeReference>)
  : Map<string, StaticType> =
  if
    not (List.isEmpty explicitArgs) && List.length names <> List.length explicitArgs
  then
    state.Error(
      ExplicitTypeArgumentCountMismatch,
      nodeId,
      None,
      None,
      Arity(List.length names, List.length explicitArgs)
    )

  let args =
    if List.isEmpty explicitArgs then
      names |> List.map (fun _ -> state.Fresh nodeId)
    else
      let converted =
        explicitArgs
        |> List.truncate (List.length names)
        |> List.map (convertType state nodeId scope)
      converted
      @ List.init (max 0 (List.length names - List.length converted)) (fun _ ->
        state.Fresh nodeId)

  List.zip names args |> Map.ofList

type internal Env =
  { locals : Map<string, TypeScheme>
    arguments : List<StaticType>
    self : Option<StaticType>
    typeVariables : Map<string, StaticType> }

let rec internal inferenceVariables (typ : StaticType) : Set<int> =
  ensureStack ()
  let recurse = inferenceVariables
  match typ with
  | TInferenceVariable var -> Set.singleton var
  | TStream inner
  | TList inner
  | TDict inner
  | TDB inner -> recurse inner
  | TTuple(first, second, rest) ->
    Set.unionMany
      [ recurse first; recurse second; rest |> List.map recurse |> Set.unionMany ]
  | TCustom(_, args) -> args |> List.map recurse |> Set.unionMany
  | TFn(args, ret) ->
    Set.union
      (args |> NEList.toList |> List.map recurse |> Set.unionMany)
      (recurse ret)
  | _ -> Set.empty

let private freeVariablesInScheme (state : State) (scheme : TypeScheme) : Set<int> =
  Set.difference
    (applySubstitutions state scheme.typ |> inferenceVariables)
    scheme.quantified

let private freeVariablesInEnv (state : State) (env : Env) : Set<int> =
  env.locals
  |> Map.values
  |> Seq.map (freeVariablesInScheme state)
  |> Set.unionMany

let internal generalize (state : State) (env : Env) (typ : StaticType) : TypeScheme =
  let typ = applySubstitutions state typ
  let typeVariables = inferenceVariables typ
  let quantified = Set.difference typeVariables (freeVariablesInEnv state env)
  let capturedConstraints, remainingConstraints =
    state.PendingFieldAccesses
    |> List.partition (fun (_, recordType, _, resultType) ->
      let constraintVariables =
        Set.union
          (applySubstitutions state recordType |> inferenceVariables)
          (applySubstitutions state resultType |> inferenceVariables)
      not (Set.isEmpty constraintVariables)
      && Set.isSubset constraintVariables quantified)
  state.PendingFieldAccesses <- remainingConstraints
  { quantified = quantified; typ = typ; fieldConstraints = capturedConstraints }

let internal instantiateScheme
  (state : State)
  (nodeId : Option<id>)
  (scheme : TypeScheme)
  : StaticType =
  let replacements =
    scheme.quantified |> Seq.map (fun var -> var, state.Fresh nodeId) |> Map.ofSeq
  let rec replace typ =
    match typ with
    | TInferenceVariable var ->
      Map.tryFind var replacements |> Option.defaultValue typ
    | TStream inner -> TStream(replace inner)
    | TList inner -> TList(replace inner)
    | TTuple(first, second, rest) ->
      TTuple(replace first, replace second, List.map replace rest)
    | TDict inner -> TDict(replace inner)
    | TCustom(name, args) -> TCustom(name, List.map replace args)
    | TFn(args, ret) -> TFn(NEList.map replace args, replace ret)
    | TDB inner -> TDB(replace inner)
    | typ -> typ
  for nodeId, recordType, fieldName, resultType in scheme.fieldConstraints do
    state.PendingFieldAccesses <-
      (nodeId, replace recordType, fieldName, replace resultType)
      :: state.PendingFieldAccesses
  replace scheme.typ


// --------------------
// Expressions and patterns
// --------------------

let internal emptyEnv : Env =
  { locals = Map.empty; arguments = []; self = None; typeVariables = Map.empty }

let internal monomorphic (typ : StaticType) : TypeScheme =
  { quantified = Set.empty; typ = typ; fieldConstraints = [] }

let internal addSchemes (env : Env) (bindings : List<string * TypeScheme>) : Env =
  let locals =
    bindings
    |> List.filter (fun (name, _) -> name <> "_")
    |> List.fold (fun locals (name, typ) -> Map.add name typ locals) env.locals
  { env with locals = locals }
