/// Static checking for serialized ProgramTypes.
///
/// This module is intentionally independent of storage, package operations, and the
/// interpreter. Callers construct an immutable TypeEnvironment and receive a
/// conservative verdict: only complete proofs are Checked; missing information is
/// Incomplete.
module LibExecution.AtRestTypeChecker

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes


// --------------------
// Public model
// --------------------

type StaticType =
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
  | TStream of StaticType
  | TList of StaticType
  | TTuple of StaticType * StaticType * List<StaticType>
  | TDict of StaticType
  | TCustom of FQTypeName.Package * List<StaticType>
  | TFn of NEList<StaticType> * StaticType
  | TDB of StaticType
  | TRigidVar of string
  | TInferenceVar of int

type Dependency =
  | TypeDependency of FQTypeName.Package
  | FunctionDependency of FQFnName.FQFnName
  | ValueDependency of FQValueName.FQValueName

type private TypeScheme =
  { quantified : Set<int>
    typ : StaticType
    fieldConstraints : List<id * StaticType * string * StaticType> }

type Proof =
  private
    { inferredType : StaticType
      scheme : TypeScheme
      dependencies : Set<Dependency> }

type DiagnosticCode =
  | TypeMismatch
  | OccursCheckFailed
  | UnknownVariable
  | InvalidArgumentIndex
  | NotCallable
  | ExplicitTypeArgumentCountMismatch
  | InvalidPattern
  | DuplicatePatternBinding
  | UnknownRecordField
  | MissingRecordField
  | DuplicateRecordField
  | UnknownEnumCase
  | EnumFieldCountMismatch
  | InvalidInfixOperand
  | DuplicateTypeParameter
  | DuplicateTypeMember

type BlockerCode =
  | UnresolvedTypeName
  | UnresolvedFunctionName
  | UnresolvedValueName
  | MissingTypeDeclaration
  | MissingFunctionSignature
  | MissingValueSignature
  | AliasCycle
  | UnknownDeclaredTypeVariable
  | AmbiguousType
  | NonExhaustiveMatch
  | UnsupportedConstruct

type Diagnostic =
  { code : DiagnosticCode
    nodeId : Option<id>
    expected : Option<StaticType>
    actual : Option<StaticType>
    context : string }

type Blocker = { code : BlockerCode; nodeId : Option<id>; context : string }

type FunctionSignature =
  { typeParams : List<string>
    parameters : NEList<TypeReference>
    returnType : TypeReference }

type TypeEnvironmentBuildError = BuiltinFunctionHasNoParameters of FQFnName.Builtin

let rec private runtimeTypeToProgramType (typ : RT.TypeReference) : TypeReference =
  let recurse = runtimeTypeToProgramType
  match typ with
  | RT.TUnit -> TypeReference.TUnit
  | RT.TBool -> TypeReference.TBool
  | RT.TInt8 -> TypeReference.TInt8
  | RT.TUInt8 -> TypeReference.TUInt8
  | RT.TInt16 -> TypeReference.TInt16
  | RT.TUInt16 -> TypeReference.TUInt16
  | RT.TInt32 -> TypeReference.TInt32
  | RT.TUInt32 -> TypeReference.TUInt32
  | RT.TInt64 -> TypeReference.TInt64
  | RT.TUInt64 -> TypeReference.TUInt64
  | RT.TInt128 -> TypeReference.TInt128
  | RT.TUInt128 -> TypeReference.TUInt128
  | RT.TInt -> TypeReference.TInt
  | RT.TFloat -> TypeReference.TFloat
  | RT.TChar -> TypeReference.TChar
  | RT.TString -> TypeReference.TString
  | RT.TUuid -> TypeReference.TUuid
  | RT.TDateTime -> TypeReference.TDateTime
  | RT.TBlob -> TypeReference.TBlob
  | RT.TStream inner -> TypeReference.TStream(recurse inner)
  | RT.TList inner -> TypeReference.TList(recurse inner)
  | RT.TTuple(first, second, rest) ->
    TypeReference.TTuple(recurse first, recurse second, List.map recurse rest)
  | RT.TDict inner -> TypeReference.TDict(recurse inner)
  | RT.TFn(args, ret) -> TypeReference.TFn(NEList.map recurse args, recurse ret)
  | RT.TVariable name -> TypeReference.TVariable name
  | RT.TDB inner -> TypeReference.TDB(recurse inner)
  | RT.TCustomType(name, args) ->
    let name : NameResolution<FQTypeName.FQTypeName> =
      { originalName = name.originalName
        resolved =
          match name.resolved with
          | Ok(RT.FQTypeName.Package(RT.Hash hash)) ->
            Ok
              { name = FQTypeName.Package(FQTypeName.package hash); location = None }
          | Error RT.NameResolutionError.NotFound ->
            Error NameResolutionError.NotFound
          | Error RT.NameResolutionError.InvalidName ->
            Error NameResolutionError.InvalidName }
    TypeReference.TCustomType(name, List.map recurse args)

let rec private runtimeTypeVariables (typ : RT.TypeReference) : Set<string> =
  let recurse = runtimeTypeVariables
  match typ with
  | RT.TVariable name -> Set.singleton name
  | RT.TStream inner
  | RT.TList inner
  | RT.TDict inner
  | RT.TDB inner -> recurse inner
  | RT.TTuple(first, second, rest) ->
    Set.unionMany
      [ recurse first; recurse second; rest |> List.map recurse |> Set.unionMany ]
  | RT.TFn(args, ret) ->
    Set.union
      (args |> NEList.toList |> List.map recurse |> Set.unionMany)
      (recurse ret)
  | RT.TCustomType(_, args) -> args |> List.map recurse |> Set.unionMany
  | RT.TUnit
  | RT.TBool
  | RT.TInt8
  | RT.TUInt8
  | RT.TInt16
  | RT.TUInt16
  | RT.TInt32
  | RT.TUInt32
  | RT.TInt64
  | RT.TUInt64
  | RT.TInt128
  | RT.TUInt128
  | RT.TInt
  | RT.TFloat
  | RT.TChar
  | RT.TString
  | RT.TUuid
  | RT.TDateTime
  | RT.TBlob -> Set.empty

type TypeEnvironment =
  private
    { types : Map<FQTypeName.Package, TypeDeclaration.T>
      functions : Map<FQFnName.FQFnName, FunctionSignature>
      unsupportedFunctions : Map<FQFnName.FQFnName, string>
      values : Map<FQValueName.FQValueName, TypeReference>
      checkedValues : Map<FQValueName.FQValueName, TypeScheme> }

module TypeEnvironment =
  let empty : TypeEnvironment =
    { types = Map.empty
      functions = Map.empty
      unsupportedFunctions = Map.empty
      values = Map.empty
      checkedValues = Map.empty }

  let addType
    (name : FQTypeName.Package)
    (declaration : TypeDeclaration.T)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    { environment with types = Map.add name declaration environment.types }

  let addFunction
    (name : FQFnName.FQFnName)
    (signature : FunctionSignature)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    { environment with functions = Map.add name signature environment.functions }

  let private addUnsupportedFunction
    (name : FQFnName.FQFnName)
    (reason : string)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    { environment with
        unsupportedFunctions = Map.add name reason environment.unsupportedFunctions }

  let addValue
    (name : FQValueName.FQValueName)
    (typ : TypeReference)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    { environment with values = Map.add name typ environment.values }

  let addCheckedValue
    (name : FQValueName.FQValueName)
    (proof : Proof)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    { environment with
        checkedValues = Map.add name proof.scheme environment.checkedValues }

  let addPackageType
    (typ : PackageType.PackageType)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    addType typ.hash typ.declaration environment

  let addPackageFunctionSignature
    (fn : PackageFn.PackageFn)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    let signature =
      { typeParams = fn.typeParams
        parameters = fn.parameters |> NEList.map (fun p -> p.typ)
        returnType = fn.returnType }
    addFunction (FQFnName.Package fn.hash) signature environment

  /// Add the runtime's builtin signatures without importing executable bodies into
  /// the checker. Invalid zero-argument builtins are reported structurally rather
  /// than throwing while a environment is assembled.
  let addBuiltins
    (builtins : RT.Builtins)
    (environment : TypeEnvironment)
    : Result<TypeEnvironment, List<TypeEnvironmentBuildError>> =
    let environment =
      builtins.values.Values
      |> Seq.fold
        (fun environment value ->
          let name =
            FQValueName.Builtin
              { name = value.name.name; version = value.name.version }
          addValue name (runtimeTypeToProgramType value.typ) environment)
        environment

    let environment, errors =
      builtins.fns.Values
      |> Seq.fold
        (fun (environment, errors) fn ->
          let builtinName : FQFnName.Builtin =
            { name = fn.name.name; version = fn.name.version }
          match fn.parameters with
          | [] -> environment, BuiltinFunctionHasNoParameters builtinName :: errors
          | first :: rest ->
            let name = FQFnName.Builtin builtinName
            let parameterVariables =
              fn.parameters
              |> List.map (fun parameter -> runtimeTypeVariables parameter.typ)
              |> Set.unionMany
            let returnVariables = runtimeTypeVariables fn.returnType
            if not (Set.isSubset returnVariables parameterVariables) then
              // A result variable no parameter constrains (`Hash -> Option<'a>`)
              // is not a universal result; it means the result type is only known
              // at runtime. Quantifying it would prove any caller's use of it.
              addUnsupportedFunction
                name
                "The declared result contains type variables unconstrained by the parameters"
                environment,
              errors
            else
              // Many older builtin declarations predate explicit typeParams.
              // Quantify their structural variables here.
              let undeclared =
                Set.union parameterVariables returnVariables
                |> Set.filter (fun name -> not (List.contains name fn.typeParams))
                |> Set.toList
              let signature =
                { typeParams = fn.typeParams @ undeclared
                  parameters =
                    NEList.ofList
                      (runtimeTypeToProgramType first.typ)
                      (rest
                       |> List.map (fun parameter ->
                         runtimeTypeToProgramType parameter.typ))
                  returnType = runtimeTypeToProgramType fn.returnType }
              addFunction name signature environment, errors)
        (environment, [])
    match errors with
    | [] -> Ok environment
    | errors -> Error(List.rev errors)

type Report =
  { inferredType : Option<StaticType>
    diagnostics : List<Diagnostic>
    blockers : List<Blocker>
    dependencies : Set<Dependency> }

type Verdict =
  | Checked of Proof
  | Failed of Report
  | Incomplete of Report

module Proof =
  let inferredType (proof : Proof) : StaticType = proof.inferredType
  let dependencies (proof : Proof) : Set<Dependency> = proof.dependencies


// --------------------
// Checker state
// --------------------

type private State(environment : TypeEnvironment) =
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
      | TInferenceVar variable -> Set.singleton variable
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
    let result = TInferenceVar nextVar
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
      context : string
    ) : unit =
    diagnostics.Add
      { code = code
        nodeId = nodeId
        expected = expected
        actual = actual
        context = context }

  member _.Block(code : BlockerCode, nodeId : Option<id>, context : string) : unit =
    blockers.Add { code = code; nodeId = nodeId; context = context }


// --------------------
// Conversion and unification
// --------------------

/// Deeply nested input is the one thing a traversal cannot recover from on its
/// own: a stack overflow kills the process, and `try ... with` never sees it.
/// Every recursive walk in this file probes the stack first; when there is not
/// enough left, .NET raises `InsufficientExecutionStackException`, which IS an
/// ordinary exception. `guardingStack` turns it into an `Incomplete` verdict for
/// the item being checked, so a pathological declaration costs one blocker
/// instead of the CLI, the LSP or the server.
let private ensureStack () : unit =
  System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()

let private tooDeepBlocker (nodeId : Option<id>) : Blocker =
  { code = UnsupportedConstruct
    nodeId = nodeId
    context =
      "Declaration is nested too deeply for the checker to walk; it was not checked" }

let private guardingStack (nodeId : Option<id>) (check : unit -> Verdict) : Verdict =
  try
    check ()
  with :? System.InsufficientExecutionStackException ->
    Incomplete
      { inferredType = None
        diagnostics = []
        blockers = [ tooDeepBlocker nodeId ]
        dependencies = Set.empty }


let private resolvedName (name : NameResolution<'a>) : Option<'a> =
  match name.resolved with
  | Ok resolved -> Some resolved.name
  | Error _ -> None

let private unresolvedName (kind : string) (name : NameResolution<'a>) : string =
  match name.originalName with
  | [] -> $"{kind} name is unresolved"
  | parts ->
    let displayName = String.concat "." parts
    $"{kind} name '{displayName}' is unresolved"

let private countNoun (count : int) (singular : string) (plural : string) : string =
  if count = 1 then $"1 {singular}" else $"{count} {plural}"

let private wasOrWere (count : int) : string = if count = 1 then "was" else "were"

let private ambiguousEnumPatternContext : string =
  "Cannot determine which enum type contains this case; "
  + "add a type annotation to the matched value"

let private ambiguousRecordContext : string =
  "Cannot determine the record type; "
  + "add a type annotation to the record value"

let private shortHash (hash : Hash) : string =
  let hash = Hash.toHexString hash
  if hash.Length <= 12 then hash else hash.Substring(0, 12)

let private displayTypeName (name : FQTypeName.Package) : string =
  $"Type #{shortHash name}"

let private displayFunctionName (name : FQFnName.FQFnName) : string =
  match name with
  | FQFnName.Builtin builtin ->
    if builtin.version = 0 then
      $"Builtin.{builtin.name}"
    else
      $"Builtin.{builtin.name}_v{builtin.version}"
  | FQFnName.Package hash -> $"package function #{shortHash hash}"

let private displayValueName (name : FQValueName.FQValueName) : string =
  match name with
  | FQValueName.Builtin builtin ->
    if builtin.version = 0 then
      $"Builtin.{builtin.name}"
    else
      $"Builtin.{builtin.name}_v{builtin.version}"
  | FQValueName.Package hash -> $"package value #{shortHash hash}"

let rec private convertType
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
      state.Block(
        UnknownDeclaredTypeVariable,
        nodeId,
        $"Type variable '{name}' is not declared in this scope"
      )
      state.FreshTainted nodeId
  | TypeReference.TCustomType(name, args) ->
    match resolvedName name with
    | Some(FQTypeName.Package packageName) ->
      state.AddDependency(TypeDependency packageName)
      TCustom(packageName, List.map recurse args)
    | None ->
      state.Block(UnresolvedTypeName, nodeId, unresolvedName "Type" name)
      state.FreshTainted nodeId

let rec private applySubstitutions (state : State) (typ : StaticType) : StaticType =
  ensureStack ()
  let recurse = applySubstitutions state
  match typ with
  | TInferenceVar var ->
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

let rec private containsTaintedInferenceVariable
  (state : State)
  (typ : StaticType)
  : bool =
  ensureStack ()
  let recurse = containsTaintedInferenceVariable state
  match typ with
  | TInferenceVar variable ->
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
  | TInferenceVar var -> var = needle
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
      state.Block(
        MissingTypeDeclaration,
        nodeId,
        $"{displayTypeName name} is not available to the checker"
      )
      None
    | Some declaration ->
      if List.length declaration.typeParams <> List.length args then
        let expected = List.length declaration.typeParams
        let actual = List.length args
        let expectedText = countNoun expected "type argument" "type arguments"
        state.Error(
          TypeMismatch,
          nodeId,
          None,
          Some typ,
          $"{displayTypeName name} expects {expectedText}, "
          + $"but {actual} {wasOrWere actual} provided"
        )
        None
      else
        match declaration.definition with
        | TypeDeclaration.Alias target ->
          if Set.contains name seen then
            state.Block(
              AliasCycle,
              nodeId,
              "A referenced type alias contains a cycle"
            )
            None
          else
            let mapping = List.zip declaration.typeParams args |> Map.ofList
            let target = convertType state nodeId mapping target
            Some(target, Set.add name seen)
        | TypeDeclaration.Record _
        | TypeDeclaration.Enum _ -> None
  | _ -> None

let rec private normalizeAliases
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

/// Validate every component reachable from a type, including components hidden inside
/// containers, aliases, records, and enums. Declaration parameters stay rigid while
/// their definitions are validated: supplied arguments are checked separately, so a
/// finite type such as Alias<Alias<Int>> is not mistaken for a recursive declaration.
/// Nominal record/enum recursion is valid and is cut off after one structural visit;
/// entering one also breaks a transparent-alias chain.
let rec private validateTypeClosureFrom
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
    | None ->
      state.Block(
        MissingTypeDeclaration,
        nodeId,
        $"{displayTypeName name} is not available to the checker"
      )
    | Some declaration ->
      if List.length declaration.typeParams <> List.length args then
        let expected = List.length declaration.typeParams
        let actual = List.length args
        let expectedText = countNoun expected "type argument" "type arguments"
        state.Error(
          TypeMismatch,
          nodeId,
          None,
          Some typ,
          $"{displayTypeName name} expects {expectedText}, "
          + $"but {actual} {wasOrWere actual} provided"
        )
      else
        match declaration.definition with
        | TypeDeclaration.Alias target ->
          if Set.contains name seenAliases then
            state.Block(
              AliasCycle,
              nodeId,
              "A referenced type alias contains a cycle"
            )
          else
            let rigidParams =
              declaration.typeParams
              |> List.map (fun name -> name, TRigidVar name)
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
              |> List.map (fun name -> name, TRigidVar name)
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
              |> List.map (fun name -> name, TRigidVar name)
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
  | TRigidVar _
  | TInferenceVar _ -> ()

let private validateTypeClosure
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

let rec private unify
  (state : State)
  (nodeId : Option<id>)
  (context : string)
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
  | TInferenceVar left, TInferenceVar right when left = right -> ()
  | TInferenceVar var, replacement
  | replacement, TInferenceVar var ->
    if containsInferenceVar state var replacement then
      if not involvesTaintedType then
        state.Error(OccursCheckFailed, nodeId, Some expected, Some actual, context)
    else
      if involvesTaintedType then state.MarkTainted replacement
      state.Substitutions <- Map.add var replacement state.Substitutions
  | TRigidVar left, TRigidVar right when left = right -> ()
  | left, right when samePrimitive left right -> ()
  | TStream left, TStream right
  | TList left, TList right
  | TDict left, TDict right
  | TDB left, TDB right -> unify state nodeId context left right
  | TTuple(l1, l2, lr), TTuple(r1, r2, rr) when List.length lr = List.length rr ->
    unify state nodeId context l1 r1
    unify state nodeId context l2 r2
    List.iter2 (unify state nodeId context) lr rr
  | TCustom(leftName, leftArgs), TCustom(rightName, rightArgs) when
    leftName = rightName && List.length leftArgs = List.length rightArgs
    ->
    List.iter2 (unify state nodeId context) leftArgs rightArgs
  | TFn(leftArgs, leftRet), TFn(rightArgs, rightRet) when
    NEList.length leftArgs = NEList.length rightArgs
    ->
    List.iter2
      (unify state nodeId context)
      (NEList.toList leftArgs)
      (NEList.toList rightArgs)
    unify state nodeId context leftRet rightRet
  | _ ->
    if not involvesTaintedType then
      state.Error(TypeMismatch, nodeId, Some expected, Some actual, context)

let private typeVariables
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
      $"Expected {List.length names} explicit type arguments but got "
      + $"{List.length explicitArgs}"
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

type private Env =
  { locals : Map<string, TypeScheme>
    arguments : List<StaticType>
    self : Option<StaticType>
    typeVariables : Map<string, StaticType> }

let rec private inferenceVariables (typ : StaticType) : Set<int> =
  ensureStack ()
  let recurse = inferenceVariables
  match typ with
  | TInferenceVar var -> Set.singleton var
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

let private generalize (state : State) (env : Env) (typ : StaticType) : TypeScheme =
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

let private instantiateScheme
  (state : State)
  (nodeId : Option<id>)
  (scheme : TypeScheme)
  : StaticType =
  let replacements =
    scheme.quantified |> Seq.map (fun var -> var, state.Fresh nodeId) |> Map.ofSeq
  let rec replace typ =
    match typ with
    | TInferenceVar var -> Map.tryFind var replacements |> Option.defaultValue typ
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

let private emptyEnv : Env =
  { locals = Map.empty; arguments = []; self = None; typeVariables = Map.empty }

let private monomorphic (typ : StaticType) : TypeScheme =
  { quantified = Set.empty; typ = typ; fieldConstraints = [] }

let private addSchemes (env : Env) (bindings : List<string * TypeScheme>) : Env =
  let locals =
    bindings
    |> List.filter (fun (name, _) -> name <> "_")
    |> List.fold (fun locals (name, typ) -> Map.add name typ locals) env.locals
  { env with locals = locals }

let private duplicateNames (names : List<string>) : List<string> =
  names
  |> List.countBy (fun name -> name)
  |> List.choose (fun (name, count) -> if count > 1 then Some name else None)

let private addBindings
  (state : State)
  (nodeId : Option<id>)
  (env : Env)
  (bindings : List<string * StaticType>)
  : Env =
  let bindings = bindings |> List.filter (fun (name, _) -> name <> "_")
  let duplicates =
    bindings
    |> List.countBy fst
    |> List.filter (fun (_, count) -> count > 1)
    |> List.map fst
  for name in duplicates do
    state.Error(
      DuplicatePatternBinding,
      nodeId,
      None,
      None,
      $"Pattern binds '{name}' more than once"
    )
  let locals =
    bindings
    |> List.fold
      (fun locals (name, typ) -> Map.add name (monomorphic typ) locals)
      env.locals
  { env with locals = locals }

let rec private checkLetPattern
  (state : State)
  (typ : StaticType)
  (pattern : LetPattern)
  : List<string * StaticType> =
  ensureStack ()
  match pattern with
  | LPVariable(_, name) -> [ name, typ ]
  | LPWildcard _ -> []
  | LPUnit nodeId ->
    unify state (Some nodeId) "Unit let pattern" TUnit typ
    []
  | LPTuple(nodeId, first, second, rest) ->
    let parts = List.init (2 + List.length rest) (fun _ -> state.Fresh(Some nodeId))
    let tuple = TTuple(parts[0], parts[1], List.skip 2 parts)
    unify state (Some nodeId) "Tuple let pattern" tuple typ
    List.zip (first :: second :: rest) parts
    |> List.collect (fun (pattern, typ) -> checkLetPattern state typ pattern)

let private declarationForCustom
  (state : State)
  (nodeId : Option<id>)
  (typ : StaticType)
  : Option<FQTypeName.Package * List<StaticType> * TypeDeclaration.T> =
  match normalizeAliases state nodeId Set.empty typ with
  | TCustom(name, args) ->
    match Map.tryFind name state.Environment.types with
    | Some declaration when List.length declaration.typeParams = List.length args ->
      Some(name, args, declaration)
    | Some _ ->
      // `normalizeAliases` already emitted the arity diagnostic. Do not return
      // mismatched arguments: callers substitute them into declaration fields.
      None
    | None ->
      state.Block(
        MissingTypeDeclaration,
        nodeId,
        $"{displayTypeName name} is not available to the checker"
      )
      None
  | _ -> None

let private declarationFieldType
  (state : State)
  (nodeId : Option<id>)
  (typeParams : List<string>)
  (typeArgs : List<StaticType>)
  (typ : TypeReference)
  : StaticType =
  if List.length typeParams = List.length typeArgs then
    let scope = List.zip typeParams typeArgs |> Map.ofList
    convertType state nodeId scope typ
  else
    // All current callers obtain these lists from a validated declaration lookup.
    // Keep this boundary total so future recovery paths cannot turn malformed
    // serialized input into an exception.
    let expected = List.length typeParams
    let actual = List.length typeArgs
    let expectedText = countNoun expected "type argument" "type arguments"
    state.Error(
      TypeMismatch,
      nodeId,
      None,
      None,
      $"Type declaration expects {expectedText}, "
      + $"but {actual} {wasOrWere actual} provided"
    )
    state.FreshTainted nodeId

let rec private patternBindingNames (pattern : MatchPattern) : List<string> =
  ensureStack ()
  match pattern with
  | MPVariable(_, name) when name <> "_" -> [ name ]
  | MPList(_, patterns) -> patterns |> List.collect patternBindingNames
  | MPListCons(_, head, tail) -> patternBindingNames head @ patternBindingNames tail
  | MPTuple(_, first, second, rest) ->
    first :: second :: rest |> List.collect patternBindingNames
  | MPEnum(_, _, fields) -> fields |> List.collect patternBindingNames
  | MPOr(_, alternatives) -> alternatives.head |> patternBindingNames
  | MPUnit _
  | MPBool _
  | MPInt8 _
  | MPUInt8 _
  | MPInt16 _
  | MPUInt16 _
  | MPInt32 _
  | MPUInt32 _
  | MPInt64 _
  | MPUInt64 _
  | MPInt128 _
  | MPUInt128 _
  | MPInt _
  | MPFloat _
  | MPChar _
  | MPString _
  | MPVariable _ -> []

let private recoverPatternBindings
  (state : State)
  (nodeId : id)
  (patterns : List<MatchPattern>)
  : List<string * StaticType> =
  patterns
  |> List.collect patternBindingNames
  |> List.map (fun name -> name, state.Fresh(Some nodeId))

let rec private checkMatchPattern
  (state : State)
  (expected : StaticType)
  (pattern : MatchPattern)
  : List<string * StaticType> =
  ensureStack ()
  let literal nodeId typ =
    unify state (Some nodeId) "Match pattern" expected typ
    []
  match pattern with
  | MPUnit nodeId -> literal nodeId TUnit
  | MPBool(nodeId, _) -> literal nodeId TBool
  | MPInt8(nodeId, _) -> literal nodeId TInt8
  | MPUInt8(nodeId, _) -> literal nodeId TUInt8
  | MPInt16(nodeId, _) -> literal nodeId TInt16
  | MPUInt16(nodeId, _) -> literal nodeId TUInt16
  | MPInt32(nodeId, _) -> literal nodeId TInt32
  | MPUInt32(nodeId, _) -> literal nodeId TUInt32
  | MPInt64(nodeId, _) -> literal nodeId TInt64
  | MPUInt64(nodeId, _) -> literal nodeId TUInt64
  | MPInt128(nodeId, _) -> literal nodeId TInt128
  | MPUInt128(nodeId, _) -> literal nodeId TUInt128
  | MPInt(nodeId, _) -> literal nodeId TInt
  | MPFloat(nodeId, _, _, _) -> literal nodeId TFloat
  | MPChar(nodeId, _) -> literal nodeId TChar
  | MPString(nodeId, _) -> literal nodeId TString
  | MPVariable(_, "_") -> []
  | MPVariable(_, name) -> [ name, expected ]
  | MPList(nodeId, patterns) ->
    let element = state.Fresh(Some nodeId)
    unify state (Some nodeId) "List pattern" (TList element) expected
    patterns |> List.collect (checkMatchPattern state element)
  | MPListCons(nodeId, head, tail) ->
    let element = state.Fresh(Some nodeId)
    unify state (Some nodeId) "List-cons pattern" (TList element) expected
    checkMatchPattern state element head
    @ checkMatchPattern state (TList element) tail
  | MPTuple(nodeId, first, second, rest) ->
    let parts = List.init (2 + List.length rest) (fun _ -> state.Fresh(Some nodeId))
    unify
      state
      (Some nodeId)
      "Tuple match pattern"
      (TTuple(parts[0], parts[1], List.skip 2 parts))
      expected
    List.zip (first :: second :: rest) parts
    |> List.collect (fun (pattern, typ) -> checkMatchPattern state typ pattern)
  | MPEnum(nodeId, caseName, fieldPatterns) ->
    match declarationForCustom state (Some nodeId) expected with
    | Some(_, typeArgs, declaration) ->
      match declaration.definition with
      | TypeDeclaration.Enum cases ->
        match
          cases |> NEList.toList |> List.tryFind (fun case -> case.name = caseName)
        with
        | None ->
          state.Error(
            UnknownEnumCase,
            Some nodeId,
            Some expected,
            None,
            $"Enum has no case named '{caseName}'"
          )
          recoverPatternBindings state nodeId fieldPatterns
        | Some case when List.length case.fields <> List.length fieldPatterns ->
          let expected = List.length case.fields
          let actual = List.length fieldPatterns
          let expectedText = countNoun expected "field" "fields"
          state.Error(
            EnumFieldCountMismatch,
            Some nodeId,
            None,
            None,
            $"Case '{caseName}' expects {expectedText}, "
            + $"but the pattern provides {actual}"
          )
          recoverPatternBindings state nodeId fieldPatterns
        | Some case ->
          let fieldTypes =
            case.fields
            |> List.map (fun field ->
              declarationFieldType
                state
                (Some nodeId)
                declaration.typeParams
                typeArgs
                field.typ)
          List.zip fieldPatterns fieldTypes
          |> List.collect (fun (pattern, typ) -> checkMatchPattern state typ pattern)
      | TypeDeclaration.Record _
      | TypeDeclaration.Alias _ ->
        state.Error(
          InvalidPattern,
          Some nodeId,
          Some expected,
          None,
          "Enum pattern used with a non-enum type"
        )
        recoverPatternBindings state nodeId fieldPatterns
    | None ->
      let tainted = containsTaintedInferenceVariable state expected
      if not tainted then
        state.Block(AmbiguousType, Some nodeId, ambiguousEnumPatternContext)
      let bindings = recoverPatternBindings state nodeId fieldPatterns
      if tainted then bindings |> List.iter (snd >> state.MarkTainted)
      bindings
  | MPOr(nodeId, alternatives) ->
    // Report a name bound twice within one alternative before the alternatives
    // are collapsed into maps; `Map.ofList` would silently keep one binding
    // and the duplicate would never reach `addBindings`.
    let alternatives =
      alternatives
      |> NEList.toList
      |> List.map (fun alternative ->
        let bindings = checkMatchPattern state expected alternative
        let duplicates =
          bindings
          |> List.filter (fun (name, _) -> name <> "_")
          |> List.countBy fst
          |> List.filter (fun (_, count) -> count > 1)
          |> List.map fst
        for name in duplicates do
          state.Error(
            DuplicatePatternBinding,
            Some nodeId,
            None,
            None,
            $"Pattern binds '{name}' more than once"
          )
        Map.ofList bindings)
    match alternatives with
    | [] -> []
    | first :: rest ->
      let firstNames = first |> Map.toList |> List.map fst |> Set.ofList
      for alternative in rest do
        let names = alternative |> Map.toList |> List.map fst |> Set.ofList
        if names <> firstNames then
          state.Error(
            InvalidPattern,
            Some nodeId,
            None,
            None,
            "Every branch of an or-pattern must bind the same names"
          )
        for KeyValue(name, typ) in alternative do
          match Map.tryFind name first with
          | Some firstType ->
            unify state (Some nodeId) "Or-pattern binding" firstType typ
          | None -> ()
      Map.toList first

let private isNumeric (typ : StaticType) : bool =
  match typ with
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
  | TFloat -> true
  | _ -> false

let private supportsNumericOperation
  (operation : InfixFnName)
  (typ : StaticType)
  : bool =
  match operation, typ with
  | ArithmeticPower, (TInt128 | TUInt128) -> false
  | _ -> isNumeric typ

let private numericOperationError
  (operation : InfixFnName)
  (typ : StaticType)
  : string =
  match operation, typ with
  | ArithmeticPower, (TInt128 | TUInt128) ->
    "Exponentiation does not support Int128 or UInt128 operands"
  | _ -> $"Operator {operation} requires numeric operands"

let rec private unguardedPatternAlternatives
  (pattern : MatchPattern)
  : List<MatchPattern> =
  ensureStack ()
  match pattern with
  | MPOr(_, alternatives) ->
    alternatives |> NEList.toList |> List.collect unguardedPatternAlternatives
  | pattern -> [ pattern ]

let rec private patternIsIrrefutable (pattern : MatchPattern) : bool =
  ensureStack ()
  match pattern with
  | MPVariable _ -> true
  | MPTuple(_, first, second, rest) ->
    patternIsIrrefutable first
    && patternIsIrrefutable second
    && List.forall patternIsIrrefutable rest
  | MPOr(_, alternatives) ->
    alternatives |> NEList.toList |> List.exists patternIsIrrefutable
  | MPUnit _
  | MPBool _
  | MPInt8 _
  | MPUInt8 _
  | MPInt16 _
  | MPUInt16 _
  | MPInt32 _
  | MPUInt32 _
  | MPInt64 _
  | MPUInt64 _
  | MPInt128 _
  | MPUInt128 _
  | MPInt _
  | MPFloat _
  | MPChar _
  | MPString _
  | MPList _
  | MPListCons _
  | MPEnum _ -> false

let rec private isNonExpansive (expr : Expr) : bool =
  ensureStack ()
  match expr with
  | EUnit _
  | EBool _
  | EInt8 _
  | EUInt8 _
  | EInt16 _
  | EUInt16 _
  | EInt32 _
  | EUInt32 _
  | EInt64 _
  | EUInt64 _
  | EInt128 _
  | EUInt128 _
  | EInt _
  | EFloat _
  | EChar _
  | EFnName _
  | EValue _
  | EArg _
  | EVariable _
  | ESelf _
  | ELambda _ -> true
  | EString(_, segments) ->
    segments
    |> List.forall (function
      | StringText _ -> true
      | StringInterpolation expr -> isNonExpansive expr)
  | EList(_, elements) -> List.forall isNonExpansive elements
  | EDict(_, entries) -> entries |> List.forall (snd >> isNonExpansive)
  | ETuple(_, first, second, rest) ->
    isNonExpansive first && isNonExpansive second && List.forall isNonExpansive rest
  | ERecord(_, _, _, fields) -> fields |> List.forall (snd >> isNonExpansive)
  | EEnum(_, _, _, _, fields) -> List.forall isNonExpansive fields
  | EIf _
  | EPipe _
  | EMatch _
  | ELet _
  | EApply _
  | EInfix _
  | ERecordFieldAccess _
  | ERecordUpdate _
  | EStatement _ -> false

type private PatternConstructor =
  | UnitConstructor
  | BoolConstructor of bool
  | ListEmptyConstructor
  | ListConsConstructor
  | TupleConstructor of int
  | EnumConstructor of string

type private MissingPattern =
  | MissingWildcard
  | MissingConstructor of PatternConstructor * List<MissingPattern>

let rec private missingPatternToString (pattern : MissingPattern) : string =
  ensureStack ()
  let recurse = missingPatternToString
  match pattern with
  | MissingWildcard -> "…"
  | MissingConstructor(UnitConstructor, _) -> "()"
  | MissingConstructor(BoolConstructor value, _) -> if value then "true" else "false"
  | MissingConstructor(ListEmptyConstructor, _) -> "[]"
  | MissingConstructor(ListConsConstructor, [ MissingWildcard; MissingWildcard ]) ->
    "a non-empty list"
  | MissingConstructor(ListConsConstructor, [ head; tail ]) ->
    $"a list matching {recurse head} :: {recurse tail}"
  | MissingConstructor(TupleConstructor _, fields) ->
    let fields = fields |> List.map recurse |> String.concat ", "
    $"({fields})"
  | MissingConstructor(EnumConstructor caseName, []) -> caseName
  | MissingConstructor(EnumConstructor caseName, fields) ->
    let fields = fields |> List.map recurse |> String.concat ", "
    $"{caseName}({fields})"
  | MissingConstructor(_, fields) -> fields |> List.map recurse |> String.concat ", "

let private finiteConstructors
  (state : State)
  (nodeId : id)
  (typ : StaticType)
  : Option<List<PatternConstructor * List<StaticType>>> =
  match normalizeAliases state (Some nodeId) Set.empty typ with
  | TUnit -> Some [ UnitConstructor, [] ]
  | TBool -> Some [ BoolConstructor false, []; BoolConstructor true, [] ]
  | TList element ->
    Some
      [ ListEmptyConstructor, []; ListConsConstructor, [ element; TList element ] ]
  | TTuple(first, second, rest) ->
    let fields = first :: second :: rest
    Some [ TupleConstructor(List.length fields), fields ]
  | TCustom(name, typeArgs) ->
    match Map.tryFind name state.Environment.types with
    | Some declaration ->
      match declaration.definition with
      | TypeDeclaration.Enum cases ->
        cases
        |> NEList.toList
        |> List.map (fun case ->
          let fields =
            case.fields
            |> List.map (fun field ->
              declarationFieldType
                state
                (Some nodeId)
                declaration.typeParams
                typeArgs
                field.typ)
          EnumConstructor case.name, fields)
        |> Some
      | TypeDeclaration.Record _
      | TypeDeclaration.Alias _ -> None
    | None -> None
  | _ -> None

let rec private specializePattern
  (constructor : PatternConstructor)
  (fieldCount : int)
  (pattern : MatchPattern)
  : List<List<MatchPattern>> =
  ensureStack ()
  let wildcards nodeId = List.init fieldCount (fun _ -> MPVariable(nodeId, "_"))
  match pattern with
  | MPVariable(nodeId, _) -> [ wildcards nodeId ]
  | MPOr(_, alternatives) ->
    alternatives
    |> NEList.toList
    |> List.collect (specializePattern constructor fieldCount)
  | MPUnit _ when constructor = UnitConstructor -> [ [] ]
  | MPBool(_, value) when constructor = BoolConstructor value -> [ [] ]
  | MPList(_, []) when constructor = ListEmptyConstructor -> [ [] ]
  | MPList(nodeId, head :: tail) when constructor = ListConsConstructor ->
    [ [ head; MPList(nodeId, tail) ] ]
  | MPListCons(_, head, tail) when constructor = ListConsConstructor ->
    [ [ head; tail ] ]
  | MPTuple(_, first, second, rest) when
    constructor = TupleConstructor(2 + List.length rest)
    ->
    [ first :: second :: rest ]
  | MPEnum(_, caseName, fields) when
    constructor = EnumConstructor caseName && List.length fields = fieldCount
    ->
    [ fields ]
  | _ -> []

let rec private findUncoveredPattern
  (state : State)
  (nodeId : id)
  (remainingDepth : int)
  (types : List<StaticType>)
  (rows : List<List<MatchPattern>>)
  : Option<List<MissingPattern>> =
  ensureStack ()
  match remainingDepth, types with
  | 0, _ -> None
  | _, [] -> if List.isEmpty rows then Some [] else None
  | _, typ :: remainingTypes ->
    match finiteConstructors state nodeId typ with
    | Some constructors ->
      constructors
      |> List.tryPick (fun (constructor, fieldTypes) ->
        let specializedRows =
          rows
          |> List.collect (function
            | [] -> []
            | pattern :: remainingPatterns ->
              specializePattern constructor (List.length fieldTypes) pattern
              |> List.map (fun fields -> fields @ remainingPatterns))
        if List.isEmpty specializedRows then
          Some(
            MissingConstructor(
              constructor,
              List.replicate (List.length fieldTypes) MissingWildcard
            )
            :: List.replicate (List.length remainingTypes) MissingWildcard
          )
        else
          findUncoveredPattern
            state
            nodeId
            (remainingDepth - 1)
            (fieldTypes @ remainingTypes)
            specializedRows
          |> Option.map (fun missing ->
            let missingFields, missingRemaining =
              List.splitAt (List.length fieldTypes) missing
            MissingConstructor(constructor, missingFields) :: missingRemaining))
    | None ->
      let rowsWithCatchAll =
        rows
        |> List.collect (function
          | [] -> []
          | pattern :: remainingPatterns ->
            unguardedPatternAlternatives pattern
            |> List.choose (fun pattern ->
              if patternIsIrrefutable pattern then Some remainingPatterns else None))
      findUncoveredPattern
        state
        nodeId
        (remainingDepth - 1)
        remainingTypes
        rowsWithCatchAll
      |> Option.map (fun missing -> MissingWildcard :: missing)

let rec private patternMatrixIsExhaustive
  (state : State)
  (nodeId : id)
  (types : List<StaticType>)
  (rows : List<List<MatchPattern>>)
  : bool =
  ensureStack ()
  match types with
  | [] -> not (List.isEmpty rows)
  | typ :: remainingTypes ->
    let rowsWithCatchAll =
      rows
      |> List.collect (function
        | [] -> []
        | pattern :: remainingPatterns ->
          unguardedPatternAlternatives pattern
          |> List.choose (fun pattern ->
            if patternIsIrrefutable pattern then Some remainingPatterns else None))
    if patternMatrixIsExhaustive state nodeId remainingTypes rowsWithCatchAll then
      true
    else
      match finiteConstructors state nodeId typ with
      | Some constructors ->
        constructors
        |> List.forall (fun (constructor, fieldTypes) ->
          let specializedRows =
            rows
            |> List.collect (function
              | [] -> []
              | pattern :: remainingPatterns ->
                specializePattern constructor (List.length fieldTypes) pattern
                |> List.map (fun fields -> fields @ remainingPatterns))
          patternMatrixIsExhaustive
            state
            nodeId
            (fieldTypes @ remainingTypes)
            specializedRows)
      | None -> false

let private uncoveredMatchPattern
  (state : State)
  (nodeId : id)
  (argType : StaticType)
  (cases : List<MatchCase>)
  : Option<MissingPattern> =
  let patterns =
    cases
    |> List.filter (fun case -> Option.isNone case.whenCondition)
    |> List.collect (fun case -> unguardedPatternAlternatives case.pat)
  let typ = normalizeAliases state (Some nodeId) Set.empty argType
  let rows = List.map List.singleton patterns
  if patternMatrixIsExhaustive state nodeId [ typ ] rows then
    None
  else
    // Witnesses are explanatory, not part of the proof. Bound recursive types so a
    // deeply recursive enum/list cannot make diagnostic rendering unbounded.
    findUncoveredPattern state nodeId 8 [ typ ] rows
    |> Option.bind List.tryHead
    |> Option.orElse (Some MissingWildcard)

/// `Builtin.unwrap : optOrRes -> 'a` has a result variable no parameter
/// constrains (Option vs Result is only known at runtime), but when its
/// argument's type is already solved the result type follows from it:
/// Option<T> -> T, Result<T, _> -> T. Only an argument whose type stays unknown
/// is genuinely uncheckable.
let private asUnwrapBuiltin
  (name : NameResolution<FQFnName.FQFnName>)
  : Option<FQFnName.FQFnName> =
  match resolvedName name with
  | Some(FQFnName.Builtin b as fqName) when b.name = "unwrap" && b.version = 0 ->
    Some fqName
  | _ -> None

let private inferUnwrapResult
  (state : State)
  (nodeId : id)
  (fqName : FQFnName.FQFnName)
  (argType : StaticType)
  : StaticType =
  state.AddDependency(FunctionDependency fqName)
  let optionName = FQTypeName.package (PackageRefs.Type.Stdlib.option ())
  let resultName = FQTypeName.package (PackageRefs.Type.Stdlib.result ())
  match normalizeAliases state (Some nodeId) Set.empty argType with
  | TCustom(name, [ inner ]) when name = optionName -> inner
  | TCustom(name, [ okType; _error ]) when name = resultName -> okType
  | _ ->
    state.Block(
      UnsupportedConstruct,
      Some nodeId,
      $"{displayFunctionName fqName} cannot be checked statically: "
      + "the argument's type is not statically known to be an Option or Result"
    )
    state.FreshTainted(Some nodeId)

/// Signed integers and Float: what unary minus accepts (`Builtin.negate`).
let private isSignedNumeric (typ : StaticType) : bool =
  match typ with
  | TInt8
  | TInt16
  | TInt32
  | TInt64
  | TInt128
  | TInt
  | TFloat -> true
  | _ -> false

/// The operator builtins (`Builtin.add`, `Builtin.lessThan`, ...) are what infix
/// syntax lowers to: `a + b` and `Builtin.add a b` execute identically. Their
/// declared `'a -> 'b -> ...` signatures overstate what they accept (the type
/// language has no numeric constraint), so a by-name call is checked with the
/// operator's own rule instead, read from the same table the lowering uses.
/// Returns the operator to check the call as.
let private asOperatorBuiltin
  (name : NameResolution<FQFnName.FQFnName>)
  : Option<FQFnName.FQFnName * Infix> =
  match resolvedName name with
  | Some(FQFnName.Builtin b as fqName) when b.version = 0 ->
    InfixFnName.all
    |> List.tryFind (fun op -> InfixFnName.toBuiltinName op = b.name)
    |> Option.map (fun op -> fqName, InfixFnCall op)
  | _ -> None

/// `Builtin.negate` is what the parser lowers unary minus on a non-literal
/// (`-x`, `-(f y)`) to. Same story as the infix operators.
let private asNegateBuiltin
  (name : NameResolution<FQFnName.FQFnName>)
  : Option<FQFnName.FQFnName> =
  match resolvedName name with
  | Some(FQFnName.Builtin b as fqName) when
    b.name = InfixFnName.negateBuiltinName && b.version = 0
    ->
    Some fqName
  | _ -> None

let private isOperatorLikeBuiltin (name : NameResolution<FQFnName.FQFnName>) : bool =
  Option.isSome (asOperatorBuiltin name) || Option.isSome (asNegateBuiltin name)

/// Unary minus: the operand must be a signed integer or Float, and the result
/// is the operand's type.
let private inferNegateResult
  (state : State)
  (nodeId : id)
  (fqName : FQFnName.FQFnName)
  (argType : StaticType)
  : StaticType =
  state.AddDependency(FunctionDependency fqName)
  let concrete = normalizeAliases state (Some nodeId) Set.empty argType
  if not (isSignedNumeric concrete) then
    match concrete with
    | TInferenceVar _ ->
      if not (containsTaintedInferenceVariable state argType) then
        state.Block(
          AmbiguousType,
          Some nodeId,
          "Cannot determine the operand type of unary minus; add a type annotation"
        )
    | _ ->
      state.Error(
        InvalidInfixOperand,
        Some nodeId,
        None,
        Some concrete,
        "Unary minus requires a signed integer or Float operand"
      )
  argType

let private instantiateFunction
  (state : State)
  (nodeId : Option<id>)
  (typeVariableScope : Map<string, StaticType>)
  (name : NameResolution<FQFnName.FQFnName>)
  (explicitTypeArgs : List<TypeReference>)
  : StaticType =
  match resolvedName name with
  | None ->
    state.Block(UnresolvedFunctionName, nodeId, unresolvedName "Function" name)
    state.FreshTainted nodeId
  | Some fqName when isOperatorLikeBuiltin name ->
    // Applied to its full argument list it is checked as the operator (see
    // `asOperatorBuiltin`); as a value or partially applied there is no
    // signature to give it, since the declared one overstates what it accepts.
    state.AddDependency(FunctionDependency fqName)
    state.Block(
      UnsupportedConstruct,
      nodeId,
      $"{displayFunctionName fqName} is an operator; apply it to all its "
      + "arguments, use the operator syntax, or wrap it in a lambda"
    )
    state.FreshTainted nodeId
  | Some name ->
    state.AddDependency(FunctionDependency name)
    match Map.tryFind name state.Environment.unsupportedFunctions with
    | Some reason ->
      state.Block(
        UnsupportedConstruct,
        nodeId,
        $"{displayFunctionName name} cannot be checked statically: {reason}"
      )
      state.FreshTainted nodeId
    | None ->
      match Map.tryFind name state.Environment.functions with
      | None ->
        state.Block(
          MissingFunctionSignature,
          nodeId,
          $"Signature for {displayFunctionName name} is not available to the checker"
        )
        state.FreshTainted nodeId
      | Some signature ->
        let vars =
          typeVariables
            state
            nodeId
            typeVariableScope
            signature.typeParams
            explicitTypeArgs
        let parameters =
          NEList.map (convertType state nodeId vars) signature.parameters
        let returnType = convertType state nodeId vars signature.returnType
        let typ = TFn(parameters, returnType)
        validateTypeClosure state nodeId typ
        typ

let private instantiateCustomType
  (state : State)
  (nodeId : Option<id>)
  (typeVariableScope : Map<string, StaticType>)
  (name : NameResolution<FQTypeName.FQTypeName>)
  (explicitTypeArgs : List<TypeReference>)
  : Option<FQTypeName.Package * List<StaticType> * TypeDeclaration.T> =
  match resolvedName name with
  | None ->
    state.Block(UnresolvedTypeName, nodeId, unresolvedName "Type" name)
    None
  | Some(FQTypeName.Package packageName) ->
    state.AddDependency(TypeDependency packageName)
    match Map.tryFind packageName state.Environment.types with
    | None ->
      state.Block(
        MissingTypeDeclaration,
        nodeId,
        $"{displayTypeName packageName} is not available to the checker"
      )
      None
    | Some declaration ->
      let vars =
        typeVariables
          state
          nodeId
          typeVariableScope
          declaration.typeParams
          explicitTypeArgs
      let args = declaration.typeParams |> List.map (fun name -> vars[name])
      args |> List.iter (validateTypeClosure state nodeId)
      Some(packageName, args, declaration)

let rec private checkExprWithContext
  (state : State)
  (env : Env)
  (expected : StaticType)
  (expr : Expr)
  (context : string)
  : unit =
  ensureStack ()
  match expr, normalizeAliases state (Some(Expr.toID expr)) Set.empty expected with
  | ELambda(nodeId, patterns, body), TFn(parameters, returnType) when
    NEList.length patterns = NEList.length parameters
    ->
    let bindings =
      List.zip (NEList.toList patterns) (NEList.toList parameters)
      |> List.collect (fun (pattern, typ) -> checkLetPattern state typ pattern)
    let lambdaEnv = addBindings state (Some nodeId) env bindings
    checkExprWithContext state lambdaEnv returnType body "Lambda return value"
  | _ ->
    let actual = inferExpr state env expr
    unify state (Some(Expr.toID expr)) context expected actual

and private checkExpr
  (state : State)
  (env : Env)
  (expected : StaticType)
  (expr : Expr)
  : unit =
  ensureStack ()
  checkExprWithContext state env expected expr "Expression"

and private applyArguments
  (state : State)
  (env : Env)
  (nodeId : id)
  (callee : StaticType)
  (arguments : List<Expr>)
  : StaticType =
  ensureStack ()
  let rec apply
    (position : int)
    (callee : StaticType)
    (arguments : List<Expr>)
    : StaticType =
    match arguments with
    | [] -> callee
    | argument :: remaining ->
      match normalizeAliases state (Some nodeId) Set.empty callee with
      | TFn(parameters, returnType) ->
        let parameterList = NEList.toList parameters
        checkExprWithContext
          state
          env
          parameterList.Head
          argument
          $"Function argument {position}"
        match parameterList.Tail with
        | [] -> apply (position + 1) returnType remaining
        | next :: rest ->
          apply (position + 1) (TFn(NEList.ofList next rest, returnType)) remaining
      | TInferenceVar _ ->
        let parameter = state.Fresh(Some nodeId)
        let result = state.Fresh(Some nodeId)
        unify
          state
          (Some nodeId)
          "Function application"
          (TFn(NEList.singleton parameter, result))
          callee
        checkExprWithContext
          state
          env
          parameter
          argument
          $"Function argument {position}"
        apply (position + 1) result remaining
      | notCallable ->
        state.Error(
          NotCallable,
          Some nodeId,
          None,
          Some notCallable,
          "Attempted to apply a value that is not a function"
        )
        state.Fresh(Some nodeId)
  apply 1 callee arguments

and private inferRecordConstruction
  (state : State)
  (env : Env)
  (nodeId : id)
  (name : NameResolution<FQTypeName.FQTypeName>)
  (typeArgs : List<TypeReference>)
  (fields : List<string * Expr>)
  : StaticType =
  ensureStack ()
  match
    instantiateCustomType state (Some nodeId) env.typeVariables name typeArgs
  with
  | None -> state.FreshTainted(Some nodeId)
  | Some(packageName, typeArgs, _) ->
    let result = TCustom(packageName, typeArgs)
    match declarationForCustom state (Some nodeId) result with
    | None -> result
    | Some(_, typeArgs, declaration) ->
      match declaration.definition with
      | TypeDeclaration.Record declaredFields ->
        let declaredFields = declaredFields |> NEList.toList
        let duplicates =
          fields
          |> List.countBy fst
          |> List.filter (fun (_, count) -> count > 1)
          |> List.map fst
        for name in duplicates do
          state.Error(
            DuplicateRecordField,
            Some nodeId,
            None,
            None,
            $"Record field '{name}' is specified more than once"
          )
        for name, value in fields do
          match declaredFields |> List.tryFind (fun field -> field.name = name) with
          | None ->
            state.Error(
              UnknownRecordField,
              Some nodeId,
              None,
              None,
              $"Record has no field named '{name}'"
            )
            let _ = inferExpr state env value
            ()
          | Some field ->
            let fieldType =
              declarationFieldType
                state
                (Some nodeId)
                declaration.typeParams
                typeArgs
                field.typ
            checkExpr state env fieldType value
        let supplied = fields |> List.map fst |> Set.ofList
        let missingFields =
          declaredFields
          |> List.map _.name
          |> List.filter (fun name -> not (Set.contains name supplied))
        if not (List.isEmpty missingFields) then
          let fieldNoun = if List.length missingFields = 1 then "field" else "fields"
          let fields =
            missingFields |> List.map (fun name -> $"'{name}'") |> String.concat ", "
          state.Error(
            MissingRecordField,
            Some nodeId,
            None,
            None,
            $"Record is missing {fieldNoun}: {fields}"
          )
      | TypeDeclaration.Enum _
      | TypeDeclaration.Alias _ ->
        state.Error(
          TypeMismatch,
          Some nodeId,
          None,
          Some result,
          "Record construction requires a record type"
        )
      result

and private inferEnumConstruction
  (state : State)
  (env : Env)
  (nodeId : id)
  (name : NameResolution<FQTypeName.FQTypeName>)
  (typeArgs : List<TypeReference>)
  (caseName : string)
  (fields : List<Expr>)
  : StaticType =
  ensureStack ()
  match
    instantiateCustomType state (Some nodeId) env.typeVariables name typeArgs
  with
  | None -> state.FreshTainted(Some nodeId)
  | Some(packageName, typeArgs, _) ->
    let result = TCustom(packageName, typeArgs)
    match declarationForCustom state (Some nodeId) result with
    | None -> ()
    | Some(_, typeArgs, declaration) ->
      match declaration.definition with
      | TypeDeclaration.Enum cases ->
        match
          cases |> NEList.toList |> List.tryFind (fun case -> case.name = caseName)
        with
        | None ->
          state.Error(
            UnknownEnumCase,
            Some nodeId,
            Some result,
            None,
            $"Enum has no case named '{caseName}'"
          )
        | Some case when List.length case.fields <> List.length fields ->
          let expected = List.length case.fields
          let actual = List.length fields
          let expectedText = countNoun expected "field" "fields"
          state.Error(
            EnumFieldCountMismatch,
            Some nodeId,
            None,
            None,
            $"Case '{caseName}' expects {expectedText}, "
            + $"but the construction provides {actual}"
          )
          fields |> List.iter (fun field -> let _ = inferExpr state env field in ())
        | Some case ->
          List.zip case.fields fields
          |> List.iter (fun (field, expr) ->
            let fieldType =
              declarationFieldType
                state
                (Some nodeId)
                declaration.typeParams
                typeArgs
                field.typ
            checkExpr state env fieldType expr)
      | TypeDeclaration.Record _
      | TypeDeclaration.Alias _ ->
        state.Error(
          TypeMismatch,
          Some nodeId,
          None,
          Some result,
          "Enum construction requires an enum type"
        )
    result

and private inferInfix
  (state : State)
  (env : Env)
  (nodeId : id)
  (infix : Infix)
  (lhs : Expr)
  (rhs : Expr)
  : StaticType =
  ensureStack ()
  match infix with
  | BinOp BinOpAnd
  | BinOp BinOpOr ->
    checkExpr state env TBool lhs
    checkExpr state env TBool rhs
    TBool
  | InfixFnCall StringConcat ->
    checkExpr state env TString lhs
    checkExpr state env TString rhs
    TString
  | InfixFnCall ComparisonEquals
  | InfixFnCall ComparisonNotEquals ->
    let lhsType = inferExpr state env lhs
    checkExpr state env lhsType rhs
    TBool
  | InfixFnCall operation ->
    let lhsType = inferExpr state env lhs
    checkExpr state env lhsType rhs
    let concrete = normalizeAliases state (Some nodeId) Set.empty lhsType
    if not (supportsNumericOperation operation concrete) then
      match concrete with
      | TInferenceVar _ ->
        if not (containsTaintedInferenceVariable state lhsType) then
          state.Block(
            AmbiguousType,
            Some nodeId,
            "Cannot determine the numeric operand type; add a type annotation"
          )
      | _ ->
        state.Error(
          InvalidInfixOperand,
          Some nodeId,
          None,
          Some concrete,
          numericOperationError operation concrete
        )
    match operation with
    | ComparisonGreaterThan
    | ComparisonGreaterThanOrEqual
    | ComparisonLessThan
    | ComparisonLessThanOrEqual -> TBool
    | ArithmeticPlus
    | ArithmeticMinus
    | ArithmeticMultiply
    | ArithmeticDivide
    | ArithmeticModulo
    | ArithmeticPower -> lhsType
    | ComparisonEquals
    | ComparisonNotEquals
    | StringConcat -> Exception.raiseInternal "Handled above" []

and private inferPipePart
  (state : State)
  (env : Env)
  (input : StaticType)
  (part : PipeExpr)
  : StaticType =
  ensureStack ()
  match part with
  | EPipeLambda(nodeId, patterns, body) ->
    let parameters = patterns |> NEList.map (fun _ -> state.Fresh(Some nodeId))
    unify state (Some nodeId) "Pipeline input" parameters.head input
    let bindings =
      List.zip (NEList.toList patterns) (NEList.toList parameters)
      |> List.collect (fun (pattern, typ) -> checkLetPattern state typ pattern)
    let bodyType =
      inferExpr state (addBindings state (Some nodeId) env bindings) body
    match parameters.tail with
    | [] -> bodyType
    | next :: rest -> TFn(NEList.ofList next rest, bodyType)
  | EPipeInfix(nodeId, infix, rhs) ->
    // The lhs has already been inferred, so apply the operator rule directly.
    let rhsType = inferExpr state env rhs
    match infix with
    | BinOp BinOpAnd
    | BinOp BinOpOr ->
      unify state (Some nodeId) "Pipeline boolean operator" TBool input
      unify state (Some nodeId) "Pipeline boolean operator" TBool rhsType
      TBool
    | InfixFnCall StringConcat ->
      unify state (Some nodeId) "Pipeline string concatenation" TString input
      unify state (Some nodeId) "Pipeline string concatenation" TString rhsType
      TString
    | InfixFnCall ComparisonEquals
    | InfixFnCall ComparisonNotEquals ->
      unify state (Some nodeId) "Pipeline comparison" input rhsType
      TBool
    | InfixFnCall operation ->
      unify state (Some nodeId) "Pipeline numeric operator" input rhsType
      let concrete = normalizeAliases state (Some nodeId) Set.empty input
      if not (supportsNumericOperation operation concrete) then
        match concrete with
        | TInferenceVar _ ->
          if not (containsTaintedInferenceVariable state input) then
            state.Block(
              AmbiguousType,
              Some nodeId,
              "Cannot determine the numeric pipeline operand type; "
              + "add a type annotation"
            )
        | _ ->
          state.Error(
            InvalidInfixOperand,
            Some nodeId,
            None,
            Some concrete,
            numericOperationError operation concrete
          )
      match operation with
      | ComparisonGreaterThan
      | ComparisonGreaterThanOrEqual
      | ComparisonLessThan
      | ComparisonLessThanOrEqual -> TBool
      | _ -> input
  | EPipeFnCall(nodeId, name, typeArgs, args) ->
    // In a pipe the first argument is the pipeline input, already inferred.
    // The same builtins as in `EApply` get their own rule.
    match
      typeArgs,
      args,
      asUnwrapBuiltin name,
      asNegateBuiltin name,
      asOperatorBuiltin name
    with
    | [], [], Some fqName, _, _ -> inferUnwrapResult state nodeId fqName input
    | [], [], _, Some fqName, _ -> inferNegateResult state nodeId fqName input
    | [], [ rhs ], _, _, Some(fqName, infix) ->
      state.AddDependency(FunctionDependency fqName)
      inferPipePart state env input (EPipeInfix(nodeId, infix, rhs))
    | _ ->
      let fnType =
        instantiateFunction state (Some nodeId) env.typeVariables name typeArgs
      let afterInput = applyKnownInput state nodeId "Pipeline function" fnType input
      applyArguments state env nodeId afterInput args
  | EPipeEnum(nodeId, name, caseName, fields) ->
    match instantiateCustomType state (Some nodeId) env.typeVariables name [] with
    | None -> state.FreshTainted(Some nodeId)
    | Some(packageName, typeArgs, _) ->
      let result = TCustom(packageName, typeArgs)
      match declarationForCustom state (Some nodeId) result with
      | None -> ()
      | Some(_, typeArgs, declaration) ->
        match declaration.definition with
        | TypeDeclaration.Enum cases ->
          match
            cases |> NEList.toList |> List.tryFind (fun case -> case.name = caseName)
          with
          | Some case when List.length case.fields = 1 + List.length fields ->
            let expressions = EUnit nodeId :: fields
            List.zip case.fields expressions
            |> List.iteri (fun index (field, expr) ->
              let fieldType =
                declarationFieldType
                  state
                  (Some nodeId)
                  declaration.typeParams
                  typeArgs
                  field.typ
              if index = 0 then
                unify state (Some nodeId) "Pipeline enum input" fieldType input
              else
                checkExpr state env fieldType expr)
          | Some case ->
            let expected = List.length case.fields
            let actual = 1 + List.length fields
            let expectedText = countNoun expected "field" "fields"
            state.Error(
              EnumFieldCountMismatch,
              Some nodeId,
              None,
              None,
              $"Case '{caseName}' expects {expectedText}, "
              + $"but the pipeline provides {actual}"
            )
          | None ->
            state.Error(
              UnknownEnumCase,
              Some nodeId,
              Some result,
              None,
              $"Enum has no case named '{caseName}'"
            )
        | _ ->
          state.Error(TypeMismatch, Some nodeId, None, Some result, "Not an enum")
      result
  | EPipeVariable(nodeId, name, args) ->
    match Map.tryFind name env.locals with
    | None ->
      state.Error(
        UnknownVariable,
        Some nodeId,
        None,
        None,
        $"Local variable '{name}' is not in scope"
      )
      state.FreshTainted(Some nodeId)
    | Some scheme ->
      // Apply the known input without making an AST literal of the wrong type.
      let fnType = instantiateScheme state (Some nodeId) scheme
      let afterInput = applyKnownInput state nodeId "Pipeline variable" fnType input
      applyArguments state env nodeId afterInput args

and private applyKnownInput
  (state : State)
  (nodeId : id)
  (context : string)
  (callee : StaticType)
  (input : StaticType)
  : StaticType =
  ensureStack ()
  match normalizeAliases state (Some nodeId) Set.empty callee with
  | TFn(parameters, returnType) ->
    unify state (Some nodeId) context parameters.head input
    match parameters.tail with
    | [] -> returnType
    | next :: rest -> TFn(NEList.ofList next rest, returnType)
  | TInferenceVar _ ->
    let parameter = state.Fresh(Some nodeId)
    let result = state.Fresh(Some nodeId)
    unify
      state
      (Some nodeId)
      context
      (TFn(NEList.singleton parameter, result))
      callee
    unify state (Some nodeId) context parameter input
    result
  | actual ->
    state.Error(NotCallable, Some nodeId, None, Some actual, context)
    state.Fresh(Some nodeId)

and private inferExpr (state : State) (env : Env) (expr : Expr) : StaticType =
  ensureStack ()
  match expr with
  | EUnit _ -> TUnit
  | EBool _ -> TBool
  | EInt8 _ -> TInt8
  | EUInt8 _ -> TUInt8
  | EInt16 _ -> TInt16
  | EUInt16 _ -> TUInt16
  | EInt32 _ -> TInt32
  | EUInt32 _ -> TUInt32
  | EInt64 _ -> TInt64
  | EUInt64 _ -> TUInt64
  | EInt128 _ -> TInt128
  | EUInt128 _ -> TUInt128
  | EInt _ -> TInt
  | EFloat _ -> TFloat
  | EChar _ -> TChar
  | EString(_, segments) ->
    segments
    |> List.iter (function
      | StringText _ -> ()
      | StringInterpolation expr -> checkExpr state env TString expr)
    TString
  | EIf(nodeId, condition, thenExpr, elseExpr) ->
    checkExpr state env TBool condition
    let thenType = inferExpr state env thenExpr
    match elseExpr with
    | Some elseExpr ->
      checkExpr state env thenType elseExpr
      thenType
    | None ->
      unify state (Some nodeId) "If expression without else" TUnit thenType
      TUnit
  | EPipe(_, lhs, parts) ->
    parts |> List.fold (inferPipePart state env) (inferExpr state env lhs)
  | EMatch(nodeId, arg, cases) ->
    let argType = inferExpr state env arg
    let resultType = state.Fresh(Some nodeId)
    let mutable patternsAreValid = true
    for case in cases do
      let issueCount = state.Diagnostics.Count + state.Blockers.Count
      let bindings = checkMatchPattern state argType case.pat
      if state.Diagnostics.Count + state.Blockers.Count > issueCount then
        patternsAreValid <- false
      let caseEnv = addBindings state (Some nodeId) env bindings
      case.whenCondition |> Option.iter (checkExpr state caseEnv TBool)
      checkExpr state caseEnv resultType case.rhs
    if patternsAreValid && not (containsTaintedInferenceVariable state argType) then
      match uncoveredMatchPattern state nodeId argType cases with
      | Some MissingWildcard ->
        state.Block(
          NonExhaustiveMatch,
          Some nodeId,
          "Match is not exhaustive; "
          + "values outside the listed patterns are not covered"
        )
      | Some missing ->
        let missing = missingPatternToString missing
        state.Block(
          NonExhaustiveMatch,
          Some nodeId,
          $"Match is not exhaustive; an uncovered pattern is {missing}"
        )
      | None -> ()
    resultType
  | ELet(nodeId, pattern, value, body) ->
    let valueType = inferExpr state env value
    let bindings = checkLetPattern state valueType pattern
    let bodyEnv =
      if isNonExpansive value then
        bindings
        |> List.map (fun (name, typ) -> name, generalize state env typ)
        |> addSchemes env
      else
        addBindings state (Some nodeId) env bindings
    inferExpr state bodyEnv body
  | EVariable(nodeId, name) ->
    match Map.tryFind name env.locals with
    | Some scheme -> instantiateScheme state (Some nodeId) scheme
    | None ->
      state.Error(
        UnknownVariable,
        Some nodeId,
        None,
        None,
        $"Local variable '{name}' is not in scope"
      )
      state.FreshTainted(Some nodeId)
  | EArg(nodeId, index) ->
    match List.tryItem index env.arguments with
    | Some typ -> typ
    | None ->
      state.Error(
        InvalidArgumentIndex,
        Some nodeId,
        None,
        None,
        $"Function has no argument at index {index}"
      )
      state.Fresh(Some nodeId)
  | EList(nodeId, elements) ->
    let elementType = state.Fresh(Some nodeId)
    elements |> List.iter (checkExpr state env elementType)
    TList elementType
  | EDict(nodeId, entries) ->
    let valueType = state.Fresh(Some nodeId)
    entries |> List.iter (snd >> checkExpr state env valueType)
    TDict valueType
  | ETuple(_, first, second, rest) ->
    TTuple(
      inferExpr state env first,
      inferExpr state env second,
      List.map (inferExpr state env) rest
    )
  | EApply(nodeId, callee, typeArgs, args) ->
    // Builtins whose declared signature is not the truth: unwrap, and the
    // operator builtins that infix syntax lowers to.
    let specialCase =
      match callee, typeArgs, NEList.toList args with
      | EFnName(_, name), [], [ arg ] when Option.isSome (asUnwrapBuiltin name) ->
        asUnwrapBuiltin name
        |> Option.map (fun fqName ->
          inferUnwrapResult state nodeId fqName (inferExpr state env arg))
      | EFnName(_, name), [], [ arg ] when Option.isSome (asNegateBuiltin name) ->
        asNegateBuiltin name
        |> Option.map (fun fqName ->
          inferNegateResult state nodeId fqName (inferExpr state env arg))
      | EFnName(_, name), [], [ lhs; rhs ] ->
        asOperatorBuiltin name
        |> Option.map (fun (fqName, infix) ->
          state.AddDependency(FunctionDependency fqName)
          inferInfix state env nodeId infix lhs rhs)
      | _ -> None
    match specialCase with
    | Some resultType -> resultType
    | None ->
      let calleeType =
        match callee with
        | EFnName(nameId, name) ->
          instantiateFunction state (Some nameId) env.typeVariables name typeArgs
        | _ ->
          if not (List.isEmpty typeArgs) then
            state.Block(
              UnsupportedConstruct,
              Some nodeId,
              "Explicit type arguments on a non-named function are not supported"
            )
          inferExpr state env callee
      applyArguments state env nodeId calleeType (NEList.toList args)
  | EFnName(nodeId, name) ->
    instantiateFunction state (Some nodeId) env.typeVariables name []
  | ELambda(nodeId, patterns, body) ->
    let parameters = patterns |> NEList.map (fun _ -> state.Fresh(Some nodeId))
    let bindings =
      List.zip (NEList.toList patterns) (NEList.toList parameters)
      |> List.collect (fun (pattern, typ) -> checkLetPattern state typ pattern)
    let bodyType = inferExpr state (addBindings state None env bindings) body
    TFn(parameters, bodyType)
  | EInfix(nodeId, infix, lhs, rhs) -> inferInfix state env nodeId infix lhs rhs
  | ERecord(nodeId, name, typeArgs, fields) ->
    inferRecordConstruction state env nodeId name typeArgs fields
  | ERecordFieldAccess(nodeId, record, fieldName) ->
    let recordType = inferExpr state env record
    match normalizeAliases state (Some nodeId) Set.empty recordType with
    | TInferenceVar _ when containsTaintedInferenceVariable state recordType ->
      state.FreshTainted(Some nodeId)
    | TInferenceVar _ ->
      let fieldType = state.Fresh(Some nodeId)
      state.PendingFieldAccesses <-
        (nodeId, recordType, fieldName, fieldType) :: state.PendingFieldAccesses
      fieldType
    | _ ->
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
            declarationFieldType
              state
              (Some nodeId)
              declaration.typeParams
              typeArgs
              field.typ
          | None ->
            state.Error(
              UnknownRecordField,
              Some nodeId,
              None,
              Some recordType,
              $"Record has no field named '{fieldName}'"
            )
            state.Fresh(Some nodeId)
        | _ ->
          state.Error(
            TypeMismatch,
            Some nodeId,
            None,
            Some recordType,
            "Field access requires a record"
          )
          state.Fresh(Some nodeId)
      | None ->
        if containsTaintedInferenceVariable state recordType then
          state.FreshTainted(Some nodeId)
        else
          state.Block(AmbiguousType, Some nodeId, ambiguousRecordContext)
          state.Fresh(Some nodeId)
  | ERecordUpdate(nodeId, record, updates) ->
    let recordType = inferExpr state env record
    let updates = NEList.toList updates
    for name in updates |> List.map fst |> duplicateNames do
      state.Error(
        DuplicateRecordField,
        Some nodeId,
        None,
        Some recordType,
        $"Record field '{name}' is updated more than once"
      )
    match declarationForCustom state (Some nodeId) recordType with
    | Some(_, typeArgs, declaration) ->
      match declaration.definition with
      | TypeDeclaration.Record fields ->
        let fields = NEList.toList fields
        for name, value in updates do
          match fields |> List.tryFind (fun field -> field.name = name) with
          | Some field ->
            let fieldType =
              declarationFieldType
                state
                (Some nodeId)
                declaration.typeParams
                typeArgs
                field.typ
            checkExpr state env fieldType value
          | None ->
            state.Error(
              UnknownRecordField,
              Some nodeId,
              None,
              Some recordType,
              $"Record has no field named '{name}'"
            )
            let _ = inferExpr state env value
            ()
      | _ ->
        state.Error(
          TypeMismatch,
          Some nodeId,
          None,
          Some recordType,
          "Record update requires a record"
        )
    | None ->
      if not (containsTaintedInferenceVariable state recordType) then
        state.Block(AmbiguousType, Some nodeId, ambiguousRecordContext)
    recordType
  | EEnum(nodeId, name, typeArgs, caseName, fields) ->
    inferEnumConstruction state env nodeId name typeArgs caseName fields
  | EValue(nodeId, name) ->
    match resolvedName name with
    | None ->
      state.Block(UnresolvedValueName, Some nodeId, unresolvedName "Value" name)
      state.FreshTainted(Some nodeId)
    | Some name ->
      state.AddDependency(ValueDependency name)
      match Map.tryFind name state.Environment.checkedValues with
      | Some scheme ->
        let typ = instantiateScheme state (Some nodeId) scheme
        validateTypeClosure state (Some nodeId) typ
        typ
      | None ->
        match Map.tryFind name state.Environment.values with
        | Some typ ->
          let typ = convertType state (Some nodeId) Map.empty typ
          validateTypeClosure state (Some nodeId) typ
          typ
        | None ->
          state.Block(
            MissingValueSignature,
            Some nodeId,
            $"Signature for {displayValueName name} is not available to the checker"
          )
          state.FreshTainted(Some nodeId)
  | EStatement(_, first, next) ->
    checkExprWithContext
      state
      env
      TUnit
      first
      "Statement before the final expression"
    inferExpr state env next
  | ESelf nodeId ->
    match env.self with
    | Some typ -> typ
    | None ->
      state.Error(
        UnknownVariable,
        Some nodeId,
        None,
        None,
        "Self is only available while checking a function"
      )
      state.Fresh(Some nodeId)


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
          unify state (Some nodeId) "Record field access" fieldType resultType
        | None ->
          state.Error(
            UnknownRecordField,
            Some nodeId,
            None,
            Some recordType,
            $"Record has no field named '{fieldName}'"
          )
      | TypeDeclaration.Enum _
      | TypeDeclaration.Alias _ ->
        state.Error(
          TypeMismatch,
          Some nodeId,
          None,
          Some recordType,
          "Field access requires a record"
        )
    | None ->
      if not (containsTaintedInferenceVariable state recordType) then
        state.Block(AmbiguousType, Some nodeId, ambiguousRecordContext)

let private displayType (scheme : TypeScheme) : StaticType =
  let names =
    scheme.quantified
    |> Set.toList
    |> List.mapi (fun index var -> var, TRigidVar $"t{index + 1}")
    |> Map.ofList
  let rec replace typ =
    match typ with
    | TInferenceVar var -> Map.tryFind var names |> Option.defaultValue typ
    | TStream inner -> TStream(replace inner)
    | TList inner -> TList(replace inner)
    | TTuple(first, second, rest) ->
      TTuple(replace first, replace second, List.map replace rest)
    | TDict inner -> TDict(replace inner)
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
  // Inference variables are implementation details unless they remain observable
  // in the checked item's type. Calls to polymorphic functions commonly create
  // variables that occur only in discarded values or unused generic fields. Those
  // variables impose no unresolved constraint and do not weaken the proof.
  //
  // Pending field/operator/callability ambiguity reports its own blocker at the
  // operation site. A diagnostic containing an inference variable is not yet a
  // definite error, so those variables remain observable too. Quantified variables
  // are complete by construction.
  let variablesInDiagnostic (diagnostic : Diagnostic) : Set<int> =
    [ diagnostic.expected; diagnostic.actual ]
    |> List.collect Option.toList
    |> List.map (applySubstitutions state >> inferenceVariables)
    |> Set.unionMany
  let diagnosticIsDefinite (diagnostic : Diagnostic) : bool =
    // A diagnostic whose remaining inference variables are not quantified is
    // provisional: solving one of those variables may make the apparent
    // mismatch disappear. Diagnostics with no such variables are definite,
    // even when an unrelated construct also prevented a complete proof.
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
    state.Block(
      AmbiguousType,
      origin,
      "Could not infer a complete type here; add a type annotation"
    ))
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
        $"Function type parameter '{name}' is declared more than once"
      )
    let rigidVars =
      fn.typeParams |> List.map (fun name -> name, TRigidVar name) |> Map.ofList
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
    checkExprWithContext state env returnType fn.body "Function return value"
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
    checkExprWithContext state emptyEnv expectedType value.body "Value body"
    finish state (Some(Expr.toID value.body)) (monomorphic expectedType))


// --------------------
// Closed package batches
// --------------------

type ItemVerdict = { item : Reference; verdict : Verdict }

type BatchResult =
  { environment : TypeEnvironment
    types : List<ItemVerdict>
    values : List<ItemVerdict>
    functions : List<ItemVerdict> }

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
        $"Type parameter '{name}' is declared more than once"
      )

    let rigidVars =
      declaration.typeParams
      |> List.map (fun name -> name, TRigidVar name)
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
          $"Record field '{name}' is declared more than once"
        )
      fields |> NEList.iter (fun field -> validateReference field.typ)
    | TypeDeclaration.Enum cases ->
      for name in cases |> NEList.toList |> List.map _.name |> duplicateNames do
        state.Error(
          DuplicateTypeMember,
          None,
          None,
          None,
          $"Enum case '{name}' is declared more than once"
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

/// Check a closed package batch against a caller-supplied base environment.
/// The environment may be empty or contain trusted builtins and stored dependencies.
///
/// Types and function signatures are predeclared, so declaration order and mutual
/// function recursion do not affect checking. Values have no declared type in
/// ProgramTypes; acyclic values are inferred in dependency order, while recursive
/// value groups conservatively remain Incomplete.
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
