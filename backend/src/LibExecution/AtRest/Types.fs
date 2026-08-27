/// The vocabulary the at-rest type checker speaks: its type language, its verdicts,
/// and the environment it checks against.
///
/// Split out of `AtRestTypeChecker` so consumers can name a diagnostic code or a
/// verdict without depending on the inference engine. Members are `internal` rather
/// than `private` only because the checker is several modules; nothing here is public
/// outside this assembly that was not public before.
module LibExecution.AtRest.Types

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

type internal TypeScheme =
  { quantified : Set<int>
    typ : StaticType
    fieldConstraints : List<id * StaticType * string * StaticType> }

type Proof =
  internal
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

let rec internal runtimeTypeToProgramType (typ : RT.TypeReference) : TypeReference =
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

let rec internal runtimeTypeVariables (typ : RT.TypeReference) : Set<string> =
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
  internal
    { types : Map<FQTypeName.Package, TypeDeclaration.T>
      functions : Map<FQFnName.FQFnName, FunctionSignature>
      unsupportedFunctions : Map<FQFnName.FQFnName, string>
      requiresExplicitTypeArguments : Set<FQFnName.FQFnName>
      values : Map<FQValueName.FQValueName, TypeReference>
      checkedValues : Map<FQValueName.FQValueName, TypeScheme> }

module TypeEnvironment =
  let empty : TypeEnvironment =
    { types = Map.empty
      functions = Map.empty
      unsupportedFunctions = Map.empty
      requiresExplicitTypeArguments = Set.empty
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

  let private requireExplicitTypeArguments
    (name : FQFnName.FQFnName)
    (environment : TypeEnvironment)
    : TypeEnvironment =
    { environment with
        requiresExplicitTypeArguments =
          Set.add name environment.requiresExplicitTypeArguments }

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

  /// Add builtin signatures without executable bodies. Invalid zero-argument
  /// builtins are returned as errors instead of throwing.
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
            let resultOnlyVariables =
              Set.difference returnVariables parameterVariables
            let declaredVariables = Set.ofList fn.typeParams
            if not (Set.isSubset resultOnlyVariables declaredVariables) then
              // An undeclared result-only variable is known only at runtime;
              // quantifying it would incorrectly accept every caller.
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
              let environment = addFunction name signature environment
              let environment =
                if Set.isEmpty resultOnlyVariables then
                  environment
                else
                  // Result-only parameters require reified type arguments, such
                  // as `jsonParse<'a>`; no value argument can infer them.
                  requireExplicitTypeArguments name environment
              environment, errors)
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
// Closed package batches
// --------------------

type ItemVerdict = { item : Reference; verdict : Verdict }

type BatchResult =
  { environment : TypeEnvironment
    types : List<ItemVerdict>
    values : List<ItemVerdict>
    functions : List<ItemVerdict> }
