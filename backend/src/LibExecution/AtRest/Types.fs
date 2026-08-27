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
  | TRigidVariable of string
  | TInferenceVariable of int

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

/// Where in the item the checker was working when it found something.
///
/// Reported next to the expected and actual types, which carry the substance. This is
/// the "in a function argument" half of "expected Int, got String, in a function
/// argument".
type Site =
  | LambdaReturnValue
  | FunctionReturnValue
  | ValueBody
  | Expression
  | StatementBeforeFinalExpression
  | FunctionApplication
  | FunctionArgument of position : int
  | IfWithoutElse
  | RecordFieldAccess
  | UnitLetPattern
  | TupleLetPattern
  | MatchPattern
  | ListPattern
  | ListConsPattern
  | TupleMatchPattern
  | OrPatternBinding
  | PipelineInput
  | PipelineFunction
  | PipelineVariable
  | PipelineEnumInput
  | PipelineBooleanOperator
  | PipelineStringConcatenation
  | PipelineComparison
  | PipelineNumericOperator

/// Which of several same-coded places a name was declared twice.
type DuplicateSite =
  | InTypeDeclaration
  | InFunctionSignature
  | InRecordConstruction
  | InRecordUpdate
  | InPattern

/// What the checker could not pin down when it reports an ambiguous type.
type AmbiguousSubject =
  | NumericOperand
  | PipelineNumericOperand
  | UnaryMinusOperand
  | RecordType
  | EnumPatternType
  | ItemType

/// Why a builtin's declared signature is not something the checker can check against.
type UntrustedBuiltin =
  /// The declared result contains type variables no parameter constrains, so the real
  /// result type is only known at runtime.
  | ResultTypeUnconstrained
  /// `unwrap` applied to something not statically known to be an Option or a Result.
  | UnwrapArgumentUnknown
  /// An operator builtin used as a value or partially applied. Applied to all its
  /// arguments it is checked by the operator rule instead.
  | OperatorNotFullyApplied
  /// The result cannot be inferred from the value parameters, so the call must say.
  | ExplicitTypeArgumentsRequired

/// What an issue is about, beyond its code.
///
/// The checker states facts and Darklang turns them into a sentence
/// (`LanguageTools.AtRestTypeChecker.contextToString`). That split is what lets each
/// surface phrase things its own way, and it is also better output: the checker has no
/// name resolver, so rendering a name here could only ever produce `Type #a1b2c3d4`,
/// while the Dark side can look the hash up and say `Stdlib.Option.Option`.
type Context =
  /// The code says everything; there is nothing to add.
  | NoDetail
  /// Where the checker was working. Pairs with expected/actual.
  | At of Site
  /// A name the resolver never resolved. The parts are what the author wrote.
  | Unresolved of attempted : List<string>
  | TypeUnavailable of FQTypeName.Package
  | FunctionUnavailable of FQFnName.FQFnName
  | ValueUnavailable of FQValueName.FQValueName
  /// One identifier the issue is about: a variable, field, case, or type parameter.
  | Identifier of name : string
  /// Several of them, e.g. the record fields a construction left out.
  | Identifiers of names : List<string>
  | Duplicate of name : string * site : DuplicateSite
  | Ambiguous of subject : AmbiguousSubject
  | Untrusted of fn : FQFnName.FQFnName * reason : UntrustedBuiltin
  | Arity of expected : int * actual : int
  | NamedArity of name : string * expected : int * actual : int
  | TypeArity of typ : FQTypeName.Package * expected : int * actual : int
  | ArgumentIndex of index : int
  /// A match that is not exhaustive, with a witness when one could be produced.
  /// CLEANUP: the witness is still rendered here, by `Patterns.missingPatternToString`.
  /// It is the one piece of prose left in F#, and it wants a `MissingPattern` mirror.
  | UncoveredPattern of witness : Option<string>
  /// An infix operator applied to an operand type it does not accept. The operation is
  /// the `InfixFnName` case name, which is what the message used before this was
  /// structured; it wants a proper mirror too.
  | InfixOperandUnsupported of operation : string
  // Situations the code alone does not distinguish.
  | RecordRequiredForConstruction
  | RecordRequiredForFieldAccess
  | RecordRequiredForUpdate
  | EnumRequiredForConstruction
  | EnumRequiredForPattern
  | SelfOutsideFunction
  | OrPatternBindingsDiffer
  | ExplicitTypeArgumentsOnNonNamedFunction
  | AliasCycleReferenced
  | DeclarationTooDeep
  | UnaryMinusOperandNotSignedNumeric
  /// The checker could not run at all. The detail is an exception or import-failure
  /// message, which genuinely has no structure worth keeping.
  | CheckerUnavailable of detail : string

type Diagnostic =
  { code : DiagnosticCode
    nodeId : Option<id>
    expected : Option<StaticType>
    actual : Option<StaticType>
    context : Context }

type Blocker = { code : BlockerCode; nodeId : Option<id>; context : Context }

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
      unsupportedFunctions : Map<FQFnName.FQFnName, UntrustedBuiltin>
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
    (reason : UntrustedBuiltin)
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
              addUnsupportedFunction name ResultTypeUnconstrained environment,
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
