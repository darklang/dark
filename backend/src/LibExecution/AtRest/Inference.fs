/// Inferring and checking the type of an expression.
///
/// Bidirectional: an expected type is used where one is available and inference fills
/// in the rest. The mutually recursive group at the bottom is one `let rec ... and`
/// chain by necessity, since every construct can contain any other.
module LibExecution.AtRest.Inference

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PackageRefs = LibExecution.PackageRefs

open LibExecution.AtRest.Types
open LibExecution.AtRest.Unification
open LibExecution.AtRest.Patterns


let internal isNumeric (typ : StaticType) : bool =
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

let internal supportsNumericOperation
  (operation : InfixFnName)
  (typ : StaticType)
  : bool =
  match operation, typ with
  | ArithmeticPower, (TInt128 | TUInt128) -> false
  | _ -> isNumeric typ

let rec internal isNonExpansive (expr : Expr) : bool =
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

/// `Result<T, _> -> T`. An unknown argument remains uncheckable.
let internal asUnwrapBuiltin
  (name : NameResolution<FQFnName.FQFnName>)
  : Option<FQFnName.FQFnName> =
  match resolvedName name with
  | Some(FQFnName.Builtin b as fqName) when b.name = "unwrap" && b.version = 0 ->
    Some fqName
  | _ -> None

let internal inferUnwrapResult
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
      Untrusted(fqName, UnwrapArgumentUnknown)
    )
    state.FreshTainted(Some nodeId)

/// Signed integers and Float: what unary minus accepts (`Builtin.negate`).
let internal isSignedNumeric (typ : StaticType) : bool =
  match typ with
  | TInt8
  | TInt16
  | TInt32
  | TInt64
  | TInt128
  | TInt
  | TFloat -> true
  | _ -> false

/// Recognize operator builtins whose loose signatures require infix rules.
let internal asOperatorBuiltin
  (name : NameResolution<FQFnName.FQFnName>)
  : Option<FQFnName.FQFnName * Infix> =
  match resolvedName name with
  | Some(FQFnName.Builtin b as fqName) when b.version = 0 ->
    InfixFnName.tryFromBuiltinName b.name
    |> Option.map (fun op -> fqName, InfixFnCall op)
  | _ -> None

/// Recognize the builtin used for non-literal unary minus.
let internal asNegateBuiltin
  (name : NameResolution<FQFnName.FQFnName>)
  : Option<FQFnName.FQFnName> =
  match resolvedName name with
  | Some(FQFnName.Builtin b as fqName) when
    b.name = InfixFnName.negateBuiltinName && b.version = 0
    ->
    Some fqName
  | _ -> None

let internal isOperatorLikeBuiltin
  (name : NameResolution<FQFnName.FQFnName>)
  : bool =
  Option.isSome (asOperatorBuiltin name) || Option.isSome (asNegateBuiltin name)

/// Check unary minus and return its operand type.
let internal inferNegateResult
  (state : State)
  (nodeId : id)
  (fqName : FQFnName.FQFnName)
  (argType : StaticType)
  : StaticType =
  state.AddDependency(FunctionDependency fqName)
  let concrete = normalizeAliases state (Some nodeId) Set.empty argType
  if not (isSignedNumeric concrete) then
    match concrete with
    | TInferenceVariable _ ->
      if not (containsTaintedInferenceVariable state argType) then
        state.Block(AmbiguousType, Some nodeId, Ambiguous UnaryMinusOperand)
    | _ ->
      state.Error(
        InvalidInfixOperand,
        Some nodeId,
        None,
        Some concrete,
        UnaryMinusOperandNotSignedNumeric
      )
  argType

let internal instantiateFunction
  (state : State)
  (nodeId : Option<id>)
  (typeVariableScope : Map<string, StaticType>)
  (name : NameResolution<FQFnName.FQFnName>)
  (explicitTypeArgs : List<TypeReference>)
  : StaticType =
  match resolvedName name with
  | None ->
    state.Block(UnresolvedFunctionName, nodeId, Unresolved name.originalName)
    state.FreshTainted nodeId
  | Some fqName when isOperatorLikeBuiltin name ->
    // Applied to its full argument list it is checked as the operator (see
    // `asOperatorBuiltin`); as a value or partially applied there is no
    // signature to give it, since the declared one overstates what it accepts.
    state.AddDependency(FunctionDependency fqName)
    state.Block(
      UnsupportedConstruct,
      nodeId,
      Untrusted(fqName, OperatorNotFullyApplied)
    )
    state.FreshTainted nodeId
  | Some name ->
    state.AddDependency(FunctionDependency name)
    match Map.tryFind name state.Environment.unsupportedFunctions with
    | Some reason ->
      state.Block(UnsupportedConstruct, nodeId, Untrusted(name, reason))
      state.FreshTainted nodeId
    | None when
      Set.contains name state.Environment.requiresExplicitTypeArguments
      && List.isEmpty explicitTypeArgs
      ->
      state.Block(
        UnsupportedConstruct,
        nodeId,
        Untrusted(name, ExplicitTypeArgumentsRequired)
      )
      state.FreshTainted nodeId
    | None ->
      match Map.tryFind name state.Environment.functions with
      | None ->
        state.Block(MissingFunctionSignature, nodeId, FunctionUnavailable name)
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

let internal instantiateCustomType
  (state : State)
  (nodeId : Option<id>)
  (typeVariableScope : Map<string, StaticType>)
  (name : NameResolution<FQTypeName.FQTypeName>)
  (explicitTypeArgs : List<TypeReference>)
  : Option<FQTypeName.Package * List<StaticType> * TypeDeclaration.T> =
  match resolvedName name with
  | None ->
    state.Block(UnresolvedTypeName, nodeId, Unresolved name.originalName)
    None
  | Some(FQTypeName.Package packageName) ->
    state.AddDependency(TypeDependency packageName)
    match Map.tryFind packageName state.Environment.types with
    | None ->
      state.Block(MissingTypeDeclaration, nodeId, TypeUnavailable packageName)
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

let rec internal checkExprWithContext
  (state : State)
  (env : Env)
  (expected : StaticType)
  (expr : Expr)
  (site : Site)
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
    checkExprWithContext state lambdaEnv returnType body LambdaReturnValue
  | _ ->
    let actual = inferExpr state env expr
    unify state (Some(Expr.toID expr)) site expected actual

and internal checkExpr
  (state : State)
  (env : Env)
  (expected : StaticType)
  (expr : Expr)
  : unit =
  ensureStack ()
  checkExprWithContext state env expected expr Expression

and internal applyArguments
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
          (FunctionArgument position)
        match parameterList.Tail with
        | [] -> apply (position + 1) returnType remaining
        | next :: rest ->
          apply (position + 1) (TFn(NEList.ofList next rest, returnType)) remaining
      | TInferenceVariable _ ->
        let parameter = state.Fresh(Some nodeId)
        let result = state.Fresh(Some nodeId)
        unify
          state
          (Some nodeId)
          FunctionApplication
          (TFn(NEList.singleton parameter, result))
          callee
        checkExprWithContext state env parameter argument (FunctionArgument position)
        apply (position + 1) result remaining
      | notCallable ->
        state.Error(NotCallable, Some nodeId, None, Some notCallable, NoDetail)
        state.Fresh(Some nodeId)
  apply 1 callee arguments

and internal inferRecordConstruction
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
            Duplicate(name, InRecordConstruction)
          )
        for name, value in fields do
          match declaredFields |> List.tryFind (fun field -> field.name = name) with
          | None ->
            state.Error(UnknownRecordField, Some nodeId, None, None, Identifier name)
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
          state.Error(
            MissingRecordField,
            Some nodeId,
            None,
            None,
            Identifiers missingFields
          )
      | TypeDeclaration.Enum _
      | TypeDeclaration.Alias _ ->
        state.Error(
          TypeMismatch,
          Some nodeId,
          None,
          Some result,
          RecordRequiredForConstruction
        )
      result

and internal inferEnumConstruction
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
            Identifier caseName
          )
        | Some case when List.length case.fields <> List.length fields ->
          let expected = List.length case.fields
          let actual = List.length fields
          state.Error(
            EnumFieldCountMismatch,
            Some nodeId,
            None,
            None,
            NamedArity(caseName, expected, actual)
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
          EnumRequiredForConstruction
        )
    result

and internal inferInfix
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
      | TInferenceVariable _ ->
        if not (containsTaintedInferenceVariable state lhsType) then
          state.Block(AmbiguousType, Some nodeId, Ambiguous NumericOperand)
      | _ ->
        state.Error(
          InvalidInfixOperand,
          Some nodeId,
          None,
          Some concrete,
          InfixOperandUnsupported(string operation)
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

and internal inferPipePart
  (state : State)
  (env : Env)
  (input : StaticType)
  (part : PipeExpr)
  : StaticType =
  ensureStack ()
  match part with
  | EPipeLambda(nodeId, patterns, body) ->
    let parameters = patterns |> NEList.map (fun _ -> state.Fresh(Some nodeId))
    unify state (Some nodeId) PipelineInput parameters.head input
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
      unify state (Some nodeId) PipelineBooleanOperator TBool input
      unify state (Some nodeId) PipelineBooleanOperator TBool rhsType
      TBool
    | InfixFnCall StringConcat ->
      unify state (Some nodeId) PipelineStringConcatenation TString input
      unify state (Some nodeId) PipelineStringConcatenation TString rhsType
      TString
    | InfixFnCall ComparisonEquals
    | InfixFnCall ComparisonNotEquals ->
      unify state (Some nodeId) PipelineComparison input rhsType
      TBool
    | InfixFnCall operation ->
      unify state (Some nodeId) PipelineNumericOperator input rhsType
      let concrete = normalizeAliases state (Some nodeId) Set.empty input
      if not (supportsNumericOperation operation concrete) then
        match concrete with
        | TInferenceVariable _ ->
          if not (containsTaintedInferenceVariable state input) then
            state.Block(AmbiguousType, Some nodeId, Ambiguous PipelineNumericOperand)
        | _ ->
          state.Error(
            InvalidInfixOperand,
            Some nodeId,
            None,
            Some concrete,
            InfixOperandUnsupported(string operation)
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
      let afterInput = applyKnownInput state nodeId PipelineFunction fnType input
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
                unify state (Some nodeId) PipelineEnumInput fieldType input
              else
                checkExpr state env fieldType expr)
          | Some case ->
            let expected = List.length case.fields
            let actual = 1 + List.length fields
            state.Error(
              EnumFieldCountMismatch,
              Some nodeId,
              None,
              None,
              NamedArity(caseName, expected, actual)
            )
          | None ->
            state.Error(
              UnknownEnumCase,
              Some nodeId,
              Some result,
              None,
              Identifier caseName
            )
        | _ ->
          state.Error(
            TypeMismatch,
            Some nodeId,
            None,
            Some result,
            EnumRequiredForConstruction
          )
      result
  | EPipeVariable(nodeId, name, args) ->
    match Map.tryFind name env.locals with
    | None ->
      state.Error(UnknownVariable, Some nodeId, None, None, Identifier name)
      state.FreshTainted(Some nodeId)
    | Some scheme ->
      // Apply the known input without making an AST literal of the wrong type.
      let fnType = instantiateScheme state (Some nodeId) scheme
      let afterInput = applyKnownInput state nodeId PipelineVariable fnType input
      applyArguments state env nodeId afterInput args

and internal applyKnownInput
  (state : State)
  (nodeId : id)
  (site : Site)
  (callee : StaticType)
  (input : StaticType)
  : StaticType =
  ensureStack ()
  match normalizeAliases state (Some nodeId) Set.empty callee with
  | TFn(parameters, returnType) ->
    unify state (Some nodeId) site parameters.head input
    match parameters.tail with
    | [] -> returnType
    | next :: rest -> TFn(NEList.ofList next rest, returnType)
  | TInferenceVariable _ ->
    let parameter = state.Fresh(Some nodeId)
    let result = state.Fresh(Some nodeId)
    unify state (Some nodeId) site (TFn(NEList.singleton parameter, result)) callee
    unify state (Some nodeId) site parameter input
    result
  | actual ->
    state.Error(NotCallable, Some nodeId, None, Some actual, At site)
    state.Fresh(Some nodeId)

and internal inferExpr (state : State) (env : Env) (expr : Expr) : StaticType =
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
      unify state (Some nodeId) IfWithoutElse TUnit thenType
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
        state.Block(NonExhaustiveMatch, Some nodeId, UncoveredPattern None)
      | Some missing ->
        let missing = missingPatternToString missing
        state.Block(NonExhaustiveMatch, Some nodeId, UncoveredPattern(Some missing))
      | None -> ()
    resultType
  | ELet(nodeId, pattern, value, body) ->
    let valueType =
      match pattern, value with
      // An unshadowed let-bound lambda can recurse by name. If an outer local has
      // the same name, the runtime captures that binding instead.
      | LPVariable(_, name), ELambda(_, patterns, _) when
        name <> "" && name <> "_" && not (Map.containsKey name env.locals)
        ->
        // Pre-shape the recursive type to the lambda's arity; inference for an
        // unknown callee otherwise builds a curried type one argument at a time.
        let parameterTypes =
          patterns |> NEList.map (fun _ -> state.Fresh(Some nodeId))
        let selfType = TFn(parameterTypes, state.Fresh(Some nodeId))
        let recEnv = addBindings state (Some nodeId) env [ name, selfType ]
        checkExpr state recEnv selfType value
        selfType
      | _ -> inferExpr state env value
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
      state.Error(UnknownVariable, Some nodeId, None, None, Identifier name)
      state.FreshTainted(Some nodeId)
  | EArg(nodeId, index) ->
    match List.tryItem index env.arguments with
    | Some typ -> typ
    | None ->
      state.Error(InvalidArgumentIndex, Some nodeId, None, None, ArgumentIndex index)
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
              ExplicitTypeArgumentsOnNonNamedFunction
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
    | TInferenceVariable _ when containsTaintedInferenceVariable state recordType ->
      state.FreshTainted(Some nodeId)
    | TInferenceVariable _ ->
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
              Identifier fieldName
            )
            state.Fresh(Some nodeId)
        | _ ->
          state.Error(
            TypeMismatch,
            Some nodeId,
            None,
            Some recordType,
            RecordRequiredForFieldAccess
          )
          state.Fresh(Some nodeId)
      | None ->
        if containsTaintedInferenceVariable state recordType then
          state.FreshTainted(Some nodeId)
        else
          state.Block(AmbiguousType, Some nodeId, Ambiguous RecordType)
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
        Duplicate(name, InRecordUpdate)
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
              Identifier name
            )
            let _ = inferExpr state env value
            ()
      | _ ->
        state.Error(
          TypeMismatch,
          Some nodeId,
          None,
          Some recordType,
          RecordRequiredForUpdate
        )
    | None ->
      if not (containsTaintedInferenceVariable state recordType) then
        state.Block(AmbiguousType, Some nodeId, Ambiguous RecordType)
    recordType
  | EEnum(nodeId, name, typeArgs, caseName, fields) ->
    inferEnumConstruction state env nodeId name typeArgs caseName fields
  | EValue(nodeId, name) ->
    match resolvedName name with
    | None ->
      state.Block(UnresolvedValueName, Some nodeId, Unresolved name.originalName)
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
          state.Block(MissingValueSignature, Some nodeId, ValueUnavailable name)
          state.FreshTainted(Some nodeId)
  | EStatement(_, first, next) ->
    checkExprWithContext state env TUnit first StatementBeforeFinalExpression
    inferExpr state env next
  | ESelf nodeId ->
    match env.self with
    | Some typ -> typ
    | None ->
      state.Error(UnknownVariable, Some nodeId, None, None, SelfOutsideFunction)
      state.Fresh(Some nodeId)
