/// Checking a pattern, and proving a match covers its type.
///
/// Two jobs that share a vocabulary. `checkMatchPattern` validates one pattern's shape
/// and extends the local environment with what it binds, recovering bindings after an
/// invalid pattern so a single error does not cascade into scope errors. The
/// constructor matrix below then answers the separate question of whether a set of
/// patterns is exhaustive, and if not, produces a witness the author can read.
module LibExecution.AtRest.Patterns

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

open LibExecution.AtRest.Types
open LibExecution.AtRest.Unification


let internal duplicateNames (names : List<string>) : List<string> =
  names
  |> List.countBy (fun name -> name)
  |> List.choose (fun (name, count) -> if count > 1 then Some name else None)

let internal addBindings
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
      Duplicate(name, InPattern)
    )
  let locals =
    bindings
    |> List.fold
      (fun locals (name, typ) -> Map.add name (monomorphic typ) locals)
      env.locals
  { env with locals = locals }

let rec internal checkLetPattern
  (state : State)
  (typ : StaticType)
  (pattern : LetPattern)
  : List<string * StaticType> =
  ensureStack ()
  match pattern with
  | LPVariable(_, name) -> [ name, typ ]
  | LPWildcard _ -> []
  | LPUnit nodeId ->
    unify state (Some nodeId) UnitLetPattern TUnit typ
    []
  | LPTuple(nodeId, first, second, rest) ->
    let parts = List.init (2 + List.length rest) (fun _ -> state.Fresh(Some nodeId))
    let tuple = TTuple(parts[0], parts[1], List.skip 2 parts)
    unify state (Some nodeId) TupleLetPattern tuple typ
    List.zip (first :: second :: rest) parts
    |> List.collect (fun (pattern, typ) -> checkLetPattern state typ pattern)

let internal declarationForCustom
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
      state.Block(MissingTypeDeclaration, nodeId, TypeUnavailable name)
      None
  | _ -> None

let internal declarationFieldType
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
    // Keep malformed serialized input at this boundary from throwing.
    let expected = List.length typeParams
    let actual = List.length typeArgs
    state.Error(TypeMismatch, nodeId, None, None, Arity(expected, actual))
    state.FreshTainted nodeId

let rec internal patternBindingNames (pattern : MatchPattern) : List<string> =
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

let internal recoverPatternBindings
  (state : State)
  (nodeId : id)
  (patterns : List<MatchPattern>)
  : List<string * StaticType> =
  patterns
  |> List.collect patternBindingNames
  |> List.map (fun name -> name, state.Fresh(Some nodeId))

let rec internal checkMatchPattern
  (state : State)
  (expected : StaticType)
  (pattern : MatchPattern)
  : List<string * StaticType> =
  ensureStack ()
  let literal nodeId typ =
    unify state (Some nodeId) MatchPattern expected typ
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
    unify state (Some nodeId) ListPattern (TList element) expected
    patterns |> List.collect (checkMatchPattern state element)
  | MPListCons(nodeId, head, tail) ->
    let element = state.Fresh(Some nodeId)
    unify state (Some nodeId) ListConsPattern (TList element) expected
    checkMatchPattern state element head
    @ checkMatchPattern state (TList element) tail
  | MPTuple(nodeId, first, second, rest) ->
    let parts = List.init (2 + List.length rest) (fun _ -> state.Fresh(Some nodeId))
    unify
      state
      (Some nodeId)
      TupleMatchPattern
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
            Identifier caseName
          )
          recoverPatternBindings state nodeId fieldPatterns
        | Some case when List.length case.fields <> List.length fieldPatterns ->
          let expected = List.length case.fields
          let actual = List.length fieldPatterns
          state.Error(
            EnumFieldCountMismatch,
            Some nodeId,
            None,
            None,
            NamedArity(caseName, expected, actual)
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
          EnumRequiredForPattern
        )
        recoverPatternBindings state nodeId fieldPatterns
    | None ->
      let tainted = containsTaintedInferenceVariable state expected
      if not tainted then
        state.Block(AmbiguousType, Some nodeId, Ambiguous EnumPatternType)
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
            Duplicate(name, InPattern)
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
            OrPatternBindingsDiffer
          )
        for KeyValue(name, typ) in alternative do
          match Map.tryFind name first with
          | Some firstType ->
            unify state (Some nodeId) OrPatternBinding firstType typ
          | None -> ()
      Map.toList first

let rec internal unguardedPatternAlternatives
  (pattern : MatchPattern)
  : List<MatchPattern> =
  ensureStack ()
  match pattern with
  | MPOr(_, alternatives) ->
    alternatives |> NEList.toList |> List.collect unguardedPatternAlternatives
  | pattern -> [ pattern ]

let rec internal patternIsIrrefutable (pattern : MatchPattern) : bool =
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

type internal PatternConstructor =
  | UnitConstructor
  | BoolConstructor of bool
  | ListEmptyConstructor
  | ListConsConstructor
  | TupleConstructor of int
  | EnumConstructor of string

type internal MissingPattern =
  | MissingWildcard
  | MissingConstructor of PatternConstructor * List<MissingPattern>

let rec internal missingPatternToString (pattern : MissingPattern) : string =
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

let internal finiteConstructors
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

let rec internal specializePattern
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

let rec internal findUncoveredPattern
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

let rec internal patternMatrixIsExhaustive
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

let internal uncoveredMatchPattern
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
