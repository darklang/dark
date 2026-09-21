/// Resolve local references to the exact binding they use. This is syntax-level
/// analysis: it understands lexical scope and does not require type inference.
module LibExecution.AtRest.LocalBindingUsage

open Prelude
open LibExecution.ProgramTypes

type BindingKind =
  | FunctionParameter
  | LetBinding
  | LambdaParameter
  | MatchBinding

type BindingUse =
  {
    nodeId : Option<id>
    /// The expression that introduced the binding. Consumers can use this to
    /// correlate facts produced by other analyses for the same construct.
    introducingNodeId : Option<id>
    name : string
    kind : BindingKind
    used : bool
  }

type private MutableBinding =
  { nodeId : Option<id>
    introducingNodeId : Option<id>
    name : string
    kind : BindingKind
    mutable used : bool }

type private WalkState =
  { bindings : ResizeArray<MutableBinding>; arguments : Map<int, int> }

let private addBinding
  (bindings : ResizeArray<MutableBinding>)
  (nodeId : Option<id>)
  (introducingNodeId : Option<id>)
  (kind : BindingKind)
  (name : string)
  : Option<int> =
  if name = "" || name = "_" then
    None
  else
    let index = bindings.Count
    bindings.Add
      { nodeId = nodeId
        introducingNodeId = introducingNodeId
        name = name
        kind = kind
        used = false }
    Some index

let private bindLetPattern
  (state : WalkState)
  (introducingNodeId : id)
  (kind : BindingKind)
  (env : Map<string, int>)
  (pattern : LetPattern)
  : Map<string, int> =
  let rec bind env pattern =
    match pattern with
    | LPVariable(nodeId, name) ->
      match
        addBinding state.bindings (Some nodeId) (Some introducingNodeId) kind name
      with
      | Some index -> Map.add name index env
      | None -> env
    | LPWildcard _
    | LPUnit _ -> env
    | LPTuple(_, first, second, rest) ->
      first :: second :: rest |> List.fold bind env
  bind env pattern

let private bindMatchPattern
  (state : WalkState)
  (introducingNodeId : id)
  (env : Map<string, int>)
  (pattern : MatchPattern)
  : Map<string, int> =
  let rec bind env pattern =
    match pattern with
    | MPVariable(nodeId, name) ->
      match
        addBinding
          state.bindings
          (Some nodeId)
          (Some introducingNodeId)
          MatchBinding
          name
      with
      | Some index -> Map.add name index env
      | None -> env
    | MPList(_, patterns)
    | MPEnum(_, _, patterns) -> patterns |> List.fold bind env
    | MPListCons(_, head, tail) -> [ head; tail ] |> List.fold bind env
    | MPTuple(_, first, second, rest) ->
      first :: second :: rest |> List.fold bind env
    // All alternatives bind the same names. They form one lexical binding, so
    // use the first alternative rather than emitting one fact per spelling.
    | MPOr(_, alternatives) -> bind env alternatives.head
    | _ -> env
  bind env pattern

let rec private walk
  (state : WalkState)
  (env : Map<string, int>)
  (nonCountingBindings : Set<int>)
  (expr : Expr)
  : unit =
  let recurse = walk state env nonCountingBindings
  let mark index =
    if not (Set.contains index nonCountingBindings) then
      state.bindings[index].used <- true
  match expr with
  | EVariable(_, name) -> Map.tryFind name env |> Option.iter mark
  | EArg(_, index) -> Map.tryFind index state.arguments |> Option.iter mark
  | ELet(nodeId, pattern, value, body) ->
    match pattern, value with
    // This is the same recursion rule as inference and execution: an
    // unshadowed lambda may see its own name. A self-reference alone does not
    // make an otherwise unreachable local function used.
    | LPVariable(_, name), ELambda _ when
      name <> "" && name <> "_" && not (Map.containsKey name env)
      ->
      let bodyEnv = bindLetPattern state nodeId LetBinding env pattern
      let initializerNonCountingBindings =
        match Map.tryFind name bodyEnv with
        | Some binding -> Set.add binding nonCountingBindings
        | None -> nonCountingBindings
      walk state bodyEnv initializerNonCountingBindings value
      walk state bodyEnv nonCountingBindings body
    | _ ->
      recurse value
      let bodyEnv = bindLetPattern state nodeId LetBinding env pattern
      walk state bodyEnv nonCountingBindings body
  | ELambda(nodeId, patterns, body) ->
    let bodyEnv =
      patterns
      |> NEList.toList
      |> List.fold (bindLetPattern state nodeId LambdaParameter) env
    walk state bodyEnv nonCountingBindings body
  | EMatch(nodeId, target, cases) ->
    recurse target
    cases
    |> List.iter (fun case ->
      let caseEnv = bindMatchPattern state nodeId env case.pat
      case.whenCondition |> Option.iter (walk state caseEnv nonCountingBindings)
      walk state caseEnv nonCountingBindings case.rhs)
  | EPipe(_, lhs, parts) ->
    recurse lhs
    parts
    |> List.iter (fun part ->
      match part with
      | EPipeLambda(nodeId, patterns, body) ->
        let bodyEnv =
          patterns
          |> NEList.toList
          |> List.fold (bindLetPattern state nodeId LambdaParameter) env
        walk state bodyEnv nonCountingBindings body
      | EPipeVariable(_, name, args) ->
        Map.tryFind name env |> Option.iter mark
        args |> List.iter recurse
      | other ->
        LibExecution.ProgramTypesAst.pipeSubExprs other |> List.iter recurse)
  | other -> LibExecution.ProgramTypesAst.subExprs other |> List.iter recurse

let private finish (state : WalkState) : List<BindingUse> =
  state.bindings
  |> Seq.map (fun binding ->
    ({ nodeId = binding.nodeId
       introducingNodeId = binding.introducingNodeId
       name = binding.name
       kind = binding.kind
       used = binding.used }
    : BindingUse))
  |> Seq.toList

/// Analyze the local bindings introduced inside an expression.
let analyzeExpression (expr : Expr) : List<BindingUse> =
  let state = { bindings = ResizeArray(); arguments = Map.empty }
  walk state Map.empty Set.empty expr
  finish state

/// Analyze a function's parameters and the local bindings in its body.
let analyzeFunction (fn : PackageFn.PackageFn) : List<BindingUse> =
  let bindings = ResizeArray()
  let mutable arguments = Map.empty
  let mutable locals = Map.empty
  fn.parameters
  |> NEList.toList
  |> List.iteri (fun index parameter ->
    match addBinding bindings None None FunctionParameter parameter.name with
    | Some binding ->
      arguments <- Map.add index binding arguments
      locals <- Map.add parameter.name binding locals
    | None -> ())
  let state = { bindings = bindings; arguments = arguments }
  walk state locals Set.empty fn.body
  finish state
