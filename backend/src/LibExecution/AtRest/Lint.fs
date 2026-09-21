/// Optional checks over inference results. Callers choose which types must be used;
/// this module has no knowledge of packages or authority to change a type verdict.
module LibExecution.AtRest.Lint

open Prelude
open LibExecution.ProgramTypes
open LibExecution.AtRest.Types

type UnusedResult = { nodeId : id; name : string }

let rec private matchBinds (name : string) (pattern : MatchPattern) : bool =
  let recurse = matchBinds name
  match pattern with
  | MPVariable(_, bound) -> bound = name
  | MPList(_, patterns)
  | MPEnum(_, _, patterns) -> List.exists recurse patterns
  | MPListCons(_, head, tail) -> recurse head || recurse tail
  | MPTuple(_, first, second, rest) -> List.exists recurse (first :: second :: rest)
  | MPOr(_, patterns) -> patterns |> NEList.toList |> List.exists recurse
  | _ -> false

/// Is this outer binding referenced? A new binding hides it only in its own scope.
let rec private uses (name : string) (expr : Expr) : bool =
  let recurse = uses name
  let binds pattern = Set.contains name (LetPattern.symbolsUsed pattern)
  let lambdaUses patterns body =
    not (patterns |> NEList.toList |> List.exists binds) && recurse body
  match expr with
  | EVariable(_, variable) -> variable = name
  | ELet(_, pattern, value, body) ->
    recurse value || (not (binds pattern) && recurse body)
  | ELambda(_, patterns, body) -> lambdaUses patterns body
  | EMatch(_, target, cases) ->
    recurse target
    || (cases
        |> List.exists (fun case ->
          not (matchBinds name case.pat)
          && (Option.exists recurse case.whenCondition || recurse case.rhs)))
  | EPipe(_, lhs, parts) ->
    recurse lhs
    || (parts
        |> List.exists (fun part ->
          match part with
          | EPipeLambda(_, patterns, body) -> lambdaUses patterns body
          | EPipeVariable(_, variable, args) ->
            variable = name || List.exists recurse args
          | other ->
            LibExecution.ProgramTypesAst.pipeSubExprs other |> List.exists recurse))
  | other -> LibExecution.ProgramTypesAst.subExprs other |> List.exists recurse

/// Find unused whole-value bindings whose types the caller requires using.
/// This detects discarded results, not whether a use contributes to the final value.
let unusedResults
  (requiresUse : StaticType -> bool)
  (bindings : List<TypedBinding>)
  : List<UnusedResult> =
  bindings
  |> List.choose (fun binding ->
    if not (requiresUse binding.typ) then
      None
    else
      let unused =
        match binding.pattern with
        | LPWildcard _ -> Some "_"
        | LPVariable(_, name) when not (uses name binding.body) -> Some name
        | _ -> None
      unused |> Option.map (fun name -> { nodeId = binding.nodeId; name = name }))

let bindingsOf (verdict : Verdict) : List<TypedBinding> =
  match verdict with
  | Checked proof -> Proof.bindingsOf proof
  | Failed report
  | Incomplete report -> report.bindings
