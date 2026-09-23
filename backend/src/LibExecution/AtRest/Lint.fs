/// Optional warning policies over package code and inference results.
module LibExecution.AtRest.Lint

open Prelude
open LibExecution.ProgramTypes
open LibExecution.AtRest.Types

module LocalBindingUsage = LibExecution.AtRest.LocalBindingUsage

/// Names beginning with `_` explicitly opt out of the unused-binding warning.
let unusedBindings
  (bindings : List<LocalBindingUsage.BindingUse>)
  : List<LocalBindingUsage.BindingUse> =
  bindings
  |> List.filter (fun binding ->
    not binding.used && not (binding.name.StartsWith("_")))


// --------------------
// Ignored-result policy
// --------------------

type UnusedResult = { nodeId : id; name : string }

/// Find unused whole-value bindings whose types the caller requires using.
/// This detects discarded results, not whether a use contributes to the final value.
let unusedResults
  (requiresUse : StaticType -> bool)
  (bindingUses : List<LocalBindingUsage.BindingUse>)
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
        | LPVariable(_, name) ->
          bindingUses
          |> List.tryFind (fun usage ->
            usage.introducingNodeId = Some binding.nodeId && usage.name = name)
          |> Option.bind (fun usage -> if usage.used then None else Some name)
        | _ -> None
      unused |> Option.map (fun name -> { nodeId = binding.nodeId; name = name }))

let bindingsOf (verdict : Verdict) : List<TypedBinding> =
  match verdict with
  | Checked proof -> Proof.bindingsOf proof
  | Failed report
  | Incomplete report -> report.bindings
