/// Checks a batch of PackageOps must pass before anything is written.
///
/// These run against the ops themselves, and none of them write. They exist so an authoring
/// surface can refuse a batch and say why, while the paths that must converge
/// without asking anyone (sync's fold, historical replay) go on accepting
/// everything and resolving it by their own rules.
module LibDB.OpValidation

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
open LibSerialization.Hashing


let reservedOwners : Set<string> = Set.ofList [ "Darklang" ]

/// Reject changes to package names under a reserved owner, such as `Darklang`.
/// A rename also updates its callers, so check their package locations too.
/// Calling a `Darklang` function is allowed; changing a `Darklang` declaration is not.
let reservedOwnerViolation ops =
  let bindings =
    ops
    |> List.collect PT.packageOpBindingEffects
    |> List.choose (function
      | PT.PackageOp.SetName(location, _, _)
      | PT.PackageOp.Unbind(location, _)
      | PT.PackageOp.Decision(_, location, _, PT.DecisionKind.Override _) ->
        Some location.owner
      | _ -> None)
  let references =
    ops
    |> List.collect PT.packageOpReferenceSnapshots
    |> List.map (fun snapshot -> snapshot.location.owner)
  bindings @ references
  |> List.tryFind (fun owner -> Set.contains owner reservedOwners)
  |> Option.map (fun owner ->
    $"cannot change a package name under the reserved owner \"{owner}\"; it is reserved for the bundled standard library")
