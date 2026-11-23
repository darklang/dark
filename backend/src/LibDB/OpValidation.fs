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


let private isExternFn (fn : PT.PackageFn.PackageFn) : bool =
  match fn.body with
  | PT.EExtern _ -> true
  | _ -> false

/// Reject an `extern` called from outside the package that declares it, that is from anywhere
/// but its own module and the modules inside it. Other code calls the plain fns there that wrap
/// it. <param storedExtern> answers for fns the batch does not add.
let externCallViolation
  (storedExtern : Hash -> bool)
  (ops : List<PT.PackageOp>)
  : Option<string> =
  let added =
    ops
    |> List.choose (function
      | PT.PackageOp.AddFn fn -> Some(fn.hash, fn)
      | _ -> None)
    |> Map.ofList
  let named =
    ops
    |> List.choose (function
      | PT.PackageOp.SetName(location, PT.PackageFn hash, _) -> Some(hash, location)
      | _ -> None)
    |> Map.ofList
  let isExtern hash =
    match Map.tryFind hash added with
    | Some fn -> isExternFn fn
    | None -> storedExtern hash
  let packageOf (location : PT.PackageLocation) = location.owner :: location.modules
  let inside (package : List<string>) (modules : List<string>) =
    List.length modules >= List.length package
    && List.take (List.length package) modules = package
  let display (location : PT.PackageLocation) =
    String.concat "." (packageOf location @ [ location.name ])
  let crossPackageCalls =
    ops
    |> List.collect (function
      | PT.PackageOp.SetName(caller, PT.PackageFn hash, _) ->
        match Map.tryFind hash added with
        | None -> []
        | Some fn ->
          DependencyExtractor.extractFromFn fn
          |> List.choose (fun dep ->
            let target = Map.tryFind dep.hash named |> Option.orElse dep.location
            match dep.itemKind, target with
            | PT.ItemKind.Fn, Some target when
              not (inside (packageOf target) (packageOf caller))
              ->
              Some(caller, target, dep.hash)
            | _ -> None)
      | _ -> [])
  let externs =
    crossPackageCalls
    |> List.map (fun (_, _, hash) -> hash)
    |> List.distinct
    |> List.filter isExtern
    |> Set.ofList
  crossPackageCalls
  |> List.tryFind (fun (_, _, hash) -> Set.contains hash externs)
  |> Option.map (fun (caller, target, _) ->
    let package = String.concat "." (packageOf target)
    $"{display caller} calls {display target}, an extern of another package; call the fns in {package} that wrap it")

/// Reject an `extern` declared outside the bundled packages.
let externDeclarationViolation (ops : List<PT.PackageOp>) : Option<string> =
  ops
  |> List.tryPick (function
    | PT.PackageOp.AddFn fn when isExternFn fn ->
      Some "extern declarations are only allowed in bundled packages"
    | _ -> None)
