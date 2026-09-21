/// Static function references in ProgramTypes source expressions.
module LibExecution.CallGraph

open Prelude

module PT = LibExecution.ProgramTypes
module Ast = LibExecution.ProgramTypesAst

type Analysis =
  {
    names : List<PT.FQFnName.FQFnName>
    /// False when every call and value can be resolved statically.
    complete : bool
    /// True when this function passes one of its function parameters to code
    /// that may call it, so the caller must supply the missing permissions.
    escapesOwnCallback : bool
  }

/// Bump whenever completeness or reachability semantics change. Approval
/// fingerprints include this so an analyzer fix cannot silently bless an old,
/// narrower review.
/// Version 4 follows builtin callback metadata through generic forwarding helpers.
let analysisVersion = 4

type BuiltinMetadata =
  { callEffects : Set<LibExecution.Effects.Effect>; callbackParameters : Set<int> }

type BuiltinMetadataFor = string * int -> Option<BuiltinMetadata>

module private Analysis =
  let empty : Analysis = { names = []; complete = true; escapesOwnCallback = false }

  let unresolved : Analysis =
    { names = []; complete = false; escapesOwnCallback = false }

  /// Knowable from the call site, but not from here.
  let callbackEscape : Analysis =
    { names = []; complete = true; escapesOwnCallback = true }

  let combine (left : Analysis) (right : Analysis) : Analysis =
    { names = List.append left.names right.names
      complete = left.complete && right.complete
      escapesOwnCallback = left.escapesOwnCallback || right.escapesOwnCallback }

  let collect (f : 'a -> Analysis) (items : List<'a>) : Analysis =
    let names = ResizeArray<PT.FQFnName.FQFnName>()
    let mutable complete = true
    let mutable escapes = false
    for item in items do
      let analysis = f item
      names.AddRange analysis.names
      complete <- complete && analysis.complete
      escapes <- escapes || analysis.escapesOwnCallback
    { names = List.ofSeq names; complete = complete; escapesOwnCallback = escapes }

/// A resolved fn name, or an explicit incomplete marker.
let private nameRef (nr : PT.NameResolution<PT.FQFnName.FQFnName>) : Analysis =
  match nr.resolved with
  | Ok resolved ->
    { names = [ resolved.name ]; complete = true; escapesOwnCallback = false }
  | Error _ -> Analysis.unresolved

/// What a pipe part itself references, beyond its nested expressions.
let private pipeOwnRefs (pe : PT.PipeExpr) : Analysis =
  match pe with
  | PT.EPipeFnCall(_, nr, _, _) -> nameRef nr
  // A function held in a variable can be an effectful callback whose target
  // is not statically known here.
  | PT.EPipeVariable _ -> Analysis.unresolved
  | PT.EPipeLambda _
  | PT.EPipeInfix _
  | PT.EPipeEnum _ -> Analysis.empty

/// Find function-typed parameters by their `EArg` positions. A callback can be
/// passed to another function without appearing as a direct call here.
let callbackParams (fn : PT.PackageFn.PackageFn) : Set<int> =
  fn.parameters
  |> NEList.toList
  |> List.indexed
  |> List.choose (fun (index, (p : PT.PackageFn.Parameter)) ->
    match p.typ with
    | PT.TFn _ -> Some index
    | _ -> None)
  |> Set.ofList

/// Every fn-name an expression references — `EFnName` and the piped
/// `EPipeFnCall` — plus whether any call target is unknowable statically.
/// Only the call-shaped nodes are handled here; everything else folds its
/// sub-expressions via `ProgramTypesAst.subExprs`.
///
/// `callbacks` is `callbackParams` of the enclosing package fn; a reference to
/// one of those positions is unknowable wherever it appears.
let rec analyze (callbacks : Set<int>) (expr : PT.Expr) : Analysis =
  let own =
    match expr with
    | PT.EFnName(_, nr) -> nameRef nr
    // A passed callback may be called by the receiving function.
    | PT.EArg(_, index) when Set.contains index callbacks -> Analysis.callbackEscape
    // A package value may contain named functions or lambdas in an arbitrarily
    // nested runtime value. Until value bodies participate in closure analysis,
    // treating the reference as complete would let returned executable code be
    // approved as effect-free.
    | PT.EValue _ -> Analysis.unresolved
    | PT.EPipe(_, _, parts) -> Analysis.collect pipeOwnRefs parts
    | PT.EApply(_, fnExpr, _, _) ->
      match fnExpr with
      | PT.EFnName _
      | PT.ELambda _
      | PT.ESelf _ -> Analysis.empty
      // The callback escape is recorded by the `EArg` child below.
      | PT.EArg(_, index) when Set.contains index callbacks -> Analysis.empty
      // A function held in a variable, record, or value can be an effectful
      // callback. Its target is not statically known here.
      | _ -> Analysis.unresolved
    | _ -> Analysis.empty
  Analysis.combine own (Analysis.collect (analyze callbacks) (Ast.subExprs expr))


/// Analyze a package function with its function-typed parameters marked as
/// caller-supplied callbacks.
let analyzeFn (fn : PT.PackageFn.PackageFn) : Analysis =
  analyze (callbackParams fn) fn.body


/// Conservative permission requirements of a package function: the union of
/// the call effects of every builtin statically reachable from it, lambda
/// bodies included, since returned code may run later. Missing package
/// functions, dynamic calls, and unclassified builtins make the result
/// incomplete; an incomplete result must never be approved or treated as
/// effect-free.
///
/// The root letting one of its own function-typed parameters escape also makes
/// it incomplete: what runs there is the caller's to decide. A *dependency*
/// doing so does not, because the root either handed it something concrete,
/// already accounted for here, or forwarded its own parameter, which raises
/// the flag on the root itself.
module Requirements =
  module E = LibExecution.Effects

  type Result = { requiredEffects : Set<E.Effect>; complete : bool }

  /// The loaded, immutable dependency closure of one root: every package fn
  /// statically reachable from it (the root included) that could be loaded,
  /// with its body's call analysis computed once at load. A member missing
  /// from it is one that could not be loaded.
  type Closure = Map<PT.FQFnName.Package, PT.PackageFn.PackageFn * Analysis>

  /// Follow callback parameters through generic wrappers until no positions change.
  /// Unknown callback expressions make the analysis incomplete.
  let withCallbackMetadata
    (builtinMetadataFor : BuiltinMetadataFor)
    (closure : Closure)
    : Closure =
    let mutable positions = closure |> Map.map (fun (fn, _) -> callbackParams fn)

    let positionsFor (nr : PT.NameResolution<PT.FQFnName.FQFnName>) =
      match nr.resolved with
      | Ok { name = PT.FQFnName.Builtin b } ->
        builtinMetadataFor (b.name, b.version)
        |> Option.map _.callbackParameters
        |> Option.defaultValue Set.empty
      | Ok { name = PT.FQFnName.Package hash } ->
        Map.tryFind hash positions |> Option.defaultValue Set.empty
      | Error _ -> Set.empty

    let inspect (fn : PT.PackageFn.PackageFn) : Set<int> * bool =
      let mutable forwarded = Set.empty
      let mutable unknown = false
      let inspectCallback =
        function
        | Some(PT.EArg(_, index)) -> forwarded <- Set.add index forwarded
        | Some(PT.EFnName _)
        | Some(PT.ELambda _) -> () // Already covered by the expression traversal.
        | _ -> unknown <- true
      let inspectArgs callbacks args =
        callbacks
        |> Set.iter (fun index ->
          // A partial application need not have supplied the callback yet.
          if index < List.length args then inspectCallback (List.item index args))
      let rec flatten target args =
        match target with
        | PT.EApply(_, inner, _, previous) ->
          flatten inner (NEList.toList previous @ args)
        | _ -> target, args
      let rec walk expr =
        match expr with
        | PT.EApply(_, target, _, args) ->
          match flatten target (NEList.toList args) with
          | PT.EFnName(_, nr), args ->
            inspectArgs (positionsFor nr) (List.map Some args)
          | PT.ESelf _, args -> inspectArgs positions[fn.hash] (List.map Some args)
          | _ -> () // Dynamic call targets are already incomplete in `analyze`.
        | PT.EPipe(_, lhs, parts) ->
          let mutable input = Some lhs
          for part in parts do
            match part with
            | PT.EPipeFnCall(_, nr, _, args) ->
              inspectArgs (positionsFor nr) (input :: List.map Some args)
            | _ -> ()
            // A later pipe stage receives a computed value, not the original lhs.
            input <- None
        | _ -> ()
        Ast.subExprs expr |> List.iter walk
      walk fn.body
      forwarded, unknown

    let mutable changed = true
    while changed do
      changed <- false
      for KeyValue(hash, (fn, _)) in closure do
        let inferred, _ = inspect fn
        let combined = Set.union positions[hash] inferred
        if combined <> positions[hash] then
          positions <- Map.add hash combined positions
          changed <- true

    closure
    |> Map.map (fun (fn, _) ->
      let _, unknown = inspect fn
      let calls = analyze positions[fn.hash] fn.body
      fn, { calls with complete = calls.complete && not unknown })

  /// The requirements of `root`, walking `closure`. Because everything reachable
  /// from a member of a closure is reachable from its root, one loaded closure
  /// serves every member's analysis, and no body is analyzed again here.
  let forFunction
    // Keyed by full builtin identity (name, version): two versions of a builtin
    // can carry different effects, and collapsing them by name alone would let a
    // requirement display or upgrade comparison use the wrong effect set.
    (callEffectsFor : BuiltinMetadataFor)
    (closure : Closure)
    (root : PT.FQFnName.Package)
    : Result =
    let mutable visited = Set.empty
    let mutable requiredEffects = Set.empty
    let mutable complete = true

    let incomplete () : unit = complete <- false

    let rec visit (name : PT.FQFnName.Package) : unit =
      if not (Set.contains name visited) then
        visited <- Set.add name visited
        match Map.tryFind name closure with
        | None -> incomplete ()
        | Some(_, calls) ->
          // `calls.escapesOwnCallback` is deliberately not consulted here; see
          // the note on the field and on this module.
          if not calls.complete then incomplete ()
          for called in calls.names do
            match called with
            | PT.FQFnName.Builtin builtin ->
              // TODO consider specializing a scoped effect when the resource
              // argument is a literal at the call site (a hardcoded path or
              // URL), so approve-time review can show an exact rule instead
              // of the bare effect. See docs/permissions-todos.md.
              match callEffectsFor (builtin.name, builtin.version) with
              | Some found ->
                requiredEffects <- Set.union requiredEffects found.callEffects
              | None -> incomplete ()
            | PT.FQFnName.Package package -> visit package

    visit root

    // The approved root must account for callbacks supplied by its caller.
    let rootEscapesCallback =
      match Map.tryFind root closure with
      | Some(_, calls) -> calls.escapesOwnCallback
      | None -> false

    { requiredEffects = requiredEffects
      complete = complete && not rootEscapesCallback }
