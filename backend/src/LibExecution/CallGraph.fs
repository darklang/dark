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
    /// True when a trait call or operator here runs whatever implementation this
    /// function's caller recorded for one of its type params (`FromTypeParam`).
    /// Like a callback, that is the caller's to supply.
    defersToTypeParam : bool
    /// Package fns referenced with no bound implementations recorded. Entering a
    /// fn that `defersToTypeParam` this way leaves the deferred call to be
    /// resolved from the store at run time.
    calledWithoutBounds : Set<PT.FQFnName.Package>
  }

/// Bump whenever completeness or reachability semantics change. Approval
/// fingerprints include this so an analyzer fix cannot silently bless an old,
/// narrower review.
/// Version 3 traverses dictionary keys; skipping them hid calls from analysis.
/// Version 4 follows the implementation an operator or trait call resolved to;
/// operators were analyzed as their operands alone.
/// Version 5 follows a reference to a package value whose stored value holds
/// only data; every value reference was incomplete before.
let analysisVersion = 5

module private Analysis =
  let empty : Analysis =
    { names = []
      complete = true
      escapesOwnCallback = false
      defersToTypeParam = false
      calledWithoutBounds = Set.empty }

  let unresolved : Analysis = { empty with complete = false }

  /// Knowable from the call site, but not from here.
  let callbackEscape : Analysis = { empty with escapesOwnCallback = true }

  /// Decided by the caller's type argument, as a callback is by its argument.
  let typeParamDeferral : Analysis = { empty with defersToTypeParam = true }

  let combine (left : Analysis) (right : Analysis) : Analysis =
    { names = List.append left.names right.names
      complete = left.complete && right.complete
      escapesOwnCallback = left.escapesOwnCallback || right.escapesOwnCallback
      defersToTypeParam = left.defersToTypeParam || right.defersToTypeParam
      calledWithoutBounds =
        Set.union left.calledWithoutBounds right.calledWithoutBounds }

  let collect (f : 'a -> Analysis) (items : List<'a>) : Analysis =
    let names = ResizeArray<PT.FQFnName.FQFnName>()
    let mutable complete = true
    let mutable escapes = false
    let mutable defers = false
    let mutable withoutBounds = Set.empty
    for item in items do
      let analysis = f item
      names.AddRange analysis.names
      complete <- complete && analysis.complete
      escapes <- escapes || analysis.escapesOwnCallback
      defers <- defers || analysis.defersToTypeParam
      withoutBounds <- Set.union withoutBounds analysis.calledWithoutBounds
    { names = List.ofSeq names
      complete = complete
      escapesOwnCallback = escapes
      defersToTypeParam = defers
      calledWithoutBounds = withoutBounds }

/// A package fn entered with `bounds` recorded for its type params.
let private packageRef
  (fn : PT.FQFnName.Package)
  (bounds : List<PT.FQFnName.BoundImpl>)
  : Analysis =
  { Analysis.empty with
      names = [ PT.FQFnName.Package fn ]
      calledWithoutBounds =
        if List.isEmpty bounds then Set.singleton fn else Set.empty }

/// The implementation a trait call or operator runs, in the three states the
/// save can record. The runtime treats them the same way (`recordedImpl`).
let private implChoice (choice : PT.FQFnName.ImplChoice) : Analysis =
  match choice with
  // Runs directly, so it is an ordinary call. Nothing records bounds for it.
  | PT.FQFnName.Chosen r -> packageRef r.name []
  // The caller decides, as it does for a callback parameter.
  | PT.FQFnName.FromTypeParam _ -> Analysis.typeParamDeferral
  // Resolved from the store at run time, by the operand's type.
  | PT.FQFnName.Unknown -> Analysis.unresolved

/// A resolved fn name with the bound implementations its call site recorded,
/// or an explicit incomplete marker.
let private nameRef
  (nr : PT.NameResolution<PT.FQFnName.FQFnName>)
  (bounds : List<PT.FQFnName.BoundImpl>)
  : Analysis =
  let own =
    match nr.resolved with
    | Ok { name = PT.FQFnName.Package fn } -> packageRef fn bounds
    | Ok { name = PT.FQFnName.TraitMethod tm } -> implChoice tm.implFn
    | Ok { name = PT.FQFnName.Builtin _ as builtin } ->
      { Analysis.empty with names = [ builtin ] }
    | Error _ -> Analysis.unresolved
  Analysis.combine
    own
    (Analysis.collect (fun (b : PT.FQFnName.BoundImpl) -> implChoice b.choice) bounds)

/// What an operator calls, mirroring `ProgramTypesToRuntimeTypes.InfixFnName.toRT`:
/// the trait method's recorded implementation for a trait operator, the builtin
/// for the rest. `&&` and `||` are instructions, not calls.
let private infixRef
  (infix : PT.Infix)
  (implFn : PT.FQFnName.ImplChoice)
  : Analysis =
  match infix with
  | PT.BinOp _ -> Analysis.empty
  | PT.InfixFnCall name ->
    match NumericTraits.ofInfix name with
    | Some _ -> implChoice implFn
    | None ->
      { Analysis.empty with
          names =
            [ PT.FQFnName.Builtin
                { name = PT.InfixFnName.toBuiltinName name; version = 0 } ] }

/// What a pipe part itself references, beyond its nested expressions.
let private pipeOwnRefs (pe : PT.PipeExpr) : Analysis =
  match pe with
  | PT.EPipeFnCall(_, nr, _, _, bounds) -> nameRef nr bounds
  | PT.EPipeInfix(_, infix, _, implFn) -> infixRef infix implFn
  // A function held in a variable can be an effectful callback whose target
  // is not statically known here.
  | PT.EPipeVariable _ -> Analysis.unresolved
  | PT.EPipeLambda _
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

/// Every fn-name an expression references (`EFnName`, the piped `EPipeFnCall`,
/// and the implementation an operator resolved to), plus whether any call
/// target is unknowable statically.
/// Only the call-shaped nodes are handled here; everything else folds its
/// sub-expressions via `ProgramTypesAst.subExprs`.
///
/// `callbacks` is `callbackParams` of the enclosing package fn; a reference to
/// one of those positions is unknowable wherever it appears.
///
/// `inertValues` are the package values whose stored value is known to hold
/// only data (`Dval.isInertData`). The caller decides that from the store; an
/// empty set is always safe.
let rec analyze
  (callbacks : Set<int>)
  (inertValues : Set<PT.FQValueName.Package>)
  (expr : PT.Expr)
  : Analysis =
  let own =
    match expr with
    | PT.EFnName(_, nr, bounds) -> nameRef nr bounds
    | PT.EInfix(_, infix, _, _, implFn) -> infixRef infix implFn
    // A passed callback may be called by the receiving function.
    | PT.EArg(_, index) when Set.contains index callbacks -> Analysis.callbackEscape
    // A value is evaluated once and stored, so reading it runs nothing. What
    // it can do is hand back code: a named fn or lambda nested anywhere in it,
    // which the reader may then call without this analysis seeing the call.
    // So a reference is followed only when the store has shown the value is
    // data all the way down. Anything else (a value holding code, one not yet
    // evaluated, one that could not be read, a builtin value) stays
    // incomplete, since treating it as complete would let returned executable
    // code be approved as effect-free.
    | PT.EValue(_, { resolved = Ok { name = PT.FQValueName.Package value } }) when
      Set.contains value inertValues
      ->
      Analysis.empty
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
  Analysis.combine
    own
    (Analysis.collect (analyze callbacks inertValues) (Ast.subExprs expr))


/// Every package value an expression references, so the caller can ask the
/// store which of them hold only data before analyzing.
let rec valueRefs (expr : PT.Expr) : Set<PT.FQValueName.Package> =
  let own =
    match expr with
    | PT.EValue(_, { resolved = Ok { name = PT.FQValueName.Package value } }) ->
      Set.singleton value
    | _ -> Set.empty
  Ast.subExprs expr |> List.map valueRefs |> Set.unionMany |> Set.union own


/// Analyze a package function with its function-typed parameters marked as
/// caller-supplied callbacks, following references to `inertValues`.
let analyzeFn
  (inertValues : Set<PT.FQValueName.Package>)
  (fn : PT.PackageFn.PackageFn)
  : Analysis =
  analyze (callbackParams fn) inertValues fn.body


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
///
/// A trait call deferred to a type param (`FromTypeParam`) is the same, with
/// one difference: a callback is always an argument, so the caller always
/// supplies one, but a bound implementation is only supplied when the call site
/// recorded it. A dependency that defers, entered from a call site that
/// recorded nothing, is resolved from the store at run time: incomplete.
module Requirements =
  module E = LibExecution.Effects

  type Result = { requiredEffects : Set<E.Effect>; complete : bool }

  /// The loaded, immutable dependency closure of one root: every package fn
  /// statically reachable from it (the root included) that could be loaded,
  /// with its body's call analysis computed once at load. A member missing
  /// from it is one that could not be loaded.
  type Closure = Map<PT.FQFnName.Package, PT.PackageFn.PackageFn * Analysis>

  /// The requirements of a body, walking the package functions it references.
  /// An incomplete walk must never be treated as effect-free.
  ///
  /// With `callbacksSupplied`, the body passing one of its own fn parameters on does not make it
  /// incomplete: the caller has already accounted for the callables it hands it. A list op
  /// deciding whether to spread does this, since it checks the arguments it applies the root to.
  /// A bound implementation the caller owes is still incomplete.
  let forExpressionWith
    (callbacksSupplied : bool)
    // Keyed by full builtin identity (name, version): two versions of a builtin
    // can carry different effects, and collapsing them by name alone would let a
    // requirement display or upgrade comparison use the wrong effect set.
    (callEffectsFor : string * int -> Option<Set<E.Effect>>)
    (closure : Closure)
    (body : Analysis)
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
        | Some(_, calls) -> visitCalls calls

    and visitCalls (calls : Analysis) : unit =
      // `calls.escapesOwnCallback` is deliberately not consulted here; see the
      // note on the field and on this module. Only the body's own is checked.
      if not calls.complete then incomplete ()
      for called in calls.names do
        match called with
        | PT.FQFnName.Package package when
          Set.contains package calls.calledWithoutBounds
          && (match Map.tryFind package closure with
              | Some(_, callee) -> callee.defersToTypeParam
              | None -> false)
          ->
          incomplete ()
          visit package
        | PT.FQFnName.Builtin builtin ->
          // TODO consider specializing a scoped effect when the resource
          // argument is a literal at the call site (a hardcoded path or
          // URL), so approve-time review can show an exact rule instead
          // of the bare effect. See docs/permissions-todos.md.
          match callEffectsFor (builtin.name, builtin.version) with
          | Some found -> requiredEffects <- Set.union requiredEffects found
          | None -> incomplete ()
        | PT.FQFnName.Package package -> visit package
        // `analyze` replaces a trait call with the implementation it
        // resolved to, so none reach here. Conservative if one does.
        | PT.FQFnName.TraitMethod _ -> incomplete ()

    visitCalls body

    // The body must account for callbacks and bound implementations supplied
    // by its caller.
    let owesCaller =
      (body.escapesOwnCallback && not callbacksSupplied) || body.defersToTypeParam

    { requiredEffects = requiredEffects; complete = complete && not owesCaller }

  /// `forExpressionWith`, for a body whose callbacks nobody has accounted for.
  let forExpression
    (callEffectsFor : string * int -> Option<Set<E.Effect>>)
    (closure : Closure)
    (body : Analysis)
    : Result =
    forExpressionWith false callEffectsFor closure body

  /// `forFunction`, for a caller that has already accounted for the callables it hands the root;
  /// see `forExpressionWith`.
  let forFunctionWith
    (callbacksSupplied : bool)
    (callEffectsFor : string * int -> Option<Set<E.Effect>>)
    (closure : Closure)
    (root : PT.FQFnName.Package)
    : Result =
    let body =
      match Map.tryFind root closure with
      | Some(_, calls) -> calls
      | None -> Analysis.unresolved
    forExpressionWith callbacksSupplied callEffectsFor closure body

  /// The requirements of `root`, walking `closure`. Because everything reachable
  /// from a member of a closure is reachable from its root, one loaded closure
  /// serves every member's analysis, and no body is analyzed again here.
  let forFunction
    (callEffectsFor : string * int -> Option<Set<E.Effect>>)
    (closure : Closure)
    (root : PT.FQFnName.Package)
    : Result =
    forFunctionWith false callEffectsFor closure root
