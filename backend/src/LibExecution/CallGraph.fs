/// Static function references in ProgramTypes source expressions.
module LibExecution.CallGraph

open Prelude

module PT = LibExecution.ProgramTypes
module Ast = LibExecution.ProgramTypesAst

type Analysis = { names : List<PT.FQFnName.FQFnName>; complete : bool }

/// Bump whenever completeness or reachability semantics change. Approval
/// fingerprints include this so an analyzer fix cannot silently bless an old,
/// narrower review.
let analysisVersion = 2

module private Analysis =
  let empty : Analysis = { names = []; complete = true }
  let unresolved : Analysis = { names = []; complete = false }

  let combine (left : Analysis) (right : Analysis) : Analysis =
    { names = List.append left.names right.names
      complete = left.complete && right.complete }

  let collect (f : 'a -> Analysis) (items : List<'a>) : Analysis =
    let names = ResizeArray<PT.FQFnName.FQFnName>()
    let mutable complete = true
    for item in items do
      let analysis = f item
      names.AddRange analysis.names
      complete <- complete && analysis.complete
    { names = List.ofSeq names; complete = complete }

/// A resolved fn name, or an explicit incomplete marker.
let private nameRef (nr : PT.NameResolution<PT.FQFnName.FQFnName>) : Analysis =
  match nr.resolved with
  | Ok resolved -> { names = [ resolved.name ]; complete = true }
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

/// Every fn-name an expression references — `EFnName` and the piped
/// `EPipeFnCall` — plus whether any call target is unknowable statically.
/// Only the call-shaped nodes are handled here; everything else folds its
/// sub-expressions via `ProgramTypesAst.subExprs`.
let rec analyze (expr : PT.Expr) : Analysis =
  let own =
    match expr with
    | PT.EFnName(_, nr) -> nameRef nr
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
      // A function held in a variable, argument, record, or value can be an
      // effectful callback. Its target is not statically known here.
      | _ -> Analysis.unresolved
    | _ -> Analysis.empty
  Analysis.combine own (Analysis.collect analyze (Ast.subExprs expr))


/// Conservative permission requirements of a package function: the union of
/// the call effects of every builtin statically reachable from it, lambda
/// bodies included, since returned code may run later. Missing package
/// functions, dynamic calls, and unclassified builtins make the result
/// incomplete; an incomplete result must never be approved or treated as
/// effect-free.
module Requirements =
  module E = LibExecution.Effects

  type Result = { requiredEffects : Set<E.Effect>; complete : bool }

  /// The loaded, immutable dependency closure of one root: every package fn
  /// statically reachable from it (the root included) that could be loaded,
  /// with its body's call analysis computed once at load. A member missing
  /// from it is one that could not be loaded.
  type Closure = Map<PT.FQFnName.Package, PT.PackageFn.PackageFn * Analysis>

  /// The requirements of `root`, walking `closure`. Because everything reachable
  /// from a member of a closure is reachable from its root, one loaded closure
  /// serves every member's analysis, and no body is analyzed again here.
  let forFunction
    // Keyed by full builtin identity (name, version): two versions of a builtin
    // can carry different effects, and collapsing them by name alone would let a
    // requirement display or upgrade comparison use the wrong effect set.
    (callEffectsFor : string * int -> Option<Set<E.Effect>>)
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
          if not calls.complete then incomplete ()
          for called in calls.names do
            match called with
            | PT.FQFnName.Builtin builtin ->
              match callEffectsFor (builtin.name, builtin.version) with
              | Some found -> requiredEffects <- Set.union requiredEffects found
              | None -> incomplete ()
            | PT.FQFnName.Package package -> visit package

    visit root
    { requiredEffects = requiredEffects; complete = complete }
