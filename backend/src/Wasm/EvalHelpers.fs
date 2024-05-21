module Wasm.EvalHelpers

open Prelude

open LibExecution.RuntimeTypes

let getStateForEval
  (builtins : Builtins)
  (_types : List<PackageType.T>)
  (_constants : List<PackageConstant.T>)
  (_fns : List<PackageFn.T>)
  : ExecutionState =

  // TODO
  let packageManager : PackageManager = PackageManager.empty

  let program : Program =
    { canvasID = CanvasID.Empty
      internalFnsAllowed = true
      // types = Map.fromListBy (_.name) types
      // constants = Map.fromListBy (_.name) constants
      dbs = Map.empty
      secrets = List.empty
    //fns = Map.fromListBy (_.name) fns
    }

  { builtins = builtins
    tracing = LibExecution.Execution.noTracing
    test = LibExecution.Execution.noTestContext
    reportException = consoleReporter
    notify = consoleNotifier

    program = program

    packageManager = packageManager
    typeSymbolTable = Map.empty }

/// Any 'loose' exprs in the source are mapped without context of previous/later exprs
/// so, a binding set in one 'let' will be unavailable in the next expr if evaluated one by one.
///
/// e.g. given these separated exprs:
///   - `let x = 1`
///   - `1+1`
///   - `x+2`
/// when evaluating `x+2`, `x` wouldn't normally exist in the symtable.
///
/// So, this fn reduces the exprs to one expr ensuring bindings are available appropriately
///   `let x = 1 in (let _ = 1+1 in x+2)`
///
/// `1+1` here may just as well be something like
///   `WASM.callJSFunction "console.log" ["X is " ++ (Int.toString x)])`
///
/// (There's no way to read the symtable from `state`, otherwise I'd eval expr by expr, and
/// update the symtable as I go.)
///
/// TODO: this would belong better somewhere else, but I don't know where.
let exprsCollapsedIntoOne (exprs : List<Expr>) : Expr =
  exprs
  |> List.rev
  |> List.fold
    (fun agg expr ->
      match agg with
      | EUnit(_) -> expr
      | _ ->
        match expr with
        | ELet(id, lp, expr, _) -> ELet(id, lp, expr, agg)
        | other -> ELet(gid (), LPVariable(gid (), "_"), other, agg))
    (EUnit(gid ()))
