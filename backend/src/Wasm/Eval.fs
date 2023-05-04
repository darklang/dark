module Wasm.Eval

open System
open System.Threading.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution

// TODO: ideally, this functionality lives in LibAnalysis,
// but since we don't exactly know what's going to happen with
// this project, leaving it here as an annoyance feels useful.
let simpleEval
  (stdlib : LibExecution.StdLib.Contents)
  (userTypes : List<RT.UserType.T>)
  (userFns : List<RT.UserFunction.T>)
  (expr : RT.Expr)
  : Task<RT.Dval> =
  task {
    let program : RT.ProgramContext =
      { canvasID = System.Guid.NewGuid()
        internalFnsAllowed = false
        userFns = userFns |> Map.fromListBy (fun fn -> fn.name)
        userTypes = userTypes |> Map.fromListBy (fun typ -> typ.name)
        dbs = Map.empty
        secrets = [] }

    let (stdlibFns, stdlibTypes) = stdlib

    let libraries : RT.Libraries =
      { stdlibTypes =
          stdlibTypes |> Map.fromListBy (fun typ -> RT.FQTypeName.Stdlib typ.name)
        stdlibFns =
          stdlibFns |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)
        packageFns = Map.empty }
    let results, traceDvalFn = Exe.traceDvals ()
    let functionResults = []

    let tracing =
      { LibExecution.Execution.noTracing RT.Real with
          traceDval = traceDvalFn
          loadFnResult = LibAnalysis.Analysis.Eval.loadFromTrace functionResults }

    let state =
      Exe.createState
        libraries
        tracing
        RT.consoleReporter
        RT.consoleNotifier
        (gid ())
        program

    let inputVars = Map.empty

    let! (result : RT.Dval) = Exe.executeExpr state inputVars expr

    // TODO: use traces in `results` for something?

    return result
  }
