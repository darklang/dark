namespace Wasm

open System
open System.Threading.Tasks

open Microsoft.JSInterop

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution


type DarkEditor() =
  // TODO: ideally, this functionality lives in LibAnalysis,
  // but since we don't exactly know what's going to happen with
  // this project, leaving it here as an annoyance feels useful.
  static let simpleEval (expr : RT.Expr) : Task<RT.Dval> =
    task {
      let program : RT.ProgramContext =
        { canvasID = System.Guid.NewGuid()
          internalFnsAllowed = false
          userFns = Map.empty
          userTypes = Map.empty
          dbs = Map.empty
          secrets = [] }

      let (stdlibFns, stdlibTypes) =
        LibExecution.StdLib.combine
          [ StdLibExecution.StdLib.contents; LibWASM.contents ]
          []
          []

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
      // TODO: use traces in `results`
      return result
    }

  [<JSInvokable("EvalExpr")>]
  static member EvalExpr(exprJSON : string) : Task<unit> =
    task {
      let expr = Json.Vanilla.deserialize<PT.Expr> exprJSON
      let! evalResult = simpleEval (PT2RT.Expr.toRT expr)
      ()
    }

  // It's just like EvalExpr, but you don't have to explicitly call WASM.callJSFunction with the results
  // (TODO: maybe this should be deprecated)
  [<JSInvokable("EvalExprAndReturnResult")>]
  static member EvalExprAndReturnResult(exprJSON : string) : Task<unit> =
    task {
      let expr = Json.Vanilla.deserialize<PT.Expr> exprJSON
      let! evalResult = simpleEval (PT2RT.Expr.toRT expr)
      let result = LibExecution.DvalReprDeveloper.toRepr evalResult
      WasmHelpers.callJSFunction "handleDarkResult" [ result ]
      ()
    }
