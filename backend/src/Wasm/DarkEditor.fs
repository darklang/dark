namespace Wasm

open System
open System.Threading.Tasks
open System.Reflection

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

      let stdlibTypes : Map<RT.FQTypeName.T, RT.BuiltInType> =
        LibExecutionStdLib.StdLib.types
        |> List.map (fun typ -> PT2RT.BuiltInType.toRT typ)
        |> Map.fromListBy (fun typ -> RT.FQTypeName.Stdlib typ.name)

      let stdlibFns =
        LibExecutionStdLib.StdLib.fns
        |> Map.fromListBy (fun fn -> RT.FQFnName.Stdlib fn.name)

      let libraries : RT.Libraries =
        { stdlibTypes = stdlibTypes; stdlibFns = stdlibFns; packageFns = Map.empty }
      let results, traceDvalFn = Exe.traceDvals ()
      let functionResults = []

      let tracing =
        { LibExecution.Execution.noTracing RT.Preview with
            traceDval = traceDvalFn
            loadFnResult = LibAnalysis.Eval.loadFromTrace functionResults }

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


  static let callbackFnName = "handleDarkResult"

  // Here's one idea for how we could manage the state of the editor
  //static let mutable state: RT.Dval = RT.DUnit

  static let mutable expr : PT.Expr =
    PT.EString(gid (), [ PT.StringText "expr not loaded yet" ])

  [<JSInvokable>]
  static member LoadExpr(exprJSON : string) : Task<unit> =
    task {
      let parsed = Json.Vanilla.deserialize<PT.Expr> exprJSON
      expr <- parsed
      WasmHelpers.postMessage callbackFnName "loaded expr"
    }

  [<JSInvokable("EvalExpr")>]
  // TODO: add ability to pass in user input
  static member EvalExpr() : Task<unit> =
    task {
      let! evalResult = simpleEval (PT2RT.Expr.toRT expr)
      let result = LibExecution.DvalReprDeveloper.toRepr evalResult
      WasmHelpers.postMessage callbackFnName result
    }
