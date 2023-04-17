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

type EvalWorker =

  [<JSInvokable>]
  static member ParseAndEvalExpr(code : string) : Task<unit> =
    task {
      let expr =
        // The parser isn't working in WASM.
        // So we'll just hardcode a simple expression for now.
        // TODO: fix parser or bite the bullet and use TreeSitter for parsing
        PT.EInfix(
          gid(),
          (PT.InfixFnCall(PT.ArithmeticPlus)),
          PT.EInt(gid(), 5),
          PT.EInt(gid(), 3)
        ) |> PT2RT.Expr.toRT

      let! evalResult = LibAnalysis.simpleEval expr
      let result = LibExecution.DvalReprDeveloper.toRepr evalResult
      WasmHelpers.postMessage "handleEvalResult" result
    }
