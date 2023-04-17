namespace Wasm

open System
open System.Threading.Tasks
open System.Reflection

open Microsoft.JSInterop

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PR2RT = LibExecution.ProgramTypesToRuntimeTypes

type DarkEditor() =
  static let callbackFnName = "handleDarkResult"

  static let mutable expr: PT.Expr =
    PT.EString(gid (), [PT.StringText "expr not loaded yet"])

  [<JSInvokable>]
  static member LoadExpr(exprJSON : string): Task<unit> =
    task {
      let parsed = Json.Vanilla.deserialize<PT.Expr> exprJSON
      expr <- parsed
      WasmHelpers.postMessage callbackFnName "loaded expr"
    }

  [<JSInvokable>]
  static member EvalExpr() : Task<unit> =
    task {
      let! evalResult = LibAnalysis.simpleEval (PR2RT.Expr.toRT expr)
      let result = LibExecution.DvalReprDeveloper.toRepr evalResult
      WasmHelpers.postMessage callbackFnName result
    }
