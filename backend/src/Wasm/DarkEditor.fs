//[<assembly:SupportedOSPlatform("browser")>]
namespace Wasm

open System
open System.Threading.Tasks

open Microsoft.JSInterop

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


open System
open System.Threading
open System.Runtime.Versioning
open System.Runtime.InteropServices.JavaScript



type DarkEditor() =

  static let debug args = WasmHelpers.callJSFunction "console.log" args

  static let stdlib =
    LibExecution.StdLib.combine
      [ StdLibExecution.StdLib.contents; LibWASM.contents ]
      []
      []


  [<JSInvokable>]
  static member LoadProgram
    (
      serializedTypes : string,
      serializedFns : string,
      stateType : string,
      serializedInitialState : string
    ) : Task<unit> =
    task {
      let types = Json.Vanilla.deserialize<List<PT.UserType.T>> serializedTypes

      let fns = Json.Vanilla.deserialize<List<PT.UserFunction.T>> serializedFns

      let stateType = Json.Vanilla.deserialize<RT.TypeReference> stateType

      let initialState =
        let availableTypes =
          types
          |> List.map PT2RT.UserType.toRT
          |> List.map (fun typ -> (RT.FQTypeName.User typ.name, typ.definition))
          |> Map

        StdLibExecution.Libs.Json.parse
          availableTypes
          stateType
          serializedInitialState
        |> Result.unwrap_unsafe

      // TODO: ensure initialState matches the type provided in typeParams

      LibWASM.program <-
        { UserTypes = types
          UserFunctions = fns
          StateType = stateType
          CurrentState = initialState }

      return ()
    }


  [<JSInvokable>]
  static member EvalExpr(exprJSON : string) : Task<string> =
    task {
      let expr = Json.Vanilla.deserialize<PT.Expr> exprJSON
      let! evalResult = Eval.simpleEval stdlib [] [] (PT2RT.Expr.toRT expr)
      let result = LibExecution.DvalReprDeveloper.toRepr evalResult
      return result
    }


  // It's just like EvalExpr, but you don't have to explicitly call WASM.callJSFunction with the results
  // (TODO: maybe this should be deprecated)
  [<JSInvokable>]
  static member EvalExprAndReturnResult(exprJSON : string) : Task<string> =
    task {
      let expr = Json.Vanilla.deserialize<PT.Expr> exprJSON
      let! evalResult = Eval.simpleEval stdlib [] [] (PT2RT.Expr.toRT expr)
      let result = LibExecution.DvalReprDeveloper.toRepr evalResult
      WasmHelpers.callJSFunction "handleDarkResult" [ result ]
      return result
    }



  static member SecondThread(): unit =
    Console.WriteLine ($"Hello from Thread {Thread.CurrentThread.ManagedThreadId}")


  [<JSInvokable>]
  static member MultiThreadingTest() : Task<unit> =
    task {
      Console.WriteLine ($"Hello from main thread: {Thread.CurrentThread.ManagedThreadId}")

      let t = new Thread(fun () ->
        Console.WriteLine ($"Hello from other thread {Thread.CurrentThread.ManagedThreadId}")
      )
      t.Start()

      Console.WriteLine "Main thread completed"

      return ()
    }
