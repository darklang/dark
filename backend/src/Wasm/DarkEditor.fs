module Wasm.DarkEditor

open System
open System.Net.Http
open System.Threading.Tasks

open Microsoft.JSInterop

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes
open EvalHelpers

let debug (arg : string) = WasmHelpers.callJSFunction "console.log" [ arg ]


/// Source of the editor
/// (types, functions, and exprs to run on start to set the initial value)
type EditorSource =
  { types : List<UserType.T>
    fns : List<UserFunction.T>
    exprs : List<Expr> }


let stdLib =
  LibExecution.StdLib.combine
    [ StdLibExecution.StdLib.contents; Wasm.StdLib.contents ]
    []
    []

/// Load the Darklang program that manages the state of and interactions with
/// the JS side of the editor.
[<JSInvokable>]
let LoadClient (sourceURL : string) : Task<string> =
  task {
    let httpClient = new HttpClient()
    let! response = httpClient.GetAsync sourceURL |> Async.AwaitTask
    let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
    let clientSource = Json.Vanilla.deserialize<EditorSource> responseBody


    let expr = exprsCollapsedIntoOne clientSource.exprs

    let! initialState =
      let state = getStateForEval stdLib clientSource.types clientSource.fns
      let inputVars = Map.empty
      LibExecution.Execution.executeExpr state inputVars expr

    Libs.Editor.editor <-
      { Types = clientSource.types
        Functions = clientSource.fns
        CurrentState = initialState }

    return LibExecution.DvalReprDeveloper.toRepr initialState
  }


[<JSInvokable>]
let HandleEvent (serializedEvent : string) : Task<string> =
  task {
    let state =
      getStateForEval stdLib Libs.Editor.editor.Types Libs.Editor.editor.Functions

    let! result =
      Ply.toTask (
        LibExecution.Interpreter.callFn
          state
          (gid ())
          (FQFnName.User { modules = []; function_ = "handleEvent"; version = 0 })
          []
          [ DString serializedEvent ]
      )

    return LibExecution.DvalReprDeveloper.toRepr result
  }
