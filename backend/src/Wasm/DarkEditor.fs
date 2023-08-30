module Wasm.DarkEditor

open System
open System.Net.Http
open System.Threading.Tasks

open Microsoft.JSInterop

open Prelude

open LibExecution.RuntimeTypes
open EvalHelpers

let debug (arg : string) = WasmHelpers.callJSFunction "console.log" [ arg ]


/// Source of the editor (types, functions)
type EditorSource =
  { types : List<UserType.T>
    fns : List<UserFunction.T>
    constants : List<UserConstant.T> }



let httpConfig : StdLibExecution.Libs.HttpClient.Configuration =
  { StdLibExecution.Libs.HttpClient.defaultConfig with
      telemetryAddException =
        (fun metadata e ->
          WasmHelpers.callJSFunction "console.warn" [ string metadata; string e ]) }


let stdLib =
  LibExecution.StdLib.combine
    [ StdLibExecution.StdLib.contents httpConfig; StdLib.contents ]
    []
    []

let ensureNonFakeDval (dv : Dval) : Result<Dval, string> =
  match dv with
  | DError(_source, rte) ->
    // CLEANUP we should stringify this better, or raise the RTE rather than hide it here
    Error $"Error calling handleEvent with provided args: {rte}"

  | DFnVal(_) -> Error $"handleError returned DFnVal"

  | result -> Ok dv


/// Load the Darklang program that manages the state of and interactions with
/// the JS side of the editor.
[<JSInvokable>]
let LoadClient (canvasName : string) : Task<string> =
  let sourceURL = $"http://{canvasName}.dlio.localhost:11003/assets/client.dark"
  let parseURL = $"http://{canvasName}.dlio.localhost:11003/get-program-json"

  task {
    let httpClient = new HttpClient()

    let! clientSource =
      task {
        // text of client.dark
        let! response = httpClient.GetAsync sourceURL
        let! responseBody = response.Content.ReadAsStringAsync()

        // parse client.dark with another endpoint,
        // which then serializes as JSON so we can deserialize below
        let! response =
          httpClient.PostAsync(parseURL, new StringContent(responseBody))
        let! responseBody = response.Content.ReadAsStringAsync()

        return Json.Vanilla.deserialize<EditorSource> responseBody
      }

    let! initialState =
      let state =
        getStateForEval
          stdLib
          clientSource.types
          clientSource.fns
          clientSource.constants
      Ply.toTask (
        LibExecution.Interpreter.callFn
          state
          Map.empty
          (gid ())
          (FnName.fqUserProgram [] "init" 0)
          []
          (NEList.singleton DUnit)
      )

    match ensureNonFakeDval initialState with
    | Ok result ->
      Libs.Editor.editor <-
        { Types = clientSource.types
          Functions = clientSource.fns
          Constants = clientSource.constants
          CurrentState = initialState }

      return LibExecution.DvalReprDeveloper.toRepr result

    | Error err ->
      WasmHelpers.callJSFunction "console.error" [ err ]
      return err
  }


[<JSInvokable>]
let HandleEvent (serializedEvent : string) : Task<string> =
  task {
    let state =
      getStateForEval
        stdLib
        Libs.Editor.editor.Types
        Libs.Editor.editor.Functions
        Libs.Editor.editor.Constants

    let! result =
      Ply.toTask (
        LibExecution.Interpreter.callFn
          state
          Map.empty
          (gid ())
          (FnName.fqUserProgram [] "handleEvent" 0)
          []
          (NEList.singleton (DString serializedEvent))
      )

    match ensureNonFakeDval result with
    | Ok result -> return LibExecution.DvalReprDeveloper.toRepr result
    | Error err ->
      WasmHelpers.callJSFunction "console.error" [ err ]
      return err
  }
