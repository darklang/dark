module Wasm.DarkEditor

open System.Net.Http
open System.Threading.Tasks

open Microsoft.JSInterop

open Prelude

open LibExecution.RuntimeTypes
open EvalHelpers
module PT = LibExecution.ProgramTypes

let debug (arg : string) = WasmHelpers.callJSFunction "console.log" [ arg ]


/// Source of the editor (types, functions)
type EditorSource =
  { types : List<PackageType.PackageType>
    constants : List<PackageConstant.PackageConstant>
    fns : List<PackageFn.PackageFn> }



let httpConfig : BuiltinExecution.Libs.HttpClient.Configuration =
  { BuiltinExecution.Libs.HttpClient.defaultConfig with
      telemetryAddException =
        (fun metadata e ->
          WasmHelpers.callJSFunction "console.warn" [ string metadata; string e ]) }


let builtin =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins httpConfig PT.PackageManager.empty
      Builtin.builtins ]
    []


/// Load the Darklang program that manages the state of and interactions with
/// the JS side of the editor.
[<JSInvokable>]
let LoadClient (canvasName : string) : Task<string> =
  let sourceURL = $"http://{canvasName}.dlio.localhost:11001/assets/client.dark"
  let parseURL = $"http://{canvasName}.dlio.localhost:11001/get-program-json"

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
          builtin
          clientSource.types
          clientSource.constants
          clientSource.fns
      LibExecution.Execution.executeFunction
        state
        (FQFnName.fqPackage (System.Guid.NewGuid())) // "TODO" [] "init")
        []
        (NEList.singleton DUnit)


    match initialState with
    | Ok result ->
      Libs.Editor.editor <-
        { types = clientSource.types
          constants = clientSource.constants
          functions = clientSource.fns
          currentState = result }

      return LibExecution.DvalReprDeveloper.toRepr result

    | Error err ->
      // TODO convert to a string
      // WasmHelpers.callJSFunction "console.error" [ err ]
      // return err
      WasmHelpers.callJSFunction "console.error" [ "error in LoadClient" ]
      return Exception.raiseInternal "TODO" [ "rte", err ]
  }


[<JSInvokable>]
let HandleEvent (serializedEvent : string) : Task<string> =
  task {
    let state =
      getStateForEval
        builtin
        Libs.Editor.editor.types
        Libs.Editor.editor.constants
        Libs.Editor.editor.functions

    let! result =
      LibExecution.Execution.executeFunction
        state
        (FQFnName.fqPackage (System.Guid.NewGuid())) //"TODO" [] "handleEvent")
        []
        (NEList.singleton (DString serializedEvent))


    match result with
    | Ok result -> return LibExecution.DvalReprDeveloper.toRepr result
    | Error err ->
      // TODO convert to a string
      // WasmHelpers.callJSFunction "console.error" [ err ]
      // return err
      WasmHelpers.callJSFunction "console.error" [ "error in HandleEvent" ]
      return Exception.raiseInternal "TODO" [ "rte", err ]
  }
