// Handles requests for evaluating expressions
namespace Wasm.Analysis

open System
open System.Threading.Tasks
open System.Reflection

open Prelude
open Tablecloth

open Microsoft.JSInterop

#nowarn "988" // "main module of Program is empty"

module WasmHelpers =
  // this gets us ahold of `this`/`self`
  let getJsRuntimeThis () : IJSInProcessRuntime =
    let assemblyName = "Microsoft.AspNetCore.Components.WebAssembly"
    let typeName =
      "Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime"

    let assembly = Assembly.Load assemblyName
    let jsRuntimeType = assembly.GetType typeName

    let jsRuntimeTypeInstance =
      let flags = BindingFlags.NonPublic ||| BindingFlags.Static
      jsRuntimeType.GetField("Instance", flags)

    jsRuntimeTypeInstance.GetValue(null) :?> IJSInProcessRuntime


/// Responsible for interacting with the JS world
///
/// Exposes fns to be called to evaluate expressions,
/// and results back to JS
type EvalWorker =
  [<JSInvokable>]
  static member InitializeDarkRuntime() : unit =
    Environment.SetEnvironmentVariable("TZ", "UTC")
    LibAnalysis.initSerializers ()

  /// Call `self.postMessage` in JS land
  ///
  /// Used in order to send the results of an expression back to the JS host
  static member postMessage(message : string) : unit =
    let jsRuntimeThis = WasmHelpers.getJsRuntimeThis ()
    let response = jsRuntimeThis.Invoke("Dark.analysis.callback", message)
    ()

  /// Receive request from JS host to be evaluated
  ///
  /// Once evaluated, an async call to `self.postMessage` will be made
  [<JSInvokable>]
  static member OnMessage(input : string) : Task<unit> =
    // Just here to ensure type-safety (serializers require known/allowed types)
    let postResponse (response : ClientTypes.Analysis.AnalysisResult) : unit =
      let serialized = Json.Vanilla.serialize response
      EvalWorker.postMessage (Json.Vanilla.serialize (response))

    let reportException (preamble : string) (e : exn) : unit =
      let metadata = Exception.nestedMetadata e
      let errorMessage = Exception.getMessages e |> String.concat " "

      System.Console.WriteLine($"Blazor failure: {preamble}")
      System.Console.WriteLine($"called with message: {input}")
      System.Console.WriteLine(
        $"caught exception: \"{errorMessage}\" \"{metadata}\""
      )
      let message = ($"exception: {errorMessage}, metadata: {metadata}")
      postResponse (Error(message))

    try
      // parse an analysis request, in JSON, from the JS world (BlazorWorker)
      let args =
        Json.Vanilla.deserialize<ClientTypes.Analysis.PerformAnalysisParams> input
      task {
        try
          let! result = LibAnalysis.performAnalysis args
          try
            // post the result back to the JS world
            return postResponse (Ok result)
          with
          | e -> return reportException "Error returning results" e
        with
        | e -> return reportException "Error running analysis" e
      }
    with
    | e -> Task.FromResult(reportException "Error parsing analysis request" e)
