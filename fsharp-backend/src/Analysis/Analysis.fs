// Handles requests for evaluating expressions
namespace Analysis

open System.Threading.Tasks

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes
module Exe = LibExecution.Execution
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module AT = LibExecution.AnalysisTypes
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated

module CTA = ClientTypes.Analysis

open System
open System.Reflection

#nowarn "988"

type GetGlobalObjectDelegate = delegate of string -> obj

type InvokeDelegate = delegate of m : string * [<ParamArray>] ps : obj [] -> obj

/// Responsible for interacting with the JS world
///
/// Exposes fns to be called to evaluate expressions,
/// and results back to JS
type EvalWorker =
  static member GetGlobalObject(_globalObjectName : string) : unit = ()

  static member InitializeDarkRuntime() : unit =
    Environment.SetEnvironmentVariable("TZ", "UTC")
    LibAnalysis.initSerializers ()

  static member selfDelegate =
    let typ =
      let sourceAssembly : Assembly =
        Assembly.Load "System.Private.Runtime.InteropServices.JavaScript"
      sourceAssembly.GetType "System.Runtime.InteropServices.JavaScript.Runtime"

    // I do not have any clue what this does
    let method = typ.GetMethod(nameof (EvalWorker.GetGlobalObject))
    let delegate_ = method.CreateDelegate<GetGlobalObjectDelegate>()
    let target = delegate_.Invoke("self")

    let typ = target.GetType()
    let invokeMethod = typ.GetMethod("Invoke")

    System.Delegate.CreateDelegate(typeof<InvokeDelegate>, target, invokeMethod)
    :?> InvokeDelegate


  /// Call `self.postMessage` in JS land
  ///
  /// Used in order to send the results of an expression back to the JS host
  static member postMessage(message : string) : unit =
    let (_ : obj) = EvalWorker.selfDelegate.Invoke("postMessage", message)
    ()

  /// Receive request from JS host to be evaluated
  ///
  /// Once evaluated, an async call to `self.postMessage` will be made
  static member OnMessage(input : string) : Task<unit> =
    // Just here to ensure type-safety (serializers require known/allowed types)
    let postResponse (response : LibAnalysis.AnalysisResult) : unit =
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
      let args = Json.Vanilla.deserialize<CTA.PerformAnalysisParams> input
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
