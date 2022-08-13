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
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
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
  static member OnMessage(input : string) =

    let reportAndRollUpExceptionIntoError (preamble : string) (e) =
      let metadata = Exception.nestedMetadata e
      let errorMessage = Exception.getMessages e |> String.concat " "

      System.Console.WriteLine($"Blazor failure: {preamble}")
      System.Console.WriteLine($"called with message: {input}")
      System.Console.WriteLine(
        $"caught exception: \"{errorMessage}\" \"{metadata}\""
      )

      Error($"exception: {errorMessage}, metadata: {metadata}")

    // parse an analysis request, in JSON, from the JS world (BlazorWorker)
    let args : Result<CTA.PerformAnalysisParams, string> =
      try
        Ok(Json.Vanilla.deserialize<CTA.PerformAnalysisParams> input)
      with
      | e -> reportAndRollUpExceptionIntoError "Error parsing analysis request" e

    // run the actual analysis (eval. the fn/handler)
    let result =
      match args with
      | Error e -> Error e
      | Ok args ->
        try
          let result = (LibAnalysis.performAnalysis args).Result
          Ok result
        with
        | e -> reportAndRollUpExceptionIntoError "Error running analysis" e

    // Serialize the result
    let serialized =
      try
        Json.Vanilla.serialize result
      with
      | e ->
        reportAndRollUpExceptionIntoError "Error serializing results" e
        |> Json.Vanilla.serialize

    // post the result back to the JS world
    EvalWorker.postMessage serialized
