namespace Wasm

open System
open Microsoft.JSInterop

open Prelude

type Init =
  [<JSInvokable>]
  static member InitializeDarkRuntime() : unit =
    Environment.SetEnvironmentVariable("TZ", "UTC")
    LibAnalysis.Analysis.initSerializers ()
    Json.Vanilla.allow<Wasm.Libs.Editor.Editor>
      "to 'Export' the editor - just for debugging"
    System.Console.WriteLine("Dark runtime initialized")
