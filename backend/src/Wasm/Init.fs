namespace Wasm

open System
open Microsoft.JSInterop

open Prelude

type Init =
  [<JSInvokable>]
  static member InitializeDarkRuntime() : unit =
    Environment.SetEnvironmentVariable("TZ", "UTC")
    LibAnalysis.Analysis.initSerializers ()
    System.Console.WriteLine("Dark runtime initialized")
