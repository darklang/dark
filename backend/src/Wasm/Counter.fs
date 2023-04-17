namespace Wasm

open System
open System.Threading.Tasks
open System.Reflection

open Microsoft.JSInterop

/// Demonstrates a simple counter, where JS handles rendering the counter and
/// handling user input, but the counter state and logic is handled in WASM.
type CounterWorker() =
  static let mutable counter = 0

  [<JSInvokable>]
  static member IncrementCounter() : Task<unit> =
    task {
      counter <- counter + 1
      WasmHelpers.postMessage "handleUpdatedCounter" (counter.ToString())
    }
