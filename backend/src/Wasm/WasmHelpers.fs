/// Supports WASM <--> JS interop
module Wasm.WasmHelpers

open System
open System.Threading.Tasks
open System.Reflection

open Microsoft.JSInterop

open Prelude

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

/// Call a function exposed in JS host
///
/// TODO: consider requiring the input of this to be SimpleJSON or something,
/// where `type SimpleJSON = JNull | JBool of bool | ...`
/// and then serialize within this bit here.
/// (we could just avail an _additional_ fn where we avail this option)
let callJSFunction (functionToCall : string) (args : List<string>) : unit =
  let jsRuntimeThis = getJsRuntimeThis ()
  let args = args |> List.toArray |> Array.map box
  let _response : obj = jsRuntimeThis.Invoke(functionToCall, args)
  ()
