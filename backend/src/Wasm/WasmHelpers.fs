/// Supports WASM <--> JS interop
module Wasm.WasmHelpers

open System
open System.Threading.Tasks
open System.Reflection

open Microsoft.JSInterop

open Prelude
open Tablecloth

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

/// Call a function exposed in JS land
let postMessage(functionToCall: string) (message : string) : unit =
  let jsRuntimeThis = getJsRuntimeThis ()
  let response = jsRuntimeThis.Invoke(functionToCall, message)
  ()
