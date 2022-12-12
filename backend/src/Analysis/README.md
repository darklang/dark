# Analysis

"Analysis" is the evaluation of Dark code from within the Editor itself, rather
than evaluating code against a backend server. This provides more immediate
feedback to Dark's users, while also reducing some demand from Dark's backend
servers.

There are two components to this project:
- an exposed `EvalWorker.OnMessage` function callable from JS
- the `BlazorWorker.js` script that loads and calls `EvalWorker.OnMessage`.

## EvalWorker

`Analysis.fsproj` is a `Blazor.WebAssembly` project that is compiled to be run
in WebAssembly. It's not compiled to webassembly directly, but rather loaded
_by_ WebAssembly, by way of BlazorWorker.js and `dotnet.wasm`.

`LibExecution` is referenced/used by this project to support basic types and
evaluation of Dark code. `LibExecutionStdLib` is referenced to provide all
functions that may be run on the client (such as math, cryptography, etc.).
`BackendOnlyStdLib` is intentionally excluded, as it contains functions (such
as DB calls) that need to be run on the backend.

`Analysis` exposes one function callable by the JS world: `EvalWorker.OnMessage`.

`EvalWorker.OnMessage`:
- takes a serialized request for analysis, from `BlazorWorker.js`
- deserializes the request
- evaluates the result of the request
- serializes the result
- uses a `self.postMessage` to send the results back to BlazorWorker.

CLEANUP: `EvalWorker` currently communicates with the Editor using types
compatible with Dark's previous (OCaml) backend server, since the client is
currently expecting. Once the client has been updated to "work in the new
types," the flow will be a bit simpler.

## BlazorWorker.js

`BlazorWorker.js` is a web worker responsible for:
- loading the .NET-compiled artifacts (dotnet.wasm, dlls, etc.)
- listening for requests from the Editor for evaluations, and sending those
  requests to `EvalWorker.OnMessage`
- listening for finished evaluations, and passing those results back to the Editor

This file has been ported from the official `blazor.webassembly.js` file that
the `dotnet/aspnetcore` repo outputs as part of their `Web.JS` project. The
porting was required, since the original file does not load well in a web
worker, due to references of `window` and other constructs. Once official
support for web workers is present, we will no longer have to support/maintain
this ported file.