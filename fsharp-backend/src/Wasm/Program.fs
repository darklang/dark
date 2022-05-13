// This file only exists as `PackAsTool` in the .fsproj
// only seems to work with an Exe project

[<EntryPoint>]
let main argv =
#if DEBUG
  printfn "WASM-compiled Dark backend running in Debug mode"
#else
  printfn "WASM-compiled Dark backend running in Release mode"
#endif

  0
