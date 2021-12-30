module LibService.Exception

// It's not possible to reraise exceptions within F# CEs. This hack adds a `Reraise`
// method to exceptions. It's in LibService because it uses reflection and would
// possibly break in Wasm.

// Sources:
// - https://github.com/fsharp/fslang-suggestions/issues/660
// - https://stackoverflow.com/questions/57383

open System
open System.Runtime.ExceptionServices

// https://github.com/fsharp/fslang-suggestions/issues/660#issuecomment-382070639
type Exception with

  member this.Reraise() =
    (ExceptionDispatchInfo.Capture this).Throw()
    Unchecked.defaultof<_>
