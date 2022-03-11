module LibService.Exception

open System
open System.Runtime.ExceptionServices

type Exception with

  /// <summary>
  /// This hack adds a `Reraise` method to exceptions, since
  /// it's not normally possible to reraise exceptions within F# CEs.
  /// </summary>
  ///
  /// <remarks>
  /// This is in LibService because it uses reflection and would possibly break in Wasm.
  ///
  /// Sources:
  /// - https://github.com/fsharp/fslang-suggestions/issues/660#issuecomment-382070639
  /// - https://stackoverflow.com/questions/57383
  /// </remarks>
  member this.Reraise() =
    (ExceptionDispatchInfo.Capture this).Throw()
    Unchecked.defaultof<_>
