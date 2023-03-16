/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module BackendOnlyStdLib.StdLib

// CLEANUP - some of these functions can be run on the client too after the switch to
// F#, esp LibCrypto and LibX509, plus at least some of LibHttpClient*

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

let fn = RT.FQFnName.stdlibFnName

let renames = []

let types : List<PT.BuiltInType> = [] |> List.concat

let fns : List<RT.BuiltInFn> =
  [ LibDB.fns
    LibDarkInternal.fns
    LibEvent.fns
    LibHttpClient.fns
    LibPassword.fns
    LibX509.fns ]
  |> List.concat
  |> RT.renameFunctions renames
