/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module BackendOnlyStdLib.StdLib

// CLEANUP - some of these functions can be run on the client too after the switch to
// F#, esp LibCrypto and LibX509, plus at least some of LibHttpClient*

module RT = LibExecution.RuntimeTypes

let fn = RT.FQFnName.stdlibFnName

let renames =
  [ fn "DB" "query" 3, fn "DB" "queryExactFields" 0
    fn "DB" "query" 2, fn "DB" "query" 3 // don't know why these are the same
    fn "DB" "queryWithKey" 2, fn "DB" "queryExactFieldsWithKey" 0
    fn "DB" "get" 1, fn "DB" "get" 2 // don't know why these are the same
    fn "DB" "queryOne" 2, fn "DB" "queryOneWithExactFields" 0
    fn "DB" "queryOneWithKey" 2, fn "DB" "queryOneWithExactFieldsWithKey" 0 ]


let fns : List<RT.BuiltInFn> =
  [ LibDB.fns
    LibCrypto.fns
    LibDarkInternal.fns
    LibEvent.fns
    LibHttpBaseClient.fns
    LibPassword.fns
    LibX509.fns ]
  |> List.concat
  |> RT.renameFunctions renames
