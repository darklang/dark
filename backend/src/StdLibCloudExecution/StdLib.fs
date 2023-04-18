/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module StdLibCloudExecution.StdLib

// CLEANUP - some of these functions can be run on the client too after the switch to
// F#, esp HttpClient and X509, plus at least some of LibHttpClient*

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

let fn = RT.FQFnName.stdlibFnName

let renames = []

let types : List<PT.BuiltInType> = [] |> List.concat

let fns : List<RT.BuiltInFn> =
  [ Libs.DB.fns
    Libs.Event.fns
    Libs.HttpClient.fns // move to StdLibExecution
    Libs.Password.fns // move to StdLibExecution?
    Libs.X509.fns ] // move to StdLibExecution?
  |> List.concat
  |> RT.renameFunctions renames
