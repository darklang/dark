/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module StdLibDarkInternal.StdLib

module RT = LibExecution.RuntimeTypes

let fn = RT.FQFnName.stdlibFnName

let renames = []

let types : List<RT.BuiltInType> =
  [ Libs.Infra.types
    Libs.UserManagement.types
    Libs.Canvases.types
    Libs.Documentation.types ]
  |> List.concat

let fns : List<RT.BuiltInFn> =
  [ Libs.Infra.fns
    Libs.UserManagement.fns
    Libs.Canvases.fns
    Libs.Documentation.fns ]
  |> List.concat
  |> RT.renameFunctions renames
