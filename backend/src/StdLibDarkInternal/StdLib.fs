/// StdLib functions that can only be run on the backend
///
/// Aggregates functions in other modules
module StdLibDarkInternal.StdLib

module RT = LibExecution.RuntimeTypes

let fn = RT.FQFnName.stdlibFnName

let renames = []

let types : List<RT.BuiltInType> = [ Libs.DarkInternal.types ] |> List.concat

let fns : List<RT.BuiltInFn> =
  [ Libs.DarkInternal.fns ] |> List.concat |> RT.renameFunctions renames
