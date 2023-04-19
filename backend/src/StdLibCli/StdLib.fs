module StdLibCli.StdLib

open Prelude
open LibExecution.RuntimeTypes


let renames : List<FQFnName.StdlibFnName * FQFnName.StdlibFnName> =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let types : List<BuiltInType> = [ Cli.types ] |> List.concat

let fns : List<BuiltInFn> =
  [ Cli.fns ] |> List.concat |> LibExecution.StdLib.renameFunctions renames
