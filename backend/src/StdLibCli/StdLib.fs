module StdLibCli.StdLib

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes


let fn = FQFnName.stdlibFnName

let renames : List<FQFnName.StdlibFnName * FQFnName.StdlibFnName> =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let types : List<PT.BuiltInType> = [] |> List.concat

let fns : List<BuiltInFn> = [ Cli.fns ] |> List.concat |> renameFunctions renames
