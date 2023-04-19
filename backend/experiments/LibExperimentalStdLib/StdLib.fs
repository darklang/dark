module LibExperimentalStdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

let renames : List<FQFnName.StdlibFnName * FQFnName.StdlibFnName> =
  // old names, new names
  // eg: fn "Http" "respond" 0, fn "Http" "response" 0
  []

let types : List<BuiltInType> = [] |> List.concat

let fns : List<BuiltInFn> =
  [ LibExperiments.fns ]
  |> List.concat
  |> LibExecution.StdLib.renameFunctions renames
