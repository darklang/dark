module Tests.Builtin

// Misc tests of Builtin (both LibCloud and LibExecution) that could not be
// tested via LibExecution.tests

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module Exe = LibExecution.Execution

open TestUtils.TestUtils

let oldFunctionsAreDeprecated =
  testTask "old functions are deprecated" {
    let mutable counts = Map.empty

    let fns = localBuiltIns.fns |> Map.values

    fns
    |> List.iter (fun fn ->
      let key = RT.FQFnName.builtinToString { fn.name with version = 0 }

      if fn.deprecated = RT.NotDeprecated then
        counts <-
          Map.update
            key
            (fun count -> count |> Option.defaultValue 0 |> (+) 1 |> Some)
            counts

      ())

    Map.iter
      (fun name count ->
        Expect.equal count 1 $"{name} has more than one undeprecated function")
      counts
  }

let tests = testList "builtin" [ oldFunctionsAreDeprecated ]
