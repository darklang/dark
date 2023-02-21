module FuzzTests.Tests

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

open Utils

let disabled config =
  // These used to be legitimate tests, but are currently useless
  // (ExecutePureFunctions was specifically testing OCaml-backend regressions)
  [ ExecutePureFunctions.tests ] |> List.map (fun fn -> fn config)

/// FSTODO Tests we know to have some issues to work out
let knownBad config =
  [ FQFnName.tests ] // fails on `User "gm32_v6"`]
  |> List.map (fun fn -> fn config)

/// Tests we generally know to be consistent
let knownGood (config : FuzzTestConfig) =
  [ Passwords.tests // passes with 10,000; 8 tests/s
    NodaTime.tests // passes with 1,000,000; 140k tests/s
    ExecutionRegression.tests ]
  |> List.map (fun fn -> fn config)

let tests config =
  [ testList "knownGood" (knownGood config); testList "knownBad" (knownBad config) ]


[<EntryPoint>]
let main args =
  LibService.Init.init "FuzzTests"

  let config : FuzzTestConfig =
    // CLEANUP figure out why --fscheck-max-tests doesn't work
    // as documented, and don't pass `config` down the fn tree
    match args
          |> List.ofArray
          |> List.pairwise
          |> List.tryFind (fun (l, _r) -> l = "--fscheck-max-tests")
      with
    | Some (_l, maxTests) -> { MaxTests = int maxTests }
    | _ -> { MaxTests = 100000 }

  // this does async stuff within it, so do not run it from a task/async
  // context or it may hang
  let exitCode = runTestsWithCLIArgs [] args (testList "FuzzTests" (tests config))

  Prelude.NonBlockingConsole.wait () // flush stdout
  exitCode
