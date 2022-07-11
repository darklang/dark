module FuzzTests.Tests

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

open Utils

/// FSTODO Tests we know to have some issues to work out
let knownBad config =
  [ FQFnName.tests // fails on `User "gm32_v6"`
    ExecutePureFunctions.tests ]
  |> List.map (fun fn -> fn config)

/// Tests we generally know to be consistent
let knownGood (config : FuzzTestConfig) =
  [ Passwords.tests // passes with 10,000; 8 tests/s
    NodaTime.tests ] // passes with 1,000,000; 140k tests/s
  |> List.map (fun fn -> fn config)

let tests config =
  [ testList "knownGood" (knownGood config); testList "knownBad" (knownBad config) ]


[<EntryPoint>]
let main args =
  Prelude.init ()
  LibService.Init.init "FuzzTests"
  LibExecution.Init.init ()
  Json.Vanilla.allow<AllowedFuzzerErrors.AllowedFuzzerErrorFileStructure> "fuzztests"

  let config : FuzzTestConfig =
    // CLEANUP figure out why --fscheck-max-tests doesn't work
    // as documented, and don't pass `config` down the fn tree
    match args
          |> List.ofArray
          |> List.pairwise
          |> List.tryFind (fun (l, r) -> l = "--fscheck-max-tests")
      with
    | Some (_l, maxTests) -> { MaxTests = int maxTests }
    | _ -> { MaxTests = 1000 }

  // this does async stuff within it, so do not run it from a task/async
  // context or it may hang
  let exitCode = runTestsWithCLIArgs [] args (testList "FuzzTests" (tests config))

  Prelude.NonBlockingConsole.wait () // flush stdout
  exitCode
