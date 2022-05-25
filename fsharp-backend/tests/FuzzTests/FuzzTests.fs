module FuzzTests.Tests

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

open Utils

/// FSTODO Tests we know to have some issues to work out
let knownBad config =
  [ OCamlInterop.tests // fails in many unique ways

    FQFnName.tests // fails on `User "gm32_v6"`

    HttpClient.Tests.knownBad // fails on `"&=&"` (that's just the simplest case. also fails on "&=揻䫇&=◱")

    // all of these fail on some odd characters maybe involving `"𥉉"`
    DvalRepr.EnduserReadable.tests
    DvalRepr.DeveloperRepr.tests
    DvalRepr.Hashing.tests
    Json.PrettyMachineJson.tests
    Json.LibJwtJson.tests
    Json.PrettyResponseJson.tests

    ExecutePureFunctions.tests ]
  |> List.map (fun fn -> fn config)

/// Tests we generally know to be consistent
let knownGood (config : FuzzTestConfig) =
  [ Passwords.tests // passes with 10,000; 8 tests/s
    OCamlInterop.Queryable.tests // passes with 1,000,000; 25 tests/s
    BytesToString.tests // passes with 10,000; 200 tests/s
    HttpClient.Tests.knownGood // passes with 1,000,000; 350 test/s
    OCamlInterop.Roundtrippable.tests // passes with 100,000; 520 tests/s
    Json.PrettyRequestJson.tests // passes with 10,000; 800 tests/s
    Json.OCamlCompatibleVsApiServer.tests
    NodaTime.tests ] // passes with 1,000,000; 140k tests/s
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
          |> List.tryFind (fun (l, r) -> l = "--fscheck-max-tests")
      with
    | Some (_l, maxTests) -> { MaxTests = int maxTests }
    | _ -> { MaxTests = 1000 }

  // this does async stuff within it, so do not run it from a task/async
  // context or it may hang
  let exitCode = runTestsWithCLIArgs [] args (testList "FuzzTests" (tests config))

  Prelude.NonBlockingConsole.wait () // flush stdout
  exitCode
