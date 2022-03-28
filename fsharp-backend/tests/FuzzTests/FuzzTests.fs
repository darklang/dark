module FuzzTests.Tests

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

/// Tests we know to have some issues to work out
/// FSTODO resolve these
let stillBuggy =
  [ //OCamlInterop.tests
    //FQFnName.tests
    //HttpClient.tests // failing eventually, on "&=&"
    //DvalRepr.EnduserReadable.tests // fails eventually on DObj (map [("𥉉", DNull); ("", DNull)])
    //DvalRepr.DeveloperRepr.tests // fails eventually on DObj (map [("𢡊", DNull); ("ﺢ", DNull)])
    //DvalRepr.Hashing.tests // fails eventually, on [DObj (map [("𥉉", DNull); ("", DNull)])]
    //Json.PrettyMachineJson.tests // fails eventually, on DObj (map [("𥳐", DNull); ("", DNull)])
    //Json.LibJwtJson.tests// fails eventually, on DObj (map [("𣏕", DNull); ("", DNull)])
    //Json.PrettyResponseJson.tests // fails eventually , on some odd characters I think
    //ExecutePureFunctions.tests // fails eventually, on some odd characters I think (module and fn names are odd)
  ]

/// Tests we generally know to be consistent
let knownGood =
  [ Passwords.tests // passes with 10,000; 8 tests/s
    OCamlInterop.Queryable.tests // passes with 1,000,000; 25 tests/s
    BytesToString.tests // passes with 10,000; 200 tests/s
    OCamlInterop.Roundtrippable.tests // passes with 100,000; 520 tests/s
    Json.PrettyRequestJson.tests // passes with 10,000; 800 tests/s
    NodaTime.tests // passes with 1,000,000; 140k tests/s
    ]

let tests =
  knownGood
  //knownGood @ stillBuggy
  //Playground.tests

[<EntryPoint>]
let main args =
  LibService.Init.init "FuzzTests"

  // this does async stuff within it, so do not run it from a task/async
  // context or it may hang
  let exitCode = runTestsWithCLIArgs [] args (testList "FuzzTests" tests)

  Prelude.NonBlockingConsole.wait () // flush stdout
  exitCode
