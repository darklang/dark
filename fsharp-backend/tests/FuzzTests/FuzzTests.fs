module FuzzTests.Tests

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

/// Tests we know to have some issues to work out
/// FSTODO resolve these
let stillBuggy = testList "still buggy" [ OCamlInterop.tests; FQFnName.tests ]

/// Tests we generally know to be consistent
let knownGood =
  testList
    "known good"
    ([ OCamlInterop.Roundtrippable.tests
       OCamlInterop.Queryable.tests
       DvalRepr.EnduserReadable.tests
       DvalRepr.Hashing.tests
       DvalRepr.DeveloperRepr.tests
       HttpClient.tests
       Json.PrettyMachineJson.tests
       Json.LibJwtJson.tests
       Json.PrettyRequestJson.tests
       Json.PrettyResponseJson.tests
       Passwords.tests
       BytesToString.tests
       NodaTime.tests
       ExecutePureFunctions.tests ])

let tests = testList "FuzzTests" [ knownGood; stillBuggy ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
