module FuzzTests.Tests

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

let stillBuggy = testList "still buggy" [ OCamlInterop.tests; FQFnName.tests ]

let knownGood =
  testList
    "known good"
    ([ OCamlInterop.Roundtrippable.tests
       OCamlInterop.Queryable.tests
       DvalRepr.EndUserReadable.tests
       DvalRepr.Hashing.tests
       DvalRepr.DeveloperRepr.tests
       HttpClient.tests
       Json.PrettyMachineJson.tests
       Json.LibJwtJson.tests
       Json.PrettyRequestJson.tests
       Json.PrettyResponseJson.tests
       Passwords.tests
       BytesToString.tests
       Date.tests
       ExecutePureFunctions.tests ])

let tests = testList "FuzzTests" [ knownGood; stillBuggy ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
