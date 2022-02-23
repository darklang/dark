module FuzzTests.Tests

// triggering build - to be removed.

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils
open FuzzTests.Utils

let stillBuggy = testList "still buggy" [ OCamlInterop.tests; FQFnName.tests ]

let knownGood =
  testList
    "known good"
    ([ OCamlInterop.Roundtrippable.tests
       OCamlInterop.Queryable.tests
       DvalRepr.EndUserReadable.tests
       DvalRepr.Hashing.tests
       DvalRepr.tests
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

// FSTODO: add fuzz test that running analysis gets the same results for different exprs

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
