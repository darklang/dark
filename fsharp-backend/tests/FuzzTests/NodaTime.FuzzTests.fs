/// FuzzTests around NodaTime dependency
module FuzzTests.NodaTime

open Expecto
open FsCheck

open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

/// Checks whether a `NodaTime.Instant` can be serialized
/// and deserialized to/from an ISO String successfully,
/// maintaining the same value
let roundtrip (date : NodaTime.Instant) : bool =
  let date = date.truncate ()
  let roundTripped =
    date.toIsoString ()
    |> NodaTime.Instant.ofIsoString
    |> fun d -> d.toIsoString ()
    |> NodaTime.Instant.ofIsoString

  roundTripped = date

let tests =
  testList
    "NodaTime"
    [ testProperty
        typeof<Generators.NodaTime.All>
        "roundtrips to/from isoString"
        roundtrip ]
