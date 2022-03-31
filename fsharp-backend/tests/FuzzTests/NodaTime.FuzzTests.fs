/// FuzzTests around NodaTime dependency
module FuzzTests.NodaTime

open Expecto
open FsCheck

open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

type Generator =
  static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
    Generators.NodaTime.LocalDateTime
  static member Instant() : Arbitrary<NodaTime.Instant> = Generators.NodaTime.Instant

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

let tests config =
  testList
    "NodaTime"
    [ testProperty config typeof<Generator> "roundtrips to/from isoString" roundtrip ]
